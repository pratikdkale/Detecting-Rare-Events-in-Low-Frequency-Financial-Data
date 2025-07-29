library(sentimentr)
library(readr)
library(dplyr)
library(readxl)
library(writexl)
library(isotree)
library(DMwR2)
library(ggplot2)
library(zoo)
library(patchwork)
library(tidyr)
library(naniar)
library(xts)
library(quantmod)
library(corrplot)



###################### NVIDIA ##########################################
##Disclaimer: In order for the code to work properly for any company, clear global environment.


# -------------------- STEP 1: SENTIMENT SCORING --------------------
news <- read_csv("NVDA_News.csv")
news$first_created <- news$news_day 

news <- news %>%
  mutate(date = as.Date(first_created, format = "%m/%d/%Y %H:%M"))

scored <- sentiment_by(news$headline)

news_sentiment <- news %>%
  select(date) %>%
  bind_cols(scored %>% select(ave_sentiment)) %>%
  group_by(date) %>%
  summarise(
    avg_sentiment = mean(ave_sentiment, na.rm = TRUE),
    headline_count = n(),
    .groups = "drop"
  )



# -------------------- STEP 2: LIQUIDITY MEASURES --------------------
df <- read_excel("Ticker data.xlsx", 
                          col_types = c("date", "text", "text", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric"))


df_nvda <- df %>% filter(TICKER == "NVDA")

df_nvda$PRC <- abs(df_nvda$PRC)

df_nvda <- df_nvda %>%
  mutate(
    ret = c(NA, diff(log(PRC))),
    TradingVolume = VOL ,
    Turnover = PRC * VOL,
    AbsSpread = ASK - BID,
    LogAbsSpread = ifelse(AbsSpread > 0, log(AbsSpread), NA),
    MidPrice = (ASK + BID) / 2,
    RelSpreadMid = ifelse(MidPrice > 0, AbsSpread / MidPrice, NA),
    RelSpreadLast = ifelse(PRC > 0, AbsSpread / PRC, NA),
    RelLogSpread = ifelse(ASK > 0 & BID > 0, log(ASK) - log(BID), NA),
    LogRelLogSpread = ifelse(!is.na(RelLogSpread) & RelLogSpread > 0, log(RelLogSpread), NA),
    LR1 = ifelse(abs(ret) > 0, (PRC * VOL) / abs(ret), NA),
    FlowRatio =  NUMTRD * Turnover,
    date = as.Date(date),
    ret = c(NA, diff(log(PRC))),
    #amihud_illiq = ifelse(VOL > 0, abs(ret) / VOL, NA)  Amihud Illiquidity
  )



# -------------------- STEP 3: MERGE --------------------
merged_df <- df_nvda %>%
  left_join(news_sentiment, by = "date") %>%
  mutate(
    avg_sentiment = ifelse(is.na(avg_sentiment), 0, avg_sentiment),
    headline_count = ifelse(is.na(headline_count), 0, headline_count)
  )



# -------------------- STEP 4: SELECT ENGINEERED + SENTIMENT FEATURES --------------------
input_data <- merged_df %>%
  select(
    date,
    PRC,
    AbsSpread,
    LogAbsSpread,
    RelSpreadMid,
    RelSpreadLast,
    RelLogSpread,
    LogRelLogSpread,
    Turnover,
    TradingVolume,
    NUMTRD,
    LR1,
    FlowRatio,
    avg_sentiment,
    headline_count,
    #amihud_illiq  
  )

# -------------------- STEP 5: STANDARDIZE FEATURES --------------------
data_scaled <- scale(input_data %>% select(-date, -PRC))
input_data$anomaly_score <- NA
input_data$rare_event_iforest <- FALSE
input_data$lof_score <- NA
input_data$rare_event_lof <- FALSE

# -------------------- STEP 6: ISOLATION FOREST --------------------
valid_rows <- complete.cases(data_scaled)
clean_data <- as.data.frame(data_scaled[valid_rows, ])

iso_model <- isolation.forest(clean_data, ntrees = 100)
iso_scores <- predict(iso_model, clean_data, type = "score")

input_data$anomaly_score[valid_rows] <- iso_scores

# Flag top 1% of valid rows
iso_threshold <- quantile(iso_scores, 0.99, na.rm = TRUE)
input_data$rare_event_iforest[valid_rows] <- iso_scores >= iso_threshold


#----------------------Visualization----------------

merged_df$anomaly_score <- input_data$anomaly_score
merged_df$rare_event_iforest <- input_data$rare_event_iforest



# Step 1: Plot time series of price
ggplot(merged_df, aes(x = date, y = PRC)) +
  geom_line(color = "Steelblue", linewidth = 0.8) +
  
  # Step 2: Mark Isolation Forest outliers as red dots
  geom_point(
    data = filter(merged_df, rare_event_iforest == TRUE),
    aes(x = date, y = PRC),
    color = "Red", size = 2, shape = 16
  ) +
  
  labs(
    title = "NVDA Price with Isolation Forest Outliers Highlighted",
    x = "Date",
    y = "Price (PRC)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )



# -------------------- STEP 7: LOCAL OUTLIER FACTOR --------------------
lof_scores <- lofactor(clean_data, k = 10)
input_data$lof_score[valid_rows] <- lof_scores

lof_threshold <- quantile(lof_scores, 0.99, na.rm = TRUE)

input_data$rare_event_lof[valid_rows] <- lof_scores >= lof_threshold


#----------------------Visualization----------------

merged_df$rare_event_lof <- input_data$rare_event_lof
merged_df$lof_score <- input_data$lof_score
ggplot(merged_df, aes(x = date, y = PRC)) +
  geom_line(color = "Steelblue", linewidth = 0.8) +
  geom_point(
    data = filter(merged_df, rare_event_lof == TRUE),
    aes(x = date, y = PRC),
    color = "Firebrick", size = 2.5, shape = 21, fill = "white", stroke = 1.2
  ) +
  labs(
    title = "NVDA Price with LOF Outliers Highlighted",
    x = "Date",
    y = "Price (PRC)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))



# -------------------- STEP 8: HYPERPLANE METHOD --------------------
input_data$z_sum <- NA
clean_data_aligned <- clean_data

# Features where high value = low liquidity → reverse z-score direction
features_to_reverse <- c(
  "AbsSpread", "LogAbsSpread", "RelSpreadMid", "RelSpreadLast",
  "RelLogSpread", "LogRelLogSpread"
)

clean_data_aligned[, features_to_reverse] <- -clean_data_aligned[, features_to_reverse]

# Compute z_sum after alignment
input_data$z_sum[valid_rows] <- rowSums(clean_data_aligned)

input_data$liquidity_class <- case_when(
  (input_data$rare_event_iforest | input_data$rare_event_lof) & input_data$z_sum > 0 ~ "High Liquidity",
  (input_data$rare_event_iforest | input_data$rare_event_lof) & input_data$z_sum < 0 ~ "Low Liquidity",
  TRUE ~ NA_character_
)

#--------------------------visuals----------------------------------------
merged_df$liquidity_class <- input_data$liquidity_class

ggplot(merged_df, aes(x = date, y = PRC)) +
  geom_line(color = "Steelblue", linewidth = 0.8) +
  
  # High Liquidity → green points
  geom_point(data = filter(merged_df, liquidity_class == "High Liquidity"),
             aes(x = date, y = PRC),
             color = "Darkgreen", size = 2, shape = 21, fill = "white", stroke = 1.2) +
  
  # Low Liquidity → red points
  geom_point(data = filter(merged_df, liquidity_class == "Low Liquidity"),
             aes(x = date, y = PRC),
             color = "Firebrick", size = 2, shape = 21, fill = "white", stroke = 1.2) +
  
  labs(
    title = "NVDA Price with Classified Rare Events (Hyperplane)",
    subtitle = "Red = Low Liquidity  |  Green = High Liquidity",
    x = "Date",
    y = "Price (PRC)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )



# -------------------- STEP 9: RARE EVENTS --------------------
df1 <- input_data %>%
  filter(rare_event_iforest | rare_event_lof) %>%
  arrange(desc(anomaly_score))


df1 <- df1 %>%
  mutate(liquidity_class = ifelse(is.na(liquidity_class), "Normal", liquidity_class))

cols_to_plot <- c(
  "PRC", "AbsSpread", "RelSpreadMid",
   "Turnover", "TradingVolume", "NUMTRD",
  "LR1", "FlowRatio", "avg_sentiment"
)

df_long <- df1 %>%
  select(all_of(cols_to_plot), liquidity_class) %>%
  pivot_longer(cols = -liquidity_class, names_to = "Feature", values_to = "Value")

# Boxplot
ggplot(df_long, aes(x = liquidity_class, y = Value, fill = liquidity_class)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  facet_wrap(~ Feature, scales = "free_y", ncol = 4) +
  labs(
    title = "Boxplots of Liquidity Features by Liquidity Class - NVDA",
    x = "Liquidity Class",
    y = "Value"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none"
  )


###################### TESLA ##########################################
##Disclaimer: In order for the code to work properly for any company, clear global environment.


# -------------------- STEP 1: SENTIMENT SCORING --------------------
news <- read_csv("TSLA_News.csv")

news <- news %>%
  mutate(date = as.Date(first_created, format = "%m/%d/%Y %H:%M"))

scored <- sentiment_by(news$headline)

news_sentiment <- news %>%
  select(date) %>%
  bind_cols(scored %>% select(ave_sentiment)) %>%
  group_by(date) %>%
  summarise(
    avg_sentiment = mean(ave_sentiment, na.rm = TRUE),
    headline_count = n(),
    .groups = "drop"
  )




# -------------------- STEP 2: LIQUIDITY MEASURES --------------------
df <- read_excel("Ticker data.xlsx", 
                 col_types = c("date", "text", "text", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric"))



df_tsla <- df %>%
  filter(TICKER == "TSLA")

df_tsla$PRC <- abs(df_tsla$PRC)

df_tsla <- df_tsla %>%
  mutate(
    ret = c(NA, diff(log(PRC))),
    TradingVolume = VOL ,
    Turnover = PRC * VOL,
    AbsSpread = ASK - BID,
    LogAbsSpread = ifelse(AbsSpread > 0, log(AbsSpread), NA),
    MidPrice = (ASK + BID) / 2,
    RelSpreadMid = ifelse(MidPrice > 0, AbsSpread / MidPrice, NA),
    RelSpreadLast = ifelse(PRC > 0, AbsSpread / PRC, NA),
    RelLogSpread = ifelse(ASK > 0 & BID > 0, log(ASK) - log(BID), NA),
    LogRelLogSpread = ifelse(!is.na(RelLogSpread) & RelLogSpread > 0, log(RelLogSpread), NA),
    LR1 = ifelse(abs(ret) > 0, (PRC * VOL) / abs(ret), NA),
    FlowRatio =  NUMTRD * Turnover,
    date = as.Date(date),
    ret = c(NA, diff(log(PRC))),
    #amihud_illiq = ifelse(VOL > 0, abs(ret) / VOL, NA)  Amihud Illiquidity
  )





# -------------------- STEP 3: MERGE --------------------
merged_df <- df_tsla %>%
  left_join(news_sentiment, by = "date") %>%
  mutate(
    avg_sentiment = ifelse(is.na(avg_sentiment), 0, avg_sentiment),
    headline_count = ifelse(is.na(headline_count), 0, headline_count)
  )




# -------------------- STEP 4: SELECT ENGINEERED + SENTIMENT FEATURES --------------------
input_data <- merged_df %>%
  select(
    date,
    PRC,
    AbsSpread,
    LogAbsSpread,
    RelSpreadMid,
    RelSpreadLast,
    RelLogSpread,
    LogRelLogSpread,
    Turnover,
    TradingVolume,
    NUMTRD,
    LR1,
    FlowRatio,
    avg_sentiment,
    headline_count,
    #amihud_illiq  
  )



# -------------------- STEP 5: STANDARDIZE FEATURES --------------------
data_scaled <- scale(input_data %>% select(-date, -PRC))
input_data$anomaly_score <- NA
input_data$rare_event_iforest <- FALSE
input_data$lof_score <- NA
input_data$rare_event_lof <- FALSE

# -------------------- STEP 6: ISOLATION FOREST (SAFE SCORING) --------------------
valid_rows <- complete.cases(data_scaled)
clean_data <- as.data.frame(data_scaled[valid_rows, ])

iso_model <- isolation.forest(clean_data, ntrees = 100)
iso_scores <- predict(iso_model, clean_data, type = "score")

input_data$anomaly_score[valid_rows] <- iso_scores

# Flag top 1% of valid rows
iso_threshold <- quantile(iso_scores, 0.99, na.rm = TRUE)
input_data$rare_event_iforest[valid_rows] <- iso_scores >= iso_threshold


#----------------------Visualization----------------

merged_df$anomaly_score <- input_data$anomaly_score
merged_df$rare_event_iforest <- input_data$rare_event_iforest



# Step 1: Plot time series of price
ggplot(merged_df, aes(x = date, y = PRC)) +
  geom_line(color = "Steelblue", linewidth = 0.8) +
  
  # Step 2: Mark Isolation Forest outliers as red dots
  geom_point(
    data = filter(merged_df, rare_event_iforest == TRUE),
    aes(x = date, y = PRC),
    color = "Red", size = 2, shape = 16
  ) +
  
  labs(
    title = "TSLA Price with Isolation Forest Outliers Highlighted",
    x = "Date",
    y = "Price (PRC)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )








# -------------------- STEP 7: LOCAL OUTLIER FACTOR --------------------
lof_scores <- lofactor(clean_data, k = 10)
input_data$lof_score[valid_rows] <- lof_scores

lof_threshold <- quantile(lof_scores, 0.99, na.rm = TRUE)

input_data$rare_event_lof[valid_rows] <- lof_scores >= lof_threshold


#----------------------Visualization----------------

merged_df$rare_event_lof <- input_data$rare_event_lof
merged_df$lof_score <- input_data$lof_score
ggplot(merged_df, aes(x = date, y = PRC)) +
  geom_line(color = "Steelblue", linewidth = 0.8) +
  geom_point(
    data = filter(merged_df, rare_event_lof == TRUE),
    aes(x = date, y = PRC),
    color = "Firebrick", size = 2.5, shape = 21, fill = "white", stroke = 1.2
  ) +
  labs(
    title = "TSLA Price with LOF Outliers Highlighted",
    x = "Date",
    y = "Price (PRC)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))



# -------------------- STEP 8: HYPERPLANE METHOD --------------------
input_data$z_sum <- NA
clean_data_aligned <- clean_data

# Features where high value = low liquidity → reverse z-score direction
features_to_reverse <- c(
  "AbsSpread", "LogAbsSpread", "RelSpreadMid", "RelSpreadLast",
  "RelLogSpread", "LogRelLogSpread"
)

clean_data_aligned[, features_to_reverse] <- -clean_data_aligned[, features_to_reverse]

input_data$z_sum[valid_rows] <- rowSums(clean_data_aligned)

input_data$liquidity_class <- case_when(
  (input_data$rare_event_iforest | input_data$rare_event_lof) & input_data$z_sum > 0 ~ "High Liquidity",
  (input_data$rare_event_iforest | input_data$rare_event_lof) & input_data$z_sum < 0 ~ "Low Liquidity",
  TRUE ~ NA_character_
)

#--------------------------visuals----------------------------------------
merged_df$liquidity_class <- input_data$liquidity_class

ggplot(merged_df, aes(x = date, y = PRC)) +
  geom_line(color = "Steelblue", linewidth = 0.8) +
  
  # High Liquidity → green points
  geom_point(data = filter(merged_df, liquidity_class == "High Liquidity"),
             aes(x = date, y = PRC),
             color = "Darkgreen", size = 2, shape = 21, fill = "white", stroke = 1.2) +
  
  # Low Liquidity → red points
  geom_point(data = filter(merged_df, liquidity_class == "Low Liquidity"),
             aes(x = date, y = PRC),
             color = "Firebrick", size = 2, shape = 21, fill = "white", stroke = 1.2) +
  
  labs(
    title = "TSLA Price with Classified Rare Events (Hyperplane)",
    subtitle = "Red = Low Liquidity  |  Green = High Liquidity",
    x = "Date",
    y = "Price (PRC)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )



# -------------------- STEP 9: RESULTS --------------------
df1 <- input_data %>%
  filter(rare_event_iforest | rare_event_lof) %>%
  arrange(desc(anomaly_score))

#Outlier Analysis

df1 <- df1 %>%
  mutate(liquidity_class = ifelse(is.na(liquidity_class), "Normal", liquidity_class))

cols_to_plot <- c(
  "PRC", "AbsSpread", "RelSpreadMid",
  "Turnover", "TradingVolume", "NUMTRD",
  "LR1", "FlowRatio", "avg_sentiment"
)

df_long <- df1 %>%
  select(all_of(cols_to_plot), liquidity_class) %>%
  pivot_longer(cols = -liquidity_class, names_to = "Feature", values_to = "Value")

# Boxplot
ggplot(df_long, aes(x = liquidity_class, y = Value, fill = liquidity_class)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  facet_wrap(~ Feature, scales = "free_y", ncol = 4) +
  labs(
    title = "Boxplots of Liquidity Features by Liquidity Class",
    x = "Liquidity Class",
    y = "Value"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none"
  )


########################## JP MORGAN ###########################################

# -------------------- STEP 1: SENTIMENT SCORING --------------------
news <- read_csv("JPM_News.csv")
news$first_created <- news$news_day

news <- news %>%
  mutate(date = as.Date(first_created, format = "%m/%d/%Y %H:%M"))

scored <- sentiment_by(news$headline)

news_sentiment <- news %>%
  select(date) %>%
  bind_cols(scored %>% select(ave_sentiment)) %>%
  group_by(date) %>%
  summarise(
    avg_sentiment = mean(ave_sentiment, na.rm = TRUE),
    headline_count = n(),
    .groups = "drop"
  )


# -------------------- STEP 2: LIQUIDITY MEASURES --------------------

df <- read_excel("Ticker data.xlsx", 
                 col_types = c("date", "text", "text", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric"))



df_JPM <- df %>% filter(TICKER == "JPM")

df_JPM$PRC <- abs(df_JPM$PRC)

df_JPM <- df_JPM %>%
  mutate(
    ret = c(NA, diff(log(PRC))),
    TradingVolume = VOL ,
    Turnover = PRC * VOL,
    AbsSpread = ASK - BID,
    LogAbsSpread = ifelse(AbsSpread > 0, log(AbsSpread), NA),
    MidPrice = (ASK + BID) / 2,
    RelSpreadMid = ifelse(MidPrice > 0, AbsSpread / MidPrice, NA),
    RelSpreadLast = ifelse(PRC > 0, AbsSpread / PRC, NA),
    RelLogSpread = ifelse(ASK > 0 & BID > 0, log(ASK) - log(BID), NA),
    LogRelLogSpread = ifelse(!is.na(RelLogSpread) & RelLogSpread > 0, log(RelLogSpread), NA),
    LR1 = ifelse(abs(ret) > 0, (PRC * VOL) / abs(ret), NA),
    #FlowRatio =  NUMTRD * Turnover,
    date = as.Date(date),
    ret = c(NA, diff(log(PRC))),
    #amihud_illiq = ifelse(VOL > 0, abs(ret) / VOL, NA)  Amihud Illiquidity
  )






# -------------------- STEP 3: MERGE --------------------
merged_df <- df_JPM %>%
  left_join(news_sentiment, by = "date") %>%
  mutate(
    avg_sentiment = ifelse(is.na(avg_sentiment), 0, avg_sentiment),
    headline_count = ifelse(is.na(headline_count), 0, headline_count)
  )




# -------------------- STEP 4: SELECT ENGINEERED + SENTIMENT FEATURES --------------------
input_data <- merged_df %>%
  select(
    date,
    PRC,
    AbsSpread,
    LogAbsSpread,
    RelSpreadMid,
    RelSpreadLast,
    RelLogSpread,
    LogRelLogSpread,
    Turnover,
    TradingVolume,
    LR1,
    avg_sentiment,
    headline_count,
    #amihud_illiq  
  )


# -------------------- STEP 5: STANDARDIZE FEATURES --------------------
data_scaled <- scale(input_data %>% select(-date, -PRC))
input_data$anomaly_score <- NA
input_data$rare_event_iforest <- FALSE
input_data$lof_score <- NA
input_data$rare_event_lof <- FALSE

# -------------------- STEP 6: ISOLATION FOREST --------------------
valid_rows <- complete.cases(data_scaled)
clean_data <- as.data.frame(data_scaled[valid_rows, ])

iso_model <- isolation.forest(clean_data, ntrees = 100)
iso_scores <- predict(iso_model, clean_data, type = "score")

input_data$anomaly_score[valid_rows] <- iso_scores

# Flag top 1% of valid rows
iso_threshold <- quantile(iso_scores, 0.99, na.rm = TRUE)
input_data$rare_event_iforest[valid_rows] <- iso_scores >= iso_threshold


#----------------------Visualization----------------

merged_df$anomaly_score <- input_data$anomaly_score
merged_df$rare_event_iforest <- input_data$rare_event_iforest



# Plot time series of price
ggplot(merged_df, aes(x = date, y = PRC)) +
  geom_line(color = "Steelblue", linewidth = 0.8) +
  
  # Mark Isolation Forest outliers as red dots
  geom_point(
    data = filter(merged_df, rare_event_iforest == TRUE),
    aes(x = date, y = PRC),
    color = "Red", size = 2, shape = 16
  ) +
  
  labs(
    title = "JPM Price with Isolation Forest Outliers Highlighted",
    x = "Date",
    y = "Price (PRC)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )








# -------------------- STEP 7: LOCAL OUTLIER FACTOR --------------------
lof_scores <- lofactor(clean_data, k = 10)
input_data$lof_score[valid_rows] <- lof_scores

lof_threshold <- quantile(lof_scores, 0.99, na.rm = TRUE)

input_data$rare_event_lof[valid_rows] <- lof_scores >= lof_threshold


#----------------------Visualization----------------

merged_df$rare_event_lof <- input_data$rare_event_lof
merged_df$lof_score <- input_data$lof_score
ggplot(merged_df, aes(x = date, y = PRC)) +
  geom_line(color = "Steelblue", linewidth = 0.8) +
  geom_point(
    data = filter(merged_df, rare_event_lof == TRUE),
    aes(x = date, y = PRC),
    color = "Firebrick", size = 2.5, shape = 21, fill = "white", stroke = 1.2
  ) +
  labs(
    title = "JPM Price with LOF Outliers Highlighted",
    x = "Date",
    y = "Price (PRC)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))



# -------------------- STEP 8: HYPERPLANE METHOD --------------------
input_data$z_sum <- NA
clean_data_aligned <- clean_data

# Features where high value = low liquidity → reverse z-score direction
features_to_reverse <- c(
  "AbsSpread", "LogAbsSpread", "RelSpreadMid", "RelSpreadLast",
  "RelLogSpread", "LogRelLogSpread"
)

clean_data_aligned[, features_to_reverse] <- -clean_data_aligned[, features_to_reverse]

# Compute z_sum after alignment
input_data$z_sum[valid_rows] <- rowSums(clean_data_aligned)

input_data$liquidity_class <- case_when(
  (input_data$rare_event_iforest | input_data$rare_event_lof) & input_data$z_sum > 0 ~ "High Liquidity",
  (input_data$rare_event_iforest | input_data$rare_event_lof) & input_data$z_sum < 0 ~ "Low Liquidity",
  TRUE ~ NA_character_
)

#--------------------------visuals----------------------------------------
merged_df$liquidity_class <- input_data$liquidity_class

ggplot(merged_df, aes(x = date, y = PRC)) +
  geom_line(color = "Steelblue", linewidth = 0.8) +
  
  geom_point(data = filter(merged_df, liquidity_class == "High Liquidity"),
             aes(x = date, y = PRC),
             color = "Darkgreen", size = 2, shape = 21, fill = "white", stroke = 1.2) +
    geom_point(data = filter(merged_df, liquidity_class == "Low Liquidity"),
             aes(x = date, y = PRC),
             color = "Firebrick", size = 2, shape = 21, fill = "white", stroke = 1.2) +
  
  labs(
    title = "JPM Price with Classified Rare Events",
    subtitle = "Red = Low Liquidity  |  Green = High Liquidity",
    x = "Date",
    y = "Price (PRC)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )



# -------------------- STEP 9: VIEW --------------------

df1 <- input_data %>%
  filter(rare_event_iforest | rare_event_lof) %>%
  arrange(desc(anomaly_score))


#Outlier Analysis


df1 <- df1 %>%
  mutate(liquidity_class = ifelse(is.na(liquidity_class), "Normal", liquidity_class))

cols_to_plot <- c(
  "PRC", "AbsSpread", "RelSpreadMid",
  "Turnover", "TradingVolume",
  "LR1", "avg_sentiment"
)

df_long <- df1 %>%
  select(all_of(cols_to_plot), liquidity_class) %>%
  pivot_longer(cols = -liquidity_class, names_to = "Feature", values_to = "Value")

# Boxplot
ggplot(df_long, aes(x = liquidity_class, y = Value, fill = liquidity_class)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  facet_wrap(~ Feature, scales = "free_y", ncol = 4) +
  labs(
    title = "Boxplots of Liquidity Features by Liquidity Class - JPM",
    x = "Liquidity Class",
    y = "Value"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none"
  )


