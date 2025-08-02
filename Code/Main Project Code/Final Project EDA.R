# -------------------- SETUP --------------------
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



########### TSLA EDA#############################
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




# ------------------------EDA for Sentiment Analysis ----------------------------------------


# -------------------- Time Series Plot of Sentiment --------------------

# Histogram
ggplot(news_sentiment, aes(x = avg_sentiment)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue", color = "white", alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "red") +
  labs(title = "Distribution of Average Sentiment Scores", x = "Sentiment Score", y = "Frequency") +
  theme_minimal()


# Ensure data is sorted by date
news_sentiment <- news_sentiment %>%
  arrange(date)

# Top plot: Daily sentiment scores (no smoothing)
p1 <- ggplot(news_sentiment, aes(x = date, y = avg_sentiment)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  labs(
    title = "Daily Average Sentiment Score",
    y = "Sentiment Score",
    x = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# Bottom plot: Headline count bars
p2 <- ggplot(news_sentiment, aes(x = date, y = headline_count)) +
  geom_col(fill = "gray30") +
  labs(
    title = "Headline Count per Day",
    y = "Count",
    x = "Date"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# Combine both plots using patchwork
p1 / p2 + plot_layout(heights = c(2, 1))




# 5-day rolling average
news_sentiment <- news_sentiment %>%
  arrange(date) %>%
  mutate(sentiment_roll5 = rollmean(avg_sentiment, 5, fill = NA))

# Plot
ggplot(news_sentiment, aes(x = date)) +
  # Raw sentiment (light gray)
  geom_line(aes(y = avg_sentiment), color = "lightblue", alpha = 0.5) +
  
  # Rolling average (main trend)
  geom_line(aes(y = sentiment_roll5), color = "darkred", size = 1) +
  
  # Highlight extreme positive sentiment
  geom_point(data = filter(news_sentiment, avg_sentiment > 0.5), 
             aes(y = avg_sentiment), color = "darkgreen", size = 2, shape = 16) +
  
  # Highlight extreme negative sentiment
  geom_point(data = filter(news_sentiment, avg_sentiment < -0.5), 
             aes(y = avg_sentiment), color = "firebrick", size = 2, shape = 16) +
  
  labs(
    title = "Smoothed Sentiment with Extreme Events Highlighted",
    subtitle = "5-day Rolling Average with Positive (>0.5) & Negative (<-0.5) Sentiment Days",
    x = "Date",
    y = "Sentiment Score"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
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
names(df_tsla) <- toupper(names(df_tsla))

unique(df_tsla$TICKER)

df_tsla$PRC <- abs(df_tsla$PRC)

df_tsla <- df_tsla %>%
  mutate(
    TradingVolume = VOL,
    Turnover = PRC * VOL,
    AbsSpread = ASK - BID,
    LogAbsSpread = ifelse(AbsSpread > 0, log(AbsSpread), NA),
    MidPrice = (ASK + BID) / 2,
    RelSpreadMid = ifelse(MidPrice > 0, AbsSpread / MidPrice, NA),
    RelSpreadLast = ifelse(PRC > 0, AbsSpread / PRC, NA),
    RelLogSpread = ifelse(ASK > 0 & BID > 0, log(ASK) - log(BID), NA),
    LogRelLogSpread = ifelse(!is.na(RelLogSpread) & RelLogSpread > 0, log(RelLogSpread), NA),
    LR1 = ifelse((PRC * VOL) > 0, NUMTRD / (PRC * VOL), NA),
    FlowRatio = LR1,
    date = as.Date(DATE),
    ret = c(NA, diff(log(PRC))),  # daily log return
    amihud_illiq = ifelse(VOL > 0, abs(ret) / VOL, NA)  # Amihud Illiquidity
  )




#-----------------------------EDA--------------------------------------------------------------------------

vis_miss(df_tsla,warn_large_data=FALSE)+
  scale_fill_manual(values=c("Navyblue","Orange3"),
                    labels=c("Present","Missing"))+
  labs(fill="Legend")+ggtitle("Missing Values Heatmap")




# ---------------------- Volume and Price
# Convert to xts
df_xts <- df_tsla %>%
  select(date, OPENPRC, BIDLO, ASKHI, PRC, VOL) %>%
  rename(
    Open = OPENPRC,
    Low = BIDLO,
    High = ASKHI,
    Close = PRC,
    Volume = VOL
  )

df_xts <- xts(df_xts[,-1], order.by = as.Date(df_xts$date))

chartSeries(df_xts, 
            name = "TSLA Price Chart with Volume", 
            theme = chartTheme("white"),
            type = "candlesticks", 
            TA = "addVo()", 
            up.col = "darkgreen", 
            dn.col = "firebrick")


# --------- 
df_xts <- df_tsla %>%
  select(date, PRC, RET) %>%
  rename(Close = PRC, Volume = RET)

df_xts <- xts(df_xts[,-1], order.by = as.Date(df_tsla$date))

chartSeries(df_xts,
            name = "TSLA Price + Daily Returns",
            type = "line",
            theme = chartTheme("white"),
            TA = "addVo()",
            up.col = "darkgreen", 
            dn.col = "firebrick")


View(df_tsla)







# -------------------- STEP 3: MERGE --------------------
merged_df <- df_tsla %>%
  left_join(news_sentiment, by = "date") %>%
  mutate(
    avg_sentiment = ifelse(is.na(avg_sentiment), 0, avg_sentiment),
    headline_count = ifelse(is.na(headline_count), 0, headline_count)
  )

str(merged_df)


#-----------------EDA--------------------------------------------------


# Prepare data (Price = PRC, Sentiment = avg_sentiment)
df_plot <- merged_df %>%
  select(date, PRC, avg_sentiment) %>%
  rename(Close = PRC, Sentiment = avg_sentiment)

# Convert to xts format
df_xts <- xts(df_plot[, c("Close", "Sentiment")], order.by = as.Date(df_plot$date))

# Plot TSLA Price with Sentiment as lower panel
chartSeries(df_xts[, "Close"],
            name = "TSLA Price with Sentiment Scores",
            type = "line",
            theme = chartTheme("white"),
            TA = NULL)

# Add sentiment as custom panel
addTA(df_xts[, "Sentiment"], type = "l", col = "darkred", legend = "Avg Sentiment")


str(merged_df)
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
    amihud_illiq  # ✅ New feature added
  )



str(input_data)


#---------------------------EDA-----------------------------------------------
summary(select(input_data, -date))  


df_long <- input_data %>%
  pivot_longer(cols = -date, names_to = "Feature", values_to = "Value")


ggplot(df_long, aes(x = date, y = Value)) +
  geom_line(color = "gray40") +
  facet_wrap(~ Feature, scales = "free_y", ncol = 3) +
  labs(title = "Time Series of Engineered Features", x = "Date", y = "Value") +
  theme_minimal()



###############################################
########### JPM EDA#############################
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



# ------------------------EDA for Sentiment Analysis ----------------------------------------


# -------------------- Time Series Plot of Sentiment --------------------

# Histogram
ggplot(news_sentiment, aes(x = avg_sentiment)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue", color = "white", alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "red") +
  labs(title = "Distribution of Average Sentiment Scores", x = "Sentiment Score", y = "Frequency") +
  theme_minimal()


# Ensure data is sorted by date
news_sentiment <- news_sentiment %>%
  arrange(date)

# Top plot: Daily sentiment scores (no smoothing)
p1 <- ggplot(news_sentiment, aes(x = date, y = avg_sentiment)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  labs(
    title = "Daily Average Sentiment Score",
    y = "Sentiment Score",
    x = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# Bottom plot: Headline count bars
p2 <- ggplot(news_sentiment, aes(x = date, y = headline_count)) +
  geom_col(fill = "gray30") +
  labs(
    title = "Headline Count per Day",
    y = "Count",
    x = "Date"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# Combine both plots using patchwork
p1 / p2 + plot_layout(heights = c(2, 1))


# Sentiment Spikes will rolling average overlay


# 5-day rolling average
news_sentiment <- news_sentiment %>%
  arrange(date) %>%
  mutate(sentiment_roll5 = rollmean(avg_sentiment, 5, fill = NA))

# Plot
ggplot(news_sentiment, aes(x = date)) +
  # Raw sentiment (light gray)
  geom_line(aes(y = avg_sentiment), color = "lightblue", alpha = 0.5) +
  
  # Rolling average (main trend)
  geom_line(aes(y = sentiment_roll5), color = "darkred", size = 1) +
  
  # Highlight extreme positive sentiment
  geom_point(data = filter(news_sentiment, avg_sentiment > 0.5), 
             aes(y = avg_sentiment), color = "darkgreen", size = 2, shape = 16) +
  
  # Highlight extreme negative sentiment
  geom_point(data = filter(news_sentiment, avg_sentiment < -0.5), 
             aes(y = avg_sentiment), color = "firebrick", size = 2, shape = 16) +
  
  labs(
    title = "Smoothed Sentiment with Extreme Events Highlighted",
    subtitle = "5-day Rolling Average with Positive (>0.5) & Negative (<-0.5) Sentiment Days",
    x = "Date",
    y = "Sentiment Score"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )







# -------------------- STEP 2: LIQUIDITY MEASURES --------------------
df <- read_excel("Ticker data.xlsx", 
                 col_types = c("date", "text", "text", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric"))

df_JPM <- df %>%
  filter(TICKER == "JPM")
names(df_JPM) <- toupper(names(df_JPM))

unique(df_JPM$TICKER)
str(df_JPM)

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





#-----------------------------EDA--------------------------------------------------------------------------

vis_miss(df_JPM,warn_large_data=FALSE)+
  scale_fill_manual(values=c("Navyblue","Orange3"),
                    labels=c("Present","Missing"))+
  labs(fill="Legend")+ggtitle("Missing Values Heatmap")




# ---------------------- Volume and Price
# Convert to xts
df_xts <- df_JPM %>%
  select(date, OPENPRC, BIDLO, ASKHI, PRC, VOL) %>%
  rename(
    Open = OPENPRC,
    Low = BIDLO,
    High = ASKHI,
    Close = PRC,
    Volume = VOL
  )

df_xts <- xts(df_xts[,-1], order.by = as.Date(df_xts$date))

chartSeries(df_xts, 
            name = "JPM Price Chart with Volume", 
            theme = chartTheme("white"),
            type = "candlesticks", 
            TA = "addVo()", 
            up.col = "darkgreen", 
            dn.col = "firebrick")


# --------- 
df_xts <- df_JPM %>%
  select(date, PRC, RET) %>%
  rename(Close = PRC, Volume = RET)

df_xts <- xts(df_xts[,-1], order.by = as.Date(df_JPM$date))

chartSeries(df_xts,
            name = "JPM Price + Daily Returns",
            type = "line",
            theme = chartTheme("white"),
            TA = "addVo()",
            up.col = "darkgreen", 
            dn.col = "firebrick")









# -------------------- STEP 3: MERGE --------------------
merged_df <- df_JPM %>%
  left_join(news_sentiment, by = "date") %>%
  mutate(
    avg_sentiment = ifelse(is.na(avg_sentiment), 0, avg_sentiment),
    headline_count = ifelse(is.na(headline_count), 0, headline_count)
  )

View(merged_df)


#-----------------EDA--------------------------------------------------


# Prepare data (Price = PRC, Sentiment = avg_sentiment)
df_plot <- merged_df %>%
  select(date, PRC, avg_sentiment) %>%
  rename(Close = PRC, Sentiment = avg_sentiment)

# Convert to xts format
df_xts <- xts(df_plot[, c("Close", "Sentiment")], order.by = as.Date(df_plot$date))

# Plot TSLA Price with Sentiment as lower panel
chartSeries(df_xts[, "Close"],
            name = "JPM Price with Sentiment Scores",
            type = "line",
            theme = chartTheme("white"),
            TA = NULL)

# Add sentiment as custom panel
addTA(df_xts[, "Sentiment"], type = "l", col = "darkred", legend = "Avg Sentiment")


str(merged_df)
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
    amihud_illiq  # ✅ New feature added
  )

str(input_data)


#---------------------------EDA-----------------------------------------------
summary(select(input_data, -date))  


df_long <- input_data %>%
  pivot_longer(cols = -date, names_to = "Feature", values_to = "Value")



ggplot(df_long, aes(x = date, y = Value)) +
  geom_line(color = "gray40") +
  facet_wrap(~ Feature, scales = "free_y", ncol = 3) +
  labs(title = "Time Series of Engineered Features", x = "Date", y = "Value") +
  theme_minimal()


####################################################################
########### NVDA EDA#############################
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




# ------------------------EDA for Sentiment Analysis ----------------------------------------


# -------------------- Time Series Plot of Sentiment --------------------

# Histogram
ggplot(news_sentiment, aes(x = avg_sentiment)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue", color = "white", alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "red") +
  labs(title = "Distribution of Average Sentiment Scores", x = "Sentiment Score", y = "Frequency") +
  theme_minimal()


# Ensure data is sorted by date
news_sentiment <- news_sentiment %>%
  arrange(date)

# Top plot: Daily sentiment scores (no smoothing)
p1 <- ggplot(news_sentiment, aes(x = date, y = avg_sentiment)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  labs(
    title = "Daily Average Sentiment Score",
    y = "Sentiment Score",
    x = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# Bottom plot: Headline count bars
p2 <- ggplot(news_sentiment, aes(x = date, y = headline_count)) +
  geom_col(fill = "gray30") +
  labs(
    title = "Headline Count per Day",
    y = "Count",
    x = "Date"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# Combine both plots using patchwork
p1 / p2 + plot_layout(heights = c(2, 1))


# Sentiment Spikes will rolling average overlay


# 5-day rolling average
news_sentiment <- news_sentiment %>%
  arrange(date) %>%
  mutate(sentiment_roll5 = rollmean(avg_sentiment, 5, fill = NA))

# Plot
ggplot(news_sentiment, aes(x = date)) +
  # Raw sentiment (light gray)
  geom_line(aes(y = avg_sentiment), color = "lightblue", alpha = 0.5) +
  
  # Rolling average (main trend)
  geom_line(aes(y = sentiment_roll5), color = "darkred", size = 1) +
  
  # Highlight extreme positive sentiment
  geom_point(data = filter(news_sentiment, avg_sentiment > 0.5), 
             aes(y = avg_sentiment), color = "darkgreen", size = 2, shape = 16) +
  
  # Highlight extreme negative sentiment
  geom_point(data = filter(news_sentiment, avg_sentiment < -0.5), 
             aes(y = avg_sentiment), color = "firebrick", size = 2, shape = 16) +
  
  labs(
    title = "Smoothed Sentiment with Extreme Events Highlighted",
    subtitle = "5-day Rolling Average with Positive (>0.5) & Negative (<-0.5) Sentiment Days",
    x = "Date",
    y = "Sentiment Score"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )







# -------------------- STEP 2: LIQUIDITY MEASURES --------------------
df <- read_excel("Ticker data.xlsx")
View(df)

df_nvda <- df %>%
  filter(TICKER == "NVDA")
names(df_nvda) <- toupper(names(df_nvda))

unique(df_nvda$TICKER)

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
    date = as.Date(DATE),
    ret = c(NA, diff(log(PRC))),
    #amihud_illiq = ifelse(VOL > 0, abs(ret) / VOL, NA)  Amihud Illiquidity
  )




#-----------------------------EDA--------------------------------------------------------------------------

vis_miss(df_nvda,warn_large_data=FALSE)+
  scale_fill_manual(values=c("Navyblue","Orange3"),
                    labels=c("Present","Missing"))+
  labs(fill="Legend")+ggtitle("Missing Values Heatmap")




# ---------------------- Volume and Price
# Convert to xts
df_xts <- df_nvda %>%
  select(date, OPENPRC, BIDLO, ASKHI, PRC, VOL) %>%
  rename(
    Open = OPENPRC,
    Low = BIDLO,
    High = ASKHI,
    Close = PRC,
    Volume = VOL
  )

df_xts <- xts(df_xts[,-1], order.by = as.Date(df_xts$date))

chartSeries(df_xts, 
            name = "NVDA Price Chart with Volume", 
            theme = chartTheme("white"),
            type = "candlesticks", 
            TA = "addVo()", 
            up.col = "darkgreen", 
            dn.col = "firebrick")


# --------- 
df_xts <- df_nvda %>%
  select(date, PRC, RET) %>%
  rename(Close = PRC, Volume = RET)

df_xts <- xts(df_xts[,-1], order.by = as.Date(df_tsla$date))

chartSeries(df_xts,
            name = "NVDA Price + Daily Returns",
            type = "line",
            theme = chartTheme("white"),
            TA = "addVo()",
            up.col = "darkgreen", 
            dn.col = "firebrick")









# -------------------- STEP 3: MERGE --------------------
merged_df <- df_nvda %>%
  left_join(news_sentiment, by = "date") %>%
  mutate(
    avg_sentiment = ifelse(is.na(avg_sentiment), 0, avg_sentiment),
    headline_count = ifelse(is.na(headline_count), 0, headline_count)
  )


#-----------------EDA--------------------------------------------------


# Prepare data (Price = PRC, Sentiment = avg_sentiment)
df_plot <- merged_df %>%
  select(date, PRC, avg_sentiment) %>%
  rename(Close = PRC, Sentiment = avg_sentiment)

# Convert to xts format
df_xts <- xts(df_plot[, c("Close", "Sentiment")], order.by = as.Date(df_plot$date))

# Plot TSLA Price with Sentiment as lower panel
chartSeries(df_xts[, "Close"],
            name = "NVDA Price with Sentiment Scores",
            type = "line",
            theme = chartTheme("white"),
            TA = NULL)

# Add sentiment as custom panel
addTA(df_xts[, "Sentiment"], type = "l", col = "darkred", legend = "Avg Sentiment")


str(merged_df)
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
  )

str(input_data)

library(tibble)
library(corrplot)

numeric_data <- input_data[sapply(input_data, is.numeric)]

cor_matrix <- cor(na.omit(numeric_data))

corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", number.cex = 0.7, 
         col = colorRampPalette(c("blue", "white", "red"))(200))

#---------------------------EDA-----------------------------------------------
summary(select(input_data, -date))  


df_long <- input_data %>%
  pivot_longer(cols = -date, names_to = "Feature", values_to = "Value")



ggplot(df_long, aes(x = date, y = Value)) +
  geom_line(color = "gray40") +
  facet_wrap(~ Feature, scales = "free_y", ncol = 3) +
  labs(title = "Time Series of Engineered Features", x = "Date", y = "Value") +
  theme_minimal()
