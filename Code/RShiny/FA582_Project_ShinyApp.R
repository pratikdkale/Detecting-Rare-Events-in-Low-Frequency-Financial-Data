# app.R

library(shiny)
library(ggplot2)
library(plotly)
library(readr)
library(dplyr)
library(sentimentr)
library(isotree)
library(DMwR2)

# --------------------
# Function to load stock & news sentiment data
# --------------------
load_data <- function(ticker) {
  stock_file <- paste0(ticker, "_Raw.csv")
  news_file  <- paste0(ticker, ".csv")
  
  stock_df <- read_csv(stock_file, show_col_types = FALSE)
  news_df  <- read_csv(news_file,  show_col_types = FALSE)
  
  names(stock_df) <- toupper(names(stock_df))
  names(news_df)  <- tolower(names(news_df))
  
  stock_df$DATE <- as.Date(stock_df$DATE, "%m/%d/%y")
  if (any(is.na(stock_df$DATE))) {
    stock_df$DATE <- as.Date(stock_df$DATE, "%m/%d/%Y")
  }
  news_df$news_day <- as.Date(news_df$news_day, "%m/%d/%y")
  if (any(is.na(news_df$news_day))) {
    news_df$news_day <- as.Date(news_df$news_day, "%Y-%m-%d")
  }
  
  news_grouped <- news_df %>%
    group_by(news_day) %>%
    summarise(ALL_HEADLINES = paste(headline, collapse = "<br>"), .groups = "drop") %>%
    rename(DATE = news_day)
  
  news_grouped$SENTIMENT <- sentiment_by(news_grouped$ALL_HEADLINES)$ave_sentiment
  
  merged_df <- left_join(stock_df, news_grouped, by = "DATE")
  merged_df$ALL_HEADLINES[is.na(merged_df$ALL_HEADLINES)] <- ""
  merged_df$SENTIMENT[is.na(merged_df$SENTIMENT)]       <- 0
  
  return(merged_df)
}

# --------------------
# UI
# --------------------
ui <- fluidPage(
  titlePanel("Stock & News Sentiment Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("ticker", "Choose Ticker:",
                  choices = c("TSLA", "JPM", "NVDA")),
      dateRangeInput("date_range", "Select Date Range:",
                     start = as.Date("2015-01-01"),
                     end   = as.Date("2024-12-31"))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Dashboard",
          div(style = "margin-bottom: 30px;",
              plotlyOutput("price_plot",     height = "300px")),
          div(style = "margin-bottom: 30px;",
              plotlyOutput("volume_plot",    height = "300px")),
          div(style = "margin-bottom: 30px;",
              plotlyOutput("sentiment_plot", height = "300px"))
        ),
        tabPanel(
          "Liquidity Events",
          plotlyOutput("liquidity_plot", height = "600px")
        )
      )
    )
  )
)

# --------------------
# Server
# --------------------
server <- function(input, output, session) {
  
  reactive_data <- reactive({
    df <- load_data(input$ticker)
    df %>%
      filter(DATE >= input$date_range[1],
             DATE <= input$date_range[2]) %>%
      arrange(DATE)
  })
  
  liquidity_data <- reactive({
    df <- reactive_data()
    
    df <- df %>%
      mutate(
        ret               = c(NA, diff(log(PRC))),
        TradingVolume     = VOL,
        Turnover          = PRC * VOL,
        AbsSpread         = ASK - BID,
        LogAbsSpread      = ifelse(AbsSpread > 0, log(AbsSpread), NA),
        MidPrice          = (ASK + BID) / 2,
        RelSpreadMid      = ifelse(MidPrice > 0, AbsSpread / MidPrice, NA),
        RelSpreadLast     = ifelse(PRC > 0, AbsSpread / PRC, NA),
        RelLogSpread      = ifelse(ASK > 0 & BID > 0, log(ASK) - log(BID), NA),
        LogRelLogSpread   = ifelse(!is.na(RelLogSpread) & RelLogSpread > 0,
                                   log(RelLogSpread), NA),
        LR1               = ifelse(!is.na(ret) & abs(ret) > 0,
                                   (PRC * VOL) / abs(ret), NA),
        FlowRatio         = NUMTRD * Turnover
      )
    
    feature_cols <- c("AbsSpread", "LogAbsSpread", "RelSpreadMid",
                      "RelSpreadLast", "RelLogSpread", "LogRelLogSpread",
                      "Turnover", "TradingVolume", "NUMTRD", "LR1", "FlowRatio")
    mat_scaled <- scale(df[ , feature_cols])
    valid_idx  <- complete.cases(mat_scaled)
    clean_data <- as.data.frame(mat_scaled[valid_idx, ])
    
    iso_mod    <- isolation.forest(clean_data, nthreads = 2)
    iso_scores <- predict(iso_mod, clean_data, type = "score")
    iso_thr    <- quantile(iso_scores, 0.99, na.rm = TRUE)
    
    lof_scores <- lofactor(clean_data, k = 10)
    lof_thr    <- quantile(lof_scores, 0.99, na.rm = TRUE)
    
    df$anomaly_score      <- NA
    df$rare_event_iforest <- FALSE
    df$lof_score          <- NA
    df$rare_event_lof     <- FALSE
    
    df$anomaly_score[valid_idx]      <- iso_scores
    df$rare_event_iforest[valid_idx] <- iso_scores >= iso_thr
    df$lof_score[valid_idx]          <- lof_scores
    df$rare_event_lof[valid_idx]     <- lof_scores >= lof_thr
    
    features_to_reverse <- c("AbsSpread", "LogAbsSpread", "RelSpreadMid",
                             "RelSpreadLast", "RelLogSpread", "LogRelLogSpread")
    aligned <- clean_data
    aligned[ , features_to_reverse] <- - aligned[ , features_to_reverse]
    z_sum <- rowSums(aligned)
    
    df$z_sum <- NA
    df$z_sum[valid_idx] <- z_sum
    
    df$liquidity_class <- case_when(
      (df$rare_event_iforest | df$rare_event_lof) & df$z_sum >  0 ~ "High Liquidity",
      (df$rare_event_iforest | df$rare_event_lof) & df$z_sum <  0 ~ "Low Liquidity",
      TRUE                                                         ~ NA_character_
    )
    
    df
  })
  
  output$price_plot <- renderPlotly({
    df <- reactive_data()
    req("PRC" %in% names(df))
    
    plot_ly(df, x = ~DATE, y = ~PRC,
            type = 'scatter', mode = 'lines+markers',
            text = ~paste0("Date: ", DATE,
                           "<br>Price: ", PRC,
                           "<br><b>News:</b><br>", ALL_HEADLINES),
            hoverinfo = "text") %>%
      layout(title = "Stock Price Over Time",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Price"))
  })
  
  output$volume_plot <- renderPlotly({
    df <- reactive_data()
    req("VOL" %in% names(df))
    
    plot_ly(df, x = ~DATE, y = ~VOL,
            type = 'bar',
            text = ~paste0("Date: ", DATE,
                           "<br>Volume: ", VOL,
                           "<br><b>News:</b><br>", ALL_HEADLINES),
            hoverinfo = "text") %>%
      layout(title = "Trading Volume Over Time",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Volume"))
  })
  
  output$sentiment_plot <- renderPlotly({
    df <- reactive_data()
    req("SENTIMENT" %in% names(df))
    
    plot_ly(df, x = ~DATE, y = ~SENTIMENT,
            type = 'scatter', mode = 'lines+markers',
            text = ~paste0("Date: ", DATE,
                           "<br>Sentiment: ", round(SENTIMENT, 3),
                           "<br><b>News:</b><br>", ALL_HEADLINES),
            hoverinfo = "text") %>%
      layout(title = "News Sentiment Over Time",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Sentiment Score"))
  })
  
  output$liquidity_plot <- renderPlotly({
    df <- liquidity_data()
    
    p <- ggplot(df, aes(x = DATE, y = PRC)) +
      geom_line(color = "steelblue", size = 0.8) +
      geom_point(data = filter(df, liquidity_class == "High Liquidity"),
                 aes(x = DATE, y = PRC),
                 shape = 21, fill = NA,
                 color = "darkgreen", stroke = 1.5,
                 size = 4) +
      geom_point(data = filter(df, liquidity_class == "Low Liquidity"),
                 aes(x = DATE, y = PRC),
                 shape = 21, fill = NA,
                 color = "red", stroke = 1.5,
                 size = 4) +
      labs(
        title    = "Liquidity Events",
        subtitle = "High Liquidity (green outline) | Low Liquidity (blue outline)",
        x        = "Date",
        y        = "Price (PRC)"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title    = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
      )
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
