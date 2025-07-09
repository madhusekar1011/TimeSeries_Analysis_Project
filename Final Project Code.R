library(shiny)
library(shinydashboard) 
library(ggplot2)
library(fpp3)
library(readxl)
library(dplyr)

# Load and clean data at startup
df1 <- read_excel("online_retail_II.xlsx", sheet = 1)
df2 <- read_excel("online_retail_II.xlsx", sheet = 2)

retail_df <- bind_rows(df1, df2) %>%
  # rename the column to CustomerID
  rename(CustomerID = `Customer ID`) %>%  
  mutate(
    CustomerID = if_else(is.na(CustomerID), "Unknown", as.character(CustomerID)),
    Total_Price = Quantity * Price
  ) %>%
  distinct()

df_UK <- retail_df %>%
  filter(Country == "United Kingdom") %>%
  mutate(YearMonth = yearmonth(InvoiceDate)) %>%
  group_by(YearMonth) %>%
  summarise(Total_Sales = sum(Total_Price, na.rm = TRUE)) %>%
  as_tsibble(index = YearMonth)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "UK Retail Sales Dashboard"),
  dashboardSidebar(
    selectInput("year","Select Year:", choices = c(2009, 2010, 2011), selected = 2010),
    selectInput("month","Select Starting Month:", choices = month.name, selected = "January"),
    selectInput("model_type","Choose Model:", choices = c("ETS", "ARIMA", "NNAR"), selected = "ARIMA"),
    actionButton("run","Generate Forecast"),
    actionButton("clear","Clear Plot")
  ),
  dashboardBody(
    fluidRow(
      box(plotOutput("salesPlot"), width = 12),
      box(plotOutput("forecastPlot"), width = 12)
    )))


# Define Server logic
server <- function(input, output) {
  # Fit ARIMA once
  arima_model <- df_UK %>% model(ARIMA(Total_Sales))
  
  # Convert month name to number
  get_month_number <- function(month_name) {
    match(month_name, month.name)
  }
  
  # Filter data
  filtered_data <- reactive({
    req(input$year, input$month)
    start_date <- yearmonth(paste(input$year, get_month_number(input$month), sep = "-"))
    df_UK%>% filter(YearMonth >= start_date)
  })
  # Reactive forecast with button
  forecast_result <- eventReactive(input$run, {
    data <- filtered_data()
    model_choice <- input$model_type
    
    fit <- switch(model_choice,
                  "ETS" =data %>% model(ETS(Total_Sales)),
                  "ARIMA" =data %>% model(ARIMA(Total_Sales)),
                  "NNAR" =data %>% model(NNETAR(Total_Sales))
    )
    forecast(fit,h=12)
  })
  
  # Render the historical sales plot
  output$salesPlot <- renderPlot({
    autoplot(df_UK) +
      labs(x = "Month", y = "Total Sales",
           title = "UK Monthly Sales (2009–2011)") +
      theme_minimal()
  })
  
  # Render the forecast plot
  output$forecastPlot <- renderPlot({
    req(forecast_result())
    autoplot(forecast_result(), filtered_data()) +
      labs(title = paste("Forecast using", input$model_type),
           x = "Month", y = "Sales") +
      theme_minimal()
  })
  
  
  #clear button
  observeEvent(input$clear, {
    output$forecastPlot <- renderPlot(NULL)
  })
}

# Run the app
shinyApp(ui, server)