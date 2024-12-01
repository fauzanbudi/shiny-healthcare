library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)

# Load your dataset
#data <- read.csv("https://github.com/fauzanbudi/shiny-healthcare/blob/1039f790ddc6359adf5ac4399d4f380e8134e810/healthcare_dataset.csv")

# Create age groups
data$age_group <- cut(data$Age, 
                      breaks = seq(0, 100, by = 10), 
                      right = FALSE, 
                      labels = paste(seq(0, 90, by = 10), seq(9, 99, by = 10), sep = "-"))

# Extract year-month from admission date
data$admission_year_month <- format(as.Date(data$Date.of.Admission),"%Y-%m-01")

ui <- fluidPage(
  titlePanel("Healthcare Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("gender", "Gender", choices = unique(data$Gender), selected = NULL, multiple = TRUE),
      sliderInput("age", "Age", min = min(data$Age, na.rm = TRUE), max = max(data$Age, na.rm = TRUE), value = c(min(data$Age, na.rm = TRUE), max(data$aAge, na.rm = TRUE))),
      dateRangeInput("admission_date", "Date of Admission", start = min(data$Date.of.Admission, na.rm = TRUE), end = max(data$Date.of.Admission, na.rm = TRUE)),
      selectInput("medical_condition", "Medical Condition", choices = unique(data$Medical.Condition), selected = NULL, multiple = TRUE),
      selectInput("insurance", "Insurance", choices = unique(data$Insurance.Provider), selected = NULL, multiple = TRUE),
      selectInput("hospital", "Hospital", choices = unique(data$Hospital), selected = NULL, multiple = TRUE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Stacked % Bar Chart", plotOutput("stackedBarChart")),
        tabPanel("Bar Chart by Group", plotOutput("barChart")),
        tabPanel("Violin by Group", plotOutput("violinPlot")),
        tabPanel("Line Chart Trend", plotOutput("lineChartTrend")),
        tabPanel("Line Chart Trend by Group", plotOutput("lineChartTrendGroup"))
      )
    )
  )
)

server <- function(input, output) {
  
  filteredData <- reactive({
    data %>%
      filter(
        (is.null(input$gender) | gender %in% input$gender),
        age >= input$age[1](citation_1) & age <= input$age[2](citation_2),
        admission_date >= input$admission_date[1](citation_1) & admission_date <= input$admission_date[2](citation_2),
        (is.null(input$medical_condition) | medical_condition %in% input$medical_condition),
        (is.null(input$insurance) | insurance %in% input$insurance),
        (is.null(input$hospital) | hospital %in% input$hospital)
      )
  })

  output$stackedBarChart <- renderPlot({
    ggplot(filteredData(), aes(x = age_group, fill = medical_condition)) +
      geom_bar(position = "fill") +
      labs(y = "Percentage", title = "Stacked % Bar Chart by Age Group") +
      scale_y_continuous(labels = scales::percent)
  })

  output$barChart <- renderPlot({
    ggplot(filteredData(), aes(x = insurance, y = billing_amount, fill = hospital)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(y = "Total Billing Amount", title = "Total Billing Amount by Insurance and Hospital")
  })

  output$violinPlot <- renderPlot({
    ggplot(filteredData(), aes(x = insurance, y = billing_amount, fill = hospital)) +
      geom_violin() +
      labs(y = "Billing Amount", title = "Billing Amount Distribution by Insurance and Hospital")
  })

  output$lineChartTrend <- renderPlot({
    ggplot(filteredData(), aes(x = admission_year_month)) +
      geom_line(stat = "count") +
      labs(y = "Number of Patients", title = "Trend of Patient Admissions Over Time")
  })

  output$lineChartTrendGroup <- renderPlot({
    ggplot(filteredData(), aes(x = admission_year_month, color = medical_condition)) +
      geom_line(stat = "count") +
      labs(y = "Number of Patients", title = "Trend of Patient Admissions by Medical Condition Over Time")
  })
}

shinyApp(ui = ui, server = server)