library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(RCurl)

# Load your dataset
data1 <- getURL("https://raw.githubusercontent.com/fauzanbudi/shiny-healthcare/main/healthcare_dataset.csv")
data <- read.csv(text = data1)
head(data)
summary(data)

# Summary for EDA purposes
by_medical_condition <- data %>% 
  group_by(Medical.Condition) %>% 
  summarise(patients = n_distinct(Name), sum_billing_amount = sum(Billing.Amount))

# Summary for EDA purposes
by_insurance_provider <- data %>% 
  group_by(Insurance.Provider) %>% 
  summarise(patients = n_distinct(Name), sum_billing_amount = sum(Billing.Amount))

# Create age groups
data$age_group <- cut(data$Age, 
                      breaks = seq(0, 100, by = 10), 
                      right = FALSE, 
                      labels = paste(seq(0, 90, by = 10), seq(9, 99, by = 10), sep = "-"))

# Extract year-month from admission date
data$admission_year_month <- format(as.Date(data$Date.of.Admission),"%Y-%m-01")

# Summary for EDA purposes
by_admission_year_month <- data %>% 
  group_by(admission_year_month) %>% 
  summarise(patients = n_distinct(Name), sum_billing_amount = sum(Billing.Amount)) %>%
  arrange(desc(patients))

ui <- fluidPage(
  titlePanel("Healthcare Patients Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("gender", "Gender", choices = unique(data$Gender), selected = NULL, multiple = TRUE),
      sliderInput("age", "Age", min = min(data$Age, na.rm = TRUE), max = max(data$Age, na.rm = TRUE), value = c(min(data$Age, na.rm = TRUE), max(data$Age, na.rm = TRUE))),
      dateRangeInput("admission_date", "Date of Admission", start = "2022-06-01", end = max(data$Date.of.Admission, na.rm = TRUE)),
      selectInput("medical_condition", "Medical Condition", choices = unique(data$Medical.Condition), selected = NULL, multiple = TRUE),
      selectInput("insurance", "Insurance", choices = unique(data$Insurance.Provider), selected = NULL, multiple = TRUE),
      br(),
      br(),
      div(style = "position: absolute; bottom: 0; left: 10; width: 100%;",
          p("Created using shiny R and a public synthetic dataset from kaggle. ",
            a("Link to Dataset", href = "https://www.kaggle.com/datasets/prasad22/healthcare-dataset/data"),
            br(),
            "Fauzan Budi Prasetya, fauzanbudiprasetya@gmail.com, ",
            a("Linkedin", href = "https://www.linkedin.com/in/fauzan-budi-prasetya/")
          )
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("% of Patients each Medical Condition by Age Group", plotOutput("stackedBarChart")),
        tabPanel("Total Billing Amount by Insurance", plotOutput("barChart")),
        tabPanel("Billing Amount Distribution by Insurance", plotOutput("violinPlot")),
        tabPanel("Trend of Patient Admissions Over Time", plotOutput("lineChartTrend")),
        tabPanel("Trend of Patient Admissions by Medical Condition Over Time", plotOutput("lineChartTrendGroup"))
      )
    )
  )
)

server <- function(input, output) {
  
  filteredData <- reactive({
    data %>%
      filter(
        (is.null(input$gender) | Gender %in% input$gender),
        Age >= input$age[1] & Age <= input$age[2],
        Date.of.Admission >= input$admission_date[1] & Date.of.Admission <= input$admission_date[2],
        (is.null(input$medical_condition) | Medical.Condition %in% input$medical_condition),
        (is.null(input$insurance) | Insurance.Provider %in% input$insurance)
      )
  })

  output$stackedBarChart <- renderPlot({
    ggplot(filteredData(), aes(x = age_group, fill = Medical.Condition)) +
      geom_bar(position = "fill") +
      labs(y = "Percentage", title = "% of Patients each Medical Condition by Age Group") +
      scale_y_continuous(labels = scales::percent)
  })

  output$barChart <- renderPlot({
    ggplot(filteredData(), aes(x = Insurance.Provider, y = Billing.Amount, fill=Insurance.Provider)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(y = "Total Billing Amount", title = "Total Billing Amount by Insurance")
  })

  output$violinPlot <- renderPlot({
    ggplot(filteredData(), aes(x = Insurance.Provider, y = Billing.Amount, fill=Insurance.Provider)) +
      geom_violin() +
      labs(y = "Billing Amount", title = "Billing Amount Distribution by Insurance")
  })

  output$lineChartTrend <- renderPlot({
    trend_data <- filteredData() %>%
      group_by(admission_year_month) %>%
      summarise(patient_count = n())
    
    ggplot(trend_data, aes(x = admission_year_month, y = patient_count, group = 1)) +
      geom_line() +
      labs(y = "Number of Patients", title = "Trend of Patient Admissions Over Time") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$lineChartTrendGroup <- renderPlot({
    trend_group_data <- filteredData() %>%
      group_by(admission_year_month, Medical.Condition) %>%
      summarise(patient_count = n())
    
    ggplot(trend_group_data, aes(x = admission_year_month, y = patient_count, color = Medical.Condition, group = Medical.Condition)) +
      geom_line() +
      labs(y = "Number of Patients", title = "Trend of Patient Admissions by Medical Condition Over Time") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

shinyApp(ui = ui, server = server)