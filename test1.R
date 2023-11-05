library(shiny)
library(ggplot2)
setwd(here::here())
getwd()
raw_data <- read.csv("filtered_data.csv")

ui <- fluidPage(
  titlePanel("Average Price per m²"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selectedYear", "Select a Year", choices = c("2010","2012","2014","2016","2018","2020"))
    ),
    mainPanel(
      plotOutput("pricePlot")
    )
  )
)

server <- function(input, output) {
  filteredData <- reactive({
    filter2 <- raw_data[raw_data$year == input$selectedYear, ]
    return(filter2)
  })
  
  output$pricePlot <- renderPlot({
    data2 <- head(filteredData())
    ggplot(data2, aes(x = locality, y = as.numeric(average_price_m2_nominal_euros), fill = locality)) +
      geom_bar(stat = "identity") +
      labs(
        title = paste("Average Price per m² in", input$selectedYear),
        x = "Locality",
        y = "Average Price per m² (Nominal Euros)"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(labels = scales::number_format(accuracy = 0.01))
  })
}

shinyApp(ui, server)
