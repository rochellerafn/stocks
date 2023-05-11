library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(dplyr)
library(ggplot2)
library(tidyquant)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("yeti"),
                
                # Application title
                titlePanel("Stock Company Comparison"),
                
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                  sidebarPanel(
                    sliderInput("date", "Choose a date range:", 
                                min = as.Date("1970-01-01"), max = Sys.Date(), 
                                value = c(as.Date("2020-01-01"), Sys.Date()), step = 1),
                    selectInput("company", "Choose a Company:",
                                choices = NULL, multiple = TRUE, selected = ""),
                    sliderInput("close", "Choose a close range:", 
                                min = 0, max = 5000, 
                                value = c(0, 5000), step = 1)
                  ),
                  
                  # Show a plot of the generated distribution
                  mainPanel(
                    plotOutput("plot"),
                    DTOutput("table")
                  )
                )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  SP500 <- tq_index("SP500") %>%
    tq_get(get = "stock.prices")
  
  # Update the company choices based on the SP500 data
  observeEvent(SP500, {
    updateSelectInput(inputId = "company",
                      choices = unique(SP500$company),
                      selected = "")
  })
  
  # Filter data based on user inputs
  filtered_data <- reactive({
    req(SP500)
    SP500 %>% 
      filter(company %in% input$company,
             close >= input$close[1] & close <= input$close[2],
             date >= input$date[1] & date <= input$date[2])
  })
  
  output$plot <- renderPlot({
    
    # draw the plot with the specified filters
    ggplot(filtered_data(), aes(x=date, y=close, color=company)) +
      geom_line() +
      labs(y = "Closing Price", color = "Company") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            plot.background = element_blank(),
            panel.background = element_blank()) +
      scale_y_continuous(labels = scales::comma)
  })
  
  output$table <- renderDT({
    # Show a data table of the filtered data
    datatable(filtered_data())
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
