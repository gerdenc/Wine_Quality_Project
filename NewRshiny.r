library(shiny)
library(ggplot2)
library(dplyr)
library(corrplot)

# Define UI
ui <- fluidPage(
  titlePanel("Wine Quality Analysis App"),
  sidebarLayout(
    sidebarPanel(
      selectInput("plotType", "Select Visualization Type:",
                  choices = c("Correlation Plot", "Bar Plot")),
      actionButton("refresh", "Refresh Plot")
    ),
    mainPanel(
      plotOutput("mainPlot"),
      tableOutput("summaryTable")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Render Plot
  output$mainPlot <- renderPlot({
    input$refresh  # Ensure the button click refreshes the plot
    
    if (input$plotType == "Correlation Plot") {
      # Correlation Plot
      numeric_data <- winetrain %>% select(where(is.numeric))
      cor_matrix <- cor(numeric_data)
      
      corrplot::corrplot(cor_matrix, method = "circle", type = "lower", tl.cex = 0.8, title = "Correlation Matrix")
      
    } else if (input$plotType == "Bar Plot") {
      # Bar Plot with Custom Colors
      winetrain %>%
        count(type) %>%
        ggplot(aes(x = type, y = n, fill = type)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = c("red" = "maroon", "white" = "gray")) +
        labs(title = "Bar Plot of Wine Types", x = "Wine Type", y = "Count") +
        theme_minimal()
    }
  })
  
  # Render Summary Table
  output$summaryTable <- renderTable({
    if (input$plotType == "Correlation Plot") {
      # Correlation Matrix as Table
      numeric_data <- winetrain %>% select(where(is.numeric))
      cor(numeric_data)
      
    } else if (input$plotType == "Bar Plot") {
      # Bar Plot Data
      winetrain %>% count(type)
      
    } else {
      NULL
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)

