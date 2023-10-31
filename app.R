#Jessica Nestor
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(ggridges)
library(ggplot2)
library("shiny")
library("shinyWidgets")

#reading/changing data and columns
accident <- read.csv("accident.csv")
tx_accident <- accident[accident$STATENAME == 'Texas',]
selection <- tx_accident[, c(15,29,31,46,61,63,80)]
conditions <- selection[,c(2,3,4)]
values <- selection[5]

# Remove rows where all values after the first column are NA
bruh <- tx_accident[!apply(is.na(tx_accident[, -1]), 1, all), ]

# remove columns where all values are 0
ix <- apply(tx_accident, 2, function(col) all(col == 0))
if (sum(ix) > 0) {
  showNotification(
    ui = paste0("Warning!!! Columns with all zero values are deleted: ",
                paste0(names(tx_accident)[ix], collapse = ", ")),
    id = "zero_expression_file",
    duration = 10,
    type = "warning"
  )
  okay <- tx_accident[, !ix]        
}

#column names
colnames(selection)[1] ="Month"
colnames(selection)[2] ="Type of Road"
colnames(selection)[3] ="Area"
colnames(selection)[4] ="Harm/Damage"
colnames(selection)[5] ="Visibility"
colnames(selection)[6] ="Weather"
colnames(selection)[7] ="Number of Fatalities"

#start of web
ui <- fluidPage(    
  
  # Give the page a title
  titlePanel("Accident Data for Texas"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("cond", "Conditions:", 
                  choices=names(selection[,c(2,3,6,5)])),
      br(),
      selectInput("values", "Number of ", 
                  choices=names(selection[,c(4,7)]))
    ),
    
    # Create a spot for the barplot
    mainPanel(
      plotOutput("crashPlot")  
    )
    
  )
)


server <- function(input, output) {
  
  # Fill in the spot we created for a plot
  output$crashPlot <- renderPlot({
    
    
    ggplot(selection, aes(x = get(input$values), 
                          y = get(input$cond),
                          fill = get(input$cond))) +
      geom_density_ridges_gradient() +
      theme(legend.position = "none")+
      xlab("")+
      ylab("")
  })
}
shinyApp(ui = ui, server = server)