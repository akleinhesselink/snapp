#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("User Identification Accuracy"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       selectInput("ref_family",
                   "Reference Family:", 
                   sort(c("Elapidae", 
                     "Viperidae", 
                     "Colubridae", 
                     "Lamprophiidae", 
                     "Boidae", 
                     "Cylindrophiidae", 
                     "Typhlopidae", 
                     "Pythonidae", 
                     "Leptotyphlopidae")), 
                   selected = 'Colubridae')
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("regionPlot"))
  )
))
