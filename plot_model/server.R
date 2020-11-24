#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$regionPlot <- renderPlot({
    
    reference_family <- input$ref_family 
    
    plot_obj <- readRDS('region_plots.rds')
    
    plot_obj[[reference_family]] + 
      theme(axis.title = element_text(size = 20), 
            axis.text = element_text(size = 14, angle = 45), 
            strip.text = element_text(size = 14), 
            legend.text = element_text( size = 14), 
            legend.title = element_text(size = 20))  
    
  })
  
  output$familyPlot <- renderPlot( { 
    reference_region <- input$ref_region
    
  plot_obj[[reference_family]] + 
    theme(axis.title = element_text(size = 20), 
          axis.text = element_text(size = 14, angle = 45), 
          strip.text = element_text(size = 14), 
          legend.text = element_text( size = 14), 
          legend.title = element_text(size = 20)) 
    })
})
