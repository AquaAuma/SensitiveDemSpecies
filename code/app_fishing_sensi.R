#############################################################
#### This is a Shiny web application. You can run the application by clicking
#### the 'Run App' button above.
#### Find out more about building applications with Shiny here:
#### http://shiny.rstudio.com/
#### Related paper: Are fish sensitive to trawling recovering in the Northeast Atlantic?
#### Coding: Aurore Maureaud, July 2020
#############################################################

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  h1('Visualize Sensitive Species'),
  p('Sensitive Species to Fishing Abundance Trends from Bottom Trawl Surveys in northwestern European Seas')
  
)

# Run the application 
shinyApp(ui = ui, server = server)