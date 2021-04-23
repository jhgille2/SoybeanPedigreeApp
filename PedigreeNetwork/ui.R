#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(igraph)
library(visNetwork)
library(tidyverse)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel(title = h1("Soybase parentage data explorer", h3("Hover over circles in the graph to go to the corresponding Soybase entry")), windowTitle = "Soybase pedigree viewer"),
    
    
    fluidRow(
        column(2, 
               selectizeInput("CultivarSelection", 
                              label    = "Cultivar", 
                              choices  = NULL,
                              selected = "NC-Roy", 
                              multiple = FALSE),
               
               sliderInput("generations",
                           "Number of generations",
                           min = 1,
                           max = 20,
                           value = 4), 
               
               textOutput("crossString"), tags$head(tags$style("#clickGene{color:red; font-size:12px; font-style:italic; 
overflow-y:scroll; max-height: 50px; background: ghostwhite;}"))), 
        
        column(10, 
               visNetworkOutput("network", height = "700px"))
    )
    
  )
)
