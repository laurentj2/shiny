library(shiny)
library(ggplot2)  # for the diamonds dataset

shinyUI(fluidPage(
  titlePanel("METAnalysis platform"),
  
  sidebarLayout(
    sidebarPanel(
    
      uiOutput("choose_dataset"),
      uiOutput("choose_dataset2"),
      uiOutput("choose_dataset3"),
      uiOutput("choose_dataset4"),
      uiOutput("choose_dataset5"),
      uiOutput("choose_dataset6"),
      
      a(href = "http://www.antoniusziekenhuis.nl", "© St. Antonius Cardiology Research"),
      br(),
      a(href = "http://www.antoniusziekenhuis.nl", "Disclaimer")
    ),   

 
    mainPanel(
      tabsetPanel(     
        tabPanel('Select studies', dataTableOutput('table')),
        tabPanel('Table', dataTableOutput('dataviewer')),
        tabPanel("Plot", plotOutput("plot1"))       
      )
    )
  )
))







  

  