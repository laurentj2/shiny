# SYNCHRONISEER MET DATABASE

library(REDCapR) #Load the package into the current R session.
library(dplyr)
library(meta)
uri <- "http://213.136.18.153/redcap/api/"
token <- "8DE2375CB2387A99D28EC00DCC48097B"

#Return all rows 1:n
desired_records_v1 <- c(1:1000)
meta <- redcap_read_oneshot(
  redcap_uri = uri, 
  token = token, 
  records = desired_records_v1
)$data


# Definieer belangrijke belangrijke eindpunt concepten voor vergelijking
#MACCE
meta$end=0
meta$end[meta$endpoint___1==1 & meta$endpoint___3==1 & meta$endpoint___4==1 & meta$endpoint___5==0 & meta$endpoint___6==0 & meta$endpoint___7==0 & meta$endpoint___8==0] <- 1

# Definieer belangrijke belangrijke eindpunt concepten voor vergelijking
#MACCE
meta$end=0
meta$end[meta$endpoint___1==1 & meta$endpoint___3==1 & meta$endpoint___4==1 & meta$endpoint___5==0 & meta$endpoint___6==0 & meta$endpoint___7==0 & meta$endpoint___8==0] <- 1
#Major bleeding
meta$end[meta$endpoint___1==0 & meta$endpoint___3==0 & meta$endpoint___4==0 & meta$endpoint___5==0 & meta$endpoint___6==1 & meta$endpoint___7==0 & meta$endpoint___8==0 & meta$endbleeding___1==1 & meta$endbleeding___2==0 & meta$endbleeding___3==0 & meta$endbleeding___4==0 & meta$endbleeding___5==0] <- 3


data_sets <- c("Acute Coronary Syndrome" = "1",
               "Atrial Fibrillation" = "2",
               "Heart failure" = "3")

data_sets2 <- c("Antiplatelet therapy" = "1",
                "Atrial Fibrillation" = "2",
                "Heart fa" = "3")

treatmenta <- c("Placebo" = "0", "Clopidogrel" = "1", "Ticagrelor" = "2", "Prasugrel" = "3")

controlantip <- c("Placebo" = "0", "Clopidogrel" = "1", "Ticagrelor" = "2", "Prasugrel" = "3")


endpoint <- c("MACCE" = "1",
              "MI" = "2",
              "TIMI Major" = "3","TIMI Minor" = "4","Custom endpoint" = "5")





shinyServer(function(input, output) {
  
  # Drop-down selection box for which data set
  output$choose_dataset <- renderUI({
    selectInput("dataset", "Topic", as.list(data_sets))
    
    
  })
  
  
  ### TREATMENT EN EINDPUNTEN VOOR ANTIPLATELET DEEL
  output$choose_dataset2 <- renderUI({
    if(input$dataset==1){
      selectInput("dataset2", "Group", as.list(data_sets2))   
      
    }
  })
  
  
  output$choose_dataset3 <- renderUI({
    if(input$dataset==1 & input$dataset2==1){
      selectInput("dataset3", "Treatment", as.list(treatmenta))   
      
    }
  })
  
  output$choose_dataset4 <- renderUI({
    if(input$dataset==1  & input$dataset2==1){
      selectInput("dataset4", "Control", as.list(controlantip))   
      
    }
  })
  
  output$choose_dataset5 <- renderUI({
    if(input$dataset==1  & input$dataset3>=0){
      selectInput("dataset5", "Endpoint", as.list(endpoint))   
      
    }
  })
  
  
  selec <- reactive({
    
  dat <- subset(meta,topic==input$dataset & group==input$dataset2 & treatment==input$dataset3 & control==input$dataset4 & end==input$dataset5)
  if(input$view_select != "") {
    selcom <- input$view_select
    selcom <- gsub(" ", "", selcom)
   seldat <- try(do.call(subset, list(dat,parse(text = selcom))), silent = TRUE)
    if(!is(seldat, 'try-error')) {
     if(is.data.frame(seldat)) {
       dat <- seldat
       seldat <- NULL
     }
   }
}
      as.data.frame(dat)
dat })
  



output$table <- renderDataTable({
    dat <- selec()
    dat <- select(dat,studie,format,nintervention,ncontrol)
    dat                     
  })

output$dataviewer <- renderDataTable({
  dat <- selec()
  dat <- select(dat,studie,format,nintervention,ncontrol)
  dat <- t(dat)
  dat                     
})
  
  
subsetTest <- reactive({
  select <- selec()
  metabin(nprimary,nintervention,nprimarycontrol,ncontrol,studie,data=select)})
  
  
  output$plot1 <- renderPlot({ 
    forest(subsetTest())
  })
  
  
  output$choose_dataset6 <- renderUI({
    textInput("view_select", "Filter:", "i.e. N > 2000 & meanage > 60")
      
  })
  
      

  
 
  
  
  

})
