
library(quantmod)
source("https://www.dropbox.com/s/3kksjgb122tgt15/original.r?dl=1")
source("https://www.dropbox.com/s/j4cshlyclih2car/wrap.r?dl=1")



descr_out <- function(descr, ret_type = 'html') {
   # if there is no data description
  if (descr %>% is_empty) return("")

  # if there is a data description and we want html output
  if (ret_type == 'html')
    descr <- markdown::markdownToHTML(text = descr, stylesheet=file.path(r_path,"base/www/empty.css"))

  descr
}



output$selectUI <- renderUI({
  textInput("symbols", "Enter Formula", "MSFT,AAPL,GS")
})


getStocks <- reactive(function() { spl(toupper(gsub('\n',',',input$symbols))) })

symbol_env <- new.env()

getData <- reactive(function() {    
  cat('getData was called\n')
  
  data <- new.env()
  for(symbol in getStocks() ) {
    if (is.null(symbol_env[[symbol]]))
      tryCatch({
        symbol_env[[symbol]] = getSymbols(symbol, from='1970-01-01', src='yahoo', auto.assign = FALSE)
      }, error = function(e) { stop(paste('Problem getting prices for',symbol)) })
    data[[symbol]] = adjustOHLC(symbol_env[[symbol]], use.Adjusted=T)  			
  }
  
  bt.prep(data, align='keep.all', dates='2000::')
  data	
})


output$selectUI2 <- renderUI({
  r_data[["diamonds"]] <- as.data.frame(getData()$prices)
})

                          
#*****************************************************************
# Update status message 
#******************************************************************    
output$status <- reactiveUI(function() {
  out = tryCatch( dataInput(), error=function( err ) paste(err))	    				
  if( is.character( out ) ) 
    HTML(paste("<b>Status</b>: <b><font color='red'>Error:</font></b>",out))
  else
    HTML("<b>Status</b>: <b><font color='green'>Ok</font></b>")		
})





saveClipboardData <- function() {
  os_type <- Sys.info()["sysname"]
  if (os_type == 'Windows') {
    write.table(.getdata(), "clipboard", sep="\t", row.names=FALSE)
  } else if (os_type == "Darwin") {
    write.table(.getdata(), file = pipe("pbcopy"), row.names = FALSE, sep = '\t')
  } else if (os_type == "Linux") {
    print("### Saving data through the clipboard is currently only supported on Windows and Mac. You can save your data to csv format to use it in a spreadsheet.")
  }
}




