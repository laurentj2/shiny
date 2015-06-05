library(quantmod)
library(xts)
test=round(runif(1),10)
test=substring(test, 3,10)
plot.pre_factor <- reactive(function(){
  setwd("/srv/shiny")
 
    data = getData()
    
    tryCatch({							
      #*****************************************************************
      # Code Strategies
      #****************************************************************** 
      prices = ROC(r_data[["diamonds"]])  
      attach(as.list(prices))
      
     
      
      rtn=na.omit(prices)
      numCycles=1
      
      ###SCRHIJF NAAR EUREQA
      for (i in 1:numCycles) {
        
        write.table(rtn, paste0("input", i,test,".txt"), quote =FALSE, row.names=FALSE)
        
        command <- sprintf("./basic_client  input%s%s.txt output%s%s.txt AAPL", i, i)
        system(paste0(command), wait=FALSE) 
        
      }
      ##HAAL BESTE MODELLEN TERUG En BACKTEST
      bestmodel <- as.list(1:2)
      for (i in 1:numCycles) {
        example <- read.delim(paste0("G:/", "output",i,test,".txt"), header=FALSE, quote="", stringsAsFactors=FALSE)
        trim <- function (x)  sub("^\\s+", "", x)
        example$V2<- trim(example$V2)
        str2expr<-function(x){eval(parse(text=x), envir=parent.frame() )} 
        bestmodel[i] <- example$V2[[which.min(example$V1)]]  
      }
      
      models = list()
      
      stock = "AAPL"
      cash = "GS"
      
      #*****************************************************************
      # Buy & Hold
      #****************************************************************** 
      data$weight[] = NA
      formule  <- str2expr(bestmodel[1])
      trigger<- ifelse(formule>0,4/4,0)
      trigger2<- ifelse(formule<0,-4/4,trigger)
      data$weight[,stock] = trigger2
      models$buy.hold = bt.run.share(data, clean.signal=T)
      
            
      rev(models)
    }, error = function(e) { stop(paste('Problem running Back Test:', e)) })
  })
  
  


  # Make table
  makeSidebysideTable <- reactive(function() {
    models = getBackTest()
    plotbt.strategy.sidebyside(models, return.table=T, make.plot=F)
  })
  
  # Make table
  makeAnnualTable <- reactive(function() {
    models = getBackTest()
    plotbt.monthly.table(models[[1]]$equity, make.plot = F)
  })
  
  # Make table
  makeTradesTable <- reactive(function() {
    models = getBackTest()
    model = models[[1]]
    
    if (!is.null(model$trade.summary)) {
      ntrades = min(20, nrow(model$trade.summary$trades))		
      last(model$trade.summary$trades, ntrades)
    }
  })

	
  
  
