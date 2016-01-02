
# Define server
shinyServer(function(input, output) {
   source("https://www.dropbox.com/s/j4cshlyclih2car/wrap.r?dl=1")
	# Create an environment for storing data
  # Create an environment for storing data
  symbol_env <- new.env()
  library(quantmod)
  #*****************************************************************
  # Shared Reactive functions
  # http://rstudio.github.com/shiny/tutorial/#inputs-and-outputs
  #******************************************************************    	
  # Get stock data
  getData <- reactive(function() {  	
    cat('getData was called\n')
    
    data <- new.env()
    for(symbol in c(getStock()) ) {
      if (is.null(symbol_env[[symbol]]))
        tryCatch({
          symbol_env[[symbol]] = getSymbols(symbol, from='2005-01-01', src='yahoo', auto.assign = FALSE)
        }, error = function(e) { stop(paste('Problem getting prices for',symbol)) })
      data[[symbol]] = symbol_env[[symbol]]
    }
    
   # bt.prep(data, align='remove.na')
    bt.prep(data, align='keep.all', dates='2005::2016') 
    data
  })

 
  # Helper fns
  getStock <- reactive(function() { toupper(input$symbol) })
  #getStock2 <- reactive(function() { toupper(input$cash) })

	getBackTest <- reactive(function() { 
		#*****************************************************************
		# Load historical data
		#****************************************************************** 
	  source("https://www.dropbox.com/s/lkv8ww25mcu0uqg/imdfa.R?dl=1")
	  source("https://www.dropbox.com/s/j4cshlyclih2car/wrap.r?dl=1")
	  library(quantmod)
		data = getData()
		
	tryCatch({							
		#*****************************************************************
		# Code Strategies
		#******************************************************************
		stock <- getStock() 
		x	 <- as.data.frame(getData()$prices)
		x <- ROC(x)
		x[1:3,]=0
		if(ncol(x)< 2) {
		  x[,2]<-x[,1]
		}
		xb <- x
		test <- as.integer(input$oos1)+as.integer(input$oos)
		x <-x[as.integer(input$oos1):test,]
	
		
		len<-length(x[,1])
			####
		### IDMFA paramters
		###
		ub<-as.double(input$ub)
		lb<-as.double(input$lb)
		cutoff<-pi/ub
		Lag<-as.double(input$lag)
		lin_expweight<-F
		weight_constraint<-rep(1/(length(x[1,])-1),length(x[1,])-1)
		d<-0
		spec_obj<-spec_comp(len,x,d)
		weight_func<-spec_obj$weight_func
		K<-length(weight_func[,1])-1
	  Gamma<-((0:K)<(K*ub))
	  #Gamma<-((0:K)<K/ub)&((0:K)>K/lb)
		L<-as.double(input$smaLen)
		
		lambda_smooth<-as.double(input$overfit)/10
		lambda_decay<-as.double(input$overfit)/100
		lambda<-0
		expweight<-as.integer(input$smooth)
		lambda_cross<-0
		i1<-F
		i2<-F
	
		
		plots=T
	  i_mdfa_obj<-IMDFA_comp(Lag,K,L,lambda,weight_func,Gamma,expweight,cutoff,i1,i2,weight_constraint,lambda_cross,lambda_decay,lambda_smooth,x,plots,lin_expweight)
	  
	  b<-i_mdfa_obj$i_mdfa$b
   
	  #prices$signal <- i_mdfa_obj$xff
	  L2<-length(b)
	  len2 <- nrow(xb)
	  xf<-NULL
	  for (i in L2:len2) #i<-20
	  {
	    # I-MDFA
	    xf[i]<-0
	    for (j in 2:length(xb[1,]))  #j<-2
	      xf[i]<-xf[i]+b[,j-1]%*%xb[i:(i-L2+1),j]
	  }
	 	#*****************************************************************
	  # Buy & Hold
	  #****************************************************************** 
	  models = list()
	  data$weight[] = NA
	  data$weight[,stock] = 1
	  models$buy.hold = bt.run.share(data, clean.signal=T)
		#*****************************************************************
		# MA cross-over strategy
		#****************************************************************** 

	  data$weight[] = NA
    trigger <- ifelse(xf<(-0.00),-1,ifelse(xf>0.000,1,0))
		data$weight[,stock] = trigger
		models$filter = bt.run.share(data, clean.signal=F, trade.summary = T)
	
		models$filter$b <- b
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
		
	# Generate a table
	makefilterTable <- reactive(function() {
	  b = getBackTest()$filter 
	  b
	})
    #*****************************************************************
    # Not Reactive helper functions
	#*****************************************************************
	
	
    #*****************************************************************
    # Update plot(s) and table(s)
    #******************************************************************    	
	# Generate a plot
	output$strategyPlot <- reactivePlot(function() {
		models = getBackTest()
		models$filter$weight2 <- models$filter$weight
		test2 <- as.integer(input$oos1)+as.integer(input$oos)
		models$filter$weight2[input$oos1:test2] <- 3
		highlight = which(models$filter$weight2 > 2)
		plota.control$col.x.highlight = col.add.alpha('green',50)
			plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = NULL,x.highlight = highlight)
		
		
		#plotbt.custom.report.part1(models, x.highlight = models$filter$highlight)  
	}, height = 600, width = 900)
	
	# Generate a table
  	output$sidebysideTable <- reactivePlot(function() {
		#temp = makeSidebysideTable()	
		#tableColor(as.matrix(temp))	
  	  models = getBackTest()
		plotbt.custom.report.part2(models)  
	}, height = 600, width = 900)
	
	# Generate a table
  	output$annualTable <- reactive(function() {
		temp = makeAnnualTable()	
		tableColor(as.matrix(temp))		
	})

	# Generate a plot
	output$transitionPlot <- reactivePlot(function() {
		models = getBackTest()
		plotbt.transition.map(models[[1]]$weight)	
	}, height = 400, width = 600)
		
	# Generate a table
  	output$tradesTable <- reactive(function() {
		temp = makeTradesTable()	
		tableColor(as.matrix(temp))		
	})
  
	
    #*****************************************************************
    # Download
    #******************************************************************    
    # Download pdf report
	output$downloadReport <- downloadHandler(
    	filename = 'report.pdf',
    	content = function(file) {
    		pdf(file = file, width=8.5, height=11)
      			
    		models = getBackTest()
    		
    		plota.control$col.x.highlight = col.add.alpha('green',50)
    		plotbt.custom.report(models, trade.summary = T, x.highlight = models$market.filter$highlight)
				plota.add.copyright()
      		
		    dev.off()
    	}
	)	
		
	 output$downloadData <- downloadHandler(
	filename = function() {
	  paste('filter-', Sys.Date(), '.csv', sep='')
   },
	  content = function(file) {
	    write.csv(makefilterTable()$b, file,col.names = FALSE,row.names = FALSE)
  }
  )
	 
	
    #*****************************************************************
    # Update status message 
    #******************************************************************    
	output$status <- reactiveUI(function() {
		out = tryCatch( getData(), error=function( err ) paste(err))	    				
		if( is.character( out ) ) 
			HTML(paste("<b>Status</b>: <b><font color='red'>Error:</font></b>",out))
		else
			HTML("<b>Status</b>: <b><font color='green'>Ok</font></b>")		
	})
	
	
})
