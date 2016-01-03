
# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(

	headerPanel(""),

	# Sidebar with a slider input for number of observations
	sidebarPanel(   
		textInput("symbol", "Input Stock:", value = "SPY"),
		#textInput("cash", "Use other symbol as predictor(experimental):",value = "SPY"),		
		#dateRangeInput('dates',
		#               label = 'Date range input: yyyy-mm-dd',
		#               start = Sys.Date() - 1900, end = Sys.Date() 
		#),
		selectInput("oos1", strong("Training set begin bar"), choices =  seq(1,4000,by=25),selected=0),
		selectInput("oos", strong("Training set size"), choices =  seq(100,3000,by=25),selected=625),
		br(),
		selectInput("smaLen", strong("Filter length:"), choices =  seq(20,250,by=10),selected=100),
		selectInput("ub", strong("Low-pass filter setting:"), choices =  seq(1,60,by=1),selected=1),
		selectInput("up", strong("Upperband:"), choices =  seq(1,70,by=1),selected=10),
		selectInput("smooth", strong("Smoothing:"), choices =  seq(0,15,by=0.5),selected=0),
		selectInput("lag", strong("Realtime/predictive filtering:"), choices =  seq(-10,0,by=1),selected=-1),
		br(),
		selectInput("overfit", strong("Overfitting protection:"), choices =  seq(0,6,by=1),selected=6),
		br(),
		br(),
		submitButton("Run"),
		htmlOutput("status")
	),

 
	# Show a plot of the generated distribution
	mainPanel(
		tabsetPanel(
			tabPanel("Main", 
				plotOutput("strategyPlot"),
				br(),
				br(),
				br(),
				br(),
				br(),
				br(),
				br(),
				br(),
				br(),
				br(),
				br(),
				plotOutput("sidebysideTable"),
				br(),
				br(),
				br(),
				br(),
				br(),
				br(),
				br(),
				br(),
				br(),
				br(),
				br(),
				#h4("Last 20 Trades"),
				#tableOutput("tradesTable"),				
				#downloadButton("downloadReport", "Download Backtest Report"),
				downloadButton("downloadData", "Download Filter"),
				br(),
				br()	
			),			
        
			tabPanel("About",
				p('This application demonstrates how to back-test using a direct filtering strategy','
				The Market Filter strategy invests in stock while the stock price is above the
				moving average and goes in cash otherwise. The periods where Market Filter strategy is
				invested are highlighted with green.'
				),
								
				br(),
				
				strong('Author'), p('Michael Kapler', a('Systematic Investor Blog', href="http://systematicinvestor.wordpress.com", target="_blank")),
				
				br(),
				
				strong('Code'), p('Original source code for this application at',
				a('GitHub', href='https://github.com/systematicinvestor/SIT/Shiny/market.filter')),
				
				br(),
				
				strong('References'),
				p(HTML('<ul>'),
        			HTML('<li>'),'The web application is built with the amazing', a("Shiny.", href="http://www.rstudio.com/shiny/", target="_blank"),HTML('</li>'),
        			HTML('<li>'),a('SIR application by Samuel M. Jenness', href="http://glimmer.rstudio.com/smjenness/SIR/", target="_blank"),HTML('</li>'),
        			HTML('<li>'),a('SIR application code by Samuel M. Jenness', href="https://github.com/smjenness/Shiny/tree/master/SIR", target="_blank"),HTML('</li>'),
				HTML('</ul>'))
			)    
		)
	)
))

