###############################
# Pre-factor analysis
###############################
# list of function arguments
pf_args <- as.list(formals(pre_factor))

# list of function inputs selected by user
pf_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for (i in names(pf_args))
    pf_args[[i]] <- input[[i]]
  if (!input$show_filter) pf_args$data_filter = ""
  pf_args
})

output$ui_pf_var <- renderUI({
	isNum <- "numeric" == .getclass() | "integer" == .getclass()
 	vars <- varnames()[isNum]
  selectInput(inputId = "pf_var", label = "Variables:", choices = vars,
  	selected = state_multiple("pf_var",vars),
  	multiple = TRUE, size = min(15, length(vars)), selectize = FALSE)
})

output$ui_pre_factor <- renderUI({
  list(
  	wellPanel(
	  	uiOutput("ui_pf_var")
	  ),
    help_and_report(modal_title = "Backtest engine",
                    fun_name = "pre_factor",
                    help_file = inclMD("tools/help/pre_factor.md"))
 	)
})


output$pre_factor <- renderUI({

	
	#register_plot_output("plot_pre_factor", ".plot_pre_factor",)

	# one output with components stacked
	pf_output_panels <- tagList(
      tabPanel("Plot", plotOutput("plot_pre_factor", height = "100%"))
  )

	stat_tab_panel(menu = "Factor",
	              tool = "Pre-factor",
	              tool_ui = "ui_pre_factor",
	             	output_panels = pf_output_panels)
})


output$plot_pre_factor <- reactivePlot(function() {
  models = plot.pre_factor()
  
  plota.control$col.x.highlight = col.add.alpha('green',50)
  plotbt.custom.report.part1(models, x.highlight = models$market.filter$highlight)  	
  strategy.performance.snapshoot(models, T, 'Backtesting Asset Allocation portfolios');
}, height = 400, width = 600)



