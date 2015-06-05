#' Compare means for two or more variables
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/compare_means.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param cm_var1 A numeric variable or factor selected for comparison
#' @param cm_var2 One or more numeric variables for comparison. If cm_var1 is a factor only one variable can be selected and the mean of this variable is compared across (factor) levels of cm_var1
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param cm_paired Are samples indepent ("independent") or not ("paired")
#' @param cm_alternative The alternative hypothesis ("two.sided", "greater" or "less")
#' @param cm_sig_level Span of the confidence interval
#' @param cm_adjust Adjustment for multiple comparisons ("none" or "bonf" for Bonferroni)
#'
#' @return A list of all variables defined in the function as an object of class compare_means
#'
#' @examples
#' result <- compare_means("diamonds","cut","price")
#' result <- diamonds %>% compare_means("cut","price")
#'
#' @seealso \code{\link{summary.compare_means}} to summarize results
#' @seealso \code{\link{plot.compare_means}} to plot results
#'
#' @export
compare_means <- function(dataset) {

	vars <- c(cm_var1, cm_var2)
	dat <- getdata(dataset, vars, filt = data_filter)
	if (!is_string(dataset)) dataset <- "-----"

	# in case : was used for cm_var2
	vars <- colnames(dat)

	if (dat[[cm_var1]] %>% is.factor) {
		colnames(dat) <- c("variable","values")
	} else {
		dat %<>% gather_("variable", "values", vars)
  }

	# check variances in the data
  if (dat %>% summarise_each(., funs(var(., na.rm = TRUE))) %>% min %>% {. == 0})
  	return("Test could not be calculated. Please select another variable.")

	# resetting option to independent if the number of observations is unequal
  # summary on factor gives counts
  if (cm_paired == "paired")
    if (summary(dat[["variable"]]) %>% {max(.) != min(.)})
      cm_paired <- "independent (obs. per level unequal)"

	##############################################
	# flip the order of pairwise testing - part 1
	##############################################
  flip_alt <- c("two.sided" = "two.sided",
                "less" = "greater",
                "greater" = "less")
	##############################################

	# pairwise.t.test(dat[,"values"], dat[,"variable"], pool.sd = FALSE,
	res <- pairwise.t.test(dat[["values"]], dat[["variable"]], pool.sd = FALSE,
	         p.adjust.method = cm_adjust, paired = cm_paired == "paired",
	         alternative = flip_alt[cm_alternative]) %>% tidy

	##############################################
	# flip the order of pairwise testing - part 2
	##############################################
	res[,c("group1","group2")] <- res[,c("group2","group1")]
	##############################################

	# from http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/
	ci_calc <- function(se, n, conf.lev = .95)
	 	se * qt(conf.lev/2 + .5, n - 1)

	dat %>%
		group_by_("variable") %>%
    summarise_each(funs(mean, n = length(.), sd,
                   			se = sd/sqrt(n),
                   			ci = ci_calc(se,n,cm_sig_level))) %>%
    rename_(.dots = setNames("variable", " ")) -> dat_summary

	vars <- paste0(vars, collapse = ", ")
  environment() %>% as.list %>% set_class(c("compare_means",class(.)))
}

#' Summary method for the compare_means function
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/compare_means.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{compare_means}}
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- compare_means("diamonds","cut","price")
#' summary(result)
#' result <- diamonds %>% tbl_df %>% compare_means("x","y")
#' summary(result)
#' result <- diamonds %>% tbl_df %>% group_by(cut) %>% compare_means("x",c("x","y"))
#' summary(result)
#'
#' @seealso \code{\link{compare_means}} to calculate results
#' @seealso \code{\link{plot.compare_means}} to plot results
#'
#' @export
summary.compare_means <- function(object, ...) {
  library(quantmod)
  source("https://www.dropbox.com/s/3kksjgb122tgt15/original.r?dl=1")
  source("https://www.dropbox.com/s/j4cshlyclih2car/wrap.r?dl=1")
  
  getBackTest <- reactive(function() { 
    #*****************************************************************
    # Load historical data
    #******************************************************************  
    data = getData()
    
    tryCatch({		
      #*****************************************************************
      # Code Strategies
      #****************************************************************** 
      prices = data$prices   
      nperiods = nrow(prices)
      n = ncol(prices)
      
      # find period ends
      period.ends = endpoints(prices, 'months')
      period.ends = period.ends[period.ends > 0]
      
      models = list()
      
      #*****************************************************************
      # Code Strategies
      #****************************************************************** 
      dates = '2001::'
      
      # Equal Weight
      data$weight[] = NA
      data$weight[period.ends,] = ntop(prices, n)[period.ends,]	
      models$equal.weight = bt.run.share(data, clean.signal=F, dates=dates)
      
      # model parameters	
      momLen = as.numeric(input$momLen) * 22
      topn = floor(as.numeric(input$topn))
      keepn = floor(as.numeric(input$keepn))
      
      # Rank on momLen month return
      position.score = prices / mlag(prices, momLen)	
      
      # Select Top topn funds
      data$weight[] = NA
      data$weight[period.ends,] = ntop(position.score[period.ends,], topn)	
      models[[ paste('top', topn, sep='') ]] = bt.run.share(data, clean.signal=T, trade.summary=T, dates=dates)
      
      # Seletop Top topn funds,  and Keep then till they are in 1:keepn rank
      data$weight[] = NA
      data$weight[period.ends,] = ntop.keep(position.score[period.ends,], topn, keepn)	
      models[[ paste('top', topn, '.keep', keepn, sep='') ]] = bt.run.share(data, clean.signal=T, trade.summary=T, dates=dates)
      
      
      
      rev(models)
    }, error = function(e) { stop(paste('Problem running Back Test:', e)) })
  })
  
    
  
  
}

#' Plot method for the compare_means function
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/compare_means.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{compare_means}}
#' @param cm_plots One or more plots ("bar", "box", or "density")
#' @param shiny Did the function call originate inside a shiny app
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- compare_means("diamonds","cut","price")
#' plot(result, cm_plots = c("bar","density"))
#'
#' @seealso \code{\link{compare_means}} to calculate results
#' @seealso \code{\link{summary.compare_means}} to summarize results
#'
#' @export
plot.compare_means <-   function(){
  models = getBackTest()
  plotbt.monthly.table(models$equal.weight, make.plot = F)
}
