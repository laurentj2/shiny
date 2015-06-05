#' Simple random sampling
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/sampling.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param smp_var The variable to sample from
#' @param smp_sample_size Number of units to select
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param smp_print_full Print full sampling frame. Default is TRUE
#'
#' @return A list of variables defined in sampling as an object of class sampling
#'
#' @examples
#' result <- sampling("rndnames","Names",10)
#'
#' @seealso \code{\link{summary.sampling}} to summarize results
#' @export
sampling <- function(dataset) {

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
      
      
      
    }, error = function(e) { stop(paste('Problem running Back Test:', e)) })
  })
  
  
  
  
}


#' Summary method for the sampling function
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/sampling} for an example in Radiant
#'
#' @param object Return value from \code{\link{sampling}}
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' set.seed(1234)
#' result <- sampling("rndnames","Names",10)
#' summary(result)
#'
#' @seealso \code{\link{sampling}} to generate the results
#'
#' @export
summary.sampling <- function(object, ...) {
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
      
      
      
    }, error = function(e) { stop(paste('Problem running Back Test:', e)) })
  })
  
  
  
  
}
