library(curl)
curl_download('https://github.com/systematicinvestor/SIT/raw/master/SIT.tar.gz', 'sit',mode = 'wb',quiet=T)
install.packages('sit', repos = NULL, type='source')
library(quantmod)
library(SIT)
### Define symbol to test here
symbol = "JO"
data <- new.env()
tickers = spl(symbol)
getSymbols(tickers, src = 'yahoo', from = '2005-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
## LOAD DATA
#### GA VERDER
bt.prep(data, align='keep.all', dates='2005::2015')
prices = data$prices
prices = ROC(prices)
prices[1:3,]=0
prices <- round(as.data.frame(prices),12)
x<-prices
x[,2] <- x[,1]
len<-length(x[,1])
filter <- read.csv("C:/Users/Administator/Desktop/test.csv")
b <- as.matrix(filter$V1)
L<-length(b)
xf<-NULL
for (i in L:len) #i<-20
{
  # I-MDFA
  xf[i]<-0
  for (j in 2:length(x[1,]))  #j<-2
    xf[i]<-xf[i]+b[,j-1]%*%x[i:(i-L+1),j]
}
prices$signal <- xf

str2expr<-function(x){eval(parse(text=x), envir=parent.frame() )} 
#TEST system
## Equalt weight
models = list()
data$weight[] = NA
weight <- sprintf("data$weight$%s = 5/5", symbol)
str2expr(weight)
models$EW  = bt.run(data, commission = 0.00)  
data$weight[] = NA
## Filtered Signal test
trigger<- ifelse(prices$signal<(-0.000),-1,ifelse(prices$signal>0.000,1,0))
weight<- sprintf("data$weight$%s = trigger", symbol)
str2expr(weight)
## Define commision costs
commission = list(cps = 0.000, fixed = 1.0, percentage = 0.0) 
models$GJ = bt.run.share(data, commission = commission, capital = 10000, clean.signal=F)
strategy.performance.snapshoot(models, T, 'Backtesting Asset Allocation portfolios');
plotbt.custom.report.part2(models$GJ)   
### Display latest 2 signal bars in R log
tail(data$weight,2)

