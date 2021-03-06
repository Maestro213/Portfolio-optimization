library(timeSeries)
library(timeDate)
library(quantmod)
library(PortfolioAnalytics)

#The function computes the log returns of a portfolio, where d is a dataframe or other object
log_return <-function(d) {
  for (i in 2:ncol(d)){
    d[,i] <-c(diff(log(d[,i])))
  }
  d <- na.omit(d)
  return(d)
}

#importing the data as Time Series object
closing_prices <- as.timeSeries(read.csv("/Users/fox2/Downloads/prices.txt"))
class(closing_prices)
#Subseting the data to the period 2015-2016
closing_prices2016 <- window(closing_prices, start = "2016-01-01",end="2016-12-31")
#Choose randomly different assets for sample-portfolio
#Since my laptop cann't process high amount of the data I will run a optimization on a part(half) of the given Assets
closing_prices2016_sample <- cbind(closing_prices2016[,sampleColnames(closing_prices2016,25)])

sample(closing_prices2016s1,5)

#Calculating the returns
portfolio_returns <- na.omit(ROC(closing_prices2016_sample))

#Setting the portfolio objectives and constrains
prf <-portfolio.spec(colnames(portfolio_returns))
#Constraints
prf <- add.constraint(prf,type="full_investment")#sum of weights must be 1
#prf<- add.constraint(prf,type="box",min_sum=0.99,max_sum=1.01) #more flexible constraine for random, GenSA and others methods
prf <- add.constraint(prf, type="long_only")# no short positions
prf <- add.constraint(prf, type = "box", min = 0, max=0.5)

#Objectives
prf <- add.objective(prf,type="return",name="mean")
prf <- add.objective(prf,type ="risk",name="StdDev")

#P-optimization
optimal_weights <- optimize.portfolio(portfolio_returns, prf,optimize_method = "ROI",trace=TRUE)
print(optimal_weights)
#Optimization with DEoptim was leaven due to the low processing power of the laptop
#optimal_weights_de <- optimize.portfolio(portfolio_returns,prf ,optimize_method = "DEoptim")
#print(optimal_weights_de)

#plot the efficient frontier
efficient_frontier <- extractEfficientFrontier(optimal_weights, match.col = "StdDev", n.portfolios = 25,
risk_aversion = NULL)
chart.EfficientFrontier(efficient_frontier, match.col = "StdDev", n.portfolios = 25,
cex.axis = 0.8, element.color = "darkgray", main = "Efficient Frontier",
RAR.text = "SR", rf = 0, tangent.line = TRUE, cex.legend = 0.8,
chart.assets = TRUE, labels.assets = TRUE, pch.assets = 21,
cex.assets = 0.8)
