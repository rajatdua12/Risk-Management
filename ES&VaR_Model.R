library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)

startDate <- "1990-01-02"
endDate <- "2000-12-29"
tickers <- c("AXP", "C")

portfolioPrices <- NULL
for(ticker in tickers) {
  portfolioPrices <- cbind(portfolioPrices,
                           getSymbols.yahoo(ticker, from=startDate, to = endDate, periodicity = 'daily', 
                                            auto.assign=FALSE)[,6])
}

portfolioReturns <- na.omit(ROC(portfolioPrices))
colnames(portfolioReturns) <- tickers

portf <- portfolio.spec(colnames(portfolioReturns))

portf <- add.constraint(portf, type="weight_sum", min_sum=0.99, max_sum=1.01)
portf <- add.constraint(portf, type="transaction_cost", ptc = 0.001)
portf <- add.constraint(portf, type="box", min=-.30, max=1.30)  #Long Short Portfolio
portf <- add.objective(portf, type="return", name="mean")
portf <- add.objective(portf, type="risk", name="StdDev", target=0.005)

rp <- random_portfolios(portf, 10000, "sample")

opt_rebal <- optimize.portfolio.rebalancing(portfolioReturns,
                                            portf,
                                            optimize_method="random",
                                            rp=rp,
                                            rebalance_on="months",
                                            training_period=1,
                                            rolling_window=10)

equal_weight <- rep(1 / ncol(portfolioReturns), ncol(portfolioReturns))
benchmark <- Return.portfolio(portfolioReturns, weights = equal_weight)
colnames(benchmark) <- "Benchmark Portfolio"

sp500prices <- getSymbols.yahoo("SPY", from=startDate, to = endDate, periodicity = 'daily', auto.assign=FALSE)[,6]
sp500Rets <- na.omit(ROC(sp500prices))
sp500Rets <- as.xts(sp500Rets)
colnames(sp500Rets) <- "SPY Portfolio"

chart.Weights(opt_rebal, main="Rebalanced Weights Over Time")

rebal_weights <-extractWeights(opt_rebal)
rebal_returns <- Return.portfolio(portfolioReturns, weights=rebal_weights)

rets_df <- cbind(rebal_returns, benchmark, sp500Rets)

charts.PerformanceSummary(rets_df, main="P/L Over Time")

#VAR Model
VAR.Hist <- VaR(portfolioReturns, p=0.95, weights = NULL, portfolio_method = "single", method = "historical")
VAR.Gaus <- VaR(portfolioReturns, p=0.95, weights = NULL, portfolio_method = "single", method = "gaussian")
VAR.Mod <- VaR(portfolioReturns, p=0.95, weights = NULL, portfolio_method = "single", method = "modified")
All.VAR <- data.frame(rbind(VAR.Hist,VAR.Gaus,VAR.Mod))

rownames(All.VAR) <- c("HistVAR", "GausVAR", "ModVAR")
weights <- apply(tail(rebal_weights, 1), 2, as.numeric)

PortVAR.Hist <- VaR(portfolioReturns, p=0.95, weights = weights, portfolio_method = "component", method = "historical")$hVaR
PortVAR.Gaus <- VaR(portfolioReturns, p=0.95, weights = weights, portfolio_method = "component", method = "gaussian")$VaR
PortVAR.Mod <- VaR(portfolioReturns, p=0.95, weights = weights, portfolio_method = "component", method = "modified")$MVaR

All.VAR$Portfolio <- 0
All.VAR$Portfolio <- c(PortVAR.Hist, PortVAR.Gaus, PortVAR.Mod)

All.VAR <- abs(All.VAR)

All.VAR$Type <- c("HistVAR", "GausVAR", "ModVAR")

#ES Model
ES.Hist <- ES(portfolioReturns, p=0.95, weights = NULL, portfolio_method = "single", method = "historical")
ES.Gaus <- ES(portfolioReturns, p=0.95, weights = NULL, portfolio_method = "single", method = "gaussian")
ES.Mod <- ES(portfolioReturns, p=0.95, weights = NULL, portfolio_method = "single", method = "modified")
All.ES <- data.frame(rbind(ES.Hist,ES.Gaus,ES.Mod))

rownames(All.ES) <- c("HistES", "GausES", "ModES")
weights <- apply(tail(rebal_weights, 1), 2, as.numeric)

PortES.Hist <- ES(portfolioReturns, p=0.95, weights = weights, portfolio_method = "component", method = "historical")$`-r_exceed/c_exceed`
PortES.Gaus <- ES(portfolioReturns, p=0.95, weights = weights, portfolio_method = "component", method = "gaussian")$ES
PortES.Mod <- ES(portfolioReturns, p=0.95, weights = weights, portfolio_method = "component", method = "modified")$MES

All.ES$Portfolio <- 0
All.ES$Portfolio <- c(PortES.Hist, PortES.Gaus, PortES.Mod)

All.ES <- abs(All.ES)

All.ES$Type <- c("HistES", "GausES", "ModES")

library(reshape2)
library("ggplot2")

plotVAR <- melt(All.VAR, variable.name = "Ticker", value.name = "VaR")
ggplot(plotVAR, aes(x = Type, y = VaR, fill = Ticker)) + geom_bar(stat = "identity", position = "dodge")

plotES <- melt(All.ES, variable.name = "Ticker", value.name = "ES")
ggplot(plotES, aes(x = Type, y = ES, fill = Ticker)) + geom_bar(stat = "identity", position = "dodge")
