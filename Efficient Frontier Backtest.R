Sys.setLanguage("en")
require(quantmod)
require(magrittr)
library(tidyverse)
options(digits = 4)

# Run with "Optimal Investment Portfolio 2.0 .R"

# We've analyzed the ten stocks from 2015 to 2025.
# Now, we ask ourselves the question: given the results from our analysis, 
# were an investor to choose an efficient portfolio, how would it perform
# in the future?

# To test this, we split our data into two time frames: training and evaluation.

# Set parameters
training.start <- as.Date("2015-01-01")
training.end <- as.Date("2022-12-31")
training.end.index <- max(which(as.Date(index(prices)) <= training.end))

evaluation.start <- as.Date(training.end + 1)
evaluation.start.index <- min(which(as.Date(index(prices)) > evaluation.start))

training.prices <- prices[1 : training.end.index]

evaluation.prices <- prices[evaluation.start.index : nrow(prices)]

evaluation.benchmark <- download.prices(benchmark, from = evaluation.start)

# Compute logarithmic returns and statistics

training.logreturns <- logret(training.prices)

evaluation.logreturns <- logret(evaluation.prices)

evaluation.benchmark.logreturns <- logret(evaluation.benchmark)

training.stats <- stat.estimation(returns = training.logreturns)

# Run Monte Carlo, 10000 observations:

training.simulation <- simulation(N = N,
                                  mu = training.stats$mu,
                                  covariance = training.stats$covariance)

# Compute the frontier from Monte Carlo simulation:

training.frontier <- frontier.compute(simulation.sd = training.simulation$sd,
                                      simulation.ER = training.simulation$ER,
                                      simulation.weights = training.simulation$weights,
                                      stat.covariance = training.stats$covariance,
                                      stat.mu = training.stats$mu)

# Using the S&P500 as a benchmark portfolio, compute a portfolio
# that strictly outperforms the index, we call it D.

training.dom.portfolio <- dominate.benchmark(frontier.weights = training.frontier$weights,
                                             frontier.sd = training.frontier$sd,
                                             benchmark.sd = benchmark.stats$stdev)


# Calculate outperformance of the portfolio that theoretically dominates the benchmark:
as.matrix(training.dom.portfolio) %*% (as.matrix(training.stats$mu)) * 252 - benchmark.stats$mu * 252
# Our portfolio seems to outperform the S&P by 2% annually.

# Let's evaluate our training: if we were to invest $100 in D on the day of 
# evaluation and we held it until today, will we outperform the S&P500?

# Set initial investment amount:

initial_investment <- 100

# Create a performance calculator: for each day, calculate the value of the
# portfolio, given an initial investment.
# investment should be a number,
# weights is the 1x10 matrix of weights
# logreturns is the table of evaluation logreturns

performance.calculate <- function(investment, weights, logreturns){
  
  performance <- matrix(NA,
                        nrow = nrow(logreturns),
                        ncol = ncol(weights))
  rownames(performance) <- index(logreturns)
  performance[1,] <- investment * unlist(weights)
  for (i in 2:nrow(logreturns)){
    performance[i,] <- performance[i-1,] * exp(logreturns[i])
  }
  
  value <- apply(performance, 1, sum)
  value <- xts(value, order.by = as.Date(rownames(logreturns)))
  return(value)
}

portfolio.performance <- performance.calculate(initial_investment,
                                               training.dom.portfolio,
                                               evaluation.logreturns)

# Calculating the S&P's performance is trivial: using the sum property
# of logreturns:

benchmark.performance <- xts(initial_investment * exp(cumsum(evaluation.benchmark.logreturns)),
                            order.by = as.Date(rownames(evaluation.benchmark.logreturns)))

portfolio.volatility <- round(sd(evaluation.logreturns) * sqrt(252),3)
benchmark.volatility <- round(sd(evaluation.benchmark.logreturns) * sqrt(252),3)


plot.data <- data.frame(portfolio.performance,
                        date = index(portfolio.performance),
                        value = as.numeric(portfolio.performance),
                        benchmark = as.numeric(benchmark.performance))

portfolio.final <- tail(portfolio.performance,1)
benchmark.final <- tail(benchmark.performance,1)
outperformance <- 100 * (portfolio.final/benchmark.final - 1)

backtest.plot <- ggplot(data = plot.data,
       aes(x = date)) +
  geom_line(aes(y = value, colour = "Optimal Portfolio")) +
  geom_line(aes(y = benchmark, colour = "S&P500")) +
  theme_bw(base_family = "serif") +
  labs(title = "Optimal Portfolio vs S&P500 performance, Backtest",
       subtitle = paste(evaluation.start,"-",Sys.Date()),
       x = "Date",
       y = "Portfolio Value",
       colour = "",
       ) +
  theme(legend.position = "bottom") +
  labs(caption = paste("Volatilities (annualized): Portfolio =", portfolio.volatility,
                       "| S&P500 =", benchmark.volatility)
  )



if (portfolio.final > benchmark.final) {
  backtest.plot <- backtest.plot +
    labs(
      caption = paste("Volatilities (annualized): Portfolio =", portfolio.volatility,
                      "| S&P500 =", benchmark.volatility, "\n",
        "The portfolio outperforms the S&P 500 by",
        round(outperformance, 2),
        "%"
      )
    )
} else {
  backtest.plot <- backtest.plot +
    labs(
      caption = paste("Volatilities (annualized): Portfolio =", portfolio.volatility,
                      "| S&P500 =", benchmark.volatility, "\n",
        "The portfolio underperforms the S&P 500 by",
        round(-outperformance, 2),
        "%"
      )
    )
}

backtest.plot

