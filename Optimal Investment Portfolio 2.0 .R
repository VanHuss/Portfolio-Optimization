# Run with "Efficient Frontier out of sample test.R"
# All initial imported data must be daily

# Given a set of stocks, how should an investor allocate wealth to maximize 
# expected return for a given level of risk?

Sys.setLanguage("en")
require(quantmod)
require(magrittr)
library(tidyverse)
options(digits = 4)

# Set parameters ###################################################

date <- "2015-01-01"  # Starting date of observations        

tickers <- c(
  "AAPL","MSFT","GOOG","AMZN","META",
  "JPM","JNJ","XOM","PG","NVDA"
)  # Chosen securities

benchmark <- "^GSPC"  # Benchmark security

td <- 252 # Trading Days: in this case, 1 year = td trading days

N <- 10000 # Number of Monte Carlo simulations

####################################################################

# Function to download adjusted closing prices into an xts

download.prices <- function(tickers, from, to = Sys.Date()){
  prices <- do.call(cbind,
                    lapply(tickers,
                           function(ticker) Ad(getSymbols(ticker,
                                                          from = from,
                                                          to = to,
                                                          auto.assign = FALSE))))
  colnames(prices) <- tickers
  return(prices)
}


# Price data is stored in this xts object
prices <- download.prices(tickers, date)

benchmark.prices <- download.prices(benchmark, date)

# Compute logarithmic returns of each security
logret <- function(prices){
  returns <- as.matrix(apply(prices, 2, function(x){diff(log(x))}))
  colnames(returns) <- colnames(prices)
  rownames(returns) <- as.character(index(prices)[-1])
  return(returns)
}

logreturns <- logret(prices)

benchmark.logreturns <- logret(benchmark.prices)

# Estimate mean, covariance, correlation

stat.estimation <- function(returns){
  mu <- apply(returns, 2, mean)
  covariance <- cov(returns)
  corr <- cor(returns)
  stdev = sqrt(diag(covariance))
  max.mu <- paste("The stock with the highest Expected return is ",
                  names(mu[which.max(mu)]),
                  "with annual ER = ",
                  mu[which.max(mu)] * td)
  return(list(mu = mu, covariance = covariance,
              correlation = corr, stdev = stdev, max.mu = max.mu))
}

# Equivalent manual computation of covariance and correlation:
# cov <- (t(rlog - mu) %*% (rlog - mu)) / (nrow(rlog) - 1)
# all.equal(cov, cov(rlog))

# lambda <- solve(sqrt(diag(diag(cov))))
# cor <- lambda %*% cov %*% lambda
# rownames(cor) <- colnames(cor) <- tickers
# all.equal(cor, cor(rlog))

stats <- stat.estimation(returns = logreturns)

benchmark.stats <- stat.estimation(benchmark.logreturns)

# Highest return
names(stats$mu[which.max(stats$mu)])

# Most to least volatile stock
sort(diag(stats$covariance), decreasing = TRUE)

# Monte Carlo simulation, N repetitions:

simulation <- function(N, mu, covariance){
  w <- replicate(N,
                 {
                   w <- runif(length(tickers))
                   w <- w/sum(w)
                 })
  rownames(w) <- tickers
  
  ER <- t(w) %*% mu
  
  V <- colSums((covariance %*% w) * w)
  # This is equivalent to diag(t(w) %*% covariance %*% w), but more efficient
  
  sd <- sqrt(V)
  
  return(list(weights = w, ER = ER, V = V, sd = sd))
}

simul <- simulation(N, mu = stats$mu, covariance = stats$covariance)



# Calculating the frontier:

frontier.compute <- function(simulation.sd, simulation.ER, simulation.weights,
                             stat.covariance, stat.mu){
  
  ind <- order(simulation.sd)
  
  sdw.ord <- simulation.sd[ind]
  ERw.ord <- simulation.ER[ind]
  
  frontier.sd <- sdw.ord[cummax(ERw.ord) == ERw.ord]
  frontier.ER <- ERw.ord[cummax(ERw.ord) == ERw.ord]
  
  frontier.ind <- ind[cummax(ERw.ord) == ERw.ord]
  
  frontier.w <- data.frame(weights = t(simulation.weights[,frontier.ind]))
  
  # Compute the Minimum Variance Portfolio
  MVP <- data.frame(weights = simulation.weights[,which.min(simulation.sd)],
                    ER = simulation.ER[which.min(simulation.sd)],
                    sd = simulation.sd[which.min(simulation.sd)])
  
  # Compute the performance of the equally weighted portfolio
  equal.w <- as.matrix(rep(1/length(tickers), length(tickers)))
  equal.ER <- t(equal.w) %*% stat.mu
  equal.sd <- sqrt(t(equal.w) %*% stat.covariance %*% equal.w)
  
  equal <- list(weights = equal.w, ER = equal.ER, sd = equal.sd)
  
  return(list(sd = frontier.sd, ER = frontier.ER, weights = frontier.w,
         MVP = MVP, equal = equal))
}

frontier <- frontier.compute(simulation.sd = simul$sd,
                             simulation.ER = simul$ER,
                             simulation.weights = simul$weights,
                             stat.covariance =  stats$covariance,
                             stat.mu = stats$mu)


# The S&P500 index is dominated by the frontier portfolios.
# Let's find which frontier portfolio performs better at the same volatility

dominate.benchmark <- function(frontier.weights, frontier.sd, benchmark.sd){
   frontier.weights[which.min(abs(frontier.sd - benchmark.sd)),]
}

w.dombenchmark <- dominate.benchmark(frontier.weights = frontier$weights,
                                     frontier.sd = frontier$sd,
                                     benchmark.sd = benchmark.stats$stdev)

# Let's compare our Monte Carlo simulation with the mathematical solution
# to the mean-variance minimization problem:

analytical.frontier <- function(){
  S <- as.matrix(rep(1, length(tickers)))
  Vinv <- solve(stats$covariance)
  
  A <- as.numeric(t(S) %*% Vinv %*% S)
  B <- as.numeric(t(S) %*% Vinv %*% stats$mu)
  C <- as.numeric(t(stats$mu) %*% Vinv %*% stats$mu)
  D <- A*C - B^2
  
  ER.grid <- seq(from = min(stats$mu),
                 to = max(stats$mu),
                 length.out = 1000)
  
  sd.grid <- sqrt((A * ER.grid^2 - 2 * B * ER.grid + C)/D)
  
  grid.keep <- sd.grid <= max(simul$sd)
  
  sd.grid <- sd.grid[grid.keep]
  ER.grid <-  ER.grid[grid.keep]
  
  return(list(sd = sd.grid, ER = ER.grid))
}

frontier.math <- analytical.frontier()

# Save results in ggplot
# Create data frame with all variables to plot
plotdata <- list(w.data = data.frame(
                       sd = simul$sd * sqrt(td),
                       ER = simul$ER * td
                       ),
                 frontier.data = data.frame(
                       sd = frontier$sd * sqrt(td),
                       ER = frontier$ER * td
                       ),
                 frontier.math = data.frame(
                       sd = frontier.math$sd * sqrt(td),
                       ER = frontier.math$ER * td
                       ),
                 MVP = data.frame(
                       sd = frontier$MVP$sd * sqrt(td),
                       ER = frontier$MVP$ER * td
                       ),
                 SP500 = data.frame(
                       ER = benchmark.stats$mu * td,
                       sd = benchmark.stats$stdev * sqrt(td)
                       ),
                 equal = data.frame(
                       ER = frontier$equal$ER * td,
                       sd = frontier$equal$sd * sqrt(td)
                       )
  )
                      
ggplot(data = plotdata$w.data, mapping = aes(sd, ER)) +
  geom_point(size = 0.2, alpha = 0.6) +
  geom_point(data = plotdata$MVP,
             aes(sd, ER, color = "Minimum Variance Portfolio (Simulated)"),
             size = 2) +
  geom_point(data = plotdata$SP500,
             aes(sd, ER, color = "S&P500 Benchmark"),
             size = 2) +
  geom_point(data = plotdata$equal,
             aes(sd, ER, color = "Equal Weight Portfolio"),
             size = 2) +
  geom_line(data = plotdata$frontier.data,
            aes(sd, ER, color = "Simulated Frontier")) +
  geom_point(data = plotdata$frontier.math,
            aes(sd, ER, color = "Theoretical Frontier"),
            size = 0.4) +
  theme_minimal(base_family = "serif") +
  labs(title =  "Return to Volatility Scatterplot (annual)",
       subtitle = paste("Monte Carlo simulation,", N, "portfolios"),
       y = ("Annualized Expected Return"),
       x = ("Annualized Volatility")) +
  scale_x_continuous(breaks = seq(0.15, 0.3, by = 0.02)) +
  scale_y_continuous(breaks = seq(0.1, 0.5, by = 0.03)) +
  scale_color_manual(name = "",
                     values = c(
    "Minimum Variance Portfolio (Simulated)" = "blue",
    "S&P500 Benchmark" = "orange",
    "Equal Weight Portfolio" = "green",
    "Simulated Frontier" = "red",
    "Theoretical Frontier"  = "purple"
))
