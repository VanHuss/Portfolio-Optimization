Efficient Frontier Portfolio Optimization

A quantitative finance project in R implementing Modern Portfolio Theory and testing an optimized portfolio out-of-sample.

METHODOLOGY

Historical daily prices are obtained for ten equities from 2015 onward. Logarithmic returns, expected returns and covariance matrices are estimated from the training sample. 10,000 long-only portfolios are generated through Monte Carlo simulation and used to approximate the efficient frontier. The analytical Markowitz frontier is also calculated for comparison.

The portfolio selected from the training period is then held during a separate evaluation period and compared with the S&P 500.

Tools: R, quantmod, tidyverse, ggplot2, matrix algebra.

Important limitation: The backtest represents a single historical train/test split and therefore does not establish that the strategy will outperform in future periods.
