# New Script to use the source and save strategy function. Will run the dual moving average
# strategy with the getsymbols function from the desired source file, plot the equity curves
# and compare to the benchmark SP500 index.

# Load Quantstrat and required packages
require(quantstrat)

# Set the system time to UTC
Sys.setenv(TZ="UTC")

# Get DMA include ile with parameters and Functions
source(paste(getwd(),"/TurtleInclude.R",sep=""))

# Strategy specific variables
breakout  <- 200  # Breakout Donchian Channel Length
stop      <- 180  # Stop Donchian Channel Length
atrMult   <- 3    # Multiple of ATR to use for stoploss and order sizing calculation
riskpct   <- 0.02 # Amount of trade balance to risk per trade

# Get all symbols, uncomment what you need
# source(paste(getwd(),"/GetCommData.R",sep=""))
# source(paste(getwd(),"/GetCurrData.R",sep=""))
 source(paste(getwd(),"/GetEqData.R",sep=""))

# Change symbol variable here if needed
# symbol <- ""

# If run previously, run this code from here down
rm.strat(portfolio.st, silent = FALSE)
rm.strat(account.st, silent = FALSE)

# Initialize the portfolio, account and orders. Starting equity and assuming data post 1995.
initPortf(portfolio.st, symbols = symbol)
initAcct(account.st, portfolios = portfolio.st, initEq = initEq)
initOrders(portfolio = portfolio.st)

# Define the strategy with a position limit to prevent multiple trades in a direction
for (sym in symbol){
  addPosLimit(portfolio = portfolio.st, sym, timestamp="2000-01-01", maxpos=100, 
              longlevels = 1, minpos=-100, shortlevels = 1)
}

load.strategy("Turtle")

# Enable Stop Rules
enable.rule(strat,type = "chain",label = "StopLONG")
enable.rule(strat,type = "chain",label = "StopSHORT")

# Apply the strategy assigning the output to a variable out
out <- applyStrategy(strategy=strat , portfolios=portfolio.st)

updatePortf(Portfolio = portfolio.st) # Update the portfolio, acct and endeq
updateAcct(name = account.st)
updateEndEq(account.st)

# Plot the charts for each symbol
for (sym in symbol){
  chart.Posn(Portfolio = portfolio.st, Symbol = sym 
             ) # Chart the position 
}

stats <- tradeStats(portfolio.st)

# Plot the returns vs buy and hold
eq1 <- getAccount(account.st)$summary$End.Eq
rt1 <- Return.calculate(eq1,"log")
getSymbols("^GSPC", from = '1995-01-01')
rt2 <- periodReturn(GSPC, period = "daily")
returns <- cbind(rt1,rt2)
colnames(returns) <- c("DMA","SP500")
chart.CumReturns(returns,colorset=c(2,4),legend.loc="topleft",
                 main="Moving Average Crossover to Benchmark Comparison",ylab="cum return",xlab="",
                 minor.ticks=FALSE)