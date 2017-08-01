# New Script to use the source and save strategy function. Will optimize the dual moving average
# strategy with the getsymbols function from the desired source file and the parameter sets
# specified and save the results to file on the fileserver for later analysis.

# Load Quantstrat and required packages
require(quantstrat)
require(doMC)

# Set the system time to UTC
Sys.setenv(TZ="UTC")

# Get Turtle include ile with parameters and Functions
source(paste(getwd(),"/TurtleInclude.R",sep=""))

# Strategy specific variables
riskpct <- 0.02 # percentage of equity to risk 
risk    <- 1

# Strategy specific variables
breakoutPset  <- seq(20, 200, by = 10)
stopPset      <- seq(20, 200, by = 10)
atrMultPset   <- seq(2, 10, by = 2)

# Get all symbols, uncomment what you need
 source(paste(getwd(),"/GetCommData.R",sep=""))
# source(paste(getwd(),"/GetCurrData.R",sep=""))
# source(paste(getwd(),"/GetEqData.R",sep=""))

# Change symbol variable here if needed
# symbol <- "CC"

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

# Add distributions and constraints
add.distribution(strategy = strat,
                 paramset.label = "Turtle_OPT",
                 component.type = "indicator",
                 component.label = "DonchBreak",
                 variable = list( n = breakoutPset ),
                 label = "DonchBreak"
)

add.distribution(strategy = strat,
                 paramset.label = "Turtle_OPT",
                 component.type = "indicator",
                 component.label = "DonchStop",
                 variable = list( n = stopPset ),
                 label = "DonchStop"
)

add.distribution(strategy = strat,
                 paramset.label = "Turtle_OPT",
                 component.type = "indicator",
                 component.label = "atrStopThresh",
                 variable = list( atr_mult=atrMultPset ),
                 label = "atr"
)

add.distribution.constraint(strategy = strat,
                            paramset.label = "Turtle_OPT",
                            distribution.label.1 = "DonchBreak",
                            distribution.label.2 = "DonchStop",
                            operator = ">",
                            label = "breakgtstop")

save.strategy("Turtle")

enable.rule(strat,type = "chain",label = "StopLONG")
enable.rule(strat,type = "chain",label = "StopSHORT")

# Register the cores for parralel procssing
registerDoMC(cores=detectCores())

# Now apply the parameter sets for optimization
out <- apply.paramset(strat, paramset.label = "Turtle_OPT",
                      portfolio=portfolio.st, account = account.st, nsamples=0, verbose = TRUE)

stats <- out$tradeStats

# Write to csv file for later analysis
out <- write.csv(stats,             
                 file = paste("/media/sjaoraid/strat/DMA/opt/TurtleCurrOpt",as.character(Sys.Date()),".csv", sep=""),
                 quote = FALSE, row.names = TRUE)
