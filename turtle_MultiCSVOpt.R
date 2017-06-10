# This strategy uses a dual donchian channels (breakout and close), to either a) go long if the close
# is above the breakout channel or b) short if the closee is below the breakout channel
# There is a rebalancing rule that enables us to compare being 100% invested in this strategy to 
# buy and hold. An ATR based stop is used which is implemented via a custom indicator. Single equity data from yahoo.
# ATR based order size function. Strategy was copied from DMA trend project initially

# Library and time zone setup
library(quantstrat)       # Required package for strategy back testing
library(doMC)       # For parrallel optimization
#library(rgl)              # Library to load 3D trade graphs
library(reshape2)         # Library to load 3D trade graphs
ttz<-Sys.getenv('TZ')     # Time zone to UTC, saving original time zone
Sys.setenv(TZ='UTC')

# Quantstrat general variables
strat        <- "Turtle"           # Give the stratgey a name variable
portfolio.st <- "portf"         # Portfolio name
account.st   <- "accnt"         # Account name
initEq       <- 100000          # this parameter is required to get pct equity rebalancing to work
csvDir       <- "/home/rjk/Financial/commodities_data" # Directory containing csv files
xtsDates     <- "2006/"        # Variable for the point in time you want your prices series to line up from

# Strategy specific variables
breakout  <- seq(20, 400, by = 20)
stop  <- seq(20, 400, by = 20)
atrMult <- seq(1, 5, by = 1)
riskpct <- 0.01

# Strategy Functions

# Custom indicator to generate the threshold multiplier to set an ATR based stop.
atrStopThresh <- function(HLC, n=14, atr_mult=2){
  ATR <- ATR(HLC = HLC, n)
  pctATR <- round((atr_mult*ATR$atr)/Cl(HLC),digits=5)
  pctATR
}

# Function to size order according to account balance, percentage risk desired and volatility (ATR)
# use a built in order size function instead to not utilize this functionality
osATRsize <- function(data = mktdata, timestamp=timestamp, orderqty = orderqty, acct = account.st, portfolio = portfolio, ...){
  # First set a multiplier to get order in the correct sign for short or long
  if(orderqty<0){
    sign <- -1
  }else{
    sign <- 1
  }
  
  # get account equity
  updatePortf(Portfolio = portfolio)
  updateAcct(name = acct)
  updateEndEq(Account = acct)
  account_eq <- getEndEq(Account = acct,Date = timestamp)
  
  # determine volatility adjusted position sizing 
  orderqty <- (account_eq * riskpct)/((data[timestamp]$atr.atrStopThresh)*(Cl(data[timestamp])))
  
  # round down, get as a number of correct sign and return
  orderqty <- (as.numeric(floor(orderqty)))*sign
  orderqty
}

# Function to use High and Low in Donchian Channel Calculation
HLDonch <- function(data, n=10, lag = TRUE){
  dataHL <- cbind(data[,2],data[,3])
  assign("test",dataHL,envir = .GlobalEnv)
  hiLoDonch <- DonchianChannel(dataHL,n=n,include.lag = lag)
  hiLoDonch
}

#Symbol Setup
# set the instument as a future and get the data from the csv file
# Setup the Environment
currency('USD')                                                        # set USD as a base currency
symbol <- c("LSU","RR","CO","NG","OJ","LB","HG",
            "LC","CT","CC","KC","WTI","XAU")   # Universe selection

for (sym in symbol){
  future(sym, currency = "USD", multiplier = 1)
}

getSymbols(Symbols = symbol, verbose = TRUE, warnings = TRUE, 
           src = 'csv', dir= csvDir, extension='csv', header = TRUE, 
           stingsAsFactors = FALSE)

for (sym in symbol){
  no_dup <- to.daily(get(sym), indexAt='days',drop.time = TRUE) # this is required to remove duplicate data
  assign(sym, no_dup[xtsDates])                                 # Here the data is subsetted to allign it so that rebalancing works
}

# if run previously, run this code from here down
rm.strat(portfolio.st, silent = FALSE)
rm.strat(account.st, silent = FALSE)
delete.paramset(strategy = strat,"Turtle_OPT")

# initialize the portfolio, account and orders. Starting equity and assuming data post 1995.
initPortf(portfolio.st, symbols = symbol, initDate = "1995-01-01")
initAcct(account.st, portfolios = portfolio.st, initEq = initEq, initDate = "1995-01-01")
initOrders(portfolio = portfolio.st, initDate = "1995-01-01")

# define the strategy with a position limit to prevent multiple trades in a direction
for (sym in symbol){
  addPosLimit(portfolio = portfolio.st, sym, timestamp="2000-01-01", maxpos=100, 
              longlevels = 1, minpos=-100, shortlevels = 1)
}

# Define Strategy
strategy(strat, store = TRUE)

# Add the indicators - - breakout donchian channel, stop donchian channel and an ATR threshold multiplier
add.indicator(strategy = strat, name = "HLDonch",
              arguments=list(data=quote(mktdata), n = breakout, lag=TRUE), 
              label = "DonchBreak"
)

add.indicator(strategy = strat, name = "HLDonch",
              arguments=list(data=quote(mktdata), n = stop, lag=TRUE ), 
              label = "DonchStop"
)

add.indicator(strategy = strat, name = "atrStopThresh",
              arguments=list(HLC=quote(mktdata),
                             n = 20, atr_mult=atrMult),
              label = "atrStopThresh"
)

# Add the signals - long on a cross of fast MA over slow MA and short on a cross of fast MA below slow MA.
add.signal(strat, name = "sigCrossover", arguments = list(columns=c("Close","high.DonchBreak"),
                                                          relationship = "gt"), label = "long_entry")

add.signal(strat, name = "sigCrossover", arguments = list(columns=c("Close","low.DonchStop"), 
                                                          relationship = "lt"), label = "long_stop")

add.signal(strat, name = "sigCrossover", arguments = list(columns=c("Close","low.DonchBreak"), 
                                                          relationship = "lt"), label = "short_entry")

add.signal(strat, name = "sigCrossover", arguments = list(columns=c("Close","high.DonchStop"), 
                                                          relationship = "gt"), label = "short_stop")


# Add the rules
# a) Entry rules - enter on moving average cross, osMaxPos is the order function initially
add.rule(strategy=strat,
         name='ruleSignal',
         arguments=list(sigcol='long_entry', sigval=TRUE, orderside='long', ordertype='market', 
                        orderqty=+100, osFUN='osATRsize', replace=FALSE
         ),
         type='enter',
         label='EnterLONG'
)

add.rule(strategy=strat,
         name='ruleSignal',
         arguments=list(sigcol='short_entry', sigval=TRUE, orderside='short', ordertype='market', 
                        orderqty=-100, osFUN='osATRsize', replace=FALSE
         ),
         type='enter',
         label='EnterSHORT'
)

# b) Exit rules - Close on cross the other way
add.rule(strategy = strat, name='ruleSignal',
         arguments=list(sigcol='long_stop' , sigval=TRUE, orderside='long', ordertype='market',
                        orderqty="all", replace=TRUE, orderset = "ocolong"
         ),
         type='exit',
         label='ExitLONG'
)

add.rule(strategy = strat, name='ruleSignal',
         arguments=list(sigcol='short_stop', sigval=TRUE, orderside='short' , ordertype='market',
                        orderqty="all", replace=TRUE, orderset = "ocoshort"
         ),
         type='exit',
         label='ExitSHORT'
)

# c) Stoploss rules using ordersets and ATR based threshold, not enabled by default
add.rule(strategy=strat,
         name='ruleSignal',
         arguments=list(sigcol='long_entry', sigval=TRUE, orderside=NULL, ordertype='stoplimit', 
                        prefer='High', orderqty="all", replace=FALSE, orderset ="ocolong",
                        tmult=TRUE, threshold=quote("atr.atrStopThresh")
         ),
         type='chain', parent = "EnterLONG",
         label='StopLONG',enabled = FALSE
)

add.rule(strategy=strat,
         name='ruleSignal',
         arguments=list(sigcol='short_entry', sigval=TRUE, orderside=NULL, ordertype='stoplimit', 
                        prefer='Low', orderqty="all", replace=FALSE, orderset ="ocoshort",
                        tmult=TRUE, threshold=quote("atr.atrStopThresh")
         ),
         type='chain', parent = "EnterSHORT",
         label='StopSHORT',enabled = FALSE
)

# Add distributions and constraints
add.distribution(strategy = strat,
                 paramset.label = "Turtle_OPT",
                 component.type = "indicator",
                 component.label = "DonchBreak",
                 variable = list( n = breakout ),
                 label = "DonchBreak"
)

add.distribution(strategy = strat,
                 paramset.label = "Turtle_OPT",
                 component.type = "indicator",
                 component.label = "DonchStop",
                 variable = list( n = stop ),
                 label = "DonchStop"
)

add.distribution(strategy = strat,
                 paramset.label = "Turtle_OPT",
                 component.type = "indicator",
                 component.label = "atrStopThresh",
                 variable = list( atr_mult=atrMult ),
                 label = "atr"
)

add.distribution.constraint(strategy = strat,
                            paramset.label = "Turtle_OPT",
                            distribution.label.1 = "DonchBreak",
                            distribution.label.2 = "DonchStop",
                            operator = ">",
                            label = "breakgtstop")

# Enable Rules
enable.rule(strat,type = "chain",label = "StopSHORT")
enable.rule(strat,type = "chain",label = "StopLONG")

# Register the cores for parralel procssing
registerDoMC(cores=detectCores())

# Now apply the parameter sets for optimization
out <- apply.paramset(strat, paramset.label = "Turtle_OPT",
                      portfolio=portfolio.st, account = account.st, nsamples=0, verbose = TRUE)
stats <- out$tradeStats

out <- write.csv(stats,             # write to file
                file = paste(getwd(),"/DonchianOpt",as.character(Sys.Date()),".csv", sep=""),
                quote = FALSE, row.names = TRUE)

# If you've done this on a  previous date
date <- "2017-03-10"
stats <- read.csv(paste(getwd(),"Opt_comm_20170310",".csv", sep=""))
stats <- stats[,-1]

portfolio_avg <- aggregate(stats[,c(1,2,3,6:33)],list(stats$Portfolio), mean)

# A loop to investigate the parameters via a 3D graph
for (sym in symbol){
  dfName <- paste(sym,"stats", sep = "")
  statSubsetDf <- subset(stats, Symbol == sym)
  assign(dfName, statSubsetDf)
  tradeGraphs(stats = statSubsetDf, 
              free.params=c("ma_fast","ma_slow"),
              statistics = c("Ann.Sharpe","Profit.To.Max.Draw","Min.Equity"), 
              title = sym)
}

# Or use a heatmap to look at one parameter at a time
for (sym in symbol){
  dfName <- paste(sym,"stats", sep = "")
  statSubsetDf <- subset(stats, Symbol == sym)
  assign(dfName, statSubsetDf)
  z <- tapply(X=statSubsetDf$Net.Trading.PL, 
              INDEX = list(statSubsetDf$DonchStop,statSubsetDf$DonchBreak), 
              FUN = median)
  x <- as.numeric(rownames(z))
  y <- as.numeric(colnames(z))
  filled.contour(x=x,y=y,z=z,color=heat.colors,xlab="ma_fast",ylab="ma_slow")
  title(sym)
}

for (sym in symbol){
  dfName <- paste(sym,"stats", sep = "")
  statSubsetDf <- subset(stats, Symbol == sym)
  hist(statSubsetDf$Net.Trading.PL, breaks = 100, main = paste(dfName,"mean =",mean(statSubsetDf$Net.Trading.PL),sep = " "))
  assign(dfName, statSubsetDf)
  
}

Sys.setenv(TZ=ttz)                                             # Return to original time zone
