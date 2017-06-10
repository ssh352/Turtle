# This strategy uses a dual donchian channels (breakout and close), to either a) go long if the close
# is above the breakout channel or b) short if the closee is below the breakout channel
# There is a rebalancing rule that enables us to compare being 100% invested in this strategy to 
# buy and hold. An ATR based stop is used which is implemented via a custom indicator. Single equity data from yahoo.
# ATR based order size function. Strategy was copied from DMA trend project initially

# Library and time zone setup
library(quantstrat)       # Required package for strategy back testing
library(doMC)
ttz<-Sys.getenv('TZ')     # Time zone to UTC, saving original time zone
Sys.setenv(TZ='UTC')

# Quantstrat general variables
strat        <- "Turtle"               # Give the stratgey a name variable
portfolio.st <- "portf"                # Portfolio name
account.st   <- "accnt"                # Account name
initEq       <- 10000                 # this parameter is required to get pct equity rebalancing to work

# Strategy specific variables
breakout  <- seq(20, 400, by = 20)
stop  <- seq(20, 400, by = 20)
atrMult <- seq(1, 5, by = 1)
riskpct <- 0.02

# Strategy Functions
# Custom indicator to generate the threshold multiplier to set an ATR based stop.
atrStopThresh <- function(HLC, n=20, atr_mult=2){
  ATR <- ATR(HLC = HLC, n)
  pctATR <- (atr_mult*ATR$atr)/Cl(HLC)
  pctATR
}

# A Function to size the order based on the ATR, use A built in order size function
# instead to not utilize this functionality
osATRsize <- function(data = mktdata, timestamp=timestamp, orderqty = orderqty, acct = account.st, symbol = symbol, portfolio = portfolio, ...){
  # First set a multiplier to get order in the correct sign for short or long
  if(orderqty<0){
    sign <- -1
  }else{
    sign <- 1
  }
  
  # get updated account equity
  updatePortf(Portfolio = portfolio)
  updateAcct(name = acct)
  updateEndEq(Account = acct)
  account_eq <- getEndEq(Account = acct,Date = timestamp)
  
  # 0 out order if already in the market
  currentPos <- getPosQty(Portfolio = portfolio, Symbol = symbol, Date = timestamp)
  if(currentPos != 0){
    sign <- 0
  }
  
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

# Symbols etc
currency('USD')             # set USD as a base currency
symbol <- "GSPC"            # Universe selection At this stage is only one symbol

# set the instument as a future and get the data from yahoo
stock(symbol, currency = "USD", multiplier = 1)
getSymbols("^GSPC", from = '1995-01-01')

# if run previously, run this code from here down to the strategy definition before re-running
rm.strat(portfolio.st, silent = FALSE)
rm.strat(account.st, silent = FALSE)
delete.paramset(strategy = strat, "Turtle_OPT")

# initialize the portfolio, account and orders. Starting equity and assuming data post 1995.
initPortf(portfolio.st, symbols = symbol, initDate = "1995-01-01")
initAcct(account.st, portfolios = portfolio.st, initEq = initEq, initDate = "1995-01-01")
initOrders(portfolio = portfolio.st, initDate = "1995-01-01")

# Add a position limit for the portfolio to prevent multiple trades in a direction
addPosLimit(portfolio = portfolio.st, symbol, timestamp="1995-01-01", maxpos=100, 
            longlevels = 1, minpos=-100, shortlevels = 1)

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
                      portfolio=portfolio.st, account = account.st, nsamples=0, verbose = TRUE) # Attempt the strategy

stats <- out$tradeStats

# Or use a heatmap to look at one parameter at a time
for (a in atrMult){
  dfName <- paste(a,"stats", sep = "")
  statSubsetDf <- subset(stats, atr == a)
  assign(dfName, statSubsetDf)
  z <- tapply(X=statSubsetDf$Net.Trading.PL, 
              INDEX = list(statSubsetDf$DonchBreak,statSubsetDf$DonchStop), 
              FUN = median)
  x <- as.numeric(rownames(z))
  y <- as.numeric(colnames(z))
  filled.contour(x=x,y=y,z=z,color=heat.colors,xlab="breakout",ylab="stop")
  title(a)
}

Sys.setenv(TZ=ttz)                                             # Return to original time zone
