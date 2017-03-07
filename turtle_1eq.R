# This strategy uses a dual donchian channels (breakout and close), to either a) go long if the close
# is above the breakout channel or b) short if the closee is below the breakout channel
# There is a rebalancing rule that enables us to compare being 100% invested in this strategy to 
# buy and hold. An ATR based stop is used which is implemented via a custom indicator. Single equity data from yahoo.
# ATR based order size function. Strategy was copied from DMA trend project initially

# Library and time zone setup
library(quantstrat)       # Required package for strategy back testing
ttz<-Sys.getenv('TZ')     # Time zone to UTC, saving original time zone
Sys.setenv(TZ='UTC')

# Quantstrat general variables
strat        <- "Turtle"               # Give the stratgey a name variable
portfolio.st <- "portf"                # Portfolio name
account.st   <- "accnt"                # Account name
initEq       <- 10000                 # this parameter is required to get pct equity rebalancing to work

# Strategy specific variables
breakout  <- 20
stop  <- 10
atrMult <- 2
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
osATRsize <- function(data = mktdata, timestamp=timestamp, orderqty = orderqty, acct = account.st, portfolio = portfolio, ...){
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

# initialize the portfolio, account and orders. Starting equity and assuming data post 1995.
initPortf(portfolio.st, symbols = symbol, initDate = "1995-01-01")
initAcct(account.st, portfolios = portfolio.st, initEq = initEq, initDate = "1995-01-01")
initOrders(portfolio = portfolio.st, initDate = "1995-01-01")

# Add a position limit for the portfolio to prevent multiple trades in a direction
addPosLimit(portfolio = portfolio.st, symbol, timestamp="1995-01-01", maxpos=100, 
            longlevels = 1, minpos=-100, shortlevels = 1)

# Define Strategy
strategy(strat, store = TRUE)

# Add the indicators - A fast moving average, a slow moving average and the custom indicator
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
                         orderqty=+100, osFUN='osMaxPos', replace=FALSE
         ),
         type='enter',
         label='EnterLONG'
)

add.rule(strategy=strat,
         name='ruleSignal',
         arguments=list(sigcol='short_entry', sigval=TRUE, orderside='short', ordertype='market', 
                        orderqty=-100, osFUN='osMaxPos', replace=FALSE
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

# Percentage Equity rebalancing rule
add.rule(strat, 'rulePctEquity',
         arguments=list(rebalance_on='months',
                        trade.percent=1,
                        refprice=quote(last(getPrice(mktdata)[paste('::',curIndex,sep='')])[,1]),
                        digits=0
         ),
         type='rebalance',
         label='rebalance')

enable.rule(strat,type = "chain",label = "StopLONG")
enable.rule(strat,type = "chain",label = "StopSHORT")
out <- applyStrategy.rebalancing(strategy=strat , portfolios=portfolio.st) # Attempt the strategy
updatePortf(Portfolio = portfolio.st)                                      # Update the portfolio
updateAcct(name = account.st)
updateEndEq(account.st)

#chart the position
donchian_break <- DonchianChannel(HL=(cbind(Hi(GSPC),Lo(GSPC))),n=breakout,include.lag = TRUE)
donchian_stop <- DonchianChannel(HL=(cbind(Hi(GSPC),Lo(GSPC))),n=stop,include.lag = TRUE)
chart.Posn(Portfolio = portfolio.st, Symbol = symbol, 
           TA = list("add_TA(donchian_break[,1],on=1,col=1)","add_TA(donchian_break[,3],on=1,col=1)",
                     "add_TA(donchian_stop[,1],on=1,col=2)","add_TA(donchian_stop[,3],on=1,col=2)"))                   # Chart the position
stats <- tradeStats(portfolio.st)
OB <- get.orderbook(portfolio.st)
orders <- OB$portf$GSPC

#plot the returns vs buy and hold
eq1 <- getAccount(account.st)$summary$End.Eq
rt1 <- Return.calculate(eq1,"log")
rt2 <- periodReturn(GSPC, period = "daily")
returns <- cbind(rt1,rt2)
colnames(returns) <- c("Donchian","SP500")
chart.CumReturns(returns,colorset=c(2,4),legend.loc="topleft",
                 main="Donchian to Benchmark Comparison",ylab="cum return",xlab="",
                 minor.ticks=FALSE)
Sys.setenv(TZ=ttz)                                             # Return to original time zone
