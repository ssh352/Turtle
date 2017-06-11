# This file will get the symbols for the portfolio of commodities

require(quantstrat)

Sys.setenv(TZ="UTC")

#Symbol Setup
# set the instument as a future and get the data from the csv file
# Setup the Environment

xtsDates     <- "2006/"        # Variable for the point in time you want your prices series to line up from
currency('USD')             # set USD as a base currency
symbol <- "GSPC"            # Universe selection At this stage is only one symbol

# set the instument as a future and get the data from yahoo
stock(symbol, currency = "USD", multiplier = 1)
getSymbols("^GSPC", from = '1995-01-01')

for(sym in symbol){
  assign(sym,na.fill(get(sym),fill='extend'))
}

#risk <- setRisk(symbol)                                 # set the risk for rebalancing using the function
