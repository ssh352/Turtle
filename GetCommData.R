# This file will get the symbols for the portfolio of commodities

require(quantstrat)

Sys.setenv(TZ="UTC")

#Symbol Setup
# set the instument as a future and get the data from the csv file
# Setup the Environment
csvDir       <- "/home/rjk/Financial/commodities_data/daily2006" # Directory containing csv files
xtsDates     <- "2006/"        # Variable for the point in time you want your prices series to line up from
currency('USD')     # set USD as a base currency

symbol <- c("LSU","RR","CO","NG","OJ","LB","HG",
            "LC","CT","KC","CC","WTI", "XAU")           # Default Universe selection
#risk <- setRisk(symbol)                                 # set the risk for rebalancing using the function

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

