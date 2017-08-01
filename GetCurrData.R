# This file will get the symbols for the portfolio of commodities

require(quantstrat)

Sys.setenv(TZ="UTC")

#Symbol Setup
# set the instument as a future and get the data from the csv file
# Setup the Environment
csvDir       <- "/home/rjk/Financial/forexData/AXI/Daily" # Directory containing csv files
xtsDates     <- "2000/"        # Variable for the point in time you want your prices series to line up from
currency(c('USD','AUD','EUR','JPY','GBP','NZD','CAD','CHF','HKD'))

symbol <- c("AUDUSD","EURGBP","EURJPY","EURUSD","GBPJPY","GBPUSD","NZDUSD","USDCAD","USDCHF",
            "USDHKD","USDJPY")   # Universe selection

#risk <- setRisk(symbol)                                 # set the risk for rebalancing using the function

for(sym in symbol){
  exchange_rate(primary_id=sym, tick_size=0.0001)
}

getSymbols(Symbols = symbol, verbose = TRUE, warnings = TRUE, 
           src = 'csv', dir= csvDir, extension='csv', header = TRUE, 
           stingsAsFactors = FALSE)

for (sym in symbol){
  no_dup <- to.daily(get(sym), indexAt='days',drop.time = TRUE) # this is required to remove duplicate data
  assign(sym, no_dup[xtsDates])                                 # Here the data is subsetted to allign it so that rebalancing works
}
