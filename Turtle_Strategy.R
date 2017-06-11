# 

library(quantstrat)       # Required package for strategy back testing
library(doMC)

source(paste(getwd(),"/TurtleInclude.R",sep=""))

# Define Strategy
strategy(strat, store = TRUE)

# Add the indicators - breakout donchian channel, stop donchian channel and an ATR threshold multiplier
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

# Percentage Equity rebalancing rule
add.rule(strat, 'rulePctEquity',
         arguments=list(rebalance_on='months',
                        trade.percent=1,
                        refprice=quote(last(getPrice(mktdata)[paste('::',curIndex,sep='')])[,1]),
                        digits=0
         ),
         type='rebalance',
         label='rebalance')

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

save.strategy(strat)
