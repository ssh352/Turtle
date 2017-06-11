# This is the include file for Dual Moving Avergae Crossover Function

Sys.setenv(TZ='UTC')

# Quantstrat general variables
strat        <- "Turtle"        # Give the stratgey a name variable
portfolio.st <- "portf"         # Portfolio name
account.st   <- "accnt"         # Account name
initEq       <- 1000000         # this parameter is required to get pct equity rebalancing to work

# Strategy specific variables
breakout  <- 200  # Breakout Donchian Channel Length
stop      <- 180  # Stop Donchian Channel Length
atrMult   <- 3    # Multiple of ATR to use for stoploss and order sizing calculation
riskpct   <- 0.02 # Amount of trade balance to risk per trade

# Strategy specific variables
breakoutPset  <- seq(20, 400, by = 20)
stopPset      <- seq(20, 400, by = 20)
atrMultPset   <- seq(1, 5, by = 1)

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

# Transaction Fee Function - Returns a numeric Fee which is a percetage multiple of the tranaction total value 
pctFee <- function(TxnQty,TxnPrice,Symbol){
  feeMult <- 0.0005
  fee <- round(-1*(feeMult*abs(TxnPrice)*abs(TxnQty)),0)
  return(fee)
}

# Custom Function to construct chart to analyse walk.forward() custom
chartForwardTraining <- function(audit.filename)
{
  .audit <- NULL  # keep codetools happy
  # # ensure correct file written by walk.forward() is provided
  # datePattern <- "[[:digit:]]{4}.[[:digit:]]{2}.[[:digit:]]{2}(.[[:digit:]]{2}.[[:digit:]]{2}.[[:digit:]]{2})?"
  # if (!grepl(paste0(datePattern, "\\.", datePattern, "\\.RData$"), audit.filename[1L])) {
  #     stop("'audit.filename' should match pattern:\n  [audit.prefix].[symbol].[start timestamp].[end timestamp].RData.")
  # }
  if (file.exists(audit.filename)) {
    load(audit.filename)
  } else {
    stop("'audit.filename', ", audit.filename, " not found.")
  }
  
  # extract all portfolio names from the audit environment
  # NB: training data only has portfolios that end in digits
  portfolios.st = ls(name=.audit, pattern='portfolio.*')
  n <- length(portfolios.st)
  
  # calculate Net.Trading.PL for each portfolio, one xts col per portfolio
  PL.xts <- xts()
  for(por in portfolios.st)
  {
    p <- getPortfolio(por, envir=.audit)
    
    from <- index(p$summary[2])
    
    #R <- cumsum(p$summary['2004-01-01/','Net.Trading.PL'])
    R <- cumsum(p$summary[paste(from, '/', sep=''),'Net.Trading.PL'])
    names(R) <- por
    
    PL.xts <- cbind(PL.xts, R)
  }
  
  # .audit$param.combo.nr contains the rowname of the best portfolio
  chosen.one <- .audit$param.combo.nr[1L]
  chosen.portfolio.st = ls(name=.audit, pattern=glob2rx(paste('portfolio', '*', chosen.one, sep='.')))
  # add a column for the chosen portfolio, doubling it and
  # making it plot last, so it's not over-plotted by other portfolios
  R <- PL.xts[,chosen.portfolio.st]
  PL.xts <- cbind(PL.xts, R)
  
  PL.xts <- na.locf(PL.xts)
  
  # add drawdown columns for all portfolio columns
  CumMax <- cummax(PL.xts)
  Drawdowns.xts <- -(CumMax - PL.xts)
  data.to.plot <- as.xts(cbind(PL.xts, Drawdowns.xts))
  
  # based on the suggestion by Ross, note that the number of
  # lines is increased by 1 since the 'chosen' portfolio is added as the last one
  # and highlighted using the blue color
  p <- plot(PL.xts, col="grey", main="Walk Forward Analysis")
  p <- lines(PL.xts[,n+1],col="blue",on=1)
  # set on=NA so it is drawn on a new panel
  p <- lines(Drawdowns.xts, col="grey", on=NA, main="Drawdowns")
  p <- lines(Drawdowns.xts [,n+1],col="blue",on=2)
  print(p)
}

#' Chart to analyse walk.forward() objective function custom

chartFor <- function(audit.filename)
{
  .audit <- NULL  # keep codetools happy
  # # ensure correct file written by walk.forward() is provided
  # if (!grepl("\\.results\\.RData$", audit.filename[1L])) {
  #     stop("'audit.filename' should match pattern:\n  [audit.prefix].results.RData")
  # }
  if (file.exists(audit.filename)) {
    load(audit.filename)
  } else {
    stop("'audit.filename', ", audit.filename, " not found.")
  }
  
  # extract all portfolio names from the audit environment,
  # except result portfolio (which doesn't end with a digit)
  portfolios.st = ls(name=.audit, pattern='portfolio.*.[0-9]+')
  n <- length(portfolios.st)
  
  # calculate Net.Trading.PL for each portfolio, one xts col per portfolio
  PL.xts <- xts()
  for(portfolio.st in portfolios.st)
  {
    p <- getPortfolio(portfolio.st, envir=.audit)
    
    from <- index(p$summary[2])
    
    #R <- cumsum(p$summary['2004-01-01/','Net.Trading.PL'])
    R <- cumsum(p$summary[paste(from, '/', sep=''),'Net.Trading.PL'])
    names(R) <- portfolio.st
    
    PL.xts <- cbind(PL.xts, R)
  }
  
  # now for the result portfolio (which doesn't end with a digit)
  portfolio.st <- ls(name=.audit, pattern='portfolio.*[^.0-9]$')
  p <- getPortfolio(portfolio.st, envir=.audit)
  from <- index(p$summary[2])
  R <- cumsum(p$summary[paste(from, '/', sep=''),'Net.Trading.PL'])
  names(R) <- portfolio.st
  
  #PL.xts <- na.locf(PL.xts)
  PL.xts <- na.locf(cbind(PL.xts, R))
  
  # add drawdown columns for all portfolio columns
  CumMax <- cummax(PL.xts)
  Drawdowns.xts <- -(CumMax - PL.xts)
  data.to.plot <- as.xts(cbind(PL.xts, Drawdowns.xts))
  
  p <- plot(PL.xts, col="grey", main="Walk Forward Analysis")
  p <- lines(PL.xts[,n+1],col="blue",on=1)
  # set on=NA so it is drawn on a new panel
  p <- lines(Drawdowns.xts, col="grey", on=NA, main="Drawdowns")
  p <- lines(Drawdowns.xts [,n+1],col="blue",on=2)
  print(p)
}