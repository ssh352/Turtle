# This script will attempt to calculate the walk forward efficiency of a strategy tested with
# the walk forward function in quantstrat. The easiest way is to ask for 2 walkforward 
# files and then extract from the training file the best parameter combo and net trading PL
# Then the more difficult part is getting extracting all the testing period data from
# the portfolio object.

startYear      <- "2000"
endYear        <- "2016"
trainingLength <- 5
testingLength  <- 1
anchored       <- FALSE
prefix         <- "wfa."
symbol         <- "USDCHF"
setwd(paste("/media/sjaoraid/strat/Turtle/wfa/",symbol,sep = "")) 

# Generate year numerics
numStartYear <- as.numeric(startYear)
numEndYear   <- as.numeric(endYear)
firstTest    <- numStartYear+trainingLength
numTests     <- ((numEndYear-firstTest)+1)/testingLength
testYears    <- seq(firstTest,numEndYear,by=testingLength)

# Get Testing Returns for each Period
# Get Testing Period File
.audit <- NULL
load(list.files(pattern=glob2rx(paste(prefix,"results.RData", sep = ""))))

# Get Portfolio Summary
summary1 <- getPortfolio(Portfolio = "portfolio.portf", envir = .audit)$summary

# Get Returns for each testing year
for(year in testYears){
  yearsumm <- summary1[as.character(year)]
  name <- paste("PL",year,sep = "")
  assign(name,sum(yearsumm[,"Net.Trading.PL"]))
}

# Set the start date of tests for anchored vs non-anchored
if (anchored==TRUE){
  startTrainDates <- rep(numStartYear,numTests)
} else{
  startTrainDates <- seq(numStartYear,(numStartYear+numTests)-1,by=testingLength)
}

n <- 1
sumWFE <- 0
while(n <= numTests){
  .audit = NULL
  load(list.files(pattern=glob2rx(paste(prefix,symbol,".",startTrainDates[n],"*",firstTest+(n-2),"*.RData",sep=""))))
  chosen.one <- .audit$param.combo.nr[1L]
  chosen.portfolio.st = ls(name=.audit, 
                           pattern=glob2rx
                           (paste('portfolio', '*', chosen.one, sep='.')))
  trainingport <- getPortfolio(chosen.portfolio.st,env = .audit)
  trainingSumm <- trainingport$summary[paste(startTrainDates[n],"/",firstTest+(n-2),sep="")]
  PLTrainAnn <- (sum(trainingSumm[,"Net.Trading.PL"]))/((firstTest+(n-1))-startTrainDates[n])
  wfe <- get(paste("PL",firstTest+(n-1),sep=""))/PLTrainAnn
  sumWFE <- sumWFE + wfe
  assign(paste("WFE",firstTest+(n-1),sep=""),wfe)
  n <- n+1 # n + testing length(?)
}

WFETotal <- sumWFE/numTests
print(WFETotal)
