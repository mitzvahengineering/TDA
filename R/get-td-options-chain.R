# Credits: @quantRoom
# Requirements:
#
# 1. jsonlite library
# 2. data.table library
#
# NOTE:
# Run the following command to install data.table if encountering package errors:
#
# install.packages("data.table")
#

# Load libraries.
require("jsonlite")
require("data.table")

# Retrieve TD API key from environment.
tdapikey = TD$APIKEY

# Function to get option chains data.
getOptionChainsTD = function(ticker) {
  url = paste0("https://api.tdameritrade.com/v1/marketdata/chains?apikey=", tdapikey, "&symbol=", ticker)
  temp = read_json(url, simplifyVector = TRUE)
}

OC = getOptionChainsTD("TSLA")

oc2df = function(OC)
{
  c.exp = names(OC$callExpDateMap)
  p.exp = names(OC$putExpDateMap)
  
  # CALLS
  calls = lapply(as.list(c.exp), function(x)
    {
      oc <- OC$callExpDateMap[paste(x)]
      strks <- names(oc[[1]])
      dat <- lapply(as.list(strks), function(y) oc[[x]][[y]])
      rbindlist(dat, use.names=TRUE, fill = TRUE)
    }
  )
  calls <- rbindlist(calls, use.names = TRUE, fill = TRUE)
  
  # PUTS
  puts = lapply(as.list(p.exp), function(x)
  {
    oc <- OC$putExpDateMap[paste(x)]
    strks <- names(oc[[1]])
    dat <- lapply(as.list(strks), function(y) oc[[x]][[y]])
    rbindlist(dat, use.names=TRUE, fill = TRUE)
  }
  )
  puts <- rbindlist(puts, use.names = TRUE, fill = TRUE)
  
  # ALL
  ALL <- rbind(calls,puts)
  ALL$tradeTimeInLong <- as.POSIXct(ALL$tradeTimeInLong/1000, origin = "1970-01-01")
  ALL$quoteTimeInLong <- as.POSIXct(ALL$quoteTimeInLong/1000, origin = "1970-01-01")
  ALL$expirationDate <- as.POSIXct(ALL$expirationDate/1000, origin = "1970-01-01")
  ALL$lastTradingDay <- as.POSIXct(ALL$lastTradingDay/1000, origin = "1970-01-01")
  ALL
}

aapl <- getOptionChainsTD(ticker = "AAPL")
aapl <- oc2df(OC=aapl)

View(subset(aapl, aapl$putCall == "CALL" & aapl$strikePrice == 180 ))
