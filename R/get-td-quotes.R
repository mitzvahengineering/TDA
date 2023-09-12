# r program name:                     get-td-quotes.R
# r program made:                     tuesday, august 22nd 2023
# r program goal:
#                                     get quotes information using the asset's symbol
#
# created by:                         munair
# updated on:                         tuesday, september 12th, 2023
#
# tda rest api endpoint:              https://api.tdameritrade.com/v1/marketdata/quotes
# quotes generation link:             https://developer.tdameritrade.com/quotes/apis/get/marketdata/quotes
#
# to do:                              support multiple quotes
#
# remove the comment delimiter in the section below to install required libraries:
# devtools::install( "../R-packages/refreshtdaccesstoken" )
# install.packages( "httr" )

# load libraries
require( "refreshtdaccesstoken" )
require( "jsonlite" )
require( "httr" )

# define input variables
assetsymbol <- "OXY" # symbol of the financial instrument to get quotes on

# refresh td ameritrade access token
accesstoken <- refreshtdaccesstoken::refreshtdaccesstoken(
  callbackurl = "https://mitzvah.capital", # define callback url
  consumerkey = paste0( TD$APIKEY, "@AMER.OAUTHAP"), # define consumer key
  storedtoken = readRDS( file = "~/Documents/tdameritrade-refresh-token.rds" ) # retrieve stored refresh token
)

# make http rest api request
bearertoken <- paste0( "Bearer ", accesstoken )
endpointurl <- paste0( "https://api.tdameritrade.com/v1/marketdata/quotes?symbol=", assetsymbol )
httprequest <- httr::GET(  endpointurl, httr::add_headers( 'Authorization' = bearertoken ) )
httpcontent <- httr::content( httprequest, "parsed", encoding = "UTF-8" )

# bind list data into one column
contentlist <- do.call( cbind, httpcontent )

# format dates
contentlist[ "quoteTimeInLong", ][[ 1 ]] <- as.POSIXct( contentlist[ "quoteTimeInLong", ][[ 1 ]] / 1000, origin = "1970-01-01" ) 
contentlist[ "tradeTimeInLong", ][[ 1 ]] <- as.POSIXct( contentlist[ "tradeTimeInLong", ][[ 1 ]] / 1000, origin = "1970-01-01" ) 
if( "lastTradingDay" %in% rownames( contentlist ) ) { contentlist[ "lastTradingDay", ][[ 1 ]] <- as.POSIXct( contentlist[ "lastTradingDay", ][[ 1 ]] / 1000, origin = "1970-01-01" ) }
if( "regularMarketTradeTimeInLong" %in% rownames( contentlist ) ) { contentlist[ "regularMarketTradeTimeInLong", ][[ 1 ]] <- as.POSIXct( contentlist[ "regularMarketTradeTimeInLong", ][[ 1 ]] / 1000, origin = "1970-01-01" ) }


# format large numbers
contentlist[ "totalVolume", ][[ 1 ]] <- prettyNum( contentlist[ "totalVolume", ][[ 1 ]], big.mark = "," )

# view response
View( contentlist, paste0( assetsymbol, " Quotation" ) ) # to verify successful formatting: View( contentlist[ "regularMarketTradeTimeInLong", ][[ 1 ]] )