# r program name:                     get-td-quotes.R
# r program made:                     tuesday, august 22nd 2023
# r program goal:
#                                     get quotes information using the asset's symbol
#
# created by:                         munair
# updated on:                         wednesday, august 23rd 2023
#
# tda rest api endpoint:              https://api.tdameritrade.com/v1/marketdata/quotes
# quotes generation link:             https://developer.tdameritrade.com/quotes/apis/get/marketdata/quotes
#
# to do:                              support multiple quotes
#
# remove the comment delimiter in the section below to install required libraries:
# install.packages("httr")
require("httr")
require("jsonlite")

# define input variables
assetsymbol = "OXY" # symbol of the financial instrument to get quotes on

# make http rest api request
bearertoken = paste0( "Bearer ", ACCESSTOKEN )
endpointurl = paste0( "https://api.tdameritrade.com/v1/marketdata/quotes?symbol=", assetsymbol )
httprequest = httr::GET(  endpointurl, httr::add_headers( 'Authorization' = bearertoken ) )
httpcontent = httr::content( httprequest, "text", encoding = "UTF-8" )
listcontent = fromJSON( httpcontent )

# format dates
listcontent[[assetsymbol]]$quoteTimeInLong <- as.POSIXct( listcontent[[assetsymbol]]$quoteTimeInLong/1000, origin = "1970-01-01" ) 
listcontent[[assetsymbol]]$tradeTimeInLong <- as.POSIXct( listcontent[[assetsymbol]]$tradeTimeInLong/1000, origin = "1970-01-01" ) 
listcontent[[assetsymbol]]$regularMarketTradeTimeInLong <- as.POSIXct( listcontent[[assetsymbol]]$regularMarketTradeTimeInLong/1000, origin = "1970-01-01" ) 

# format large numbers
listcontent[[assetsymbol]][["totalVolume"]] <- prettyNum( listcontent[[assetsymbol]][["totalVolume"]], big.mark = "," )

# view transposed dataframe of response
dataframery <- data.frame( listcontent )
dataframery <- t( dataframery )
colnames( dataframery ) <- assetsymbol
View( dataframery )