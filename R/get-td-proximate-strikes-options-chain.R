# r program name:                     get-td-proximate-strikes-options-chain.R
# r program made:                     thursday, august 31st 2023
# r program goal:
#                                     get entire options chain from TD Ameritrade, but present and analyze forward atm strikes
#
# created by:                         munair
# updated on:                         thursday, september 14th 2023
#
# tda oauth documentation reference:  https://developer.tdameritrade.com/content/simple-auth-local-apps
# option chain data generation link:  https://developer.tdameritrade.com/option-chains/apis/get/marketdata/chains
# other documentation for reference:  https://r4ds.had.co.nz/vectors.html#vectors 
#                                     https://stackoverflow.com/questions/27361081/assign-or-copy-column-classes-from-a-dataframe-to-another
#
# notes:                              ** denotes optional sections of cosmetic code
#
# remove the comment delimiter in the section below to install required libraries:
# install( "../../Rpackages/refreshtdaccesstoken" )
# install.packages( "httr" )

# load libraries
require( "refreshtdaccesstoken" )
require( "jsonlite" )
require( "httr" )

# define input variables
underlyingasset <- "EOG" # define underlying financial instrument

# refresh td ameritrade access token
refreshedaccess <- refreshtdaccesstoken::refreshtdaccesstoken(
  callbackurl = "https://mitzvah.capital", # define callback url
  consumerkey = paste0( TD$APIKEY, "@AMER.OAUTHAP"), # define consumer key
  storedtoken = readRDS( file = "~/Documents/tdameritrade-refresh-token.rds" ) # retrieve stored refresh token
)

# define http get request
authorizedtoken <- paste0( "Bearer ", refreshedaccess )
resourcelocator <- paste0( "https://api.tdameritrade.com/v1/marketdata/chains?apikey=", TD$APIKEY, "&symbol=", underlyingasset )

# make request and convert the response from text to lists of lists
restapiresponse <- httr::GET( resourcelocator, httr::add_headers( 'Authorization' = authorizedtoken ), accept_json() )
responselisting <- httr::content( restapiresponse, "parsed", encoding = "UTF-8" )

# make expiration date variables
bearishmappings <- "putExpDateMap"
bullishmappings <- "callExpDateMap"

# make a list of all bearish (i.e. long put) options
bearishlistings <- sapply( names( responselisting[[ bearishmappings ]] ), function( bearexpiration ) {
  sapply( names( responselisting[[ bearishmappings ]][[ bearexpiration ]] ), function( strikeoffering ) {
    cbind( responselisting[[ bearishmappings ]][[ bearexpiration ]][[ strikeoffering ]][[1]] )
  } )
} )

# make a list of all bullish (i.e. long call) options
bullishlistings <- sapply( names( responselisting[[ bullishmappings ]] ), function( bullexpiration ) {
  sapply( names( responselisting[[ bullishmappings ]][[ bullexpiration ]] ), function( strikeoffering ) {
    cbind( responselisting[[ bullishmappings ]][[ bullexpiration ]][[ strikeoffering ]][[1]] )
  } )
} )

# bind the columns of the two lists as a matrix
bearoptiontable <- do.call( cbind, bearishlistings )
bulloptiontable <- do.call( cbind, bullishlistings ) 

# get meta data
expirationcodes <- names( responselisting[[ bullishmappings ]] )
optionspecifics <- names( responselisting[[ bearishmappings ]][[ 1 ]][[ 1 ]][[ 1 ]] )

# name rows and columns of matrices
rownames( bearoptiontable ) <- optionspecifics
rownames( bulloptiontable ) <- optionspecifics
colnames( bearoptiontable ) <- bearoptiontable[ "symbol", ]
colnames( bulloptiontable ) <- bulloptiontable[ "symbol", ]

# define function to convert string of numbers to POSIX data and time
convertdatetime <- function( textdata ) { as.POSIXct( as.numeric( textdata ) / 1000, origin = "1970-01-01" ) }

# get proximate puts by expiration by transposing the response list for bearish options
p <- as.data.frame( t( sapply( names( responselisting[[ bearishmappings ]] ), function( expirations ) {
  strikedistances <- abs( as.numeric( names( responselisting[[ bearishmappings ]][[ expirations ]] ) ) - responselisting[[ "underlyingPrice" ]] )
  proximatestrike <- which( strikedistances == min( strikedistances ) )
  responselisting[[ bearishmappings ]][[ expirations ]][[ proximatestrike ]][[ 1 ]]
})) )

# get proximate puts by expiration by transposing the response list for bullish options
c <- as.data.frame( t( sapply( names( responselisting[[ bullishmappings ]] ), function( expirations ) {
  strikedistances <- abs( as.numeric( names( responselisting[[ bullishmappings ]][[ expirations ]] ) ) - responselisting[[ "underlyingPrice" ]] )
  proximatestrike <- which( strikedistances == min( strikedistances ) )
  responselisting[[ bullishmappings ]][[ expirations ]][[ proximatestrike ]][[ 1 ]]
})) )

# subset most traded options
m <- rbind( subset( p, p[[ "openInterest" ]] == max( unlist( p[[ "openInterest" ]] ) ) ), subset( c, c[[ "openInterest" ]] == max( unlist( c[[ "openInterest" ]] ) ) ) )

# combine options
o <- rbind( p, c )

# evaluate strike liquidity
pricedifference <- as.numeric( o[[ "ask" ]] ) - as.numeric( o[[ "bid" ]] )
askdifferential <- sapply( pricedifference / as.numeric( o[[ "ask" ]] ), function( x ) { ifelse( is.nan( x ), 1, x ) } )
bidsizeanalysis <- as.numeric( sapply( o[[ "bidSize" ]], function( size ){ if( size > 20 ) { 100 } else { 0 } } ) )
asksizeanalysis <- as.numeric( sapply( o[[ "askSize" ]], function( size ){ if( size > 20 ) { 100 } else { 0 } } ) )
oldvolumerating <- as.numeric( sapply( o[[ "openInterest" ]], function( contracts ) { if( contracts > 20 ) { 100 } else { 0 } } ) )
pennyevaluation <- as.numeric( sapply( o[[ "pennyPilot" ]], function( pennyincrement ) { if( pennyincrement ) { 100 } else { 0 } } ) )
underlyingscore <- as.numeric( sapply( responselisting[[ "underlying" ]][[ "totalVolume" ]], function( transactionvolume ) { if( transactionvolume > 1000000 ) { 100 } else { 0 } } ) )
widespreadscore <- as.numeric( sapply( askdifferential, function( differential ){ if( differential < 0.1 ) { 100 } else { 0 } } ) )

# make and display table of evaluations
evaluatedstrike <- cbind( bidsizeanalysis, asksizeanalysis, oldvolumerating, pennyevaluation, underlyingscore, widespreadscore )
evaluatedstrike <- cbind( evaluatedstrike, liquidityrating = rowMeans( evaluatedstrike ) )
rownames( evaluatedstrike ) <- o[[ "symbol" ]]
o <- cbind( evaluatedstrike, o )

# plot derived factors **
bearishexpiries <- as.integer( p[[ "daysToExpiration" ]] )
bullishexpiries <- as.integer( c[[ "daysToExpiration" ]] )
pdeltadecayrate <- as.numeric( p[[ "delta" ]] ) / as.numeric( p[[ "theta" ]] ) / as.numeric( p[[ "mark" ]] )
cdeltadecayrate <- - as.numeric( c[[ "delta" ]] ) / as.numeric( c[[ "theta" ]] ) / as.numeric( c[[ "mark" ]] )
pdeltadecayrate <- pdeltadecayrate[ is.finite( pdeltadecayrate ) ]
cdeltadecayrate <- cdeltadecayrate[ is.finite( cdeltadecayrate ) ]
yaxisboundaries <- c( min( pdeltadecayrate, cdeltadecayrate ), max( pdeltadecayrate, cdeltadecayrate ) )
plot( x = bearishexpiries, y = pdeltadecayrate, type = "p", pch = 25, bg = "orange", xlab = "days to expiration", ylab = "", ylim = yaxisboundaries, main = paste0( underlyingasset, " delta per theta per dollar" ), sub = "", col = "darkred" )
lines( x = bullishexpiries, y = cdeltadecayrate, type = "p", pch = 24, bg = "yellow", col = "limegreen" )

quartz()

par( mfrow = c( 2, 4) )

plot( x = bearishexpiries, y = p[[ "mark" ]], type = "p", xlab = "days to expiration", ylab = "", main = paste0( underlyingasset, " put mark v/s last" ), sub = "", col = "brown" )
lines( x = bearishexpiries, y = p[[ "last" ]], type = "p", col = "darkorange" )
plot( x = bearishexpiries, y = - as.numeric( p[[ "delta" ]] ) / as.numeric( p[[ "mark" ]] ), type = "p", xlab = "days to expiration", ylab = "", main = paste0( underlyingasset, " put delta benefit per dollar" ), sub = "", col = "blue" )
plot( x = bearishexpiries, y = - as.numeric( p[[ "theta" ]] ) / as.numeric( p[[ "mark" ]] ), type = "p", xlab = "days to expiration", ylab = "", main = paste0( underlyingasset, " put theta expense per dollar" ), sub = "", col = "orange" )
plot( x = bearishexpiries, y = as.numeric( p[[ "openInterest" ]] ) + as.numeric( p[[ "totalVolume" ]] ), type = "p", xlab = "days to expiration", ylab = "", main = paste0( underlyingasset, " put contracts traded" ), sub = "", col = "darkgrey" )
lines( x = bearishexpiries, y = as.numeric( p[[ "totalVolume" ]] ), type = "p", col = "purple" )

plot( x = bullishexpiries, y = c[[ "mark" ]], type = "p", xlab = "days to expiration", ylab = "", main = paste0( underlyingasset, " call mark v/s last" ), sub = "", col = "brown" )
lines( x = bullishexpiries, y = c[[ "last" ]], type = "p", col = "darkorange" )
plot( x = bullishexpiries, y = as.numeric( c[[ "delta" ]] ) / as.numeric( c[[ "mark" ]] ), type = "p", xlab = "days to expiration", ylab = "", main = paste0( underlyingasset, " call delta benefit per dollar" ), sub = "", col = "blue" )
plot( x = bullishexpiries, y = - as.numeric( c[[ "theta" ]] ) / as.numeric( c[[ "mark" ]] ), type = "p", xlab = "days to expiration", ylab = "", main = paste0( underlyingasset, " call theta expense per dollar" ), sub = "", col = "orange" )
plot( x = bullishexpiries, y = as.numeric( c[[ "openInterest" ]] ) + as.numeric( c[[ "totalVolume" ]] ), type = "p", xlab = "days to expiration", ylab = "", main = paste0( underlyingasset, " call contracts traded" ), sub = "", col = "darkgrey" )
lines( x = bullishexpiries, y = as.numeric( c[[ "totalVolume" ]] ), type = "p", col = "purple" )

par( mfrow = c( 1, 1) )

# prioritize columns **
prioritycolumns <- c( "putCall", "daysToExpiration", "strikePrice", "totalVolume", "openInterest",
                      "delta", "gamma", "theta", "vega", "rho",
                      "last", "mark", "bid", "ask", "bidSize", "askSize", "lastSize", "highPrice", "lowPrice", "closePrice" )
p <- p[ c( prioritycolumns, setdiff( names( p ), prioritycolumns )) ]
c <- c[ c( prioritycolumns, setdiff( names( c ), prioritycolumns )) ]
m <- m[ c( prioritycolumns, setdiff( names( m ), prioritycolumns )) ]
o <- o[ c( prioritycolumns, setdiff( names( o ), prioritycolumns )) ]

# convert timestrings to POSIX dates and times for readability **
datedcolumnlist <- c( "tradeTimeInLong", "quoteTimeInLong", "expirationDate", "lastTradingDay" )
for( datedcolumn in datedcolumnlist ) {
  p[ ,datedcolumn ]<- convertdatetime( p[ ,datedcolumn ] )
  c[ ,datedcolumn ]<- convertdatetime( c[ ,datedcolumn ] )
  m[ ,datedcolumn ]<- convertdatetime( m[ ,datedcolumn ] )
  o[ ,datedcolumn ]<- convertdatetime( o[ ,datedcolumn ] )
}

# format large numbers **
largenumberlist <- c( "openInterest", "totalVolume" )
for( largenumber in largenumberlist ) {
  p[ ,largenumber ] <- prettyNum( p[ ,largenumber ], big.mark = ",")
  c[ ,largenumber ] <- prettyNum( c[ ,largenumber ], big.mark = ",")
  m[ ,largenumber ] <- prettyNum( m[ ,largenumber ], big.mark = ",")
  o[ ,largenumber ] <- prettyNum( o[ ,largenumber ], big.mark = ",")
}

# display dataframes **
View( p, paste0( underlyingasset, "'s Proximate Puts") )
View( c, paste0( underlyingasset, "'s Proximate Calls") )
View( m, paste0( underlyingasset, "'s Highest OI Proximate Strikes") )
View( o[ order( o[ ,"liquidityrating" ], o[ ,"widespreadscore" ], decreasing = TRUE ),  ], paste0( underlyingasset, "'s Most Liquid Options") )