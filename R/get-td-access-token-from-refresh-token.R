# r program name:                     get-td-access-token-from-refresh-token.R
# r program made:                     friday, august 18th 2023
# r program goal:
#                                     get access token using stored refresh token
#                                     this access token is good for 30 minutes
#                                     an http request's authorization header uses this access token [preceded by "Bearer "]
#
# created by:                         munair
# updated on:                         sunday, august 20th 2023
#
# tda oauth documentation reference:  https://developer.tdameritrade.com/content/simple-auth-local-apps
# post access token generation link:  https://developer.tdameritrade.com/authentication/apis/post/token-0
#
# remove the comment delimiter in the section below to install required libraries:
# install.packages("httr")
require("httr")

# definitions
callbackurl = "https://mitzvah.capital" # define callback url
consumerkey = paste0( TD$APIKEY, "@AMER.OAUTHAP") # define consumer key
storedtoken = readRDS( file = "~/Documents/tdameritrade-refresh-token.rds" ) # retrieve stored refresh token

# derivatives
tdaclientid = URLencode( consumerkey, reserved = TRUE ) # define encoded client id
redirecturi = URLencode( callbackurl, reserved = TRUE ) # define encoded redirect uri
refreshcode = URLencode( storedtoken, reserved = TRUE ) # define encoded refresh token

encodedlist = list( grant_type = 'refresh_token', 
                    refresh_token = refreshcode, 
                    access_type = '',
                    code = '',
                    client_id = tdaclientid,
                    redirect_uri = redirecturi )

stringified = paste( names( encodedlist ), encodedlist, sep = "=", collapse = "&" )

encodedpost = httr::POST( 'https://api.tdameritrade.com/v1/oauth2/token', 
                          httr::add_headers( 'Content-Type' = 'application/x-www-form-urlencoded' ),
                          body = stringified, 
                          encode = 'form' )

httpcontent = httr::content( encodedpost )
accesstoken = httpcontent$access_token
assign("ACCESSTOKEN",accesstoken, envir = .GlobalEnv)
