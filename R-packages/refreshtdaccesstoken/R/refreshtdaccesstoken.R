#' Get Access Token from TD Ameritrade Via Stored Refresh Token
#'
#' This function uses a callback URL, an API key, and a monthly generated Refresh Token (for example saved to an RDS file) to generate an Access Token using a TD Ameritrade REST API endpoint.
#'
#' The Access Token generated is good for 30 minutes. An HTTP request's authorization header uses this access token \[preceded by "Bearer "\] to retrieve data from TD Ameritrade's REST API servers.
#'
#' \[First draft: Friday, August 18th 2023\]
#' \[Last update: Tuesday, September 12th 2023\]
#'
#' @param callbackurl The callback URL defined in the TD Ameritrade Application.
#' @param consumerkey The "@AMER.OAUTHAP" appended TD Ameritrade API Key (called the "Consumer Key").
#' @param storedtoken The most recently issued Refresh Token... if this function breaks, this Refresh Token must be reissued.
#'
#' @return A string of characters that TD Ameritrade requires as an "Access Token".
#'
#' @author munair, \email{munair@@gmail.com}
#' @references TD Ameritrade's OAUTH Documentation/Reference (\url{https://developer.tdameritrade.com/content/simple-auth-local-apps}), Post Access Token Generation Link (\url{https://developer.tdameritrade.com/authentication/apis/post/token-0})
#' @seealso \code{\link{refreshtdaccesstoken}}
#' @keywords td, ameritrade, access, token
#'
#' @examples
#' refreshtdaccesstoken( "https://mitzvah.capital", paste0( TD$APIKEY, "@AMER.OAUTHAP" ), readRDS( file = "~/Documents/tdameritrade-refresh-token.rds" ) )
#'
#' @export

refreshtdaccesstoken <- function( callbackurl, consumerkey, storedtoken ) {

  # parameter derivatives
  tdaclientid <- utils::URLencode( consumerkey, reserved = TRUE ) # define encoded client id
  redirecturi <- utils::URLencode( callbackurl, reserved = TRUE ) # define encoded redirect uri
  refreshcode <- utils::URLencode( storedtoken, reserved = TRUE ) # define encoded refresh token

  encodedlist <- list( grant_type = 'refresh_token',
                      refresh_token = refreshcode,
                      access_type = '',
                      code = '',
                      client_id = tdaclientid,
                      redirect_uri = redirecturi )

  stringified <- paste( names( encodedlist ), encodedlist, sep = "=", collapse = "&" )

  encodedpost <- httr::POST( 'https://api.tdameritrade.com/v1/oauth2/token',
                            httr::add_headers( 'Content-Type' = 'application/x-www-form-urlencoded' ),
                            body = stringified,
                            encode = 'form' )

  httpcontent <- httr::content( encodedpost )
  httpcontent[[ "access_token" ]]
}
