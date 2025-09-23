#' Convert numeric time to proper string representation.
#'
#' This takes a raw numerical time value (e.g., 900, 1300) and returns
#'   a properly configured string representation.
#'
#' @param x A numerical value for the time (e.g., 900, 1430 etc.).
#' @returns A properly configured time version (e.g., "9:30 AM", "2:30 PM").
#' @importFrom stringr str_sub
#' @export
#' @examples
#' times <- c(900, 1430)
#' format_for_hm( times )
#'
format_for_hm <- function( x ) {

  # catch errors
  if( length( x ) == 0 ) { return(NA) }
  if( all( is.na( x ) ) ) { return( rep(NA, length(x) ) ) }
  if( any( !is.numeric( x ) )  )  { stop("You must pass a numeric represenatation of time") }

  # this included function is inside (and hidden)
  # and does one instance of the conversion, checking
  # for various issues.
  makeTime <- function( raw ) {
    if( is.na(raw) ) { return( NA ) }
    tm <- as.numeric( raw )
    if( tm < 0 | tm > 2400 ) {
      stop("The hours you passed is not in the interval [0000 - 2400].")
      return( NA )
    }
    am.pm <- ifelse( tm < 1200, " AM", " PM")
    corr <- ifelse( tm > 1300, 12, 0 )
    hrs <- floor( tm / 100 ) - corr
    min = as.numeric( stringr::str_sub(raw, -2 ) )
    if( min < 0 | min > 59 ) {
      stop("You must specify minutes on the interval [00-59].")
      return( NA )
    }
    ret <- c( sprintf("%02d",hrs),
              ":",
              sprintf("%02d",min),
              am.pm )
    return( paste( ret, collapse = "" ) )
  }

  # This allows you to apply that included function to an array
  #  of values or to just a single value so both work and we can
  #  use this in tidy piping functions.
  return( unlist( Map( makeTime, x ) ) )
}


