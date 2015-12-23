#' Ping ScienceBase to see if it's available
#' 
#' @export
#' @param ... Additional parameters are passed on to \code{\link[httr]{GET}}
#' @return list, with \code{"OK"} if ScienceBase is up
#' @examples \dontrun{
#' sb_ping()
#' }
sb_ping <- function(...) {
	x <- GET(paste0(pkg.env$url_item, 'ping'), ...)
	jsonlite::fromJSON(content(x, "text"))
}
