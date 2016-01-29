#' @title Get session info
#' 
#' @export
#' @param ... Additional parameters are passed on to \code{\link[httr]{GET}}
#' @param session SB session object from \code{\link{authenticate_sb}}
#' @return list, if not logged in states that, but if logged in, user details
#' @examples \dontrun{
#' session_info()
#' }
session_details <- function(..., session = current_session()) {
	x <- GET(paste0(pkg.env$url_base, "jossoHelper/sessionInfo"), 
					 handle = session, ...)
	stop_for_status(x)
	jsonlite::fromJSON(content(x, "text"))
}
