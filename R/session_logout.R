#' Logout of a ScienceBase session
#' 
#' @export
#' @param ... Additional parameters are passed on to \code{\link[httr]{GET}}
#' @param session SB session object from \code{\link{authenticate_sb}}
#' @return invisible, returns nothing if logged out, or errors with message
#' @examples \dontrun{
#' session_logout()
#' }
session_logout <- function(..., session = current_session()) {
	if (!is_logged_in(session = session)) {
		stop("You're not logged in. See ?authenticate_sb", call. = FALSE)
	}
	ret <- httr::POST(paste0(pkg.env$url_base, 'j_spring_security_logout'), handle = session, ...)
	if (ret$status_code != 200) {
		stop("Logout did not succeed", call. = FALSE)
	}
	pkg.env$session <- NULL
	try(pkg.env$uid <- NULL, silent = TRUE)
	invisible()
}
