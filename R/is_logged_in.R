#' Check whether you're logged into a ScienceBase session
#' 
#' @export
#' @param ... Additional parameters are passed on to \code{\link[httr]{GET}}
#' @param session SB session object from \code{\link{authenticate_sb}}
#' @return Logical, \code{TRUE} or \code{FALSE}
#' @examples \dontrun{
#' is_logged_in()
#' }
is_logged_in <- function(..., session = current_session()) {
	session_details(..., session = session)$isLoggedIn
}
