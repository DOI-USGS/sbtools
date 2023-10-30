#' Check whether you're logged into a ScienceBase session
#' 
#' @export
#' @return Logical, \code{TRUE} or \code{FALSE}
#' @examples \dontrun{
#' is_logged_in()
#' }
is_logged_in <- function() {
	!is.null(current_session()) && !session_expired()
}
