#' Set the session to a user-specified value
#' 
#' @param session The new session, or NULL
#' @keywords internal
#' @export
session_set <- function(session=NULL) {
	if(!session_validate(session)) warning("new session is not valid; setting anyway")
	pkg.env$session <- session
}