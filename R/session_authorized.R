#' Determine whether a session is authenticated
#' 
#' Returns TRUE if the session is a valid, non-NULL, active SB session.
#' 
#' @param session The session to check
#' @export
session_authorized <- function(session=current_session()){
	
	return(session_validate(session) && !is.null(session))
	
}