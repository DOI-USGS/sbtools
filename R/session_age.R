#' a convienence function for getting the age of a session. 
#' 
#' See pkg.env$session_birthdate for time when session was created.
#' See pkg.env$session_expires for session age when session goes stale
#' 
#' @return difftime object
#' @examples 
#' \dontrun{
#' authenticate_sb('bbadger@@usgs.gov')
#' sbtools::session_age()
#' }
#' @export
#' @keywords internal
session_age <- function(session = current_session()){
	
	if(is.null(session)){
		return(NULL)
	}
	return(Sys.time() - attr(session, "birthdate"))
	
}
#' @export
#' @keywords internal
session_expired <- function(session = current_session()){
	
	if(is.null(session)){
		return(FALSE)
	}
	return(session_age(session) > pkg.env$session_expires)
	
}

