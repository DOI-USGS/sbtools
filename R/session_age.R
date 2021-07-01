#' a convienence function for getting the age of a session. 
#' 
#' See pkg.env$session_birthdate for time when session was created.
#' See pkg.env$session_expires for session age when session goes stale
#' 
#' @template manipulate_item
#' @return difftime object
#' @examples \dontrun{
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
#' Check whether an SB session is expired
#' 
#' Check the expiration using session_age
#' 
#' @template manipulate_item
#' 
#' @export
#' @keywords internal
session_expired <- function(session = current_session()){
	
	if(is.null(session)){
		return(FALSE)
	}
	return(session_age(session) > pkg.env$session_expires)
	
}

