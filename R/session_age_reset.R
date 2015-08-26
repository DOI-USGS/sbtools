#' session reset age
#' 
#' internal function for refreshing session age to 0 using Sys.time()
#' 
#' @param session session object (from \link{authenticate_sb})
#' @keywords internal
#' @export
session_age_reset <- function(session){
	
	if (missing(session)){
		session = current_session()
		if (!is.null(session)){
			attr(session, "birthdate") <- Sys.time()
		}
		session_set(session)
		
	} else {
		if (!is.null(session)){
			attr(session, "birthdate") <- Sys.time()
		}
	}
	
	invisible(session)
}
