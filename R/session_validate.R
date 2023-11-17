#' Validate sbtools session state
#' 
#' A session is considered valid if it is NULL or a true, non-expired SB session
#' 
#' This function only operates on the active initialized session.
#' 
#' @return \code{TRUE/FALSE} indicating if session is valid and can be used. 
#'   Returns TRUE if session is NULL as well.
#' 
#' @export
session_validate = function(){
	
	if(is.null(session_age()) & !is.null(current_session())) {
		return(FALSE)
	}
	
	if(is.null(current_session())){
		return(TRUE)
	}
	
	if(session_expired()){
		return(FALSE)
	}
	
	TRUE

}

session_val <- function() {
	if (!session_validate()) {
		stop('Session state is invalid, please re-authenticate', call. = FALSE)
	}
}