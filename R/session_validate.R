#' Validate sbtools session state
#' 
#' A session is considered valid if it is NULL or a true, non-expired SB session
#' 
#' @param session sbtools session object (from \code{\link{authenticate_sb}})
#' @return \code{TRUE/FALSE} indicating if session is valid and can be used. 
#'   Returns TRUE if session is NULL as well.
#'   
#' @details This validates the underyling RCurl session. The session object
#' becomes invalid if the R session has been saved to disk or persisted through
#' an R restart. This verifies that the session object is either valid, or is a
#' NULL object, which means no session state is being persisted. Note, this does
#' not verify the credentials are valid or that you have permission to access
#' the SB item, so it does not guarantee a successful request.
#' 
#' @importFrom methods new
#' 
#' @examples \dontrun{
#' session = authenticate_sb('user@@usgs.gov')
#' 
#' #return true as underlying RCurl session is valid
#' session_validate(session)
#' }
#' 
#' @export
session_validate = function(session=current_session()){
	
	
	if(!is.null(session) & !is(session, 'handle')){
		return(FALSE)
	}
	
	if(is.null(session)){
		return(TRUE)
	}
	
	if(session_expired(session)){
		return(FALSE)
	}
	
	
	#from here:
	#https://stackoverflow.com/questions/26666614/how-do-i-check-if-an-externalptr-is-null-from-within-r
	#may have to implement C code
	bare_ptr = new('externalptr')
	attr(bare_ptr, 'class') = 'curl_handle'
	return(!identical(session[['handle']], bare_ptr))
}

