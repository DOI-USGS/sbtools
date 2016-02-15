#' Determine whether sbtools session state is valid
#' 
#' A session is valid if it is NULL or a true, non-expired SB session
#' 
#' @param session sbtools session object (from \code{\link{authenticate_sb}})
#' @return \code{TRUE/FALSE} indicating if session is valid and can be used. 
#'   Returns TRUE if session is NULL as well.
#'   
#' @details This validates the underyling RCurl session. The session object 
#'   becomes invalid if the R session has been saved to disk or persisted 
#'   through an R restart or been untouched for more than an hour. This
#'   functionverifies that the session object is either valid, or is a NULL
#'   object, which means no session state is being persisted. Note, this does
#'   not verify the credentials are valid or that you have permission to access
#'   the SB item, so it does not guarantee a successful request.
#'   
#' @examples
#' \dontrun{
#' 
#' # TRUE if not logged in at all
#' if(is_logged_in()) session_logout()
#' session_validate()
#' 
#' # TRUE if logged in & current
#' authenticate_sb() 
#' session_validate()
#' 
#' # FALSE after 60+ mins of inactivity
#' }
#' 
#' @export
session_validate = function(session=current_session()){
	
	is.null(session) || is_logged_in(session=session)
	
}

