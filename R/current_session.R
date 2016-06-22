#' @title Return current cached session
#' 
#' @description 
#' Returns the currently cached SB session. If there
#' is no authenticated session, returns NULL. Emits a
#' warning if the session has expired. 
#' 
#' 
#' 
#' @examples
#' 
#' session = current_session()
#' #null unless currently authenticated
#' session
#'
#'@export
current_session = function(){
	if(session_expired(pkg.env$session)){
		warning('SB authentication expired, SB interaction may fail. Please re-authenticate using authenticate_sb().')
	}
	return(pkg.env$session)
}