#' @title Return current cached session
#' 
#' @description 
#' Returns the currently cached SB session token. If there
#' is no authenticated session, returns NULL. Emits a
#' warning if the session has expired. 
#' 
#' @examples \donttest{
#' 
#' session = current_session()
#' #null unless currently authenticated
#' session
#' }
#'@export
current_session = function(){
	if(session_expired()){
		warning('SB authentication expired, SB interaction may fail. Please re-authenticate.')
	}
	return(pkg.env$keycloak_token)
}