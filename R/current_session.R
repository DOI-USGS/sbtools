#'@title Return current cached session
#'
#'@description 
#'Returns the currently cached SB session. If there
#'is no authenticated session, returns NULL. Emits a
#'warning if the session has expired. 
#'
#'@author Luke Winslow
#'
#'@examples
#'
#'session = current_session()
#'
#'@export
current_session = function(){
	if(session_expired(pkg.env$session)){
		warning('SB authentication expired, SB interaction may fail. Please re-authenticate using authenticate_sb().')
	}
	return(pkg.env$session)
}