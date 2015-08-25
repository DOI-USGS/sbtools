#'@title Return current cached session
#'
#'@description 
#'Returns the currently cached SB session. If there
#'is no authenticated session, returns NULL.
#'
#'@author Luke Winslow
#'
#'@examples
#'
#'session = current_session()
#'
#'@export
current_session = function(){
	
	if(!session_validate(pkg.env$session)){		
		#if the session is invalid, NULL it out just because		
		pkg.env$session = NULL		
	}
	
	return(pkg.env$session)
}