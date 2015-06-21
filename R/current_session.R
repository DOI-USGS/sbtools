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

	
	return(pkg.env$session)
}