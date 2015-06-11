#'@title Checks current session and re-authenticates if necessary
#'
#'@inheritParams authenticate_sb
#'@param session (Advanced) Alternatively supply a specific session to be checked
#'@param ... Any additional parameters are ignored. 
#'
#'@description
#'This checks the state of your Sciencebase session and, if invalid or if you've supplied
#'a new user name, it will re-authenticate. This is primarily a helper function for 
#'packages that reverse-depend on sbtools.
#'
#'@examples
#'
#'\dontrun{
#'
#'#will prompt for username/pass if needed
#'session_check_reauth()
#'
#'session_check_reauth('user@@usgs.gov')
#'
#'#default behavior
#'session_check_reauth(session=current_session())
#'
#'}
#'
#'
#'@export
session_check_reauth = function(username, password, session, ...){
	
	
	if(!missing(username)){
		
		#if new username, re-auth
		if(username != pkg.env$username){
			if(missing(password)){
				authenticate_sb(username)
			}else{
				authenticate_sb(username, password)
			}
		}
		
	}else if(!missing(session)){ #if we've passed in a session, check
		
		if(!session_validate(session) && pkg.env$username != ""){
			authenticate_sb(pkg.env$username)
		}else{
			authenticate_sb()
		}
			
	}else{ #if we have passed nothing, use cached session
		
		session = current_session()
		if(!session_validate(session) || is.null(session)){
			if(pkg.env$username != ""){
				authenticate_sb(pkg.env$username)
			}else{
				authenticate_sb()
			}
		}
	}
}