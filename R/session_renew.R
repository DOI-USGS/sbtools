#' Checks current session and re-authenticates if necessary
#' 
#' Checks the state of your Sciencebase session, re-authenticates if the session
#' is expired, and simply renews if the session is active.
#' 
#' @param password The password to use, if needed, to renew the session.
#' @param username Optional. Used only to confirm that the current username is 
#'   what you expect; if you want to switch usernames, use 
#'   \code{authenticate_sb()} instead of this function.
#' @param session SB session object from \code{\link{authenticate_sb}}. Default 
#'   is the current session.
#' @param ... Any additional parameters are currently ignored.
#'   
#' @return Returns the session object.
#'   
#' @examples \dontrun{
#' # an empty call is sufficient if the session is current, 
#' # but will break if haven't been logged in before
#' session_renew()
#' 
#' # include a password if session may be expired
#' session_renew('newpass')
#' 
#' # optionally confirm the value of the current username
#' session_renew(username='olduser@usgs.gov', 'newpass')
#' }
#' @import httr
#' @export
session_renew = function(password, ..., username, session=current_session()){
	
	# if we'll need the current username, find it now. use the existing session
	# info on SB if available; otherwise use the username stored locally
	if(!missing(username) || !is_logged_in(session=session)) {
		sb_username <- session_details(session=session)$username
		if(is.null(sb_username)) sb_username <- pkg.env$username
	}
	
	# if username is provided, confirm that it matches the stored username
	if(!missing(username)) {
		if(username != sb_username) {
			stop("username argument does not match session username")
		}
	}
	
	# either renew or re-authenticate as needed
	if(is_logged_in(session=session)) {
		# the GET call to 'status' resets the remote (SB) info on session age, while 
		# sbtools_GET resets the local info on session age
		sbtools_GET(url=paste0(pkg.env$url_base, "status?format=json"), session=session)
		invisible(session)
	} else {
		# re-authenticate, handling missing parameters as needed
		if(sb_username=="") stop("new authentication is necessary; call authenticate_sb()")
		if(missing(password)) stop("re-authentication is necessary; need password")
		invisible(authenticate_sb(sb_username, password))
	}
}
