#' Checks current session and re-authenticates if necessary
#' 
#' Checks the state of your Sciencebase session, re-authenticates if the session
#' is expired, and simply renews if the session is active.
#' 
#' @param password The password to use, if needed, to renew the session.
#' @param username Optional. Used only to confirm that the current username is 
#'   what you expect; if you want to switch usernames, use 
#'   \code{authenticate_sb()} instead of this function.
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
session_renew = function(password, ..., username){
	
	# # if we'll need the current username, find it now. use the existing session
	# # info on SB if available; otherwise use the username stored locally
	# if(!missing(username) || !is_logged_in()) {
	# 	# TODO: bring back session_details?
	# 	# sb_username <- session_details(session=session)$username
	# 	sb_username <- pkg.env$username
	# }
	
	sb_username <- pkg.env$username
	
	# if username is provided, confirm that it matches the stored username
	if(!missing(username)) {
		if(username != sb_username) {
			stop("username argument does not match session username")
		}
	}

	# TODO bring back session_details?
	# if(missing(username) && !exists("sb_username"))
	# 	sb_username <- session_details(session=session)$username
	
	password <- try(keyring::key_get("sciencebase", sb_username))
		
	# either renew or re-authenticate as needed
	if(is_logged_in()) {
		
		# if(!inherits(password, "try-error")) {
		# 	
		# 	# just reauthenticate
		# 	invisible(authenticate_sb(username, password))
		# 	
		# } else {
			
			token_refresh()
			
		# }
	} else {
		
		# re-authenticate, handling missing parameters as needed
		if(sb_username=="" | inherits(sb_username, "try-error")) stop("new authentication is necessary")
		if(inherits(password, "try-error")) stop("re-authentication is necessary")
		
		invisible(authenticate_sb(sb_username, password))
	}
}


refresh_token_before_expired <- function(refresh_amount_seconds = 600) {
	
	current_time <- Sys.time() + refresh_amount_seconds
	
	if(!is.null(pkg.env$keycloak_token) && 
		 (is.null(pkg.env$keycloak_expire) || pkg.env$keycloak_expire - current_time < 0)) {
		return(token_refresh())
	}
	return(invisible(FALSE))
}

token_refresh <- function(client_id = pkg.env$keycloak_client_id) {
	
	data = list(
		client_id = client_id,
		grant_type = "refresh_token",
		refresh_token = get_refresh_token())
	
	token <- RETRY("POST", pkg.env$token_url, body = data, encode = "form")
	
	if(!token$status_code == 200) {
		warning('Unable to refresh SB cloud token. Some functionality may not work.')
		return(invisible(FALSE))
	} else {
		set_keycloak_env(token)
	}
	
	return(invisible(TRUE))
}
