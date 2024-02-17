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

#' @title Get session info (deprecated)
#' 
#' @description
#' Get the details associated with current ScienceBase user session. 
#' 
#' @return list, if not logged in states that, but if logged in, user details
#' 
#' @examples \dontrun{
#' 
#' session_details()
#' 
#' }
#' @export
session_details <- function() {
	stop("not working with new login requirements")
	x <- RETRY("GET", paste0(pkg.env$url_base, "jossoHelper/sessionInfo?includeJossoSessionId=true"), 
						 get_token_header(), timeout = httr::timeout(default_timeout()))
	stop_for_status(x)
	jsonlite::fromJSON(content(x, "text"))
}


#' Validate sbtools session state
#' 
#' A session is considered valid if it is NULL or a true, non-expired SB session
#' 
#' This function only operates on the active initialized session.
#' 
#' @return \code{TRUE/FALSE} indicating if session is valid and can be used. 
#'   Returns TRUE if session is NULL as well.
#' 
#' @export
session_validate = function(){
	
	if(is.null(session_age()) & !is.null(current_session())) {
		return(FALSE)
	}
	
	if(is.null(current_session())){
		return(TRUE)
	}
	
	if(session_expired()){
		return(FALSE)
	}
	
	TRUE
	
}

session_val <- function() {
	if (!session_validate()) {
		stop('Session state is invalid, please re-authenticate', call. = FALSE)
	}
}

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

token_refresh <- function(client_id = pkg.env$keycloak_client_id, warn_on_fail = TRUE) {
	
	data = list(
		client_id = client_id,
		grant_type = "refresh_token",
		refresh_token = get_refresh_token())
	
	token <- RETRY("POST", pkg.env$token_url, body = data, encode = "form", quiet = TRUE)
	
	if(!token$status_code == 200) {
		if(warn_on_fail)
			warning('Unable to refresh SB cloud token. Some functionality may not work.')
		return(invisible(FALSE))
	} else {
		set_keycloak_env(token)
	}
	
	return(invisible(TRUE))
}

#' a convienence function for getting the age of a session. 
#' 
#' @return difftime object
#' @examples \dontrun{
#' authenticate_sb('bbadger@@usgs.gov')
#' sbtools::session_age()
#' }
#' @export
#' @keywords internal
session_age <- function(){
	
	if(is.null(pkg.env$keycloak_expire)){
		return(NULL)
	}
	return(Sys.time() - pkg.env$keycloak_expire)
	
}
#' Check whether an SB session is expired
#' 
#' Check the expiration using session_age
#' 
#' @export
#' @keywords internal
session_expired <- function() {
	
	if(is.null(session_age())){
		return(FALSE)
	}
	
	return(session_age() > 0)
	
}



