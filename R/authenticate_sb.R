
#' Authenticate to SB for subsequent calls
#' 
#' This connects to SB, authenticates and gets a session token for communicating
#' with SB. If you do not supply a username or password, you will be prompted to
#' enter them.
#' 
#' @param username Sciencebase username
#' @param password Sciencebase password, prompts user if not supplied and
#' no password is returned by `keyring::key_get("sciencebase", username)`.
#' See \code{\link[keyring]{keyring-package}} documentation for more details.
#'   
#' @import httr
#'   
#' @export
authenticate_sb = function(username, password){
	
	if(missing(username)) {
		username <- try(session_details()$username)
	}
	
	if((inherits(username, "try-error") | is.null(username)) && !interactive()){
		
		stop('username required for authentication')
	
	}else if(is.null(username) && interactive()){
		
		username = readline('Please enter your username:')
		if(username == ""){
			stop('Empty username supplied, stopping')
		}
		
	}
	
	pkg.env$username <- username
	
	keyring_pass = FALSE
	if(missing(password)) {
		
		password <- try(keyring::key_get("sciencebase", username), silent = TRUE)
		
		if(!inherits(password, "try-error")) keyring_pass = TRUE
		
	}
	
	if(!interactive() & inherits(password, "try-error")){
		
		stop('No password supplied to authenticate_sciencebase in a non-interactive session.')
		
	} else {
		
		password = ifelse(missing(password) | inherits(password, "try-error"), 
											readPassword('Please enter your Sciencebase password:'), 
											password)
		
	}
	
	h = handle(pkg.env$url_base)
	
	## authenticate
	resp = RETRY("GET", pkg.env$url_base, accept_json(), 
							 authenticate(username, password, type='basic'),
							 handle=h, timeout = httr::timeout(default_timeout()))
	
	if(!any(resp$cookies$name %in% 'JSESSIONID')){
		if(keyring_pass) stop("Sciencebase login failed with stored password?")
		warning('Unable to authenticate to legacy SB.')
	}
	
	token_url <- pkg.env$token_url
	
	pkg.env$keycloak_client_id <- "sciencebasepy"
	
	token <- RETRY("POST", token_url, 
								 body = list(
								 	client_id = pkg.env$keycloak_client_id,
								 	grant_type = "password",
								 	username = username,
								 	password = password
								 ), encode = "form")
	
	if(!token$status_code == 200) {
		
		warning('Unable to authenticate to SB cloud. Standard login is available.')
		
	} else {
	
		set_keycloak_env(token)
		
	}
	
	return(invisible(TRUE))
}

set_keycloak_env <- function(token_resp) {
	pkg.env$keycloak_token <- jsonlite::fromJSON(rawToChar(token_resp$content))
	
	pkg.env$keycloak_expire <- Sys.time() + pkg.env$keycloak_token$expires_in
}

#' Initialize ScienceBase Session
#' @description opens a browser for two factor authentication. A token can be
#' retrieved from the user drop down in the upper right once logged in.
#' The token should be pasted into the console. Can also be called with a pre-
#' fetched token.
#' @param token_text character json formatted token text
#' @param username email address of sciencebase user.
#' @export
#' 
initialize_sciencebase_session <- function(username, token_text = NULL) {
	if(is.null(token_text)) {
		message("A browser will open ", pkg.env$manager_app)
		message("Log in and retrieve a token from the user menu in the upper right.")
		message("Paste the token in the dialogue box that opens.")
		utils::browseURL(pkg.env$manager_app)
		token_text <- readPassword('Please enter your Sciencebase token string:')
	}
	
	pkg.env$username <- username
	
	try(initialize_keycloack_env(token_text))
}

initialize_keycloack_env <- function(token_text) {
	pkg.env$keycloak_token <- jsonlite::fromJSON(token_text)
	
	pkg.env$keycloak_expire <- Sys.time()
	
	token_refresh()
}

#' Read in a password from the user
#' 
#' Uses the RStudio pop-up password window if available
#' 
#' @importFrom utils globalVariables
#' @keywords internal
readPassword <- function(prompt) {
	# found this cool function in rstudio
	if (exists(".rs.askForPassword", mode = "function")) {
		pass <- .rs.askForPassword(prompt)
	} else {
		pass <- readline(prompt)
	}
	return (pass)
}

globalVariables('.rs.askForPassword')

get_refresh_token <- function() {
	token <- pkg.env$keycloak_token$refresh_token
	
	if(is.null(token)) {
		stop("no token found, must call authenticate_sb()")
	}
	
	token
}

get_access_token <- function() {
	token <- pkg.env$keycloak_token$access_token
	
	if(is.null(token)) {
		stop("no token found, must call authenticate_sb()")
	}
	
	token
}

get_token_header <- function() {
	if(is.null(current_session())) return(httr::add_headers())
	
	httr::add_headers(
		.headers = c(authorization = paste("Bearer", 
																			 get_access_token())))
}
