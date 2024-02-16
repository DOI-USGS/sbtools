
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

	message(
		"authenticate_sb no longer works for individual login sessions, because ",
		"sciencebase now requires multi-factor authentication.\n\n",
		"Use initialize_sciencebase_session() ",
		"instead (except in the rare case where you are using a role account)."
	)
	
	# TODO: bring back session_details?	
	username <- try(get_username(username), silent = TRUE)
	
	if((inherits(username, "try-error") | is.null(username)) && !interactive()){
		
		stop('username required for authentication')
	
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
	}
	
	token_url <- pkg.env$token_url
	
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

get_username <- function(username = NULL) {
	if(is.null(username)) {
		
		username <- Sys.getenv("sb_user")
		
		if(username != "") {
			pkg.env$username <- username
			return(username)
		}
		
		if(interactive()) {
			
			username = readline('Please enter your username:')
			
			if(username == ""){
				stop('Empty username supplied, stopping')
			} 
			
		}else {
			
			stop("username required for authentication")
		}
		# username <- try(session_details()$username)
	} 
	
	pkg.env$username <- username
	
	username
	
}

set_keycloak_env <- function(token_resp) {
	try({
		json <- jsonlite::fromJSON(rawToChar(token_resp$content))
		
		if(!"error" %in% names(json)) {
			pkg.env$keycloak_token <- json
			
			pkg.env$keycloak_expire <- Sys.time() + pkg.env$keycloak_token$expires_in

		}
	}, silent = TRUE)
}

#' Initialize ScienceBase Session
#' @description Unless `token_text` is provided, will open a browser for two 
#' factor authentication. 
#' 
#' Once logged in, retrieve the token from the user drop down in the upper 
#' right hand corner of the browser. Click the icon with the silhouette of 
#' a person, and select 'Copy API Token.' The token should be pasted into the 
#' popup prompt. 
#' 
#' @param token_text character json formatted token text. `token_text` is 
#' stashed in \link[tools]{R_user_dir} and does not need to be re-entered unless 
#' it becomes stale.
#' 
#' If the token text is provided as input, no popup prompt will be raised.
#' 
#' @param username email address of sciencebase user. Will be retrieved from the 
#' `sb_user` environment variable if set. A prompt will be raised if not provided.
#' @export
#' 
initialize_sciencebase_session <- function(username = NULL, token_text = NULL) {
	
	username <- get_username(username)
	
	if(is.null(token_text)) {
		
		token <- gsub("[\r\n]", "", grab_token())
		
		if(token != "") {
			check_current <- try(
				initialize_keycloack_env(
					token, warn_on_fail = FALSE), 
				silent = TRUE)
			
			if(isTRUE(check_current)) {
				pkg.env$username <- username
				return(invisible(TRUE))
			}
		}
		
		message("A browser will open ", pkg.env$manager_app)
		message("Log in and copy a token from the 'User' menu in the upper right.")
		message("Paste the token in the dialogue box that opens.")
		utils::browseURL(pkg.env$manager_app)
		token_text <- readPassword('Please enter your Sciencebase token string:')
		
	}
	
	pkg.env$username <- username
	
	worked <- try(initialize_keycloack_env(token_text))
	
	if(!inherits(worked, "try-error")) {
		stache_token(token_text)
		return(invisible(TRUE))
	} else {
		return(invisible(FALSE))
	}
}

initialize_keycloack_env <- function(token_text, warn_on_fail = TRUE) {
	pkg.env$keycloak_token <- jsonlite::fromJSON(token_text)
	
	pkg.env$keycloak_expire <- Sys.time()
	
	token_refresh(warn_on_fail = warn_on_fail)
}

# utility to clean environment for testing
clean_session <- function() {
	pkg.env$keycloak_token <- NULL
	
	pkg.env$keycloak_expire <- NULL
	
	pkg.env$keycloak_client_id <- "catalog"
	
	pkg.env$username = ""
	
	pkg.env$uid <- NULL
}

#' Get or set token stache data directory
#' @description if left unset, will return the user data dir
#' as returned by `tools::R_user_dir` for this package.
#' @param dir path of desired token stache file
#' @return character path of data directory (silent when setting)
#' @importFrom tools R_user_dir
#' @noRd
#'
token_stache_path <- function(dir = NULL) {
	
	if(is.null(dir)) {
		token_stache <- try(get("token_stache", envir = pkg.env), silent = TRUE)
	
		if(inherits(token_stache, "try-error")) {
			assign("token_stache", 
						 file.path(tools::R_user_dir(package = "sbtools"), "token"),
						 envir = pkg.env)
		}
		
		return(get("token_stache", envir = pkg.env))
	} else {
		assign("token_stache", 
					 dir,
					 envir = pkg.env)
		return(invisible(get("token_stache", envir = pkg.env)))
	}
	
	
}

stache_token <- function(token_text) {
	dir.create(dirname(token_stache_path()), recursive = TRUE, showWarnings = FALSE)
	
	write(token_text, file = token_stache_path())
}

grab_token <- function() {
	
	if(file.exists(token_stache_path())) {
		readChar(token_stache_path(), file.info(token_stache_path())$size)
	} else {
		""
	}
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
		message("paste your token - expecting up to four lines")
		pass <- readLines(n = 4)
		pass <- paste(token, collapse = "")
	}
	return (pass)
}

globalVariables('.rs.askForPassword')

get_refresh_token <- function() {
	token <- try(pkg.env$keycloak_token$refresh_token, silent = TRUE)
	
	if(inherits(token, "try-error") | is.null(token)) {
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
	if(is.null(current_session()) | is.null(session_age())) return(httr::add_headers())
	
	httr::add_headers(
		.headers = c(authorization = paste("Bearer", 
																			 get_access_token())))
}

#' Check whether you're logged into a ScienceBase session
#' 
#' @export
#' @return Logical, \code{TRUE} or \code{FALSE}
#' @examples \dontrun{
#' is_logged_in()
#' }
is_logged_in <- function() {
	!is.null(current_session()) && !session_expired()
}
