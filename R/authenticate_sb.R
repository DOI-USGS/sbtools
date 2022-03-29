
#' Authenticate to SB for subsequent calls
#' 
#' This connects to SB, authenticates and gets a session token for communicating
#' with SB. If you do not supply a username or password, you will be prompted to
#' enter them.
#' 
#' @param username Sciencebase username
#' @param password Sciencebase password, prompts user if not supplied
#'   
#' @import httr
#'   
#' @export
authenticate_sb = function(username, password){
	
	if(missing(username) && !interactive()){
		
		stop('username required for authentication')
	
	}else if(missing(username) && interactive()){
		
		username = readline('Please enter your username:')
		if(username == ""){
			stop('Empty username supplied, stopping')
		}
		
	}
	
	if(!interactive() & missing(password)){
		stop('No password supplied to authenticate_sciencebase in a non-interactive session.')
	}else{
		password = ifelse(missing(password), readPassword('Please enter your Sciencebase password:'), password)
	}
	
	h = handle(pkg.env$url_base)
	
	## authenticate
	resp = GET(pkg.env$url_base, accept_json(), 
						 authenticate(username, password, type='basic'),
						 handle=h, timeout = httr::timeout(default_timeout()))
	
	if(!any(resp$cookies$name %in% 'JSESSIONID')){
		stop('Unable to authenticate to SB. Check username and password')
	}
	
	token_url <- pkg.env$token_url
	
	token <- POST(token_url, 
								body = list(
									client_id = pkg.env$keycloak_client_id,
									grant_type = "password",
									username = username,
									password = password
								), encode = "form")
	
	if(!token$status_code == 200) {
		stop('Unable to authenticate to SB cloud. Check username and password')
	}
	
	pkg.env$keycloak_token <- jsonlite::fromJSON(rawToChar(token$content))
	
	pkg.env$keycloak_expire <- Sys.time() + pkg.env$keycloak_token$expires_in
	
	attributes(h) <- c(attributes(h), list(birthdate=Sys.time()))
	pkg.env$session  = h
	pkg.env$username = username
	
	invisible(h)
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
		stop("no token found, must call athenticate_sb()")
	}
	
	token
}

get_access_token <- function() {
	token <- pkg.env$keycloak_token$access_token
	
	if(is.null(token)) {
		stop("no token found, must call athenticate_sb()")
	}
	
	token
}