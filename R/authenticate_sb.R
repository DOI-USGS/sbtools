
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
	resp = GET(pkg.env$url_base, accept_json(), authenticate(username, password, type='basic'),
						 handle=h)
	
	if(!any(resp$cookies$name %in% 'JSESSIONID')){
		stop('Unable to authenticate to SB. Check username and password')
	}
	
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