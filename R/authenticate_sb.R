
#'
#'@title Authenticate to SB for subsequent calls
#'
#'
#'
#'
#'
#'
#'
#'@import httr
#'@export
authenticate_sb = function(username, password){
	
	if(missing(username)){
		stop('username required for authentication')
	}
	
	if(!interactive() & missing(password)){
		stop('No password supplied to authenticate_sciencebase in a non-interactive session.')
	}else{
		password = ifelse(missing(password), readPassword('Please enter your Sciencebase password:'), password)
	}
	
	h = handle(url_base)
	
	## authenticate
	resp = GET(url_base, accept_json(), authenticate(username, password, type='basic'),
						 handle=h)
	
	sid = resp$cookies$JSESSIONID
	
	if(is.null(sid)){
		stop('Unable to authenticate to SB. Check username and password')
	}
	
	return(h)
}


readPassword <- function(prompt) {
	# found this cool function in rstudio
	if (exists(".rs.askForPassword")) {
		pass <- .rs.askForPassword(prompt)
	} else {
		pass <- readline(prompt)

	}
	return (pass)
}
