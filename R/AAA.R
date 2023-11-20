
#for storing session state and service URLs
pkg.env <- new.env()
pkg.env$username = ""


.onLoad = function(libname, pkgname){
	set_endpoint()
}

default_timeout <- function(){
	return(10) #seconds
}

check_session <- function(check_logged_in = FALSE) {
	check <- !session_validate()
	
	if(check_logged_in) check <- !(session_validate() & is_logged_in())
	
	if (check) {
		warning('session is not authorized. See ?authenticate_sb')
		FALSE 
	} else {
		TRUE
	}
}


