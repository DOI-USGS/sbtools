
#for storing session state and service URLs
pkg.env <- new.env()
pkg.env$username = ""


.onLoad = function(libname, pkgname){
	set_endpoint()
}

default_timeout <- function(){
	return(10) #seconds
}

check_session <- function(x) {
	if (!session_validate()) {
		warning('session is not authorized. See ?authenticate_sb')
		FALSE 
	} else {
		TRUE
	}
}


