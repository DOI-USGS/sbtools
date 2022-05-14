
#for storing session state and service URLs
pkg.env <- new.env()
pkg.env$session = NULL
pkg.env$username = ""


.onLoad = function(libname, pkgname){
	set_endpoint()
}

#' Set the expiration
#' 
#' Called on package build
#' 
#' @param expiration_age a difftime describing the length of a session
#' @export
#' @keywords internal
set_expiration <- function(expiration_age = as.difftime("00:59:00")){
	if (!is(expiration_age, 'difftime') | length(expiration_age) != 1)
		stop('expiration_age must be a difftime vector of length 1')
	pkg.env$session_expires = expiration_age
}

default_timeout <- function(){
	return(10) #seconds
}

set_expiration()

check_session <- function(x) {
	if (!session_authorized(x))
		stop('session is not authorized. See ?authenticate_sb', call. = FALSE)	
}

session_authorized <- function(session){
	
	return(session_validate(session) && !is.null(session))
	
}

session_val <- function(x) {
	if (!session_validate(x)) {
		stop('Session state is invalid, please re-authenticate', call. = FALSE)
	}
}

comp <- function(l) Filter(Negate(is.null), l)
