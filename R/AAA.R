
#for storing session state and service URLs
pkg.env <- new.env()
pkg.env$session = NULL
pkg.env$username = ""


.onAttach <- function(libname, pkgname) {
	packageStartupMessage(
		"\nThis information is preliminary or provisional and \nis subject to revision. It is being provided to meet \nthe need for timely best science. The information \nhas not received final approval by the U.S. Geological \nSurvey (USGS) and is provided on the condition that \nneither the USGS nor the U.S. Government shall be held \nliable for any damages resulting from the authorized \nor unauthorized use of the information.")
}
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

# helper fxn to pull out items by name, using either lapply or vapply
pluck <- function(x, name, type) {
	if (missing(type)) {
		lapply(x, "[[", name)
	} else {
		vapply(x, "[[", name, FUN.VALUE = type)
	}
}
