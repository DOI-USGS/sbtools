
#for storing session state and service URLs
pkg.env <- new.env()
pkg.env$session = NULL
pkg.env$username = ""


.onAttach <- function(libname, pkgname) {
	packageStartupMessage("This information is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The information has not received final approval by the U.S. Geological Survey (USGS) and is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the information.")
}
.onLoad = function(libname, pkgname){
	set_endpoint()
}


.onAttach <- function(libname, pkgname) {
	packageStartupMessage("This information is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The information has not received final approval by the U.S. Geological Survey (USGS) and is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the information.")
}

#' @export
#' @keywords internal
set_expiration <- function(expiration_age = as.difftime("00:59:00")){
	if (!is(expiration_age, 'difftime') | length(expiration_age) != 1)
		stop('expiration_age must be a difftime vector of length 1')
	pkg.env$session_expires = expiration_age
}

set_expiration()