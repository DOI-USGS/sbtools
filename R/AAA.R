#for storing session state and service URLs

pkg.env <- new.env()
pkg.env$session = NULL

.onLoad = function(libname, pkgname){
	set_endpoint()
}
