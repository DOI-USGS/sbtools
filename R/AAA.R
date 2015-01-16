
pkg.env <- new.env()

.onLoad = function(libname, pkgname){
	set_endpoint()
}
