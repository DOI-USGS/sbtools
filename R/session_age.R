#' a convienence function for getting the age of a session. 
#' 
#' @template manipulate_item
#' @return difftime object
#' @examples \dontrun{
#' authenticate_sb('bbadger@@usgs.gov')
#' sbtools::session_age()
#' }
#' @export
#' @keywords internal
session_age <- function(){
	
	if(is.null(pkg.env$keycloak_expire)){
		return(NULL)
	}
	return(Sys.time() - pkg.env$keycloak_expire)
	
}
#' Check whether an SB session is expired
#' 
#' Check the expiration using session_age
#' 
#' @template manipulate_item
#' 
#' @export
#' @keywords internal
session_expired <- function() {
	
	if(is.null(session_age())){
		return(FALSE)
	}
	
	return(session_age() > 0)
	
}

