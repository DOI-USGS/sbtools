session_authorized <- function(session){
	
	return(session_validate(session) && !is.null(session))
	
}