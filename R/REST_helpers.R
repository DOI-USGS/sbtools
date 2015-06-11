
#' @export
#' @keywords internal
sbtools_POST <- function(url, body, session, on_error=c("error","ask"), ...){
	
	on_error <- match.arg(on_error)
	if (is.null(session) & nrow(expand.grid(...)) == 0){
		session <- switch(on_error, 
											error = stop('no authenticate info or session specified. see ?authenticate_sb'),
											ask = authenticate_sb())
		
	} else {
		session <- session_check_reauth(session, ...)
	}
	
	!session_authorized(session)
	
	r = POST(url, accept_json(), 
					 body=body, handle=session) # need query too somewhere...

	return(r)
}