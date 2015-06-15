
#' @export
#' @keywords internal
sbtools_POST <- function(url, body, session, timeout = default_timeout(), ...){
	
	if (!session_authorized(session))
			stop('session is not authorized. See ?authenticate_sb')
	
	r = POST(url, accept_json(), 
					 body=body, handle=session, timeout(timeout)) 

	if (!is(content(r), 'list'))
		stop('POST failed to ',url,'. check authorization and/or content')
	
	
	return(r)
}

#' @export
#' @keywords internal
sbtools_GET <- function(url, query, session, timeout = default_timeout(), ...){
	
	if (!session_validate(session))
		stop('session is not valid. See ?authenticate_sb')
	
	r = GET(url = url, query = query, handle=session, ...)
	
	if('errors' %in% names(content(r))){
		stop(content(r)$errors$message)
	}

	return(r)
}