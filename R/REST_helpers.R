
#' @export
#' @keywords internal
sbtools_POST <- function(url, body, session, timeout = default_timeout()){
	
	if (is.null(timeout))
		timeout <- pkg.env$POST.timeout
	
	if (!session_authorized(session))
			stop('session is not authorized. See ?authenticate_sb')
	
	r = POST(url, accept_json(), 
					 body=body, handle=session, timeout(timeout)) 

	if (!is(content(r), 'list'))
		stop('POST failed to ',url,'. check authorization and/or content')
	
	
	return(r)
}