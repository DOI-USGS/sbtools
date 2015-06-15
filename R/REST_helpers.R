#' generic POSTs for sbtools package
#' package wrapped for generic POSTs that test sessions internally and wrap some errors
#' 
#' @param url a base url for the POST
#' @param body a POST body
#' @param session a sbtools session object
#' @param timeout a timeout for the request in seconds
#' @param ... additional params passed to \code{\link[httr]{GET}}
#' @export
#' @keywords internal
#' @import XML
sbtools_POST <- function(url, body, session, timeout = default_timeout(), ...){
	
	if (!session_authorized(session))
			stop('session is not authorized. See ?authenticate_sb')
	
	r = POST(url, accept_json(), 
					 body=body, handle=session, timeout(timeout)) 

	if (!is(content(r), 'list'))
		stop('POST failed to ',url,'. check authorization and/or content')
	
	
	return(r)
}

#' generic GETs for sbtools package
#' package wrapped for generic GETs that test sessions internally and wrap some errors
#' 
#' @param url a base url for the GET
#' @param query a list of query params 
#' @param session a sbtools session object
#' @param timeout a timeout for the request in seconds
#' @param ... additional params passed to \code{\link[httr]{GET}}
#' @export
#' @keywords internal
#' @import XML
sbtools_GET <- function(url, query, session, timeout = default_timeout(), ...){
	
	if (!session_validate(session))
		stop('session is not valid. See ?authenticate_sb')
	
	r = GET(url = url, query = query, handle=session, timeout(timeout), ...)
	
	if('errors' %in% names(content(r))){
		stop(content(r)$errors$message)
	}

	return(r)
}