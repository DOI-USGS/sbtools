#' generic POSTs for sbtools package
#' package wrapped for generic POSTs that test sessions internally and wrap some errors
#' 
#' @param url a base url for the POST
#' @param body a POST body
#' @param session a sbtools session object
#' @param ... additional params passed to \code{\link[httr]{POST}}
#' @export
#' @keywords internal
sbtools_POST <- function(url, body, session, ...){
	supported_types <- c('text/plain','application/json')
	if (!session_authorized(session))
			stop('session is not authorized. See ?authenticate_sb')
	
	r = POST(url, accept_json(), 
					 body=body, handle=session, config=config, ...) 

	if (!strsplit(headers(r)[['content-type']], '[;]')[[1]][1] %in% supported_types)
		stop('POST failed to ',url,'. check authorization and/or content')
	
	return(r)
}

#' generic GETs for sbtools package
#' 
#' package wrapped for generic GETs that test sessions internally and wrap some
#' errors
#' 
#' @param url a base url for the GET
#' @param ... additional params passed to \code{\link[httr]{GET}}, often 
#'   including \code{query}
#' @param session a sbtools session object
#' @export
#' @keywords internal
sbtools_GET <- function(url, ..., session){
	
	supported_types <- c('text/plain','application/json')
	if (!session_validate(session))
		stop('session is not valid. See ?authenticate_sb')
	
	r = GET(url = url, handle=session, ...)
	
	if (!strsplit(headers(r)[['content-type']], '[;]')[[1]][1] %in% supported_types)
		stop('GET failed to ',url,'. check authorization and/or content')
	
	if('errors' %in% names(content(r))){
		stop(content(r)$errors$message)
	}

	return(r)
}