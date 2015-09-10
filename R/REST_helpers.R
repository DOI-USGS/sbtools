#' generic POSTs for sbtools package
#' package wrapped for generic POSTs that test sessions internally and wrap some errors
#' 
#' @param url a base url for the POST
#' @param body a POST body
#' @param session a sbtools session object
#' @param ... additional params passed to \code{\link[httr]{POST}}
#' @import httr
#' @export
#' @keywords internal
sbtools_POST <- function(url, body, ..., session){
	
	supported_types <- c('text/plain','application/json')
	if (!session_authorized(session))
			stop('session is not authorized. See ?authenticate_sb')
	
	r = POST(url=url, ..., accept_json(), body=body, handle=session) 

	if (!strsplit(headers(r)[['content-type']], '[;]')[[1]][1] %in% supported_types)
		stop('POST failed to ',url,'. check authorization and/or content')
	
	session_age_reset()
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
#' @import httr
#' @export
#' @keywords internal
sbtools_GET <- function(url, ..., session) {
	
	supported_types <- c('text/plain','text/csv','text/tab-separated-values','application/json','application/x-gzip')
	if (!session_validate(session))
		stop('session is not valid. See ?authenticate_sb')
	
	r = GET(url=url, ..., handle=session)
	
	if (!strsplit(headers(r)[['content-type']], '[;]')[[1]][1] %in% supported_types)
		stop('GET failed to ',url,'. check authorization and/or content')
	
	if('errors' %in% names(content(r))){
		stop(content(r)$errors$message)
	}
	
	session_age_reset()
	return(r)
}

#' generic PUTs for sbtools package
#' 
#' package wrapped for generic PUTs that test sessions internally and wrap some 
#' errors
#' 
#' @param url a base url for the PUT
#' @param ... additional params passed to \code{\link[httr]{PUT}}, e.g.,
#'   \code{accept_json()}
#' @param body the PUT body as in \code{\link[httr]{PUT}}
#' @param session a sbtools session object
#' @import httr
#' @export
#' @keywords internal
sbtools_PUT <- function(url, body, ..., session) {
	if (!session_validate(session))
		stop('session is not valid. See ?authenticate_sb')
	
	r = PUT(url=url, ..., body=body, handle=session)
	session_age_reset()
	return(r)
}

#' generic DELETEs for sbtools package
#' 
#' package wrapped for generic DELETEs that test sessions internally and wrap
#' some errors
#' 
#' @param url a base url for the DELETE
#' @param ... additional params passed to \code{\link[httr]{DELETE}}, e.g., 
#'   \code{accept_json()}
#' @param session a sbtools session object
#' @import httr
#' @export
#' @keywords internal
sbtools_DELETE <- function(url, ..., session) {
	if (!session_validate(session))
		stop('session is not valid. See ?authenticate_sb')
	
	r = DELETE(url=url, ..., handle=session)
	session_age_reset()
	return(r)
}