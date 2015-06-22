#'@title Update a SB item with new metadata
#'
#'@param id SB item ID
#'@param info list of metadata info for the new item
#' @param ... Additional parameters are passed on to \code{\link[httr]{PUT}}
#'@param session Authenticated session object (from \link{authenticate_sb})
#'
#'@return List serialization of complete metadata for SB item
#'
#'@import httr
#'@import jsonlite
#'
#'@export
item_update = function(id, info, ..., session=current_session()){
	
	if(!is.list(info)){
		stop('Info must be a list of name-value pairs that can be serialized into JSON')
	}
	
	r = sbtools_PUT(url=paste0(pkg.env$url_item, id), 
									body=toJSON(info, auto_unbox=TRUE),
									..., accept_json(), session=session)
	
	#catch 405, which is, I think, expired login
	
	if('errors' %in% names(content(r))){
	  stop(content(r)$errors$message)
	}
	return(content(r))
}