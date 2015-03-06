#'@title Update a SB item with new metadata
#'
#'@param id SB item ID
#'@param info list of metadata info for the new item
#'@param session Authenticated session object (from \link{authenticate_sb})
#'
#'@return List serialization of complete metadata for SB item
#'
#'@import httr
#'@import jsonlite
#'
#'@export
item_update = function(id, info, session){
	
	if(!is.list(info)){
		stop('Info must be a list of name-value pairs that can be serialized into JSON')
	}
	
	if(!session_validate(session)){
		stop('Session state is invalid, please re-authenticate')
	}
	
	r = PUT(paste0(pkg.env$url_item, id), handle=session,
					body=toJSON(info, auto_unbox=TRUE), accept_json())
	
	#catch 405, which is, I think, expired login
	
	if('errors' %in% names(content(r))){
	  stop(content(r)$errors$message)
	}
	return(content(r))
}