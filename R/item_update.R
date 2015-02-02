#'
#'@title Update a SB item with new metadata
#'
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
	
	return(r)
}