#'
#'@title Query SB for item based on custom identifier
#'
#'@param scheme The identifier scheme
#'@param type The identifier type
#'@param key The identifier key
#'@param session (optional) SB Session to use, not provided queries public items only
#'
#'@return The SB item id for the matching item
#'
#'
#'@export
query_item_identifier = function(scheme, type, key, session=NULL){
	
	
	#not sure if this is necessary
	if(missing(session) || is.null(session)){
		r = GET(url_item, query=list(scheme=scheme, type=type, key=key), handle=session)
	}else{
		r = GET(url_item, query=list(scheme=scheme, type=type, key=key))
	}
	
	if(r$status_code == 409){
		stop('Multiple items described by that ID')
	}
	
	return(content(r,'parsed')$id)
}
