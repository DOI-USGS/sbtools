#'
#'@title Query SB for items based on custom identifier
#'
#'@param scheme The identifier scheme
#'@param type (optional) The identifier type
#'@param key (optional) The identifier key
#'@param session (optional) SB Session to use, not provided queries public items only
#'@param limit Max number of matching items to return
#'
#'@return The SB item id for the matching item. NULL if no matching item found.
#'
#'@import httr
#'@export
query_item_identifier = function(scheme, type=NULL, key=NULL, session=current_session(), limit=20){
	
	if(!session_validate(session)){
		stop('Session state is invalid, please re-authenticate')
	}
	
	filter_all = list('scheme'=scheme, 'type'=type, 'key'=key)
	
	filter_items = Filter(Negate(is.null), filter_all)

	
	filter = paste0('itemIdentifier=', toJSON(filter_items, auto_unbox=TRUE))
	
	query = list('filter'=filter, 'max'=limit, 'format'='json')
	
	r = GET(pkg.env$url_items, query=query, handle=session)
	
	if(r$status_code == 409){
		stop('Multiple items described by that ID')
	}
	
	response = content(r, 'parsed')
	
	#check if no items matched
	if(length(response$items) == 0){
		return(data.frame())
	}
	
	out = data.frame(title=NA, id=NA)
	# if we have items, populate data.frame and return
	for(i in 1:length(response$items)){
		out[i,]$title = response$items[[i]]$title
		out[i,]$id = response$items[[i]]$id
	}
	
	return(out)
}


