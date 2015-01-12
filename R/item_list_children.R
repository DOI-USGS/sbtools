#'
#'@title Return IDs for all child items
#'
#'@param id SB item ID
#'@param session (optional) SB session from \link{authenticate_sb}
#'@param limit Max children returned
#'
#'@return \code{data.frame} with a row for each child item 
#'
#'@examples
#'
#'item_list_children('5060b03ae4b00fc20c4f3c8b')
#'
#'
#'@export
item_list_children = function(id, session, limit=20){
	
	if(missing(session)){
		session = handle(url_base)
	}
	
	r = GET(url_items, 
					query=list('parentId'=id, 'max'=limit, 'format'='json', 'fields'='id'), 
					handle = session)
	
	items = content(r, 'parsed')$items
	
	if(length(items) < 1){
		return(data.frame())
	}
	
	out = data.frame(id = rep(NA, length(items)))
	for(i in 1:length(items)){
	  out$id[i] = items[[i]]$id
	}
	
	return(out)
}
