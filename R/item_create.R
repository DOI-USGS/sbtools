
#'@title Create a new SB item
#'
#'@param parent_id ID of the parent item
#'@param title The title of the new SB item
#'@param session Authenticated session object (from \link{authenticate_sb})
#'@param info (optional) list of metadata info for the new item
#'@return SB ID for the newly created item
#'
#'x
#'@import httr 
#'
#'@export
item_create = function(parent_id, title, session=current_session(), info){
	
	
	body = list('parentId'=parent_id, 'title'=title)
	
	#Bring in additional metadata if supplied
	if(!missing(info) && is.list(info)){
		body = c(body, info)
	}
	
	r = sbtools_POST(url=pkg.env$url_item, body=toJSON(body, auto_unbox=TRUE), session = session)
	
	return(content(r,'parsed')$id)
}