
#'@title Create a new SB item
#'
#'@param parent_id ID of the parent item
#'@param session Authenticated session object (from \link{authenticate_sb})
#'@param info (optional) list of metadata info for the new item
#'@return SB ID for the newly created item
#'
#'
#'@import httr 
#'
#'@export
item_create = function(parent_id, title, session, info){
	
	if(!session_validate(session)){
		stop('Session state is invalid, please re-authenticate')
	}
	
	body = list('parentId'=parent_id, 'title'=title)
	
	#Bring in additional metadata if supplied
	if(!missing(info) && is.list(info)){
		body = c(body, info)
	}
	
	r = POST(pkg.env$url_item, body=toJSON(body, auto_unbox=TRUE), handle=session, encode="json")
	
	if(r$status_code != 200){
		stop('Error creating new item')
	}
	
	#content(r,'parsed')$id
	
	return(content(r,'parsed')$id)
}