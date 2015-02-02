#'@title Get an item's parent ID
#'
#'@param id ID of SB item
#'@param session SB session object from \code{\link{authenticate_sb}}
#'
#'@description 
#'Returns the parent ID of an item.
#'
#'
#'@export
item_get_parent = function(id, session=NULL){
	
	if(!session_validate(session)){
		stop('Session state is invalid, please re-authenticate')
	}
	
	item_json = item_get(id, session)
	
	if('errors' %in% names(content(r))){
		stop(content(r)$errors$message)
	}
	
	parentid = item_json[['parentId']]
	return(parentid)
}