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
	
	item_json = item_get(id, session)
	
	parentid = item_json[['parentId']]
	return(parentid)
}