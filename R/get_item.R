#'
#'@title Retrieve SB item
#'
#'
#'@param id SB item ID
#'@param session Session object from \code{\link{authenticate_sb}}
#'
#'@return List serialization of complete metadata for SB item
#'
#'@export
item_get = function(id, session){
	r = GET(paste0(url_item, id, '?type=json'), handle=session)
	
	##TODO: Check that item successfully found
	
	return(content(r))
	
}
