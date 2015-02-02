#'@title Remove item from SB
#'
#'@param SB item ID
#'@param session Session object from \code{\link{authenticate_sb}}
#'
#'@return TRUE to indicate success
#'
#'@import httr
#'
#'@export
item_rm = function(id, session){
	
	if(!session_validate(session)){
		stop('Session state is invalid, please re-authenticate')
	}
	
	children = item_list_children(id, session, limit=2)
	
	if(nrow(children) > 0){
		stop('Item has children. Remove all children before deleting.')
	}
	
	r = DELETE(paste0(pkg.env$url_item, id), handle=session, accept_json())
	
	return(url_ok(r))
	
}