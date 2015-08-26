#' @title Get an item's parent ID
#'
#' @param id ID of SB item
#' @param ... Additional parameters are passed on to \code{\link[httr]{GET}}
#' @param session SB session object from \code{\link{authenticate_sb}}
#' @return The parent ID of an item.
#' @export
#' @examples \dontrun{
#' item_get_parent("4f4e4b24e4b07f02db6aea14")
#' }
item_get_parent = function(id, ..., session=current_session()){
	
	item_json = item_get(id, ..., session = session)
	
	parentid = item_json[['parentId']]
	return(parentid)
}
