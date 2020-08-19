#' @title Get an item's parent ID
#'
#' @template manipulate_item
#' @return An item object representing the parent of the supplied item.
#' 
#' @description 
#' Retrieves the parent of a supplied item based on the ScienceBase
#' item tree hierarchy. 
#' 
#' @export
#' @examples \donttest{
#' item_get_parent("4f4e4b24e4b07f02db6aea14")
#' 
#' item_get_parent(item_get("4f4e4b24e4b07f02db6aea14"))
#' }
item_get_parent = function(sb_id, ..., session = current_session()) {
	res <- as.sbitem(sb_id, ...)
	
	if(is.null(res)) return(NULL)
	
	return(as.sbitem(res$parentId))
}
