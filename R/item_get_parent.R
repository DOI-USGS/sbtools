#' @title Get an item's parent ID
#'
#' @template manipulate_item
#' @return The parent ID of an item.
#' @export
#' @examples \dontrun{
#' item_get_parent("4f4e4b24e4b07f02db6aea14")
#' 
#' item_get("4f4e4b24e4b07f02db6aea14") %>% item_get_parent()
#' }
item_get_parent = function(id, ..., session = current_session()) {
	res <- as.sbitem(id, ...)
	res$parentId
}
