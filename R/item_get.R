#' @title Retrieve SB item
#'  
#' @export
#' @template manipulate_item
#' @return An object of class \code{sbitem}
#' @examples \dontrun{
#' # Get an item
#' item_get("4f4e4b24e4b07f02db6aea14")
#' 
#' # Search for item IDs, then pass to item_get
#' library("httr")
#' res <- query_items(list(s = "Search", q = "water", format = "json"))
#' ids <- vapply(httr::content(res)$items, "[[", "", "id")
#' lapply(ids[1:3], item_get)
#' }
item_get <- function(id, ..., session=current_session()) {
	get_item(as.sbitem(id)$id, ..., session = session)
}
