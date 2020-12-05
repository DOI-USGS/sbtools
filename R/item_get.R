#' @title Retrieve SB item
#'  
#' @export
#' @template manipulate_item
#' @return An object of class \code{sbitem}
#' 
#' @description
#' Retrieves an item and its metadata from ScienceBase based on its
#' unique ID. Errors if the requested item ID does not exist or
#' access is restricted due to permissions. 
#' 
#' 
#' @examples 
#' \donttest{
#' # Get an item
#' item_get("4f4e4b24e4b07f02db6aea14")
#' 
#' # Search for item IDs, then pass to item_get
#' library("httr")
#' res <- query_items(list(s = "Search", q = "water", format = "json"))
#' 
#' if(res$status != 404) {
#'   ids <- vapply(httr::content(res)$items, "[[", "", "id")
#'   lapply(ids[1:3], item_get)
#' }
#' 
#' }
item_get <- function(sb_id, ..., session=current_session()) {
	get_item(as.sbitem(sb_id)$id, ..., session = session)
}

get_item <- function(id, ..., session=current_session()) {
	res <- sbtools_GET(url = paste0(pkg.env$url_item, id), ..., 
										 query = list(type = 'json'), session = session)
	
	if(is(res, "list")) {
		if(res$status == 404) return(NULL)
	}
	
	return(as.sbitem(content(res)))
}
