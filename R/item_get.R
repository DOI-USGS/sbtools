#' @title Retrieve SB item
#'  
#' @param id A ScienceBase ID or something that can be coerced to a SB item ID
#' by \code{\link{as.sbitem}}
#' @param ... Additional parameters are passed on to \code{\link[httr]{GET}}
#' @param session Session object from \code{\link{authenticate_sb}}
#'  
#' @return An object of class \code{sbitem}
#'  
#' @export
#'
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
	item <- as.sbitem(id)
	r <- sbtools_GET(url = paste0(pkg.env$url_item, item$id), ..., 
									 query = list(type = 'json'), session = session)
	return(as.sbitem(content(r)))
}
