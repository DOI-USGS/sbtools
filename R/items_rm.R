#' @title Remove items from SB
#' 
#' @description FIXME: not working yet - httr doesn't allow body in DELETE
#'   
#' @keywords internal
#' @template manipulate_item
#' @param recursive logical, FALSE by default. CAUTION: setting recursive=TRUE
#'   means that not only will this item be deleted, but so will all its child
#'   items and their child items and so on.
#' @return \pkg{httr} \code{\link[httr]{response}} object
#' @importFrom stats setNames
#' @examples \dontrun{
#' # helper function to make a random name
#' aname <- function() paste0(sample(letters, size = 5, replace = TRUE), collapse = "")
#' 
#' # Create some items - by default we use your user ID
#' res <- items_create(title = c(aname(), aname()))
#' items_rm(res)
#' }
items_rm <- function(sb_id, ..., recursive=FALSE){
	
	if (recursive) return(lapply(sb_id, item_rm_recursive, ...))
	
	item <- vapply(sb_id, function(z) as.sbitem(z)$id, "")
	
	invisible(lapply(item, function(x) {
		children <- item_list_children(x, ..., limit = 2)
		if (NROW(children) > 0) {
			stop('Item has children. To remove children, grandchildren, etc. set recursive=TRUE.', 
					 call. = FALSE)
		}
	}))
	
	body <- lapply(item, function(z) setNames(as.list(z), "id"))
	sbtools_DELETE(paste0(pkg.env$url_items, "?format=json"), 
								 body = jsonlite::toJSON(body, auto_unbox = TRUE), 
								 ..., accept_json())
}
