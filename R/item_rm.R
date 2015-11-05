#' @title Remove item from SB
#'   
#' @param id A ScienceBase ID or something that can be coerced to a SB item ID
#' by \code{\link{as.sbitem}}
#' @param ... Additional parameters are passed on to \code{\link[httr]{GET}} and
#'   \code{\link[httr]{DELETE}}
#' @param recursive logical, FALSE by default. CAUTION: setting recursive=TRUE
#'   means that not only will this item be deleted, but so will all its child
#'   items and their child items and so on.
#' @param session Session object from \code{\link{authenticate_sb}}
#'   
#' @return \pkg{httr} \code{\link[httr]{response}} object
#'   
#' @import httr
#'   
#' @export
#' 
#' @examples \dontrun{
#' res <- item_create(user_id(), "item-to-delete")
#' item_rm(res)
#' }
item_rm = function(id, ..., recursive=FALSE, session=current_session()){
	
	if (isTRUE(recursive)) return(item_rm_recursive(id, ..., session = session))
	
	item <- as.sbitem(id)
	children = item_list_children(item$id, ..., limit = 2, session = session)
	
	if (nrow(children) > 0) {
		stop('Item has children. To remove children, grandchildren, etc. set recursive=TRUE.', 
				 call. = FALSE)
	}
	
	r = sbtools_DELETE(paste0(pkg.env$url_item, item$id, '?format=json'), ..., 
										 accept_json(), session = session)
	
	return(r)
	
}
