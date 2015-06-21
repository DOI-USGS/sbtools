#' @title Remove item from SB
#'   
#' @param id item ID
#' @param ... Additional parameters are passed on to \code{\link[httr]{GET}} and
#'   \code{\link[httr]{DELETE}}
#' @param session Session object from \code{\link{authenticate_sb}}
#'   
#' @return TRUE to indicate success
#'   
#' @import httr
#'   
#' @export
item_rm = function(id, ..., session=current_session()){
	
	children = item_list_children(id, ..., limit=2, session=session)
	
	if(nrow(children) > 0){
		stop('Item has children. Remove all children before deleting.')
	}
	
	r = sbtools_DELETE(paste0(pkg.env$url_item, id), ..., accept_json(), session=session)
	
	return(r)
	
}