#' Query SB for items using generic query parameters
#' 
#' See also \code{\link{query_item_identifier}}
#' 
#' @param query_list List of item query selectors
#' @param ... Additional parameters are passed on to \code{\link[httr]{GET}}
#' @param session Session object from \code{\link{authenticate_sb}}
#' @export
query_items = function(query_list, ..., session=current_session()){
	
	return(sbtools_GET(url = pkg.env$url_items, ..., query=query_list, session=session))
	
}