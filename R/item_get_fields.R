#' Retrieve specific fields from an SB item
#' 
#' @param id SB item ID
#' @param fields a vector of fields
#' @param ... Additional parameters are passed on to \code{\link[httr]{GET}}
#' @param drop logical. If only one field is selected, should the
#'   list format be dropped?
#' @param session Session object from \code{\link{authenticate_sb}}
#' @return List serialization of chosen metadata for an SB item
#' @import httr
#' @export
#' @examples \dontrun{
#' # Get certain fields from an item
#' item_get_fields("4f4e4b24e4b07f02db6aea14", c('title', 'citation', 'contacts'))
#' 
#' # If only 1 field selection, do or don't drop list format
#' item_get_fields("4f4e4b24e4b07f02db6aea14", 'title')
#' item_get_fields("4f4e4b24e4b07f02db6aea14", 'title', drop = FALSE)
#' }
item_get_fields = function(id, fields, ..., drop=TRUE, session=current_session()){
	query <- list('fields'=paste0(fields, collapse=","), 'type'='json')
	r <- sbtools_GET(url=paste0(pkg.env$url_item, id), ..., query = query, session=session)
	out <- content(r)
	return(if(length(fields)==1 && drop) out[[fields]] else out[fields])
}
