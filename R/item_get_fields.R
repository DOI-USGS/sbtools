#' Retrieve specific fields from an SB item
#' 
#' @template manipulate_item
#' @param fields a vector of fields
#' @param drop logical. If only one field is selected, should the
#'   list format be dropped?
#' @return List serialization of chosen metadata for an SB item
#' @import httr
#' @export
#' @examples \donttest{
#' # Get certain fields from an item
#' item_get_fields("63cb38b2d34e06fef14f40ad", c('title', 'citation', 'contacts'))
#' 
#' #' # If only 1 field selection, do or don't drop list format
#' item_get_fields("63cb38b2d34e06fef14f40ad", 'title')
#' item_get_fields("63cb38b2d34e06fef14f40ad", 'title', drop = FALSE)
#' }
item_get_fields = function(sb_id, fields, ..., drop=TRUE){
	
	sb_id = as.sbitem(sb_id)
	
	if(is.null(sb_id)) return(NULL)
	
	query <- list('fields'=paste0(fields, collapse=","), 'type'='json')
	
	r <- sbtools_GET(url=paste0(pkg.env$url_item, sb_id$id), ..., query = query)
	
	if(is(r, "list") && r$status == 404) {
		return(NULL)
	}
	
	out <- try(content(r))
	
	if(inherits(out, "try-error")) return(NULL)
	
	return(if(length(fields)==1 && drop) out[[fields]] else out[fields])
}
