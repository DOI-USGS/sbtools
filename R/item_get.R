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
#' @examples \donttest{
#' # Get an item
#' item_get("4f4e4b24e4b07f02db6aea14")
#' 
#' # Search for item IDs, then pass to item_get
#' library("httr")
#' res <- query_items(list(s = "Search", q = "water", format = "json"))
#' 
#' if(inherits(res, "response") && res$status != 404) {
#'   ids <- vapply(httr::content(res)$items, "[[", "", "id")
#'   lapply(ids[1:3], item_get)
#' }
#' 
#' }
item_get <- function(sb_id, ...) {
	get_item(as.sbitem(sb_id)$id, ...)
}

get_item <- function(id, ...) {
	res <- sbtools_GET(url = paste0(pkg.env$url_item, id), ..., 
										 query = list(type = 'json'))
	
	if(is(res, "list")) {
		if(res$status == 404) return(NULL)
	} else if(is.null(res)) {
		return(NULL)
	}
	
	return(as.sbitem(content(res)))
}

#' @title Get an item's parent ID
#'
#' @template manipulate_item
#' @return An item object representing the parent of the supplied item.
#' 
#' @description 
#' Retrieves the parent of a supplied item based on the ScienceBase
#' item tree hierarchy. 
#' 
#' @export
#' @examples \donttest{
#' item_get_parent("4f4e4b24e4b07f02db6aea14")
#' 
#' item_get_parent(item_get("4f4e4b24e4b07f02db6aea14"))
#' }
item_get_parent = function(sb_id, ...) {
	res <- as.sbitem(sb_id, ...)
	
	if(is.null(res)) return(NULL)
	
	return(as.sbitem(res$parentId))
}

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


