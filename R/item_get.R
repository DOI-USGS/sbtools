#'@title Retrieve SB item
#'  
#'  
#'@param id SB item ID
#'@param ... Additional parameters are passed on to \code{\link[httr]{GET}}
#'@param session Session object from \code{\link{authenticate_sb}}
#'  
#'@return List serialization of complete metadata for SB item
#'  
#'@import httr
#'  
#'@export
item_get = function(id, ..., session=current_session()){

	r <- sbtools_GET(url = paste0(pkg.env$url_item, id), ..., query = list('type'='json'), session=session)

	return(content(r))
}

#' Retrieve specific fields from an SB item
#' 
#' @param id SB item ID
#' @param fields a vector of fields
#' @param ... Additional parameters are passed on to \code{\link[httr]{GET}}
#' @param drop logical. If the selected fields are 1-dimensional, should the
#'   list format be dropped?
#' @param session Session object from \code{\link{authenticate_sb}}
#' @return List serialization of complete metadata for SB item
#' @import httr
#' @export
item_get_fields = function(id, fields, ..., drop=TRUE, session=current_session()){
	
	r <- sbtools_GET(url = paste0(pkg.env$url_item, id, "?fields=", paste0(fields, collapse=",")), ..., query = list('type'='json'), session=session)
	out <- content(r)
	return(if(length(fields)==1 && drop) out[[fields]] else out[fields])
}