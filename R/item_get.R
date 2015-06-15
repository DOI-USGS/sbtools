#'@title Retrieve SB item
#'
#'
#'@param id SB item ID
#'@param session Session object from \code{\link{authenticate_sb}}
#'
#'@return List serialization of complete metadata for SB item
#'
#'@import httr
#'
#'@export
item_get = function(id, session=current_session()){

	r <- sbtools_GET(url = paste0(pkg.env$url_item, id),query = list('type'='json'), session=session)

	return(content(r))
}
