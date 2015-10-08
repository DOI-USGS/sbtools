#' @title Update a SB item with new metadata
#'
#' @param id A ScienceBase ID or something that can be coerced to a SB item ID
#' by \code{\link{as.sbitem}}
#' @param info list of metadata info (key-value pairs) to change on the item
#' @param ... Additional parameters are passed on to \code{\link[httr]{PUT}}
#' @param session Authenticated session object (from \link{authenticate_sb})
#'
#' @return An object of class \code{sbitem}
#'
#' @import httr
#' @import jsonlite
#'
#' @export
#' 
#' @examples \dontrun{
#' res <- item_create(user_id(), "item-to-update") 
#' out <- item_update(res, list(title = "item-updated"))
#' out$title
#' }
item_update = function(id, info, ..., session=current_session()){
	
	item <- as.sbitem(id)
	
	if (!is.list(info)) {
		stop('Info must be a list of name-value pairs that can be serialized into JSON')
	}
	
	r = sbtools_PUT(url = paste0(pkg.env$url_item, item$id), 
									body = toJSON(info, auto_unbox = TRUE),
									..., accept_json(), session = session)
	
	# catch 405, which is, I think, expired login
	if ('errors' %in% names(content(r))) {
	  stop(content(r)$errors$message)
	}
	return(as.sbitem(content(r)))
}
