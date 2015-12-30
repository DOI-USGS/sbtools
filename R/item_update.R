#' @title Update a SB item with new metadata
#'
#' @template manipulate_item
#' @param info list of metadata info (key-value pairs) to change on the item
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
