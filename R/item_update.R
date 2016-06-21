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
#' @description 
#' Updates metadata associated with a ScienceBase item based on 
#' supplied list of new or updated metadata elements.
#' 
#' @examples \dontrun{
#' res <- item_create(user_id(), "item-to-update") 
#' out <- item_update(res, list(title = "item-updated"))
#' out$title
#' }
#' @export
item_update = function(sb_id, info, ..., session=current_session()){
	
	item <- as.sbitem(sb_id)
	is_info_list(info)
	
	r = sbtools_PUT(url = paste0(pkg.env$url_item, item$id), 
									body = toJSON(info, auto_unbox = TRUE, null='null'),
									..., accept_json(), session = session)
	
	# catch 405, which is, I think, expired login
	if ('errors' %in% names(content(r))) {
	  stop(content(r)$errors$message)
	}
	return(as.sbitem(content(r)))
}
