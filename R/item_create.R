#' @title Create a new SB item
#'
#' @template item_with_parent
#' @param title The title of the new SB item
#' @param info (optional) list of metadata info for the new item
#' @return An object of class \code{sbitem}
#'
#' @export
#' @examples \dontrun{
#' # Create an item - by default we use your user ID
#' item_create(title = "testing 123")
#' 
#' # Pass an object of class sbitem
#' x <- folder_create(user_id(), "foobar456")
#' item_create(x, "foobar456-item")
#' }
item_create = function(parent_id = user_id(), title, ..., info, session=current_session()){
		
	item <- as.sbitem(parent_id)
	body = list('parentId' = item$id, 'title' = title)
	
	#Bring in additional metadata if supplied
	if (!missing(info) && is.list(info)) {
		body = c(body, info)
	}
	
	r = sbtools_POST(url = pkg.env$url_item, ..., body = toJSON(body, auto_unbox = TRUE), session = session)
	
	return(as.sbitem(content(r)))
}
