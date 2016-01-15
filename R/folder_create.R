#' @title Create a folder
#' 
#' 
#' @template item_with_parent
#' @param name (character) the folder name
#' @return A \code{\link[httr]{response}} object
#' @examples \dontrun{
#' folder_create(name="foobar345")
#' }
#' @export
folder_create = function(parent_id = user_id(), name, ..., session = current_session()) {
	item <- as.sbitem(parent_id)
	
 	body <- list(id = unbox(""), 
 							 title = unbox(name), 
 							 parentId = unbox(item$id), 
 							 systemTypes = "Folder")

	r = sbtools_POST(url = pkg.env$url_item, ..., body = toJSON(body, auto_unbox = FALSE), session = session)
	
	stop_for_status(r)
	return(as.sbitem(content(r)))
}
