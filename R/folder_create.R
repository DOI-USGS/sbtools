#' @title Create a folder
#' @template item_with_parent
#' @param name (character) the folder name
#' @return A \code{\link[httr]{response}} object
#' 
#' @description 
#' Create a special kind of item on ScienceBase that is intended to be 
#' a "folder" that contains one or more child items. This is similar to 
#' a standard item (\code{\link{item_create}}) but defaults to showing 
#' child-items on the ScienceBase web interface. 
#' 
#' @examples \dontrun{
#' folder_create(name="foobar345")
#' }
#' @export
folder_create = function(parent_id = user_id(), name, ...) {
	item <- as.sbitem(parent_id)
	
 	body <- list(id = unbox(""), 
 							 title = unbox(name), 
 							 parentId = unbox(item$id), 
 							 systemTypes = "Folder")

	r = sbtools_POST(url = pkg.env$url_item, ..., body = toJSON(body, auto_unbox = FALSE))
	
	stop_for_status(r)
	return(as.sbitem(content(r)))
}
