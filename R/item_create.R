#' @title Create a new SB item
#'
#' @template item_with_parent
#' @param title The title of the new SB item
#' @param info (optional) list of metadata info for the new item
#' @return An object of class \code{sbitem}
#'
#' @description 
#' Create a new item on ScienceBase with the requested parent and item title. 
#' Info can be provided to populate metadata at the time of creation. 
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
item_create = function(parent_id = user_id(), title, ..., info){
		
	item <- as.sbitem(parent_id)
	body = list('parentId' = item$id, 'title' = title)
	
	#Bring in additional metadata if supplied
	if (!missing(info) && is.list(info)) {
		body = c(body, info)
	}
	
	r = sbtools_POST(url = pkg.env$url_item, ..., body = toJSON(body, auto_unbox = TRUE))
	
	return(as.sbitem(content(r)))
}

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
