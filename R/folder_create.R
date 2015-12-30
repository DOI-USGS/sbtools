#' Create a folder
#' 
#' @export
#' @template manipulate_item
#' @param name (character) the folder name
#' @return A \code{\link[httr]{response}} object
#' @examples \dontrun{
#' folder_create(user_id(), "foobar345")
#' }
folder_create = function(id, name, ..., session = current_session()) {
	item <- as.sbitem(id)
	body <- list(id = unbox(""), 
							 title = unbox(name), 
							 parentId = unbox(item$id), 
							 systemTypes = "Folder")
	tt <- POST(paste0(pkg.env$url_item, item$id), 
							body = jsonlite::toJSON(body),
							encode = "json",
							content_type_json(),
							accept_json(),
							handle = session,
							...)
	stop_for_status(tt)
	as.sbitem(content(tt))
}
