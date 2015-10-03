#' Create a folder
#' 
#' @export
#' @param parent_id (character) the id of the parent folder/item under which to create this one
#' @param name (character) the folder name
#' @param ... Additional parameters are passed on to \code{\link[httr]{GET}} and
#'   \code{\link[httr]{DELETE}}
#' @param session Session object from \code{\link{authenticate_sb}}
#' @return A \code{\link[httr]{response}} object
#' @examples \dontrun{
#' folder_create(user_id(), "foobar")
#' }
folder_create = function(parent_id, name, ..., session = current_session()) {
	body <- list(id = unbox(""), 
							 title = unbox(name), 
							 parentId = unbox(parent_id), 
							 systemTypes = "Folder")
	tt <- POST(paste0(pkg.env$url_item, parent_id), 
							body = jsonlite::toJSON(body),
							encode = "json",
							content_type_json(),
							accept_json(),
							handle = session,
							...)
	stop_for_status(tt)
	as.sbitem(content(tt))
}
