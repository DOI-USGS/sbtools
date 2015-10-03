#' Create a folder
#' 
#' @export
#' @param id A ScienceBase ID or something that can be coerced to a SB item ID
#' by \code{\link{as.sbitem}}
#' @param name (character) the folder name
#' @param ... Additional parameters are passed on to \code{\link[httr]{GET}} and
#'   \code{\link[httr]{DELETE}}
#' @param session Session object from \code{\link{authenticate_sb}}
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
