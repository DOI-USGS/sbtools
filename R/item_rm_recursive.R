#' Remove an item completely by recursively removing its children
#'   
#' @export
#' @param id (character) item ID
#' @param ... Additional parameters are passed on to \code{\link[httr]{GET}} and
#'   \code{\link[httr]{DELETE}}
#' @param session Session object from \code{\link{authenticate_sb}}
#' @return \code{TRUE} to indicate success
#' @details BEWARE: This removes all folders/files in an item completely.
#' @examples \dontrun{
#' # Create an item with nested structure
#' fname <- "chairs"
#' folder_create(name = fname)
#' df <- item_list_children(user_id())
#' id <- df[ df$title == fname, "id" ]
#' folder_create(id, name = "one")
#' folder_create(id, name = "two")
#' df2 <- item_list_children(id)
#' folder_create(df2$id[1], name = "nested")
#' 
#' # then delete the whole folder
#' item_rm_recursive(id)
#' }
item_rm_recursive = function(id, ..., session = current_session()) {
	ogid <- id
	while (length(id) > 0) {
		whileid <- id
		k <- lapply(id, item_list_children_all, session = session)
		todel <- unlist(lapply(k, function(z) pluck(Filter(function(x) !x$hasChildren, z), "id", "")))
		invisible(lapply(todel, file_delete, session = session))
		id <- unlist(lapply(k, function(z) pluck(Filter(function(x) x$hasChildren, z), "id", "")))
		any_left <- lapply(whileid, item_list_children_all, session = session)
		if (length(id) == 0 && is.null(unlist(any_left))) {
			if (whileid == ogid) {
				id <- character(0)
			} else {
				id <- ogid
			}
		}
		# maybe FIXME - there's a lag time between the requests above to delete things
		# and when it actually happens - sleep 1 second before starting next 
		# loop iteration
		Sys.sleep(1)
	}
	# maybe FIXME - same as above note
	Sys.sleep(2)
	# Finally, delete the top level item
	invisible(item_rm(ogid, ..., session = session))
	return(TRUE)
}

file_delete <- function(x, ..., session = session) {
	res <- DELETE(paste0(pkg.env$url_item, x),
								query = list(format = "json"),
								content_type_json(),
								handle = session, ...)
	if (res$status_code == 200) TRUE
}

item_list_children_all <- function(id, ..., session = current_session(), limit = 100) {
	query <- list(parentId = id, max = limit, format = 'json')
	r <- sbtools_GET(url = pkg.env$url_items, ..., query = query, session = session)
	content(r, 'parsed')$items
}
