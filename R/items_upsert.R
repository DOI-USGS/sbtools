#' @title Upsert many SB items
#' 
#' @description Either creates or updates (if items already exist)
#'
#' @export
#' @template item_with_parent
#' @param title The title of the new SB item
#' @param info (optional) list of metadata info for the new item
#' @return An object of class \code{sbitem}
#' @examples \dontrun{
#' # helper function to make a random name
#' aname <- function() paste0(sample(letters, size = 5, replace = TRUE), collapse = "")
#' 
#' # Create some item - by default we use your user ID
#' z1 <- item_create(title = aname())
#' z2 <- item_create(title = aname())
#' 
#' # Upsert items
#' (x <- items_upsert(list(z1, z2), title = c(aname(), aname())))
#' 
#' # Call item_upsert again, updates this time
#' items_upsert(x, info = list(
#' 		contacts = list(list(name = "Suzy"))
#' 	)
#' )
#' }
items_upsert <- function(parent_id = user_id(), title = NULL, ...,
												info = NULL, session=current_session()){
	
	if (!length(parent_id) > 0) stop("parent_id must be of length > 0", call. = FALSE)
	if (length(parent_id) > 1) {
		if (length(parent_id) != length(title)) {
			stop("If parent_id length > 1, it must be of same length as title and info", call. = FALSE)
		}
	}
	
	item <- lapply(parent_id, as.sbitem)
	ids <- if (length(item) < 2) rep(item$id, 2) else vapply(item, "[[", "", "id")
	if (is.null(title)) {
		message("title is NULL - re-using titles from input SB items")
		title <- vapply(item, "[[", "", "title")
	}
	
	body <- unname(
		Map(function(x, y) {
			list('parentId' = x, 'title' = y)
		}, ids, title
		)
	)
	
	if (!is.null(info)) {
		body <- Map(function(a, b) c(a, b), body, info)
	}
	
	res <- sbtools_POST(url = paste0(pkg.env$url_items, "upsert"),
											..., body = toJSON(body, auto_unbox = TRUE),
											session = session)
	
	lapply(content(res), as.sbitem)
}
