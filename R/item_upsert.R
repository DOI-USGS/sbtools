#' @title Upsert an SB item
#' 
#' @description Either creates or updates (if item already exists)
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
#' # Create an item - by default we use your user ID
#' (x <- item_upsert(title = aname()))
#' 
#' # Call item_upsert again, updates this time
#' item_upsert(x, info = list(
#' 		contacts = list(list(name = "Suzy"))
#' 	)
#' )
#' }
item_upsert <- function(parent_id = user_id(), title = NULL, ...,
												info = NULL){
	
	item <- as.sbitem(parent_id)
	if (is.null(title)) {
		message("title is NULL - re-using title from input SB item")
		title <- item$title
	}
	
	body <- Filter(Negate(is.null), list('parentId' = item$id, 'title' = title))
	
	#Bring in additional metadata if supplied
	if (!is.null(info) && is.list(info)) {
		body <- c(body, info)
	}
	
	res <- sbtools_POST(url = paste0(pkg.env$url_item, "upsert"),
										..., body = toJSON(body, auto_unbox = TRUE),
										session = session)
	
	as.sbitem(content(res))
}
