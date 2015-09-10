#' Get your parent ID
#' 
#' Required for creating items
#' 
#' @export
#' @param ... Additional parameters are passed on to \code{\link[httr]{POST}}
#' @param session Session object from authenticate_sb
#' @examples \dontrun{
#' user_id()
#' }
user_id <- function(..., session = current_session()) {
	args <- list(s = "Search", 
							 parentId = "4f4e4772e4b07f02db47e231", 
							 lq = paste0("title.untouched:", pkg.env$username), 
							 format = "json")
	res <- content(query_items(args, ...))
	url <- res$items[[1]]$link$url
	basename(url)
}
