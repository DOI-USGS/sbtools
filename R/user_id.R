#' Get your parent ID
#' 
#' Required for creating items
#' 
#' @export
#' @param ... Additional parameters are passed on to \code{\link[httr]{POST}}
#' @param session Session object from \code{\link{authenticate_sb}}
#' @return A single character string, your user id
#' @examples \dontrun{
#' user_id()
#' }
user_id <- function(..., session = current_session()) {
	if(!is.null(pkg.env$uid)) return(pkg.env$uid)
	
	if (is.null(session)) stop("Please authenticate first. See ?authenticate_sb", call. = FALSE)
	args <- list(s = "Search", 
							 parentId = "4f4e4772e4b07f02db47e231", 
							 lq = paste0("title.untouched:", pkg.env$username), 
							 format = "json")
	res <- content(query_items(args, ...))
	url <- res$items[[1]]$link$url

	out <- basename(url)
	
	pkg.env$uid <- out
	
	out
}
