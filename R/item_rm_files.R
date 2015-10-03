#' @title Remove all files associated with an item
#' 
#' @param id A ScienceBase ID or something that can be coerced to a SB item ID
#' by \code{\link{as.sbitem}}
#' @param ... Additional parameters are passed on to \code{\link[httr]{PUT}}
#' @param session Authenticated session object (from \link{authenticate_sb})
#' @description Removes existing files associated with an item.
#' @return An object of class \code{sbitem}
#' @export
#' @examples \dontrun{
#' res <- item_create(user_id(), "item456") 
#' cat("foo bar", file = "foobar.txt")
#' item_append_files(res, "foobar.txt")
#' res <- item_get(res)
#' res$files[[1]]$name
#' res2 <- item_rm_files(res)
#' res2$files
#' }
item_rm_files <- function(id, ..., session=current_session()){
	item <- as.sbitem(id)
	as.sbitem(item_update(item$id, info = list(files = vector()), ..., session = session))
}
