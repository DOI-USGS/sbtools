#' @title Replace files associated with an item
#' @template manipulate_item
#' @param files a character vector of file paths
#' @description replaces existing files associated with an item with a new one. 
#'   (Currently does not support multi-file uploads.) This function will not
#'   append an existing collection of files. If that is desired, use
#'   \code{\link{item_append_files}}
#' @export
item_replace_files <- function(id, files, ..., session=current_session()){	
	item_rm_files(id, ..., session=session)
	item_append_files(id = id, files = files, ..., session=session)
}
