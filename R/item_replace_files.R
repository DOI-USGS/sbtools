#'@title Replace files associated with an item
#'@param item_id a sciencebase item identifier
#'@param files a character vector of file paths
#'@param session Authenticated session object (from \link{authenticate_sb})
#'@description replaces existing files associated with an item with a new one. 
#'(Currently does not support multi-file uploads.) 
#'This function will not append an existing collection of files. If that 
#'is desired, use \code{\link{item_append_file}}
#'@export
item_replace_files <- function(item_id, files, session){	
	item_remove_files(item_id, session)
	item_append_files(id = item_id, filename = files, session)
}
