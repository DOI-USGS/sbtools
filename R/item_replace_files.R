#' @title Replace files associated with an item
#' @template manipulate_item
#' @param files A character vector of file paths
#' @param all A boolean indicating if all attached files should be removed before
#' uploading new files. FALSE if only files with matching names should be replaced
#' @description replaces existing files associated with an item with a new one. 
#'   (Currently does not support multi-file uploads.) This function will not
#'   append an existing collection of files. If that is desired, use
#'   \code{\link{item_append_files}}
#' @export
item_replace_files <- function(sb_id, files, ..., all=TRUE, session=current_session()){
	if(all){
		item_rm_files(sb_id, ..., session=session)
	}else{
		item_rm_files(sb_id, files, ..., session=session)
	}
	item_append_files(id = id, files = files, ..., session=session)
}
