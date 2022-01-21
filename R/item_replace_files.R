#' @title Replace files associated with an item
#' @template manipulate_item
#' @param files A character vector of file paths
#' @param all A boolean indicating if all attached files should be removed before
#' uploading new files. FALSE if only files with matching names should be replaced. 
#' If you wish to upload files with duplicate names, see \code{\link{item_append_files}}.
#' Defaults to FALSE.
#' 
#' @description replaces existing files associated with an item with a new one. 
#'   (Currently does not support multi-file uploads.) This function will not
#'   append an existing collection of files. If that is desired, use
#'   \code{\link{item_append_files}}
#'   
#' NOTE: This function will not replace files stored in facets. Until and if 
#' facet support is added, direct alteration of the science base item object 
#' is required to manipulate facets. 
#' 
#' @export
item_replace_files <- function(sb_id, files, ..., all=FALSE, session=current_session()){
	
	if(length(files) > 50){
		warning('Trying to attach a large number of files to a SB item. SB imposes file limits which may cause this to fail')
	}
	
	if(all){
		item_rm_files(sb_id, ..., session=session)
	}else{
		item_rm_files(sb_id, files, ..., session=session)
	}
	item_append_files(sb_id, files = files, ..., session=session)
}
