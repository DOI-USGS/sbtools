#' @title Replace files associated with an item
#' @template manipulate_item
#' @param files A character vector of file paths
#' @param all A boolean indicating if all attached files should be removed before
#' uploading new files. FALSE if only files with matching names should be replaced. 
#' If you wish to upload files with duplicate names, see \code{\link{item_append_files}}.
#' Defaults to FALSE.
#' @inheritParams item_upload_create
#' 
#' @description replaces existing files associated with an item with a new one. 
#'   
#' NOTE: This function will not replace files stored in facets. Until and if 
#' facet support is added, direct alteration of the science base item object 
#' is required to manipulate facets. 
#' 
#' @export
item_replace_files <- function(sb_id, files, ..., all=FALSE, 
															 scrape_files = FALSE){
	
	if(length(files) > 50){
		warning('Trying to attach a large number of files to a SB item. SB imposes file limits which may cause this to fail')
	}
	
	if(all){
		item <- item_rm_files(sb_id, ...)
	}else{
		item <- item_rm_files(sb_id, files, ...)
	}
	
	if(!is.null(item))
		item_append_files(sb_id, files = files, ..., scrape = scrape_files)
}
