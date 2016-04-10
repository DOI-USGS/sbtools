#' @title Rename item attached files
#' 
#' @description Renames files attached to an SB item. 
#'
#' 
#' @template manipulate_item
#' @param names List of names of files to rename
#' @param new_names List of new file names to use
#' 
#' @examples 
#' \dontrun{
#' 
#' names = c('file1.txt', 'file2.txt')
#' new_names = c('newname1.txt', 'newname2.txt')
#' 
#' item_rename_files('sbid', names, new_names)
#' 
#' }
#' 
#' @export
item_rename_files = function(sb_id, names, new_names, ..., session=current_session()){
	
	if(length(names) != length(new_names)){
		stop('`names` and `new_names` must be identical length character vectors')
	}
	
	#force download refresh of item
	it = item_get(sb_id, ..., session=session)
	name_list = sapply(it$files, FUN=function(x){x$name})
	
	if(!all(names %in% name_list)){
		stop('`names` supplied has does not match names attached to item')
	}
	
	for(i in 1:length(names)){
		indx = which(name_list == names[i])
		it$files[[indx]]$name = new_names[i]
	}
	
	item_update(it, info = list(files=it$files), ..., session=session)
	
}