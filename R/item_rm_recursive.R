#' @title Remove an item completely by recursively removing its child items
#' 
#' @keywords internal
#' @template manipulate_item
#' @return \code{TRUE} to indicate success
#' @description BEWARE: This completely removes ALL CHILD ITEMS AND THEIR CHILDREN
#'   as well as the item itself.
#'   
#' @examples \dontrun{
#' # Create an item with nested structure
#' authenticate_sb()
#' fname <- "chairs"
#' folder = folder_create(user_id(), name = fname)
#' item_create(folder, title='child1')
#' item_create(folder, title='child2')
#' 
#' item_list_children(folder)
#' 
#' # then delete the whole folder
#' sbtools:::item_rm_recursive(folder)
#' }
item_rm_recursive = function(id, ..., limit) {
	
	id <- as.sbitem(id)$id
	# check args
	if(length(id) != 1) stop('expecting exactly 1 id')
	
	# get list of children. no need to identify or delete files; these are deleted
	# along with their containing items automatically
	kids <- item_list_children(id, fields=c('id', 'hasChildren'), raw=FALSE, limit=limit)

	if(length(kids) > 0) {
		# recursive case: has children. delete the children first
		
		# delete the children with children recursively.
		lapply(kids, function(itm, ...){if(itm$hasChildren){item_rm_recursive(itm, ...)}else{item_rm(itm, ...)} }, ...)
		
		
		# maybe FIXME - there's a lag time between the requests above to delete things
		# and when it actually happens - sleep 2 seconds before moving up one level in
		# the recursion
		Sys.sleep(2)
	}
	# base case: has no children. just delete.
	item_rm(id, ..., recursive=FALSE)	
	
	# return TRUE if we made it this so far
	return(TRUE)
}

