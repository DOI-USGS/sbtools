#' @title Remove item from SB
#'   
#' @template manipulate_item
#' @param recursive logical, FALSE by default. CAUTION: setting recursive=TRUE
#'   means that not only will this item be deleted, but so will all its child
#'   items and their child items and so on.
#' @param limit The maximum number of child items to remove when called with
#'   recursive=TRUE. 
#' 
#' @description 
#' Remove an item from ScienceBase. This is not reversible and will delete 
#' an item and its attached files. (advanced) Recursive is to be used with care
#' and could result in unexpected file deletion.
#' 
#' @return \pkg{httr} \code{\link[httr]{response}} object
#'   
#' @import httr
#'   
#' @export
#' 
#' @examples \dontrun{
#' res <- item_create(user_id(), "item-to-delete")
#' item_rm(res)
#' }
item_rm = function(sb_id, ..., limit=1000, recursive=FALSE) {
	
	if (isTRUE(recursive)) return(item_rm_recursive(sb_id, ..., limit=limit))
	
	item <- as.sbitem(sb_id)
	children = item_list_children(item$id, ..., limit = 2)
	
	if (length(children) > 0) {
		stop('Item has children. To remove children, grandchildren, etc. set recursive=TRUE.', 
				 call. = FALSE)
	}
	
	if(is.null(item)) return(NULL)
	
	r = delete_item_query(item$id)
	
	return(invisible(r))
	
}

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
