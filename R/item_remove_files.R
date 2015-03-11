#'@title Remove all files associated with an item
#'@param item_id a sciencebase item identifier
#'@param session Authenticated session object (from \link{authenticate_sb})
#'@description Removes existing files associated with an item. 
#'@export
item_rm_files <- function(item_id, session){
	item_update(item_id,list('files'=vector()), session = session)
}