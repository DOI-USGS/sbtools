#'@title Remove all files associated with an item
#'@param id a sciencebase item identifier
#' @param ... Additional parameters are passed on to \code{\link[httr]{PUT}}
#'@param session Authenticated session object (from \link{authenticate_sb})
#'@description Removes existing files associated with an item. 
#'@export
item_rm_files <- function(id, ..., session=current_session()){
	item_update(id=id, info=list('files'=vector()), ..., session = session)
}