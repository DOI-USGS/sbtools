#' @title Search within an SB folder
#' 
#' @description 
#' Search for text in the title, abstract, etc. within an SB folder and any
#' subfolders.
#' 
#' @param text text in the title, abstract, etc. of the desired item
#' @param folder an SB item ID for the folder to search in
#' @param ... Additional parameters are passed on to \code{\link[httr]{GET}}
#' @param session (optional) SB Session to use, not provided queries public 
#'   items only
#' @param limit Max number of matching items to return
#' 
#' @return A list of matching items as sbitem objects.
#' 
#' @export
query_item_in_folder <- function(text, folder, ..., limit=20) {
	# create and run the query
	
	res = query_sb(list(q=text, folderId=folder), ..., limit=limit)
	
	return(res)
}
