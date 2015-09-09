#' Upload file(s) and create a new item
#' 
#' Create a new item with files attached, all in one call to SB
#' 
#' @param parent_id SB id of parent item for the newly created item
#' @param files A string vector of paths to files to be uploaded
#' @param ... Additional parameters are passed on to \code{\link[httr]{POST}}
#' @param session Session object from authenticate_sb
#' @export
#' @examples \dontrun{
#' # You'll need a parent id for a folder/item
#' ## here, using your highest level parent folder
#' file <- system.file("examples", "books.json", package = "sbtools")
#' item_upload_create(user_id(), file)
#' }
item_upload_create = function(parent_id, files, ..., session=current_session()){
	

	r = sbtools_POST(url = paste0(pkg.env$url_upload_create, parent_id,'?title=title'), 
									 ...,
									 body = multi_file_body(files), 
									 session = session)

	
	#check to see if we've been redirected to the login page
	if (grepl('josso/signon', r$url)) {
		stop('Not authenticated or lack of permission to parent object\nAunthenticate with the authenticate_sb function.')
	}
	
	return(content(r)$id)
}
