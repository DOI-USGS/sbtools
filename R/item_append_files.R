#'@title Upload File to Item
#'@description Adds a file to an item
#'
#'@param id A ScienceBase item ID to upload to.
#'@param files A file path to upload.
#'@param session A ScienceBase session object from authenticate_sb.
#'@param ... additional parameters passed to \code{\link{session_check_reauth}}
#'
#'@return ScienceBase Item.
#'
#'@import httr
#'
#'@examples 
#'\dontrun{
#'  session<-authenticate_sb(sbusername)
#'  item_append_files("54e265a4e4b08de9379b4dfb", '/foo/bar/baz.zip', session)
#' }
#'@export
item_append_files = function(id, files, session=current_session(), on_error="error", ...){	
	
	
	r = sbtools_POST(url = paste0(pkg.env$url_upload,'?id=', id), 
									 body = multi_file_body(files), 
									 session, on_error, ...)
  
  return(content(r))
	
}