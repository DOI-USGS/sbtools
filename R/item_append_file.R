#'@title Upload File to Item
#'@description Adds a file to an item
#'
#'@param id A ScienceBase item ID to upload to.
#'@param filename A file path to upload.
#'@param sb_session A ScienceBase session object from authenticate_sb.
#'
#'@return ScienceBase Item.
#'
#'@import httr
#'
#'@examples \dontrun{
#'  sb_session<-authenticate_sb(sbusername)
#'  item_append_file("54e265a4e4b08de9379b4dfb", '/foo/bar/baz.zip', sb)
#' }
#'@export
item_append_file = function(id, filename, session){	
	
	if(!session_validate(session)){
		stop('Session state is invalid, please re-authenticate')
	}
	
	POST(paste0(pkg.env$url_upload,'?id=', id), accept_json(), 
			 body=list(file=upload_file(filename)), handle=session)
	
}