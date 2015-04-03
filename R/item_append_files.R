#'@title Upload File to Item
#'@description Adds a file to an item
#'
#'@param id A ScienceBase item ID to upload to.
#'@param files A file path to upload.
#'@param session A ScienceBase session object from authenticate_sb.
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
item_append_files = function(id, files, session){	
	
	if(!session_validate(session)){
		stop('Session state is invalid, please re-authenticate')
	}
	
	body <- multi_file_body(files)
  url<-paste0(pkg.env$url_upload,'?id=', id)
  
	r = POST(url, accept_json(), 
			 body=body, handle=session)
  
  return(content(r))
	
}