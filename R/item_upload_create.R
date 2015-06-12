#'
#'@title Upload file(s) and create a new item
#'
#'@param parent_id SB id of parent item for the newly created item
#'@param files A string vector of paths to files to be uploaded
#'@param session Session object from authenticate_sb
#'
#'
#'@import httr
#'
#'@export
item_upload_create = function(parent_id, files, session=current_session()){
	

	r = sbtools_POST(url = paste0(pkg.env$url_upload_create, parent_id,'?title=title'),
									 body = multi_file_body(files), 
									 session = session)

	
	return(content(r)$id)
}
