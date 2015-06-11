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
	
	if(!session_validate(session)){
		stop('Session state is invalid, please re-authenticate')
	}
	
	body <- multi_file_body(files)
	
	url = paste0(pkg.env$url_upload_create, parent_id)
	r = POST(url, body=body, accept_json(), handle=session, query=list(title='title')) 
	
	return(content(r)$id)
}
