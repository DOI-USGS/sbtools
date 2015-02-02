#'@export
item_append_file = function(id, filename, session){	
	
	if(!session_validate(session)){
		stop('Session state is invalid, please re-authenticate')
	}
	
	POST(paste0(pkg.env$url_upload,'?id=', id), accept_json(), 
			 body=list(file=upload_file(filename)), handle=session)
	
}