#'@export
item_append_file = function(id, filename, session){	
	
	POST(paste0(pkg.env$url_upload,'?id=', id), accept_json(), 
			 body=list(file=upload_file(filename)), handle=session)
	
}