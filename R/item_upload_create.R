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
item_upload_create = function(parent_id, files, session){
	
	body = list()
	for(i in 1:length(files)){
		if(!file.exists(files[i])){
			stop('This file does not exist or cannot be accessed: ', files[i])
		}
		
		body[[paste0('file', i)]] = upload_file(files[i])
	}
	names(body) = rep('file', length(body))
	
	url = paste0(url_upload_create, parent_id)
	r = POST(url, body=body, accept_json(), handle=session, verbose(), query=list(title='title'))
	
	return(content(r)$id)
}