#'@title Get list of files attached to SB item
#'
#'@param id item ID
#'@param Session Session object from \link{authenticate_sb}
#'
#'@return 
#'A data.frame with columns fname, size, and url. 
#'If item has no attached files, returns a zero row data.frame.
#'
#'
#'@export
item_list_files = function(id, session=NULL){
	
	if(!session_validate(session)){
		stop('Session state is invalid, please re-authenticate')
	}
	
	item = item_get(id, session)
	
	files = item$files
	out = data.frame()
	
	if(length(files)==0){
		return(out)
	}
	
	for(i in 1:length(files)){
		out[i,'fname'] = files[[i]]$name
		out[i,'size'] = files[[i]]$size
		out[i,'url'] = files[[i]]$url
	}
	
	return(out)
}