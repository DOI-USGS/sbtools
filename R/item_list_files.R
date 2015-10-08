#' @title Get list of files attached to SB item
#'
#' @param id item ID
#' @param session Session object from \link{authenticate_sb}
#' @param ... Additional parameters are passed on to \link[httr]{GET}
#'
#' @return 
#' A data.frame with columns fname, size, and url. 
#' If item has no attached files, returns a zero row data.frame.
#'
#' @export
#' @examples \dontrun{
#' item_list_files("4f4e4b24e4b07f02db6aea14")
#' }
item_list_files = function(id, ..., session=current_session()){
	
	if(!session_validate(session)){
		stop('Session state is invalid, please re-authenticate')
	}
	
	item = item_get(id, ..., session=session)
	
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
