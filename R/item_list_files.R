#'@title Get list of files attached to SB item
#'
#'@param id item ID
#'@param Session Session object from \link{authenticate_sb}
#'
#'@return A data.frame with columns fname, size, and url.
#'
#'
#'@export
item_list_files = function(id, session){
	item = item_get(id, session)
	
	files = item$files
	out = data.frame(fname=NA, size=NA, url=NA)
	
	for(i in 1:length(files)){
		out[i,]$fname = files[[i]]$name
		out[i,]$size = files[[i]]$size
		out[i,]$url = files[[i]]$url
	}
	
	return(out)
}