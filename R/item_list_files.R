#'@title Get list of files attached to SB item
#'
#'@param SB item ID
#'@param Session from \link{authenticate_sb}
#'
#'
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