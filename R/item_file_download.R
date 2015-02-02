#'
#'@title Download files attached to item
#'
#'@param id SB item ID
#'@param names String vector list of file names attached to item that you wish to download. 
#'@param destinations String vector list of destinations for requested files. Must be same length as \code{names}
#'@param session Session object from \code{\link{authenticate_sb}}
#'
#'@author Luke Winslow
#'
#'@export
item_file_download = function(id, names, destinations, session=NULL, overwrite_file = FALSE){
	
	if(!session_validate(session)){
		stop('Session state is invalid, please re-authenticate')
	}
	
	if(length(names) != length(destinations)){
		stop('Length of names and destinations must be identical')
	}
	
	flist = item_list_files(id, session)
	
	
	if(!all(names %in% flist$fname)){
		stop('Item does not contain all requested files')
	}
	
	flist = merge(flist, data.frame(fname=names, dest=destinations, stringsAsFactors=FALSE))
	
	for(i in 1:nrow(flist)){
		GET(url=flist[i,]$url, handle=session, write_disk(flist[i,]$dest, overwrite = overwrite_file))
	}
	
	return(TRUE)
}