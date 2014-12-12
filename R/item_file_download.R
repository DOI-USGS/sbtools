#'
#'@title Download files attached to item
#'
#'
#'
#'
#'@export
item_file_download = function(id, names, destinations, session=NULL){
	
	if(length(names) != length(destinations)){
		stop('Length of names and destinations must be identical')
	}
	
	flist = item_list_files(id, session)
	
	
	if(!all(names %in% flist$fname)){
		stop('Item does not contain all requested files')
	}
	
	flist = merge(flist, data.frame(fname=names, dest=destinations, stringsAsFactors=FALSE))
	
	for(i in 1:nrow(flist)){
		GET(url=flist[i,]$url, handle=session, write_disk(flist[i,]$dest))
	}
	
	return(TRUE)
}