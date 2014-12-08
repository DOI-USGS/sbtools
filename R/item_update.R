#'
#'@title Update a SB item with new metadata
#'
#'
#'@import httr
#'@import jsonlite
#'
#'@export
item_update = function(id, info, session){
	if(!is.list(info)){
		stop('Info must be a list of name-value pairs that can be serialized into JSON')
	}
	
	r = PUT(paste0(url_item, id), handle=session,
					body=toJSON(info, auto_unbox=TRUE))
	
	#catch 405, which is, I think, expired login
	
	return(r)
}