#'@title Add custom identifier to an existing item
#'  
#' @template manipulate_item
#'@param scheme The identifier scheme
#'@param type The identifier type
#'@param key The identifier key
#'  
#'@author Luke Winslow
#'  
#'@examples
#'
#'\dontrun{
#'
#'session = authenticate_sb("user@@usgs.gov")
#'item_update_identifier("5485fd99e4b02acb4f0c7e81", "scheme", "type", "key", session=session)
#'
#'}
#'
#'@export
item_update_identifier = function(id, scheme, type, key, ..., session=current_session()) {
	
	#first, query for that identifier
	existing_id = query_item_identifier(scheme=scheme, ..., type=type, key=key, session=session)
	
	#if it exists, but has a different ID, we are trying to set a duplicate
	if(!is.null(existing_id$id) && existing_id$id != id) {
		stop('Item with that identifier already exists')
		
		#if it exists and is the same item, then we don't need to change it
	} else if(!is.null(existing_id$id) && existing_id$id == id) {
		return(TRUE)
	}
	
	#now, try to fetch the item. This is the item we will be updating
	original = item_get(id=id, ..., session=session)
	
	if(!is.null(original$errors)) {
		stop('Item with that ID does not exist or you do not have permission to read it.')
	}
	
	data = list(scheme=unbox(scheme), type=unbox(type), key=unbox(key))

	data = merge_identifiers(original$identifiers, data)
	
	info = list(identifiers=data)
	
	r = sbtools_PUT(url=paste0(pkg.env$url_item, id), 
									body=toJSON(info), 
									..., accept_json(), session=session)
	
	return(r)
	
}

## Check and merge identifiers
merge_identifiers = function(original, updated){
	
	if(is.null(original)){
		return(list(updated))
	}
	
	for(i in 1:length(original)){
		if(original[[i]]$scheme == updated$scheme && original[[i]]$type == updated$type){
			original[[i]]$key = updated$key
			return(original)
		}
	}
	#if we get here, none matched, so just add it on the end
	original[[length(original)+1]] = updated
	return(original)
}
