
#'@title Add custom identifier to an existing item
#'
#'@param id SB item ID
#'@param scheme The identifier scheme
#'@param type The identifier type
#'@param key The identifier key
#'@param session
#'
#'
#'
#'@export
item_update_identifier = function(id, scheme, type, key, session){
	
	#first, query for that identifier
	existing_id = query_item_identifier(scheme, type, key, session)
	if(!is.null(existing_id) && existing_id != id){
		stop('Item with that identifier already exists')
	}else if(!is.null(existing_id) && existing_id == id){
		original = item_get(existing_id)$identifiers
	}else{
		original=NULL
	}
	
	data = list(list(scheme=scheme, type=type, key=key))

	data = merge_identifiers(original$identifiers, data)
	
	return(item_update(id, list(identifiers=data), session))
	
}

## Check and merge identifiers
merge_identifiers = function(original, updated){
	
	if(is.null(original)){
		return(updated)
	}
	
	for(i in 1:length(original)){
		if(original[[i]]$scheme == updated$scheme && original[[i]]$type == updated$type){
			original[[i]]$key = updated$key
			return(original)
		}
	}
	#if we get here, none matched, so just add it on the end
	original[[lenght(original)+1]] = updated
	return(original)
}