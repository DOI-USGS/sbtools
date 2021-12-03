#' @title Add custom identifier to an existing item
#'  
#' @template manipulate_item
#' @param scheme The identifier scheme
#' @param type The identifier type
#' @param key The identifier key
#'  
#' @description 
#' Adds or updates an item's alternative identifier. This can add 
#' additional identifiers or update those already in place. See
#' \code{\link{query_item_identifier}} for finding items based on alternative
#' identifier.
#'  
#' @examples \dontrun{
#'
#' session = authenticate_sb("user@@usgs.gov")
#' item_update_identifier("5485fd99e4b02acb4f0c7e81", "scheme", "type", "key", session=session)
#'
#' }
#'
#'@export
item_update_identifier = function(sb_id, scheme, type, key, ..., session=current_session()) {
	
	sb_id = as.sbitem(sb_id)
	
	#first, query for that identifier
	existing_id = query_item_identifier(scheme=scheme, type=type, key=key, ..., session=session)
	
	#if it exists, but has a different ID, we are trying to set a duplicate
	if(length(existing_id) > 0 && existing_id[[1]]$id != sb_id$id) {
		stop('Item with that identifier already exists')
		
		#if it exists and is the same item, then we don't need to change it
	} else if(length(existing_id) > 0 && existing_id[[1]]$id == sb_id$id) {
		return(TRUE)
	}
	
	#now, try to fetch the item. This is the item we will be updating
	original = item_get(sb_id, ..., session=session)
	
	if(!is.null(original$errors)) {
		stop('Item with that ID does not exist or you do not have permission to read it.')
	}
	
	data = list(scheme=unbox(scheme), type=unbox(type), key=unbox(key))
	
	data = merge_identifiers(unbox_identifiers(original$identifiers), data)
	
	info = list(identifiers=data)
	
	r = sbtools_PUT(url=paste0(pkg.env$url_item, sb_id$id), 
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

unbox_identifiers = function(idents){
	if(length(idents) < 1){
		return(idents)
	}
	
	for(i in 1:length(idents)){
		idents[[i]]$type   = unbox(idents[[i]]$type)
		idents[[i]]$scheme = unbox(idents[[i]]$scheme)
		idents[[i]]$key    = unbox(idents[[i]]$key)
	}
	return(idents)
}
