#' Return IDs for all child items
#'
#' Returns a list of child IDs for a ScienceBase item
#'
#' @template manipulate_item
#' @param limit Max children returned
#' @param fields
#' A character vector of requested data fields. Defaults to 'id' and 'title'. Full list of possible fields 
#' is available online in \href{https://my.usgs.gov/confluence/display/sciencebase/ScienceBase+Catalog+Item+REST+Services#ScienceBaseCatalogItemRESTServices-ItemCoreObjects}{SB documentation}. 
#' @param raw 
#' Boolean flag indicating if the raw parsed JSON result (as a list) should be returned. If FALSE (default), 
#' the items are coerced into a data.frame when possible. No guarantees are made when a field with mmultiple
#' values (like files, tags, contacts, etc) is requested and coerced into a data.frame.
#'  
#'
#' @return \code{data.frame} with a row for each child item
#'
#' @examples \dontrun{
#' item_list_children(user_id())
#'
#' as.sbitem('5060b03ae4b00fc20c4f3c8b') %>% item_list_children
#' item_get('5060b03ae4b00fc20c4f3c8b') %>% item_list_children
#' }
#' @export
item_list_children = function(sb_id, fields=c('id', 'title'), raw=FALSE, ..., session=current_session(), limit=20){

	if(!session_validate(session)){
		stop('Session state is invalid, please re-authenticate')
	}

	item <- as.sbitem(sb_id)
	
	#always get the ID field
	if(!any('id' %in% tolower(fields))){
		fields = c('id', fields)
	}

	query=list('parentId'=item$id, 'max'=limit, 'format'='json', 'fields'=paste(fields, collapse=',', sep=','))
	r = sbtools_GET(url = pkg.env$url_items, ..., query=query, session=session)

	items = content(r, 'parsed')$items

	if(raw){
		return(items)
	}
	
	if(length(items) < 1){
		return(data.frame())
	}

	#Create data.frame and populate empty columns
	out = data.frame(id = rep(NA, length(items)))
	for(i in 1:length(fields)){
		out[,fields[i]] = NA
	}
	
	for(i in 1:length(items)){
		for(j in 1:length(fields)){
			out[i,fields[j]] = if(fields[j] %in% names(items[[i]])) items[[i]][fields[j]] else NA
		}
		#out$title[i] = if("title" %in% names(items[[i]])) items[[i]]$title else NA
	  #out$id[i] = items[[i]]$id
	}

	return(out)
}
