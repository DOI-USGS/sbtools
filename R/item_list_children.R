#' Return IDs for all child items
#'
#' Returns a list of child IDs for a ScienceBase item
#'
#' @template manipulate_item
#' @param limit Max children returned.
#' @param fields
#' A character vector of requested data fields. Defaults to 'id' and 'title'. Full list of possible fields 
#' is available online in \href{https://my.usgs.gov/confluence/display/sciencebase/ScienceBase+Catalog+Item+REST+Services#ScienceBaseCatalogItemRESTServices-ItemCoreObjects}{SB documentation}. 
#'  
#'
#' @return List of \code{sbitem} for each child item.
#'
#' @examples \dontrun{
#' item_list_children(user_id())
#' }
#' 
#' \donttest{
#' item_list_children(as.sbitem('5060b03ae4b00fc20c4f3c8b'))
#' item_list_children(item_get('5060b03ae4b00fc20c4f3c8b'))
#' }
#' @export
item_list_children = function(sb_id, fields=c('id', 'title'), ..., session=current_session(), limit=20){

	if(!session_validate(session)){
		stop('Session state is invalid, please re-authenticate')
	}

	item <- as.sbitem(sb_id)
	
	#always get the ID field
	if(!any('id' %in% tolower(fields))){
		fields = c('id', fields)
	}

	out = query_sb(list(parentId=item$id, fields=paste(fields, collapse=',', sep=',')), limit=limit, session=session)
	
	return(out)
}
