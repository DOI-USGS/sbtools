#' Query SB for items based on custom identifier
#' 
#' Find all items under a scheme or also query by for a specific type and key
#' 
#' @param scheme The identifier scheme
#' @param ... Additional parameters are passed on to \code{\link[httr]{GET}}
#' @param type (optional) The identifier type
#' @param key (optional) The identifier key
#' @param session (optional) SB Session to use, not provided queries public 
#'   items only
#' @param limit Max number of matching items to return
#' @return The SB item id for the matching item. NULL if no matching item found.
#' @import jsonlite
#' @import httr
#' 
#' @examples \dontrun{
#' authenticate_sb()
#' 
#' ex_item = item_create(title='identifier example')
#' item_update_identifier(ex_item, 'project1', 'dataset1', 'key1')
#' ex2_item = item_create(title='identifier example 2')
#' item_update_identifier(ex2_item, 'project1', 'dataset1', 'key2')
#' 
#' 
#' #query the specific item
#' query_item_identifier('project1', 'dataset1', 'key1')
#' 
#' #or get the collection of items based on the ID hierarchy
#' query_item_identifier('project1')
#' 
#' item_rm(ex_item)
#' item_rm(ex2_item)
#' }
#' 
#' @export
query_item_identifier = function(scheme, type=NULL, key=NULL, ..., limit=20){
	
	# prepare query
	filter_all = list('scheme'=scheme, 'type'=type, 'key'=key)
	filter_items = Filter(Negate(is.null), filter_all)
	filter = paste0('itemIdentifier=', toJSON(filter_items, auto_unbox=TRUE))
	query = list('filter'=filter)
	
	return(query_sb(query_list=query, ..., limit=limit))
	
}

# Get an item by its title
# 
# Keeping internal for now because only returns the first 20 records
# 
# Uses the ScienceBase catalog service, e.g., 
# https://www.sciencebase.gov/catalog/csw?service=CSW&version=2.0.2&request=getRecords&resultType=results&constraintlanguage=CQL_TEXT&constraint=Title=nwis_08062500&format=json
#
# import httr
# import XML
# keywords internal
# query_item_title <- function(title, ...) {
# 
# 	if(length(title) != 1) stop("need exactly 1 title")
# 	url = paste0("https://www.sciencebase.gov/catalog/csw?service=CSW&version=2.0.2&request=getRecords&resultType=results&",
# 							 "constraintlanguage=CQL_TEXT&constraint=Title=", paste0(title, collapse=","))
# 	r = GET(url=url, ..., handle=session)
# 	
# 	if(r$status_code == 409){
# 		stop('Multiple items described by that ID')
# 	}
# 	
# 	xml = content(r)
# 	xmllist <- XML::xmlToList(xml)$SearchResults
# 	
# 	out = data.frame(title="", id="", stringsAsFactors=FALSE)[c(),]
# 	if('.attrs' %in% names(xmllist)) {
# 		count <- as.numeric(xmllist$.attrs["numberOfRecordsReturned"]) 
# 		# could look here to see if number of records available > those returned, then
# 		# follow links to get more. for now only returns the first 20 records.
# 		if(count > 0) {
# 			out <- do.call(rbind, lapply(1:count, function(i) {
# 				as.data.frame(xmllist[[i]][c('title','identifier')], stringsAsFactors=FALSE) %>%
# 					setNames(c('title','id'))
# 			}))
# 		}
# 	}
# 	
# 	return(out)
# }

