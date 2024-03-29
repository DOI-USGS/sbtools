#' Query SB for items using generic query parameters
#' 
#' @param query_list List of item query selectors. See Details.
#' @param ... Additional parameters are passed on to \code{\link[httr]{GET}}
#' @return An object of class \code{\link[httr]{response}}
#' @export
#' @details The following is a list of query parameters you can use 
#' in the \code{query_list} parameter.
#' 
#' \itemize{
#'  \item s (character): Only option: "Search"
#'  \item format (character): One of "json", "xml", "csv", or "atom"
#'  \item q (character): Query string
#'  \item q (character): Lucene query string
#'  \item max (integer): Number of records to return. Default: 20
#'  \item offset (integer): Record to start at. Default: 1
#'  \item fields (character): Character vector of fields to return
#'  \item folderId (character): Alphanumeric string representing folder ID
#'  \item parentId (character): Alphanumeric string representing folder ID. This
#'  can be used to return all children items within the folder, but not within 
#'  sub-folders.
#'  \item sort (character) One of "firstContact", "dateCreated", "lastUpdated", 
#'  or "title". By default sorted by search score
#'  \item order (character) One of "asc" or "desc"
#'  \item ids Vector of item ids.
#'  \item ancestors (character): Alphanumeric string representing folder ID. This
#'  can be used to return all children items within the folder, even within 
#'  sub-folders. Used as a filter 
#'  \item tags Filter by tags, e.g, "distribution". Used as a filter 
#'  \item browseCategory One of .... Used as a filter 
#'  \item browseType One of .... Used as a filter 
#'  \item dateRange A json string with keys dateType and choice. Where dateType is one of
#'  Acquisition, Award, Collected, dateCreated, Received, Reported, Transmitted, Due, End,
#'  Info, lastUpdated, Publication, Release, or Start. And where choice is one of 
#'  day, week, month, year, or range (if range selected, also supply start and end
#'  keys with dates of the form YYYY-MM-DD). Used as a filter 
#'  \item projectStatus One of Active, Approved, Completed, In Progress, Proposed. Used as a filter 
#'  \item spatialQuery A WKT string. Used as a filter 
#'  \item extentQuery Use existing extents (footprints) to search against item bounding 
#'  boxes and representational points. This is a alphanumeric string.
#' }
#' @seealso \code{\link{query_item_identifier}}, \code{\link{query_item_in_folder}}
#' @examples \dontrun{
#' # Basic query
#' library("httr")
#' res <- query_items(list(s = "Search", q = "water", format = "json"))
#' httr::content(res)
#' 
#' # Paging
#' ## max - number of results
#' res <- query_items(list(s = "Search", q = "water", format = "json", max = 2))
#' length(httr::content(res)$items)
#' res <- query_items(list(s = "Search", q = "water", format = "json", max = 30))
#' length(httr::content(res)$items)
#' ## offset - start at certain record
#' res <- query_items(list(s = "Search", q = "water", format = "json", 
#' max = 30, offset = 10))
#' httr::content(res)
#' ## links - use links given in output for subsequent queries
#' httr::content(httr::GET(
#' 		content(res)$nextlink$url
#' ))
#' 
#' # Return only certain fields
#' res <- query_items(list(s = "Search", q = "water", format = "json", fields = 'title'))
#' httr::content(res)$items[[1]]
#' 
#' # Search a folder ID
#' res <- query_items(list(s = "Search", q = "water", format = "json", 
#' folderId = '504216b9e4b04b508bfd337d'))
#' httr::content(res)$items
#' 
#' # Filter by ancestor
#' query_items(list(s = "Search", ancestors = "4f831626e4b0e84f6086809b", format = "json"))
#' 
#' # Filter by tags
#' content(query_items(list(s = "Search", tags = "distribution", format = "json")))
#' 
#' # Filter by browse category
#' content(query_items(list(s = "Search", browseCategory = "Image", format = "json")))
#' 
#' # Filter by browse type
#' content(query_items(list(s = "Search", browseType = "Collection", format = "json")))
#' 
#' # Filter by WKT geometry string
#' wkt1 <- "POLYGON((-104.4 41.0,-95.1 41.0,-95.1 37.5,-104.4 37.5,-104.4 41.0))"
#' wkt2 <- "POLYGON((-104.4 38.3,-95.2 38.3,-95.2 33.7,-104.4 34.0,-104.4 38.3))"
#' content(query_items(list(s = "Search", spatialQuery = wkt1, format = "json")))
#' content(query_items(list(s = "Search", spatialQuery = wkt1, 
#' 		spatialQuery = wkt2, format = "json")))
#' 
#' # Project status
#' content(query_items(list(s = "Search", projectStatus = "Active", format = "json")))
#' 
#' # Date range
#' query_items(list(s = "Search", 
#' 		dateRange = '{"dateType":"Collected","choice":"year"}', format = "json"))
#' query_items(list(s = "Search", 
#' 		dateRange = '{"dateType":"lastUpdated","choice":"month"}', format = "json"))
#' query_items(list(s = "Search", 
#' 		dateRange = 
#' 		'{"dateType":"Release","choice":"range","start":"2014-09-01","end":"2015-09-01"}', 
#' 		format = "json"))
#' 
#' # Extent query
#' ## just a alphanumeric code
#' content(query_items(list(s = "Search", extentQuery = '2873462', format = "json")))
#' ## with buffering, intersect
#' content(query_items(list(s = "Search", extentQuery = '{"extent":2873462,
#' 		"relation":"intersects","buffer":"5"}', format = "json")))
#' ## with buffering, within
#' content(query_items(list(s = "Search", extentQuery = '{"extent":2873462,
#' 		"relation":"within","buffer":"5"}', format = "json")))
#' ## with buffering, within
#' content(query_items(list(s = "Search", extentQuery = '{"extent":2873462,
#' 		"relation":"disjoint","buffer":"5"}', format = "json")))
#' 		
#' # Lucene query
#' ## note, you have to pass the q parameter if you pass the lq parameter
#' content(query_items(list(s = "Search", q = "", lq = '"sage OR grouse"')))
#' }
query_items = function(query_list, ...) {
	qury <- query_list[!names(query_list) %in% query_filters()]
	filters <- query_list[names(query_list) %in% query_filters()]
	filters <- paste(names(filters), unname(filters), sep = "=")
	query <- c(qury, as.list(stats::setNames(filters, rep("filter", length(filters)))))
	return(sbtools_GET(url = pkg.env$url_items, ..., query = query))
}

query_filters <- function(x) {
	c("projectStatus", "spatialQuery", "tags", "ancestors", "browseCategory", 
		"browseType", "extentQuery", "dateRange")
}

#' @title Search within an SB folder
#' 
#' @description 
#' Search for text in the title, abstract, etc. within an SB folder and any
#' subfolders.
#' 
#' @param text text in the title, abstract, etc. of the desired item
#' @param folder an SB item ID for the folder to search in
#' @param ... Additional parameters are passed on to \code{\link[httr]{GET}}
#' @param limit Max number of matching items to return
#' 
#' @return A list of matching items as sbitem objects.
#' 
#' @export
query_item_in_folder <- function(text, folder, ..., limit=20) {
	# create and run the query
	
	res = query_sb(list(q=text, folderId=folder), ..., limit=limit)
	
	return(res)
}

#' Query SB for items based on custom identifier
#' 
#' Find all items under a scheme or also query by for a specific type and key
#' 
#' @param scheme The identifier scheme
#' @param ... Additional parameters are passed on to \code{\link[httr]{GET}}
#' @param type (optional) The identifier type
#' @param key (optional) The identifier key
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

#' check if identifier touple already exists on SB
#'
#' returns TRUE if touple already belongs to a sciencebase item, FALSE if not
#'
#' @param scheme the identifier scheme
#' @param type the identifier type
#' @param key the identifier key
#' @param ... Additional parameters are passed on to \code{\link[httr]{GET}}
#' @return boolean for whether item exists
#'
#' @examples \dontrun{
#' item_exists('mda_streams','ts_doobs','nwis_01018035')
#' item_exists('mda_streams','site_root','nwis_01018035')
#' }
#' @export
item_exists = function(scheme, type, key, ...){
	
	if(!session_validate()){
		stop('Session state is invalid, please re-authenticate')
	}
	
	items <- query_item_identifier(scheme=scheme, type=type, key=key, ...)
	if (length(items) > 0){
		return(TRUE)
	} else {
		return(FALSE)
	}
}

