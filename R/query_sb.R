#' @title Query SB for items using generic query parameters
#' 
#' @param query_list List of item query selectors. See Details.
#' @param limit Maximum number of returned items. Will do paging to retrieve
#' results when limit is over 1000. Use with caution, queries 10k results
#' are slow.
#' @param ... Additional parameters are passed on to \code{\link[httr]{GET}}
#' @param session Session object from \code{\link{authenticate_sb}}
#' 
#' @seealso \code{\link{query_items}}
#' 
#' @return 
#' A list of \code{\link{sbitem}} objects
#' 
#' @description 
#' Generic SB query function to construct advanced queries.
#' 
#' The following is a list of query parameters you can use 
#' in the \code{query_list} parameter.
#' 
#' \itemize{
#'  \item q (character): Query string
#'  \item q (character): Lucene query string
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
#' 
#' @examples 
#' \dontrun{
#' query_sb(list(q = "water"))
#' 
#' # Search by project status
#' query_sb(list(projectStatus = "Active"))
#' 
#' # Search a folder ID
#' query_sb(list(q = "water", folderId = '504216b9e4b04b508bfd337d'))
#' 
#' # Filter by ancestor
#' query_sb(list(ancestors = "4f831626e4b0e84f6086809b"))
#' 
#' # Filter by tags
#' query_sb(list(tags = "distribution"))
#' 
#' # Filter by browse category
#' query_sb(list(browseCategory = "Image"))
#' 
#' # Filter by browse type
#' query_sb(list(browseType = "Map Service"))
#' 
#' # Filter by WKT geometry string
#' wkt1 <- "POLYGON((-104.4 41.0,-95.1 41.0,-95.1 37.5,-104.4 37.5,-104.4 41.0))"
#' wkt2 <- "POLYGON((-104.4 38.3,-95.2 38.3,-95.2 33.7,-104.4 34.0,-104.4 38.3))"
#' query_sb(list(spatialQuery = wkt1))
#' query_sb(list(spatialQuery = wkt1, spatialQuery = wkt2))
#' 
#' # Date range
#' query_sb(list(dateRange = '{"dateType":"Collected","choice":"year"}'))
#' query_sb(list(dateRange = '{"dateType":"lastUpdated","choice":"month"}'))
#' query_sb(list(dateRange = 
#' 		'{"dateType":"Release","choice":"range","start":"2014-09-01","end":"2015-09-01"}'))
#' }
#' 
#'
#' @export
query_sb = function(query_list, ..., limit=20, session = current_session()){
	
	if(!is(query_list, 'list')){
		stop('query_list must be a list of query parameters')
	}
	
	#automatically add 's=Search' if it isn't in the query_list, I don't know
	#of a situation where you don't want this yet
	query_list['s'] = 'Search'
	
	#force JSON format
	query_list['format'] = 'json'
	
	#ensure we're querying for the bare minimum set of fields
	if('fields' %in% names(query_list)){
		query_list[['fields']] = paste0(query_list[['fields']], ',id,title,parentId,dateCreated,lastUpdated,createdBy,lastUpdatedBy,files')
	}else{
		query_list[['fields']] = 'id,title,parentId,dateCreated,lastUpdated,createdBy,lastUpdatedBy,files'
	}
	
	
	#we will need to set max based on limit
	if(limit > 1000){
		query_list['max'] = 1000
	}else{
		query_list['max'] = limit
	}
	
	tryCatch({
	result = query_items(query_list, ..., session=session)
	}, error = function(e) {
		result <- list(status = 404)
		warning(paste("unhandled error with sciencebase request. \n", 
						"Error was: \n", e))
	})
	
	if(is(result, "list") && result$status == 404) {
		return(NULL)
	}
	
	#parse the result and turn it into a list of sbitems
	res_obj = httr::content(result, type='application/json')
	
	out = lapply(content(result)$items, as.sbitem)
	
	# we may need to page to get all the results
	if(is.null(res_obj)) {
	  warning('empty https response. returning NULL')
	  return(NULL)
	}
	if(limit > 1000 && res_obj$total > 1000){
		offset = 1000
		while(offset < limit){
			query_list['offset'] = offset
			
			result = query_items(query_list, ..., session=session)
			res_obj = httr::content(result, type='application/json')
			#concatenate it onto output list
			out = c(out, lapply(content(result)$items, as.sbitem))
			offset = offset + 1000 #need to get in chunks of 1k
		}
	}
	
	#truncate to limit requested as we queried in blocks of 1000
	if(length(out) > limit){
		out = out[1:limit]
	}
	return(out)
}

