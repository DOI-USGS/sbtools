#' @title Query SB for items using generic query parameters
#' 
#' @param query_list List of item query selectors. See Details.
#' @param limit Maximum number of returned items. Will do paging to retrieve
#' results when limit is over 1000. Use with caution, queries 10k results
#' are slow.
#' @param ... Additional parameters are passed on to \code{\link[httr]{GET}}
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
#' @examples \dontrun{
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
query_sb = function(query_list, ..., limit=20){
	
	tryCatch({
		
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
		result <- query_items(query_list, ...)
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
			
			result = query_items(query_list, ...)
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
	},
	error = function(e) {
		warning(paste("Something unexpected went wrong with a web request \n",
									"Original error was:\n", e))
		return(NULL)
	})
	
	if(exists("out")) {
		return(out)
	} else {
		NULL
	}
	
}

#' @title Query SB for items containing specific text
#' @param text Text string for search
#' @inheritParams query_sb
#' 
#' @return 
#' A list of \code{\link{sbitem}} objects. List of length 0 
#' means no matches were found.
#' 
#' @description 
#' Queries for ScienceBase items that have matching text in the title or 
#' description
#' 
#' @examples \donttest{
#' #query for a person's name
#' query_sb_text('Luna Leopold')
#' 
#' #query for one of the old river gaging stations
#' query_sb_text('Lees Ferry')
#' }
#' 
#' @export
query_sb_text = function(text, ..., limit=20){
	
	res = query_sb(list(q=text), limit=limit)
	
	return(res)
}

#' @title Query SB based on spatial extent
#' 
#' @inheritParams query_sb
#' @param bbox An sf spatial data object. The bounding box of the object is used for the query.
#' @param long A vector of longitude values that will define the boundaries of a bounding box. Min and Max of supplied longitudes are used. (alternate option to bbox).
#' @param lat A vector of latitude values that will define the boundaries of a bounding box. Min and Max of supplied latitude are used. (alternate option to bbox).
#' @param bb_wkt A character string using the Well Known Text (WKT) standard for defining spatial data. Must be a POLYGON WKT object. 
#' 
#' @description 
#' Queries ScienceBase based on a spatial bounding box. Accepts either an sp spatial data object
#' (uses the spatial object's bounding box) or long/lat coordinates defining the bounding box limits. 
#' 
#' 
#' @examples \donttest{
#' #specify the latitude and longitude points to define the bounding box range. 
#' # This is simply bottom left and top right points
#' query_sb_spatial(long=c(-104.4, -95.1), lat=c(37.5, 41.0), limit=3)
#' 
#' #use a pre-formatted WKT polygon to grab data
#' query_sb_spatial(bb_wkt="POLYGON((-104.4 41.0,-95.1 41.0,-95.1 37.5,-104.4 37.5,-104.4 41.0))", 
#' 	                limit=3)
#' }
#' @export
#' 
query_sb_spatial = function(bbox, long, lat, bb_wkt, ..., limit=20){
	
	if(!missing(bbox)){
		bbox <- sf::st_bbox(bbox)
		
		bb = matrix(nrow=2, ncol=2)
		bb[1,1] = bbox[1]
		bb[2,1] = bbox[2]
		bb[1,2] = bbox[3]
		bb[2,2] = bbox[4]
		
		bb_wkt = make_wkt(bb)
	}else if(!missing(long) & !missing(lat)){
		bb = matrix(nrow=2, ncol=2)
		bb[1,1] = min(long)
		bb[2,1] = min(lat)
		bb[1,2] = max(long)
		bb[2,2] = max(lat)
		
		bb_wkt = make_wkt(bb)
	}else if(!missing(bb_wkt)){
		#We'll just assume this is fine then and will be passed to query_sb
		
	}else{
		stop('Must supply either sf object or bb_min and bb_max vectors')
	}
	
	query_sb(list(spatialQuery = bb_wkt), limit=limit)
}

make_wkt = function(bb){
	paste0('POLYGON((', bb[1,1], ' ', bb[2,2], ',',
				 bb[1,2], ' ', bb[2,2], ',', 
				 bb[1,2], ' ', bb[2,1], ',',
				 bb[1,1], ' ', bb[2,1], ',',
				 bb[1,1], ' ', bb[2,2], '))')
}

#' @title Query SB for specific DOI (Digital Object Identifier)
#' @param doi DOI to search for as character
#' @inheritParams query_sb
#' 
#' @return 
#' A list of \code{\link{sbitem}} objects. List of length 0 
#' means no matches were found.
#' 
#' @description 
#' Queries for ScienceBase items with a specific DOI identifier. 
#' In ScienceBase, these are stored as additional unique identifiers.
#' 
#' @examples \donttest{
#' #Two example DOI-specific queries
#' query_sb_doi('10.5066/F7M043G7')
#' 
#' query_sb_doi('10.5066/F7Z60M35')
#' }
#' @export
query_sb_doi = function(doi, ..., limit=20){
	
	#query twice with and without DOI appended. 
	
	res = query_item_identifier(scheme='https://www.sciencebase.gov/vocab/category/item/identifier', 
															type='DOI', key=doi, ..., limit=limit)
	
	res2 = query_item_identifier(scheme='https://www.sciencebase.gov/vocab/category/item/identifier', 
															 type='DOI', key=paste0('doi:', doi), ..., limit=limit)
	
	res_ids = c(res, res2)
	
	return(res_ids)
}

#' @title Query SB for items within a date range
#' @param start Start date as \code{\link{POSIXct}} object. Defaults to 1970-01-01
#' @param end   End date as \code{\link{POSIXct}} object. Defaults to today.
#' @param date_type Which object timestamp to query against. Options are (case sensitive): 
#' 'Acquisition', 'Award', 'Collected', 'dateCreated', 'Received', 'Reported', 
#' 'Transmitted', 'Due', 'End', 'Info', 'lastUpdated', 'Publication', 'Release', 
#' 'Repository Created', 'Repository Updated', 'Start'.
#' 
#' 
#' @inheritParams query_sb
#' 
#' @description 
#' Queries ScienceBase for items with timestamps within a certain date/time range.
#' 
#' @examples \dontrun{
#' # find items updated today
#' query_sb_date(Sys.time(), Sys.time())
#' 
#' # find items with publications from the 1970's
#' query_sb_date(as.POSIXct('1970-01-01'), as.POSIXct('1980-01-01'), 
#'   date_type='Publication', limit=1000)
#' 
#' }
#' 
#' @export
query_sb_date = function(start=as.POSIXct('1970-01-01'), end=Sys.time(), date_type='lastUpdated', ..., limit=20){
	
	if(!date_type %in% valid_date_types){
		stop('date_type choice must be identical (including case) to listed options in documentation. See ?query_sb_date')
	}
	
	date_range = paste0('{"dateType":"', date_type, '","choice":"range","start":"', format(start, '%Y-%m-%d'),
											'","end":"', format(end, '%Y-%m-%d'), '"}')
	
	query_sb(list(dateRange = date_range), ..., limit=limit)
	
}

valid_date_types = c('Acquisition', 'Award', 'Collected', 'dateCreated', 'Received', 'Reported',
										 'Transmitted', 'Due', 'End', 'Info', 'lastUpdated', 'Publication', 'Release', 
										 'Repository Created', 'Repository Updated', 'Start')

#' @title Query SB for specific data type
#' @param datatype Character string indicating datatype. See \code{\link{sb_datatypes}} for full
#' list of available datatypes.
#' 
#' @inheritParams query_sb
#' 
#' @return 
#' A list of \code{\link{sbitem}} objects. List of length 0 
#' means no matches were found.
#' 
#' @description 
#' Queries ScienceBase for items with matching datatype.
#' 
#' @examples \donttest{
#' #query for items with WFS Layer data
#' query_sb_datatype('Static Map Image')
#' 
#' #query for US Topo maps
#' query_sb_datatype('Map Service')
#' }
#' 
#' @export
query_sb_datatype = function(datatype, ..., limit=20){
	
	res = query_sb(list(filter=paste0('browseType=', datatype)), ..., limit=limit)
	
	return(res)
}


#' @title Query SB for all available datatypes
#' 
#' @inheritParams query_sb
#' 
#' @description 
#' Queries ScienceBase for the list of all available datatypes. This can be
#' coupled with \code{\link{query_sb_datatype}} to query based on the type of data
#' 
#' 
#' @examples \dontrun{
#' #return all datatypes (limit 50 by default)
#' sb_datatypes()
#' }
#' 
#' @export
sb_datatypes = function(limit=50){
	
	query = list(q="", format="json", max=limit)
	
	r = sbtools_GET(paste0(pkg.env$domain, 'vocab/categories/browseTypes'), query=query)
	
	if(is(r, "list") && r$status == 404) {
		return(NULL)
	}
	
	result = content(r, type='application/json')
	
	return(sapply(result$list, function(x)x$name))
}
