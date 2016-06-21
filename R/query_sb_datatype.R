#' @title Query SB for specific data type
#' 
#' 
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
#' @examples 
#' #query for items with WFS Layer data
#' query_sb_datatype('Static Map Image')
#' 
#' #query for US Topo maps
#' query_sb_datatype('Map Service')
#' 
#' 
#' @export
query_sb_datatype = function(datatype, ..., limit=20, session=current_session()){
	
	res = query_sb(list(filter=paste0('browseType=', datatype)), ..., session=session, limit=limit)
	
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
#' @examples 
#' #return all datatypes (limit 50 by default)
#' sb_datatypes()
#' 
#' 
#' @export
sb_datatypes = function(limit=50, session=current_session()){

	query = list(q="", format="json", max=limit)
	
	r = sbtools_GET(paste0(pkg.env$domain, 'vocab/categories/browseTypes'), query=query, session=session)
	
	result = content(r, type='application/json')
	
	return(sapply(result$list, function(x)x$name))
}


