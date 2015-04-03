#'
#'@title Query SB for items using generic query parameters
#'
#'@param query_list 
#'
#'@param query_list List of item query selectors
#'@param session Session object from \code{\link{authenticate_sb}}
#'
#'
#'@export
query_items = function(query_list, session){
	
	return(GET(pkg.env$url_items, handle=session, query=query_list))
	
}