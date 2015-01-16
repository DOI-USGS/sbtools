#'
#'@title Query SB for items using generic query parameters
#'
#'@param query_list 
#'
#'
#'
#'
#'
#'@export
query_items = function(query_list, session){
	
	return(GET(pkg.env$url_items, handle=session, query=query_list))
	
}