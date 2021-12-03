#' @title Query SB for items containing specific text
#' 
#' 
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
query_sb_text = function(text, ..., limit=20, session=current_session()){
	
	res = query_sb(list(q=text), limit=limit, session=session)
	
	return(res)
}