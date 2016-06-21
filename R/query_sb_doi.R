#' @title Query SB for specific DOI (Digital Object Identifier)
#' 
#' 
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
#' @examples 
#' #Two example DOI-specific queries
#' query_sb_doi('10.5066/F7M043G7')
#' 
#' query_sb_doi('10.5066/F7Z60M35')
#' 
#' @export
query_sb_doi = function(doi, ..., limit=20, session=current_session()){
	
	#query twice with and without DOI appended. 
	
	res = query_item_identifier(scheme='https://www.sciencebase.gov/vocab/category/item/identifier', 
															type='DOI', key=doi, ..., session=session, limit=limit)
	
	res2 = query_item_identifier(scheme='https://www.sciencebase.gov/vocab/category/item/identifier', 
															 type='DOI', key=paste0('doi:', doi), ..., session=session, limit=limit)
	
	res_ids = c(res, res2)
	
	return(res_ids)
}
