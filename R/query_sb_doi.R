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
#' @details 
#' Queries for ScienceBase items with a specific DOI identifier. 
#' In ScienceBase, these are stored as additional unique identifiers.
#' 
#' @examples 
#' #some item DOI's have 'doi:' appended to the front of them for some reason
#' query_sb_doi('doi:10.5066/F7M043G7')
#' 
#' #others do not
#' query_sb_doi('10.1126/science.aab1345')
#' 
#' @export
query_sb_doi = function(doi, ..., limit=20, session=current_session()){
	
	
	res = query_item_identifier(scheme='https://www.sciencebase.gov/vocab/category/item/identifier', 
															type='DOI', key=doi, ..., session=session, limit=limit)
	
	return(lapply(res$id, as.sbitem))
}