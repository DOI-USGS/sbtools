#' Query SB for items using generic query parameters
#' 
#' @param query_list List of item query selectors. See Details.
#' @param ... Additional parameters are passed on to \code{\link[httr]{GET}}
#' @param session Session object from \code{\link{authenticate_sb}}
#' @return An object of class \code{\link[httr]{response}}
#' @export
#' @details The following is a list of query parameters you can use 
#' in the \code{query_list} parameter.
#' 
#' \itemize{
#'  \item s (character): Only option: "Search"
#'  \item format (character): One of "json", "xml", or "atom"
#'  \item q (character): Query string
#'  \item max (integer): Number of records to return. Default: 20
#'  \item offset (integer): Record to start at. Default: 1
#'  \item fields (character): Character vector of fields to return
#'  \item folderId (character): Alphanumeric string representing folder ID
#'  \item parentId (character): Alphanumeric string representing folder ID. This
#'  can be used to return all children items within the folder, but not within 
#'  sub-folders.
#'  \item ancestors (character): Alphanumeric string representing folder ID. This
#'  can be used to return all children items within the folder, even within 
#'  sub-folders.
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
#' }
query_items = function(query_list, ..., session=current_session()){
	
	return(sbtools_GET(url = pkg.env$url_items, ..., query=query_list, session=session))
	
}
