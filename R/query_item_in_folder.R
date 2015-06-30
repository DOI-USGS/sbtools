#' Search within an SB folder
#' 
#' Search for text in the title, abstract, etc. within an SB folder and any
#' subfolders.
#' 
#' @param text text in the title, abstract, etc. of the desired item
#' @param folder an SB item ID for the folder to search in
#' @param ... Additional parameters are passed on to \code{\link[httr]{GET}}
#' @param session (optional) SB Session to use, not provided queries public 
#'   items only
#' @param limit Max number of matching items to return
#' @import httr
#' @export
query_item_in_folder <- function(text, folder, ..., session=current_session(), limit=20) {
	# create and run the query
	url=paste0(pkg.env$url_base,"items")
	query=list('q'=text, 'folderId'=folder, 'max'=limit, 'format'='json')
	r <- sbtools_GET(url, ..., query=query, session=session)
	
	# check results
	if(r$status_code == 409){
		stop('Multiple items described by that ID')
	}
	
	response = content(r, 'parsed')
	
	#check if no items matched
	if(length(response$items) == 0){
		return(data.frame())
	}
	
	out = data.frame(title=rep(NA, length(response$items)), id=NA)
	# if we have items, populate data.frame and return
	for(i in 1:length(response$items)){
		out[i,]$title = response$items[[i]]$title
		out[i,]$id = response$items[[i]]$id
	}
	
	return(out)
}