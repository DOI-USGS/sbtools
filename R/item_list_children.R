#' Return IDs for all child items
#'
#' Returns a list of child IDs for a ScienceBase item
#'
#' @template manipulate_item
#' @param limit Max children returned
#'
#' @return \code{data.frame} with a row for each child item
#'
#' @examples \dontrun{
#' item_list_children('5060b03ae4b00fc20c4f3c8b')
#'
#' as.sbitem('5060b03ae4b00fc20c4f3c8b') %>% item_list_children
#' item_get('5060b03ae4b00fc20c4f3c8b') %>% item_list_children
#' }
#' @export
item_list_children = function(sb_id, ..., session=current_session(), limit=20){

	if(!session_validate(session)){
		stop('Session state is invalid, please re-authenticate')
	}

	item <- as.sbitem(sb_id)

	query=list('parentId'=item$id, 'max'=limit, 'format'='json', 'fields'='id,title')
	r = sbtools_GET(url = pkg.env$url_items, ..., query=query, session=session)

	items = content(r, 'parsed')$items

	if(length(items) < 1){
		return(data.frame())
	}

	out = data.frame(title=NA, id = rep(NA, length(items)))
	for(i in 1:length(items)){
		out$title[i] = if("title" %in% names(items[[i]])) items[[i]]$title else NA
	  out$id[i] = items[[i]]$id
	}

	return(out)
}
