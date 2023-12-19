#' @title Update a SB item with new metadata
#'
#' @template manipulate_item
#' @param info list of metadata info (key-value pairs) to change on the item
#'
#' @return An object of class \code{sbitem}
#'
#' @import httr
#' @import jsonlite
#'
#' @description 
#' Updates metadata associated with a ScienceBase item based on 
#' supplied list of new or updated metadata elements.
#' 
#' @examples \dontrun{
#' res <- item_create(user_id(), "item-to-update") 
#' out <- item_update(res, list(title = "item-updated"))
#' out$title
#' }
#' @export
item_update = function(sb_id, info, ...){
	
	item <- as.sbitem(sb_id)
	is_info_list(info)
	
	r = sbtools_PUT(url = paste0(pkg.env$url_item, item$id), 
									body = toJSON(info, auto_unbox = TRUE, null='null'),
									..., accept_json())
	
	# catch 405, which is, I think, expired login
	if ('errors' %in% names(content(r))) {
	  stop(content(r)$errors$message)
	}
	return(as.sbitem(content(r)))
}

#' @title Update many SB items with new metadata
#'
#' @export
#' @param sb_id An \code{\link{sbitem}} object or a character ScienceBase ID 
#' corresponding to the item
#' @param info list of metadata info (key-value pairs) to change on the item
#' @param ... Additional parameters are passed on to \code{\link[httr]{PUT}}
#' 
#' @description 
#' A method to update multiple ScienceBase items with a single call and a single HTTP service
#' request. Can be useful for improving performance of updating a large number of items at once.
#' 
#' @return One or more objects of class \code{sbitem} in a list
#' @details If length of \code{sb_id} > 1, then length of \code{info} input must be the same
#' @examples \dontrun{
#' # helper function to make a random name
#' aname <- function() paste0(sample(letters, size = 5, replace = TRUE), collapse = "")
#' 
#' res <- items_create(user_id(), title = c(aname(), aname())) 
#' out <- items_update(res, info = list( list(title = aname()), list(title = aname()) ) )
#' vapply(out, "[[", "", "title")
#' }
items_update <- function(sb_id, info, ...){
	
	if (length(sb_id) == 1) sb_id <- list(sb_id)
	item <- vapply(sb_id, function(z) as.sbitem(z)$id, "")
	invisible(lapply(info, is_info_list))
	
	body <- unname(
		Map(function(x, y) {
			c(list('id' = x, null = "null"), y)
		}, item, info
		)
	)
	
	res <- sbtools_PUT(url = pkg.env$url_items, 
										 body = toJSON(body, auto_unbox = TRUE), 
										 ..., accept_json())
	
	# catch 405, which is, I think, expired login
	if ('errors' %in% names(content(res))) {
		stop(content(res)$errors$message, call. = FALSE)
	}
	
	lapply(content(res), as.sbitem)
}

is_info_list <- function(x) {
	if (!is.list(x)) {
		stop('Info must be a list of name-value pairs that can be serialized into JSON', call. = FALSE)
	}
}
