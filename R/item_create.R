#' @title Create a new SB item
#'
#' @template item_with_parent
#' @param title The title of the new SB item
#' @param info (optional) list of metadata info for the new item
#' @return An object of class \code{sbitem}
#'
#' @description 
#' Create a new item on ScienceBase with the requested parent and item title. 
#' Info can be provided to populate metadata at the time of creation. 
#'
#' @export
#' @examples \dontrun{
#' # Create an item - by default we use your user ID
#' item_create(title = "testing 123")
#' 
#' # Pass an object of class sbitem
#' x <- folder_create(user_id(), "foobar456")
#' item_create(x, "foobar456-item")
#' }
item_create = function(parent_id = user_id(), title, ..., info){
		
	item <- as.sbitem(parent_id)
	body = list('parentId' = item$id, 'title' = title)
	
	#Bring in additional metadata if supplied
	if (!missing(info) && is.list(info)) {
		body = c(body, info)
	}
	
	r = sbtools_POST(url = pkg.env$url_item, ..., body = toJSON(body, auto_unbox = TRUE))
	
	return(as.sbitem(content(r)))
}

#' @title Create many new SB items
#'
#' @export
#' @param parent_id An \code{\link{sbitem}} object or character ScienceBase ID 
#' corresponding to the parent item (folder). This must be of length 1 or more. 
#' If length 1, then we recycle it for every item.
#' @param title Two or more titles for the new SB items
#' @param info (optional) list of metadata info for the new items. for each 
#' item include a named list of variables
#' @param ... Additional parameters are passed on to \code{\link[httr]{GET}}, \code{\link[httr]{POST}},
#' \code{\link[httr]{HEAD}}, \code{\link[httr]{PUT}}, or \code{\link[httr]{DELETE}}
#' @return One or more objects of class \code{sbitem} in a list
#' @details The length of the \code{title} and \code{info} values must be the same
#' length - however, the \code{parent_id} can be of length 1 or equal to the length 
#' of each of \code{title} and \code{info} parameters
#' 
#' @description 
#' A method to create multiple ScienceBase items with a single call and a single HTTP service
#' request. Can be useful for improving performance of creating a large number of items at once.
#' 
#' @examples \dontrun{
#' # helper function to make a random name
#' aname <- function() paste0(sample(letters, size = 5, replace = TRUE), collapse = "")
#' 
#' # Create some items - by default we use your user ID
#' items_create(title = c(aname(), aname()))
#' 
#' # add additional items in the info parameter - by default we use your user ID
#' items_create(title = c(aname(), aname()), 
#' 		info = list(
#' 						list(contacts = list(list(name = "Suzy"))), 
#' 						list(contacts = list(list(name = "Brandy")))
#' 					)
#' 	)
#' 	
#' # another example with more information - by default we use your user ID
#' items_create(title = c(aname(), aname()), 
#' 		info = list(
#' 						list(contacts = list(list(name = "Suzy"))), 
#' 						list(contacts = list(list(name = "Brandy")))
#' 					)
#' 	)	
#' 
#' # Pass an object of class sbitem
#' (x <- folder_create(user_id(), aname()))
#' items_create(x, title = c(aname(), aname()))
#' }
items_create <- function(parent_id = user_id(), title, ..., info = NULL) {
	
	if (!length(parent_id) > 0) stop("parent_id must be of length > 0", call. = FALSE)
	if (length(parent_id) > 1) {
		if (length(parent_id) != length(title)) {
			stop("If parent_id length > 1, it must be of same length as title and info", call. = FALSE)
		}
	}
	
	item <- lapply(parent_id, as.sbitem)
	item <- if (length(item) == 1) rep(item[[1]]$id, length(title)) else item[[1]]$id 
	body <- unname(
		Map(function(x, y) {
			list('parentId' = x, 'title' = y)
		}, item, title
		)
	)
	
	if (!is.null(info)) {
		body <- Map(function(a, b) c(a, b), body, info)
	}
	
	res <- sbtools_POST(url = pkg.env$url_items, ..., 
											body = toJSON(body, auto_unbox = TRUE))
	
	lapply(content(res), as.sbitem)
}
