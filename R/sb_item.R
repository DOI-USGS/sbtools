#' ScienceBase item class
#' 
#' @export
#' @name sbitem
#' @param x Input, variety of things, character, list, or sbitem class 
#' object
#' @param ... Further args passed on to \code{\link{item_get}}, only in 
#' the method for character class inputs
#' @examples \donttest{
#' # Single item from item_get()
#' item_get("4f4e4b24e4b07f02db6aea14")
#' 
#' # Get many w/ e.g., an lapply() call
#' library("httr")
#' res <- query_items(list(s = "Search", q = "water", format = "json"))
#' ids <- vapply(httr::content(res)$items, "[[", "", "id")
#' (out <- lapply(ids[1:3], item_get))
#' 
#' # create item class from only an item ID
#' as.sbitem("4f4e4b24e4b07f02db6aea14")
#' 
#' # sbitem gives back itself
#' (x <- as.sbitem("4f4e4b24e4b07f02db6aea14"))
#' as.sbitem(x)
#' }

#' @export
#' @rdname sbitem
as.sbitem <- function(x, ...) UseMethod("as.sbitem")

#' @export
#' @rdname sbitem
as.sbitem.default <- function(x, ...) stop("No 'as.sbitem' method for class ", class(x), call. = FALSE)

#' @export
as.sbitem.character <- function(x, ...) get_item(x, ...)

#' @export
as.sbitem.sbitem <- function(x, ...) x

#' @export
as.sbitem.list <- function(x, ...) sb_item(x)

#' @export
print.sbitem <- function(x, ...) {
	cat("<ScienceBase Item>", "\n")
	cat("  Title: ", x$title, "\n", sep = "")
	cat("  Creator/LastUpdatedBy:     ", x$provenance$createdBy, " / ", x$provenance$lastUpdatedBy, "\n", sep = "")
	cat("  Provenance (Created / Updated):  ", x$provenance$dateCreated, 
			" / ", x$provenance$lastUpdated, "\n", sep = "")
	cat("  Children: ", x$hasChildren, "\n", sep = "")
	cat("  Item ID: ", x$id, "\n", sep = "")
	cat("  Parent ID: ", x$parentId, "\n", sep = "")
}

#' @export
#' @rdname sbitem
is.sbitem <- function(x) {
	if (is(x, "sbitem")) TRUE else FALSE
}

sb_item <- function(x) {
	structure(x, class = "sbitem")
}

pluck <- function(x, name, type) {
	if (missing(type)) {
		lapply(x, "[[", name)
	} else {
		vapply(x, "[[", name, FUN.VALUE = type)
	}
}

get_item <- function(id, ..., session=current_session()) {
	res <- sbtools_GET(url = paste0(pkg.env$url_item, id), ..., 
									 query = list(type = 'json'), session = session)
	return(as.sbitem(content(res)))
}
