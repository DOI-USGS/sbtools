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
#' item_get("57976a0ce4b021cadec97890")
#' 
#' # Get many w/ e.g., an lapply() call
#' library("httr")
#' res <- query_items(list(s = "Search", q = "water", format = "json"))
#' if(res$status == 200) {
#'   ids <- vapply(httr::content(res)$items, "[[", "", "id")
#'   (out <- lapply(ids[1:3], item_get))
#' }
#' # create item class from only an item ID
#' as.sbitem("5ebe92af82ce476925e44b8f")
#' 
#' # sbitem gives back itself
#' (x <- as.sbitem("5ebe92af82ce476925e44b8f"))
#' as.sbitem(x)
#' }

#' @export
#' @rdname sbitem
as.sbitem <- function(x, ...) UseMethod("as.sbitem")

#' @export
#' @rdname sbitem
as.sbitem.default <- function(x, ...) stop(paste("No 'as.sbitem' method for class ", 
																								 paste(class(x), collapse = ", ")), 
																					 call. = FALSE)

#' @export
as.sbitem.NULL <- function(x, ...) NULL

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
