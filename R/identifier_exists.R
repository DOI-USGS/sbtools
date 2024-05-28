#' @title Check if identifier exists
#' 
#' @export
#' @template manipulate_item
#' @description This function quickly checks to see if an identifier exists. 
#' It does a quick head request to skip the overhead of item metadta retrieval.
#' This will also return \code{FALSE} if the identifier exists but is associated
#' with an item that is unavailable due to permission restrictions.
#' 
#' @importFrom methods is
#' 
#' @return Logical, \code{TRUE} or \code{FALSE}
#' @examples \donttest{
#' # identifier exists
#' identifier_exists(sb_id = "57976a0ce4b021cadec97890")
#' 
#' # identifier does not exist
#' identifier_exists(sb_id = "aaaaaaakkkkkkkbbbbbb")
#' }
identifier_exists <- function(sb_id, ...) {
	#sb_id = as.sbitem(sb_id)
	if(is(sb_id, 'sbitem')){
		sbtools_HEAD(url = paste0(pkg.env$url_item, sb_id$id), ...)
	}else{
		sbtools_HEAD(url = paste0(pkg.env$url_item, sb_id), ...)
	}
}

