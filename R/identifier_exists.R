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
#' @examples
#' 
#' # identifier exists
#' identifier_exists(sb_id = "4f4e4b24e4b07f02db6aea14")
#' 
#' # identifier does not exist
#' identifier_exists(sb_id = "aaaaaaakkkkkkkbbbbbb")
#' 
identifier_exists <- function(sb_id, ..., session = current_session()) {
	#sb_id = as.sbitem(sb_id)
	if(is(sb_id, 'sbitem')){
		sbtools_HEAD(url = paste0(pkg.env$url_item, sb_id$id), ..., session = session)
	}else{
		sbtools_HEAD(url = paste0(pkg.env$url_item, sb_id), ..., session = session)
	}
}

