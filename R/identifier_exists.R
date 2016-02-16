#' @title Check if identifier exists
#' 
#' @export
#' @template manipulate_item
#' @details This function does not accept a sbitem class object because 
#' we'd run \code{\link{as.sbitem}} on it, which would make a GET request,
#' and we only want to make a quick HEAD request.
#' @return Logical, \code{TRUE} or \code{FALSE}
#' @examples \dontrun{
#' # identifier exists
#' identifier_exists(sb_id = "4f4e4b24e4b07f02db6aea14")
#' 
#' # identifier does not exist
#' identifier_exists(sb_id = "aaaaaaakkkkkkkbbbbbb")
#' }
identifier_exists <- function(sb_id, ..., session = current_session()) {
	sb_id = as.sbitem(sb_id)
	sbtools_HEAD(url = paste0(pkg.env$url_item, sb_id$id), ..., session = session)
}
