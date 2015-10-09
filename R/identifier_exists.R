#' Check if identifier exists
#' 
#' @export
#' @param id (character) A ScienceBase ID. See Details.
#' @param ... Additional parameters are passed on to \code{\link[httr]{GET}}
#' @param session (optional) SB session from \link{authenticate_sb}
#' @details This function does not accept a sbitem class object because 
#' we'd run \code{\link{as.sbitem}} on it, which would make a GET request,
#' and we only want to make a quick HEAD request.
#' @return Logical, \code{TRUE} or \code{FALSE}
#' @examples \dontrun{
#' # identifier exists
#' identifier_exists(id = "4f4e4b24e4b07f02db6aea14")
#' 
#' # identifier does not exist
#' identifier_exists(id = "aaaaaaakkkkkkkbbbbbb")
#' }
identifier_exists <- function(id, ..., session = current_session()) {
	sbtools_HEAD(url = paste0(pkg.env$url_item, id), ..., session = session)
}
