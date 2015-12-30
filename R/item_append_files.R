#' @title Upload File to Item
#' @description Adds a file to an item
#'
#' @template manipulate_item
#' @param files A file path to upload.
#'
#' @return An object of class \code{sbitem}
#'
#' @import httr
#'
#' @examples 
#' \dontrun{
#' res <- item_create(user_id(), "testing 123")
#' cat("foo bar", file = "foobar.txt")
#' item_append_files(res$id, "foobar.txt")
#' }
#' @export
item_append_files = function(id, files, ..., session=current_session()){

	item <- as.sbitem(id)
	r = sbtools_POST(url = paste0(pkg.env$url_upload,'?id=', item$id), ...,
									 body = multi_file_body(files), 
									 session = session)
  
  return(as.sbitem(content(r)))
	
}
