#' Ping ScienceBase to see if it's available
#' 
#' @export
#' @param ... Additional parameters are passed on to \code{\link[httr]{GET}}
#' @return Boolean (TRUE) indicating if a connection to ScienceBase can be established 
#' and if it is responding as expected. FALSE otherwise. 
#' @examples \dontrun{
#' sb_ping()
#' }
sb_ping <- function(...) {

	tryCatch({
		x <- GET(paste0(pkg.env$url_item, 'ping'), ...)
		res = jsonlite::fromJSON(content(x, "text"))
		if(is(res, 'list') & !is.null(res$result) & res$result == 'OK'){
			return(TRUE)
		}
	}, error=function(e){
		return(FALSE)
	})
	
	return(FALSE)
}
