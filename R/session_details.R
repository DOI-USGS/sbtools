#' @title Get session info
#' 
#' @param ... Additional parameters are passed on to \code{\link[httr]{GET}}
#' @param session SB session object from \code{\link{authenticate_sb}}
#' 
#' @description
#' Get the details associated with current ScienceBase user session. 
#' 
#' @return list, if not logged in states that, but if logged in, user details
#' If logged in, will include a "jossoSessionId" that can be used for 
#' authenticated file downloads by appending josso={'jossoSessionId'} to the 
#' url for the file requiring authentication. This is helpful when passing
#' a url to a library that will handle the download of a remote file automatically 
#' such as jsonlite::readJSON or readr::read_csv.
#' @examples \dontrun{
#' 
#' session_details()
#'  
#' # If logged in, can use jossoSessionId for downloads.
#' authenticate_sb()
#' 
#' temp_json <- tempfile(fileext = ".json")
#' 
#' jsonlite::write_json(list(test = "test"), temp_json)
#' 
#' item <- item_upload_create(sbtools::user_id(), temp_json)
#' 
#' token <- session_details()$jossoSessionId
#' 
#' (base_url <- item$file[[1]]$downloadUri)
#' 
#' # will fail
#' try(jsonlite::read_json(base_url))
#' 
#' url <- paste0(base_url, "&josso=", token)
#' 
#' jsonlite::read_json(url)
#' 
#' item_rm(item$id)
#' 
#' }
#' @export
session_details <- function(..., session = current_session()) {
	x <- GET(paste0(pkg.env$url_base, "jossoHelper/sessionInfo?includeJossoSessionId=true"), 
					 handle = session, timeout = httr::timeout(default_timeout()),
					 ...)
	stop_for_status(x)
	jsonlite::fromJSON(content(x, "text"))
}
