#' #' Upload file(s) and create a new item
#' 
#' Create a new item with files attached, all in one call to SB
#' 
#' @template item_with_parent
#'
#' @param files A string vector of paths to files to be uploaded
#' @param scrape_files logical should the files be scraped for metadata? 
#' If TRUE, sciencebase will attempt to create extensions based on the files.
#' 
#' For example, for shapefiles, this will result in a shapefile extension 
#' to be returned as a facet of the sciencebase item. 
#' See item: "58069258e4b0824b2d1d422e" for an example.
#' 
#' @export
#' @return An object of class \code{sbitem}
#' @examples \dontrun{
#' # You'll need a parent id for a folder/item
#' ## here, using your highest level parent folder
#' file <- system.file("examples", "books.json", package = "sbtools")
#' item_upload_create(user_id(), file)
#' }
item_upload_create = function(parent_id, files, ..., scrape_files = TRUE, session=current_session()){
	
	if(length(files) > 50){
		warning('Trying to attach a large number of files to a SB item. SB imposes file limits which may cause this to fail')
	}
	
	item <- as.sbitem(parent_id)
	
	params <- '?title=title'
	
	if(!scrape_files) {
		params <- paste0(params, '&scrapeFile=false')
	}
	
	r = sbtools_POST(url = paste0(pkg.env$url_upload_create, item$id, params), 
									 ...,
									 body = multi_file_body(files), 
									 session = session)
	
	
	#check to see if we've been redirected to the login page
	if (grepl('josso/signon', r$url)) {
		stop('Not authenticated or lack of permission to parent object\nAunthenticate with the authenticate_sb function.')
	}
	
	item <- as.sbitem(content(r))
	
	return(check_upload(item, files))
}

#' 
#' @title Upload File to Item
#' @description Adds a file to an item
#'
#' @template manipulate_item
#' @inheritParams item_upload_create
#'
#' @return An object of class \code{sbitem}
#'
#' @import httr
#'
#' @examples \dontrun{
#' res <- item_create(user_id(), "testing 123")
#' cat("foo bar", file = "foobar.txt")
#' item_append_files(res$id, "foobar.txt")
#' }
#' @export
item_append_files = function(sb_id, files, ..., scrape_files = TRUE, session=current_session()){
	
	if(length(files) > 50){
		warning('Trying to attach a large number of files to a SB item. SB imposes file limits which may cause this to fail')
	}
	
	item <- as.sbitem(sb_id)
	
	if(is.null(item)) return(NULL)
	
	params <- paste0("?id=", item$id)
	
	if(!scrape_files) {
		params <- paste0(params, "&scrapeFile=false")
	}
	
	r = sbtools_POST(url = paste0(pkg.env$url_upload, params), ...,
									 body = multi_file_body(files), 
									 session = session)
  
	item <- as.sbitem(content(r))
	
	return(check_upload(item, files))
	
}

check_upload <- function(item, files) {
	
	if(!all(basename(files) %in% sapply(item$files, function(x) x$name))) {
		warning("Not all files ended up in the item files. \n",
		"This indicates that a sciencebase extension was created with the file. \n",
		"set 'scrape_files' to FALSE to avoid this behavior. \n",
		"NOTE: 'scrape_files' will default to FALSE in a future version of sbtools.")
	}
	
	item
}

multi_file_body <- function(files){
	body = list()
	for(i in 1:length(files)){
		if(!file.exists(files[i])){
			stop('This file does not exist or cannot be accessed: ', files[i])
		}
		
		body[[paste0('file', i)]] = upload_file(files[i])
	}
	names(body) = rep('file', length(body))
	return(body)
}
