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
#' item_rm(res)
#' }
#' @export
item_append_files = function(sb_id, files, ..., scrape_files = TRUE, session=current_session()){
	
	if(length(files) > 50){
		warning('Trying to attach a large number of files to a SB item. SB imposes file limits which may cause this to fail')
	}
	
	item <- as.sbitem(sb_id)
	
	if(is.null(item)) return(NULL)
	
	sums <- lapply(files, tools::md5sum)
	
	params <- paste0("?id=", item$id, paste(paste0("&md5Checksum=", sums), collapse = ""))
	
	if(!scrape_files) {
		params <- paste0(params, "&scrapeFile=false")
	}
	
	r = sbtools_POST(url = paste0(pkg.env$url_upload, params), ...,
									 body = multi_file_body(files), 
									 session = session)
  
	if(!is.null(r)) {
		item <- as.sbitem(content(r))
	} else {
		return(NULL)
	}
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

#' @title Upload File to Item Cloud Storage
#' @description Adds a file to an item in cloud storage
#'
#' @template manipulate_item
#' @inheritParams item_upload_create
#' @param status logical display upload status?
#'
#' @return Success message invisibly. NOTE: cloud processing
#' can take some time so the added file may not appear immediately. 
#' For this reason, a sciencebase item json is NOT returned as is
#' done with other similar functions.
#'
#' @import httr
#' @importFrom mime guess_type
#' @importFrom utils setTxtProgressBar txtProgressBar
#'
#' @examples \dontrun{
#' res <- item_create(user_id(), "testing 123")
#' cat("foo bar", file = "foobar.txt")
#' item_append_files(res$id, "foobar.txt")
#' }
#' @export
item_upload_cloud <- function(sb_id, files, ..., status = TRUE, session=current_session()) {

	try(sb_id <- sb_id$id, silent = TRUE)
	
	for(file in files) {
		if(status)
			message(paste("Uploading:", file))
		
		mimetype <- guess_type(file, unknown = 'application/octet-stream')
		
		cloud_upload(file, mimetype, sb_id)
	}
	
	return(invisible("Success"))
	
}

cloud_upload <- function(file, mimetype, itemid, chunk_size_bytes = pkg.env$chunk_size_bytes,
												 session = current_session(), status = TRUE) {
	
	f_size_bytes <- file.size(file)
	f_chunks <- as.integer(f_size_bytes / chunk_size_bytes) + 1
	
	f_path <- paste(itemid, basename(file), sep = "/")
	
	gql <- httr::handle(url = pkg.env$graphql_url)
	
	session_id = create_multipart_upload_session(
		f_path, mimetype, 
		session_details(session = session)$username, gql)
		
	refresh_token_before_expired()
	
	f <- file(description = file, open = "rb")
	
	on.exit(close(f), add = TRUE)
	
	part_number <- 0
	parts_header <- list()
	
	if(status)
		pb <- txtProgressBar(0, f_chunks, 1, "-", width = 50, style = 3)
	
	while ( length(chunk <- readBin(con = f, what = "raw", 
																	n = chunk_size_bytes)) > 0 ) {
		
		part_number <- part_number + 1
		
		refresh_token_before_expired()
		session_renew()
		
		presignedUrl <- get_presigned_url_for_chunk(f_path, session_id, part_number, gql)
		
		put_chunk <- httr::PUT(presignedUrl, body = chunk)
		
		if(!put_chunk$status_code == 200) stop(paste("Error uploading file. \n",
																					 "status:", put_chunk$status_code,
																					 "content:", rawToChar(put_chunk$content)))
		
		# verify the chunk recieved is the same as held locally
		if(cli::hash_raw_md5(chunk) != gsub("\"", "", put_chunk$headers$ETag)) {
			stop(paste("Error uploading file. \n",
								 "A chunk recieved by the server is different than the origin. \n",
								 "Please try again."))
		}
		
		parts_header[[part_number]] <- list(ETag = put_chunk$headers$ETag,
																				PartNumber = part_number)
		if(status)
			setTxtProgressBar(pb, part_number)
	}
	
	if(status)
		close(pb)
	
	complete <- complete_multipart_upload(f_path, session_id, parts_header, gql)
	
	item <- wait_till_up(itemid, basename(file))
	
	fi <- which(sapply(item$files, function(x, file) {
		x$name == basename(file)
	}, file = file))
	
	item$files[[fi]]$checksum <- list(value = as.character(tools::md5sum(file)), 
																	 type = 'MD5')
	
	res <- sbtools_PUT(paste0(pkg.env$url_item, item$id), 
										 body = toJSON(unclass(item), 
										 							auto_unbox = TRUE, 
										 							null = "null"), 
										 httr::accept_json(), 
										 session = session)
	
	return(as.sbitem(content(res)))
	
}

wait_till_up <- function(item, f) {
	found <- FALSE
	w <- 1
	
	wait_time <- 5
	
	while(!found) {
		
		refresh_token_before_expired()
		session_renew()
		
		files <- item_list_files(item)
		
		if(nrow(files > 1) && grepl(f, attr(files, "cloud")[[1]]$key)) {
			found <- TRUE
		} else {
			Sys.sleep(wait_time)
		}
		
		w <- w + 1
		
		message("checking for uploaded file")
		
		if(w > 20) stop("cloud upload failed?")
	
		wait_time <- wait_time * 2	
	}
	
	return(as.sbitem(item))
}
