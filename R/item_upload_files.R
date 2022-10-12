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
  
	if(!is.null(r)) {
		item <- as.sbitem(content(r))
	} else {
		return(NULL)
	}
	return(check_upload(item, files))
	
}


#' @title Upload File to Item With md5sum checks
#' @description Adds a file to an item using the same
#' method as is implemente by the Web User Interface.
#' This method uses multiple web requests and may be
#' more fragile than the standard append functions.
#'
#' @template manipulate_item
#' @inheritParams item_upload_create
#' @param title character titles for appended files must be same length as files.
#' @return An object of class \code{sbitem}
#'
#' @import httr
#'
#' @examples \dontrun{
#' res <- item_create(user_id(), "testing 123")
#' cat("foo bar", file = "foobar.txt")
#' item_append_check(res$id, "foobar.txt")
#' item_rm(res)
#' }
#' @export
item_append_check <- function(sb_id, files, ..., title = rep("", length(files)), 
															session = current_session()) {
	
	item <- as.sbitem(sb_id)
	
	f <- 1
	
	for(file in files) {
		
		checksum <- as.character(tools::md5sum(file))
		
		resp <- sbtools_POST(pkg.env$url_upload_file, 
												 body = list(`files[]` = httr::upload_file(file), md5Checksum = checksum), 
												 encode = "multipart", 
												 session = session)
		
		json <- fromJSON(rawToChar(resp$content), simplifyVector = FALSE)[[1]]
		
		# includes everything from what the web UI does.
		requ <- toJSON(list(
			files = list(list(name = basename(file), 
												contentType = mime::guess_type(file), 
												pathOnDisk = json$fileKey, 
												imageWidth = NULL, 
												imageHeight = NULL, 
												processed = FALSE, 
												stored = FALSE, 
												uploadStatus = "done", 
												uploadProgress = 0, 
												dateUploaded = json$dateUploaded,
												uploadedBy = json$uploadedBy, 
												originalMetadata = FALSE, 
												useForPreview = FALSE,
												title = title[f], 
												checksum = list(value = checksum, 
																				type = "MD5"), 
												s3Object = NULL, 
												viewUrl = sprintf("/catalog/file/get/%s?f=%s", 
																					item$id, json$fileKey)))),
			auto_unbox = TRUE, null = "null")
		
		resp2 <- sbtools_POST(pkg.env$url_scrape, 
													body = list(r=requ), 
													encode = "form", 
													session = session)
		
		json2 <- fromJSON(rawToChar(resp2$content), 
											simplifyVector = FALSE)$files[[1]]
		
		f_url <- paste0(pkg.env$domain, json2$viewUrl)
		
		# Only includes things we want to set for now.
		new_f <- list(name = json2$name, 
									title = json2$title, 
									contentType = json2$contentType, 
									pathOnDisk = json2$pathOnDisk, 
									size = file.size(file), 
									dateUploaded = json2$dateUploaded, 
									uploadedBy = json2$uploadedBy, 
									originalMetadata = json2$originalMetadata, 
									useForPreview = json2$useForPreview, 
									checksum = json2$checksum, 
									url = f_url, 
									downloadUri = f_url)
		
		new_f <- list(name = json2$name, 
									title = json2$title, 
									contentType = json2$contentType, 
									pathOnDisk = json2$pathOnDisk, 
									size = file.size(file), 
									dateUploaded = json2$dateUploaded, 
									uploadedBy = json2$uploadedBy, 
									originalMetadata = json2$originalMetadata, 
									useForPreview = json2$useForPreview, 
									checksum = json2$checksum, 
									viewUrl = json2$viewUrl)

		if(is.null(item$files)) {
			
			item$files <- list(new_f)
		} else {
			
			item$files <- c(item$files, list(new_f))	
		}
		
		res <- sbtools_PUT(paste0(pkg.env$url_item, item$id), 
											 body = toJSON(unclass(item), 
											 							auto_unbox = TRUE, 
											 							null = "null"), 
											 httr::accept_json(), 
											 session = session)
		
		item <- as.sbitem(content(res))
		
		f <- f + 1
	}
	
	item
	
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
	
	f <- which(sapply(item$files, function(x, file) {
		x$name == basename(file)
	}, file = file))
	
	item$files[[f]]$checksum <- as.character(tools::md5sum(file))
	
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
	
	while(!found) {
		
		files <- item_list_files(item)
		
		if(nrow(files > 1) && grepl(f, attr(files, "cloud")[[1]]$key)) {
			found <- TRUE
		} else {
			Sys.sleep(5)
		}
		
		w <- w + 1
		
		message("checking for uploaded file")
		
		if(w > 12) stop("cloud upload failed?")
		
	}
	
	return(as.sbitem(item))
}
