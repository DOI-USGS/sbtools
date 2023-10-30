#' Download files attached to item
#'
#' Function to downlod files attached to an item on SB. Either files can be
#' specified directly using the \code{names} and \code{destinations} parameters,
#' or a \code{dest_dir} can be supplied where all attached files will be written
#' with the names as stored on SB.
#'
#' @template manipulate_item
#' @param names String vector list of file names attached to item that you wish
#'   to download.
#' @param destinations String vector list of destinations for requested files.
#'   Must be same length as \code{names}
#' @param dest_dir A directory path for saving files when \code{names} destinations 
#' parameter is not specified.
#' @param overwrite_file Boolean indicating if file should be overwritten if it
#'   already exists locally
#'   
#' @return Character vector of full paths to local files 
#'
#' @examples \dontrun{
#'
#' #downloads all files attached to this item
#' item_file_download('627f1572d34e3bef0c9a30d8', dest_dir=tempdir())
#'
#' #downloads a specific file attached to this item
#' item_file_download('627f1572d34e3bef0c9a30d8', names='example.txt',
#' 		destinations=file.path(tempdir(), 'out.txt'))
#' 		
#' }
#' @export
item_file_download = function(sb_id, ..., names, destinations, 
															dest_dir = getwd(), 
															overwrite_file = FALSE){
	
	sb_id = as.sbitem(sb_id)
	
	if(!session_validate())
		stop('Session state is invalid, please re-authenticate')

	flist <- item_list_files(sb_id, fetch_cloud_urls = FALSE, ...)
	
	if(nrow(flist) < 1)
		stop(sb_id$id, ':Item has no attached files')
	
	if(missing(names)) {
		
		names <- flist$fname

	} else {
		
		if(!missing(destinations) & length(names) != length(destinations))
				stop('Length of names and destinations must be identical')

	}

	if(!all(names %in% flist$fname)) stop(sb_id$id, 'Item does not contain all requested files')
	
	if(!exists("destinations") | missing(destinations)) {
		destinations <- file.path(dest_dir, names)
	}
	
	flist <- merge(cbind(flist, do.call(rbind.data.frame, attr(flist, "cloud"))), 
								 data.frame(fname=names, dest=destinations))

	for(i in seq_len(nrow(flist))) {
		tryCatch({
			
			if(flist[i, ]$cuid != "") {
				
				if(!exists("gql")) gql <- httr::handle(url = pkg.env$graphql_url)
				
				message("retrieving S3 URL")
				
				flist[i, ]$url <- get_cloud_download_url(flist[i, c("cuid", "key", "title", "useForPreview")], 
																								 gql)[[1]]$getS3DownloadUrl$downloadUri[1]
				
			}
			
			message(paste("downloading file", flist[i,]$dest))
			
			RETRY("GET", url=flist[i,]$url, ..., 
						write_disk(flist[i,]$dest, overwrite = overwrite_file), 
						get_token_header(), timeout = httr::timeout(default_timeout()),
						httr::progress())
			
		}, error = function(e) {
			if(file.exists(flist[i,]$dest)) {
				warning(paste(basename(flist[i,]$dest), "exists, and overwrite is false. Skipping."))
			} else {
				stop(paste("Error downloading", flist[i,]$dest, "Original error: \n", e))
			}
		})
	}

	return(path.expand(flist$dest))
}
