#' @title Get list of files attached to SB item
#'
#' @template manipulate_item
#' @param recursive (logical) List files recursively. Default: \code{FALSE}
#'
#' @return 
#' A data.frame with columns fname, size, url, and facet. 
#' If item has no attached files, returns a zero row data.frame.
#'
#' @description 
#' Lists all files attached to a SB item. Files can be downloaded 
#' from ScienceBase using \code{\link{item_file_download}}. 
#' 
#' (advanced) Recursive options lists all files attached to an item and all children items. 
#'
#' NOTE: A sciencebase item can contain so-called "extensions". The 
#' sciencebase item data model refers to the information that describes
#' an extension as a "facet". Some extension facets contain files 
#' (such as with a shapefile). The "facet" attribute of the return from
#' this function will contain the name of the facet the file came from
#' if the file was found in a facet. 
#'
#' @export
#' @examples \dontrun{
#' 
#' #regular files
#' item_list_files("4f4e4b24e4b07f02db6aea14")
#' 
#' # files in facets
#' item_list_files("5f6a285d82ce38aaa244912e")
#' 
#' # list files recursively
#' ## create item
#' id <- item_create(user_id(), title="some title")
#' ## 1. create nested item w/ file
#' file <- system.file("examples", "books.json", package = "sbtools")
#' id2 <- item_create(id, title = "newest-thing")
#' item_upload_create(id2, file)
#' ## 2. create nested item w/ file
#' file <- system.file("examples", "species.json", package = "sbtools")
#' id3 <- item_create(id, title = "a-new-thing")
#' item_upload_create(id3, file)
#' ## 3. create nested item w/ file
#' file <- system.file("examples", "data.csv", package = "sbtools")
#' id4 <- item_create(id, title = "another-thing")
#' item_upload_create(id4, file)
#' item_list_files(id = '56562348e4b071e7ea53e09d', recursive = FALSE) # default
#' item_list_files(id = '56562348e4b071e7ea53e09d', recursive = TRUE)
#' }
item_list_files = function(sb_id, recursive = FALSE, ..., session=current_session()){
	
	session_val(session)
	
	id <- as.sbitem(sb_id)
	item <- item_get(id, session = session)
	
	if (recursive) {
		if (item$hasChildren) {
			
			children <- list(item)
			
			i <- 1
			
			while (i <= length(children)) {

				next_children <- item_list_children(children[[i]], session = session)

				children <- c(children, next_children)
				
				i <- i + 1
				
				message(paste("Checked child item", i - 1, "found", length(next_children), "more children."))
				
			}
			
			files <- unlist(lapply(children, function(x) x$files), recursive = FALSE)
			
		} else {
			files <- list()
			message("no child items found")
		}
	} else {
		files <- item$files
	}
	
	files <- c(files, get_facet_files(item))
	
	lf <- length(files)
	
	out <- data.frame(fname = rep("", lf),
										size = rep(NA_integer_, lf),
										url = rep("", lf),
										facet = rep("", lf))
	
	cloud <- rep(list(list(cuid = "", key = "", 
														 title = "", useForPreview = "")), 
									 lf)
	
	if (length(files) == 0) {
		return(out)
	}
	
	for (i in 1:lf) {
		out[i,'fname'] = files[[i]]$name
		out[i,'size'] = files[[i]]$size
		out[i,'url'] = files[[i]]$url
		if(!is.null(f <- files[[i]]$facet_name))
			out[i, "facet"] = f
		if(!is.null(files[[i]]$cuid)) {
			cloud[[i]]$cuid <- files[[i]]$cuid
			cloud[[i]]$key <- files[[i]]$key
			cloud[[i]]$title <- ifelse(!is.null(files[[i]]$title), 
																				files[[i]]$title, "")
			cloud[[i]]$useForPreview <- ifelse(!is.null(files[[i]]$useForPreview), 
																								 files[[i]]$useForPreview, FALSE)
		}
	}
	
	attr(out, "cloud") <- cloud
	
	return(out)
}

get_facet_files <- function(item) {
	
	unlist(lapply(item$facets, function(x) {
		
		lapply(x$files, 
					 function(y, n) {
					 	
					 	list(name = y$name,
					 			 size = y$size,
					 			 url = y$downloadUri,
					 			 facet_name = x$name)
					 	
					 }, n = x$name)
		
	}), recursive = FALSE)
}

