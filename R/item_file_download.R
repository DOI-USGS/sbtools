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
#' @param dest_dir A directory path for saving files when \code{names} parameter
#'   is omitted
#' @param overwrite_file Boolean indicating if file should be overwritten if it
#'   already exists locally
#'   
#' @return Character vector of full paths to local files 
#'
#' @author Luke Winslow
#'
#' @examples \dontrun{
#'
#' #downloads two files attached to this item
#' item_file_download('548b2b31e4b03f64633662a4', dest_dir=tempdir())
#'
#' #downloads a specific file attached to this item
#' item_file_download('548b2b31e4b03f64633662a4', names='gdp.txt',
#' 		destinations=file.path(tempdir(), 'fname.txt'))
#' }
#' @export
item_file_download = function(sb_id, ..., names, destinations, 
															dest_dir = getwd(), session=current_session(), 
															overwrite_file = FALSE){
	
	sb_id = as.sbitem(sb_id)
	if(!session_validate(session)){
		stop('Session state is invalid, please re-authenticate')
	}

	#We have two states, missing names and destinations, which means we need a dest_dir
	if(missing(names) && missing(destinations)){

		#populate names and destinations from files that are on SB
		flist = item_list_files(sb_id, ..., session=session)
		if(nrow(flist) < 1){
			stop(sb_id$id, ':Item has no attached files')
		}
		names = flist$fname
		destinations = file.path(dest_dir, names)

	#or we have names and destinations
	}else if(!missing(names) & !missing(destinations)){
		if(length(names) != length(destinations)){
			stop('Length of names and destinations must be identical')
		}

		flist = item_list_files(sb_id, ..., session=session)

		if(!all(names %in% flist$fname)){
			stop(sb_id$id, 'Item does not contain all requested files')
		}
	#otherwise in some other error condition
	}else{
		stop('Must have either names & destinations, or dest_dir for all files')
	}


	flist = merge(flist, data.frame(fname=names, dest=destinations, stringsAsFactors=FALSE))

	for(i in 1:nrow(flist)){
		GET(url=flist[i,]$url, ..., 
				write_disk(flist[i,]$dest, overwrite = overwrite_file), 
				handle=session, timeout = httr::timeout(default_timeout()))
	}

	return(path.expand(flist$dest))
}
