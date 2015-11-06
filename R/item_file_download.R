#' Download files attached to item
#'
#' Function to downlod files attached to an item on SB. Either files can be
#' specified directly using the \code{names} and \code{destinations} parameters,
#' or a \code{dest_dir} can be supplied where all attached files will be written
#' with the names as stored on SB.
#'
#' @param id SB item ID
#' @param ... Additional parameters are passed on to \code{\link[httr]{GET}}
#' @param names String vector list of file names attached to item that you wish
#'   to download.
#' @param destinations String vector list of destinations for requested files.
#'   Must be same length as \code{names}
#' @param dest_dir A directory path for saving files when \code{names} parameter
#'   is omitted
#' @param session Session object from \code{\link{authenticate_sb}}
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
item_file_download = function(id, ..., names, destinations, dest_dir, session=current_session(), overwrite_file = FALSE){

	if(!session_validate(session)){
		stop('Session state is invalid, please re-authenticate')
	}

	#We have two states, missing names and destinations, which means we need a dest_dir
	if(missing(names) && missing(destinations)){
		if(missing(dest_dir)){
			stop('Must have either names & destinations, or dest_dir for all files')
		}

		#populate names and destinations from files that are on SB
		flist = item_list_files(id, ..., session=session)
		if(nrow(flist) < 1){
			stop(id, ':Item has no attached files')
		}
		names = flist$fname
		destinations = file.path(dest_dir, names)

	#or we have names and destinations
	}else{
		if(length(names) != length(destinations)){
			stop('Length of names and destinations must be identical')
		}

		flist = item_list_files(id, ..., session=session)

		if(!all(names %in% flist$fname)){
			stop('Item does not contain all requested files')
		}
	}


	flist = merge(flist, data.frame(fname=names, dest=destinations, stringsAsFactors=FALSE))

	for(i in 1:nrow(flist)){
		sbtools_GET(url=flist[i,]$url, ..., write_disk(flist[i,]$dest, overwrite = overwrite_file), session=session)
	}

	return(path.expand(flist$dest))
}
