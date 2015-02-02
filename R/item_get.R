#'
#'@title Retrieve SB item
#'
#'
#'@param id SB item ID
#'@param session Session object from \code{\link{authenticate_sb}}
#'
#'@return List serialization of complete metadata for SB item
#'
#'@export
item_get = function(id, session=NULL){
	
	if(!session_validate(session)){
		stop('Session state is invalid, please re-authenticate')
	}
	
	r = GET(paste0(pkg.env$url_item, id, '?type=json'), handle=session)
	
	
	if('errors' %in% names(content(r))){
		stop(content(r)$errors$message)
	}
	return(content(r))
	
}
