#' check if identifier touple already exists on SB
#'
#' returns TRUE if touple already belongs to a sciencebase item, FALSE if not
#'
#' @param scheme the identifier scheme
#' @param type the identifier type
#' @param key the identifier key
#' @param ... Additional parameters are passed on to \code{\link[httr]{GET}}
#' @param session an SB session
#' @return boolean for whether item exists
#'
#' @examples \dontrun{
#' item_exists('mda_streams','ts_doobs','nwis_01018035')
#' item_exists('mda_streams','site_root','nwis_01018035')
#' }
#' @export
item_exists = function(scheme, type, key, ..., session=current_session()){

	if(!session_validate(session)){
		stop('Session state is invalid, please re-authenticate')
	}

	items <- query_item_identifier(scheme=scheme, type=type, key=key, ..., session=session)
	if (length(items) > 0){
		return(TRUE)
	} else {
		return(FALSE)
	}
}
