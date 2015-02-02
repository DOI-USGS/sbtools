#'@title check if identifier touple already exists on SB
#'@description returns TRUE if touple already belongs to a sciencebase item, FALSE if not
#'
#'@param scheme
#'@param type
#'@param key
#'@param session
#'@return boolean for whether item exists
#'
#'@examples
#'\dontrun{
#'item_exists('mda_streams','ts_doobs','nwis_01018035')
#'item_exists('mda_streams','site_root','nwis_01018035')
#'}
#'@export
item_exists = function(scheme, type, key, session = NULL){
	
	if(!session_validate(session)){
		stop('Session state is invalid, please re-authenticate')
	}
	
	items <- query_item_identifier(scheme, type, key, session)
	if (nrow(items) > 0){
		return(TRUE)
	} else {
		return(FALSE)
	}
}
