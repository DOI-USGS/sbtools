#'@title Set SB endpoint
#'
#'@param endpoint Indicate which SB endpoint 
#' you want to use options: \code{c('production','development')}
#'
#'@description Sets the internal URLS used to either the production
#' or development (beta) SB server. URLS are stored internally to the package
#'
#'@author Luke Winslow
#'
#'@examples
#'
#'\dontrun{
#'set_endpoint('prod')
#'
#'# getting item from production SB servers
#'item_get('54b94448e4b0ed73b67550ea')
#'
#'set_endpoint('dev')
#'# getting item from beta SB servers
#'item_get('54b94448e4b0ed73b67550ea')
#'
#'}
#'
#'@export
set_endpoint = function(endpoint="production"){
	
	endpoint = tolower(endpoint)
	match.arg(endpoint, c('production', 'prod', 'development', 'dev'))
	
	if(endpoint=="production" || endpoint=="prod"){
		pkg.env$url_base = "https://www.sciencebase.gov/catalog/"
		pkg.env$domain = "https://www.sciencebase.gov/"
		
	}else if(endpoint=="development" || endpoint=="dev"){
		pkg.env$url_base = "https://beta.sciencebase.gov/catalog/"
		pkg.env$domain   = "https://beta.sciencebase.gov/"
	}
	
	pkg.env$url_items = paste0(pkg.env$url_base, "items/")
	pkg.env$url_item = paste0(pkg.env$url_base, "item/")
	pkg.env$url_upsert = paste0(pkg.env$url_base, "item/upsert")
	pkg.env$url_upload = paste0(pkg.env$url_base, 'file/uploadAndUpsertItem/')
	pkg.env$url_upload_create = paste0(pkg.env$url_base, 'file/uploadAndCreateItem/')
	pkg.env$url_upload_update = paste0(pkg.env$url_base, 'file/uploadAndUpdateItem/')
	pkg.env$url_download = paste0(pkg.env$url_base, 'file/get/')
	pkg.env$url_login = 'https://my.usgs.gov/josso/signon/usernamePasswordLogin.do'
	
	
}
