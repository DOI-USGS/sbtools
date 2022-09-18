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
#'@examples \donttest{
#'set_endpoint('prod')
#'
#'# getting item from production SB servers
#'item_get('5060b03ae4b00fc20c4f3c8b')
#'
#'set_endpoint('dev')
#'# getting item from beta SB servers
#'item_get('521e4686e4b051c878dc35d0')
#'
#'}
#'
#'@export
set_endpoint = function(endpoint=c("production", "development")){
	
	endpoint = match.arg(endpoint)
	
	if(endpoint=="production"){
		pkg.env$domain = "https://www.sciencebase.gov/"
		pkg.env$graphql_url = "https://api.sciencebase.gov/graphql"
		
	}else if(endpoint=="development"){
		pkg.env$domain   = "https://beta.sciencebase.gov/"
		pkg.env$graphql_url = "https://api-beta.staging.sciencebase.gov/graphql"
	}
	
	pkg.env$url_base = paste0(pkg.env$domain, "catalog/")
	pkg.env$url_items = paste0(pkg.env$url_base, "items/")
	pkg.env$url_item = paste0(pkg.env$url_base, "item/")
	pkg.env$url_upsert = paste0(pkg.env$url_base, "item/upsert")
	pkg.env$url_upload = paste0(pkg.env$url_base, 'file/uploadAndUpsertItem/')
	pkg.env$url_upload_create = paste0(pkg.env$url_base, 'file/uploadAndCreateItem/')
	pkg.env$url_upload_update = paste0(pkg.env$url_base, 'file/uploadAndUpdateItem/')
	pkg.env$url_download = paste0(pkg.env$url_base, 'file/get/')
	pkg.env$url_upload_file = paste0(pkg.env$url_base, 'file/upload')
	pkg.env$url_scrape = paste0(pkg.env$url_base, 'file/scrape')
	pkg.env$url_login = 'https://my.usgs.gov/josso/signon/usernamePasswordLogin.do'
	pkg.env$auth_server_url = paste0(pkg.env$domain, "auth")
	pkg.env$token_url = paste0(pkg.env$domain, 
														 "auth/realms/ScienceBase/protocol/openid-connect/token")
  pkg.env$keycloak_client_id = "sciencebasepy"
  pkg.env$chunk_size_bytes = 104857600  # 104857600 == 100MB
	
}
