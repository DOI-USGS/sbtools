
#for storing session state and service URLs
pkg.env <- new.env()
pkg.env$username = ""

.onLoad = function(libname, pkgname){
	set_endpoint()
}

default_timeout <- function(){
	return(10) #seconds
}

check_session <- function(check_logged_in = FALSE) {
	check <- !session_validate()
	
	if(check_logged_in) check <- !(session_validate() & is_logged_in())
	
	if (check) {
		warning('session is not authorized. See ?authenticate_sb')
		FALSE 
	} else {
		TRUE
	}
}

#' Ping ScienceBase to see if it's available
#' 
#' @export
#' @param ... Additional parameters are passed on to \code{\link[httr]{GET}}
#' @return Boolean (TRUE) indicating if a connection to ScienceBase can be established 
#' and if it is responding as expected. FALSE otherwise. 
#' @examples \donttest{
#' #TRUE if all is well and SB can be contacted
#' sb_ping()
#' }
sb_ping <- function(...) {
	
	tryCatch({
		x <- GET(paste0(pkg.env$url_item, 'ping'), 
						 timeout = httr::timeout(default_timeout()),
						 ...)
		res = jsonlite::fromJSON(content(x, "text"))
		if(is(res, 'list') & !is.null(res$result) & res$result == 'OK'){
			return(TRUE)
		}
	}, error=function(e){
		return(FALSE)
	})
	
	return(FALSE)
}

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
		pkg.env$manager_app = "https://sciencebase.usgs.gov/manager/"
		pkg.env$keycloak_client_id = "catalog"
		
	}else if(endpoint=="development"){
		pkg.env$domain   = "https://beta.sciencebase.gov/"
		pkg.env$graphql_url = "https://api-beta.staging.sciencebase.gov/graphql"
		pkg.env$manager_app = "https://beta.staging.sciencebase.gov/manager/"
		pkg.env$keycloak_client_id = "catalog"
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
	if(endpoint == "production") {
		pkg.env$token_url = paste0("https://www.sciencebase.gov/", 
															 "auth/realms/ScienceBase/protocol/openid-connect/token")
	} else if(endpoint == "development") {
		pkg.env$token_url = paste0("https://www.sciencebase.gov/", 
															 "auth/realms/ScienceBase-B/protocol/openid-connect/token")
	}
	
	pkg.env$chunk_size_bytes = 104857600  # 104857600 == 100MB
	
}

#' Get your parent ID
#' 
#' Required for creating items
#' 
#' @export
#' @param ... Additional parameters are passed on to \code{\link[httr]{POST}}
#' @return A single character string, your user id
#' @examples \dontrun{
#' user_id()
#' }
user_id <- function(...) {
	if(!is.null(pkg.env$uid)) return(pkg.env$uid)
	
	if (is.null(current_session())) stop("Please authenticate first. See ?initialize_sciencebase_session", call. = FALSE)
	args <- list(s = "Search", 
							 parentId = "4f4e4772e4b07f02db47e231", 
							 lq = paste0("title.untouched:", pkg.env$username), 
							 format = "json")
	res <- content(query_items(args, ...))
	url <- res$items[[1]]$link$url
	
	out <- basename(url)
	
	pkg.env$uid <- out
	
	out
}

