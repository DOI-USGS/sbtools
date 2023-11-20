#' generic POSTs for sbtools package
#' package wrapped for generic POSTs that test sessions internally and wrap some errors
#' 
#' @param url a base url for the POST
#' @param body a POST body
#' @param ... additional params passed to \code{\link[httr]{POST}}
#' @import httr
#' @export
#' @keywords internal
sbtools_POST <- function(url, body, ...){
	
	supported_types <- c('text/plain', 'application/json')
	if(!check_session(TRUE))
		return(NULL)
	
	r = RETRY(verb = "POST", url=url, ..., httrUserAgent(), accept_json(), body=body, get_token_header(), 
					 timeout = httr::timeout(default_timeout())) 
	r <- handle_errors(r, url, "POST", supported_types)	
	# if (!strsplit(headers(r)[['content-type']], '[;]')[[1]][1] %in% supported_types)
	# 	stop('POST failed to ',url,'. check authorization and/or content')
	
	refresh_token_before_expired()
	return(r)
}

#' generic GETs for sbtools package
#' 
#' package wrapped for generic GETs that test sessions internally and wrap some
#' errors
#' 
#' @param url a base url for the GET
#' @param ... additional params passed to \code{\link[httr]{GET}}, often 
#'   including \code{query}
#' @import httr
#' @export
#' @keywords internal
sbtools_GET <- function(url, ...) {
	supported_types <- c('text/plain','text/csv','text/tab-separated-values','application/json','application/x-gzip', 'application/pdf')
	r <- tryCatch({
		RETRY(verb = "GET", url = url, ..., httrUserAgent(), get_token_header(), 
								timeout = httr::timeout(default_timeout()))
	}, error = function(e) {
		if(grepl("Item not found", e))  {
			warning(e)
			return(list(status = 404))
		}
		
		warning(paste("Error when calling ScienceBase,", 
																		"internet or server down? Original", 
																		"error was:\n", e))
																 return(list(status = 404))
	})
	r <- handle_errors(r, url, "GET", supported_types)
	
	if(!is.null(r)) {
		refresh_token_before_expired()
	}
	
	return(r)
}

#' generic PUTs for sbtools package
#' 
#' package wrapped for generic PUTs that test sessions internally and wrap some 
#' errors
#' 
#' @param url a base url for the PUT
#' @param ... additional params passed to \code{\link[httr]{PUT}}, e.g.,
#'   \code{accept_json()}
#' @param body the PUT body as in \code{\link[httr]{PUT}}
#' @import httr
#' @export
#' @keywords internal
sbtools_PUT <- function(url, body, ...) {
	
	if(!check_session(TRUE))
		return(NULL)
	
	r <- RETRY(verb = "PUT", url = url, ..., httrUserAgent(), body = body, 
						 get_token_header(), timeout = httr::timeout(default_timeout()))
	r <- handle_errors(r, url, "PUT", NULL)
	refresh_token_before_expired()
	return(r)
}

#' generic DELETEs for sbtools package
#' 
#' package wrapped for generic DELETEs that test sessions internally and wrap
#' some errors
#' 
#' @param url a base url for the DELETE
#' @param ... additional params passed to \code{\link[httr]{DELETE}}, e.g., 
#'   \code{accept_json()}
#' @import httr
#' @export
#' @keywords internal
sbtools_DELETE <- function(url, ...) {
	
	if(!check_session(TRUE))
		return(NULL)
	
	uid <- tryCatch(user_id(), 
									error = function(e) "0")
	
	if(uid != 0 && grepl(uid, url)) {
		stop("Deleting a user id is not supported.") #notest
	}
	
	r = RETRY(verb = "DELETE", url = url, ..., httrUserAgent(), accept_json(), 
						 get_token_header(), timeout = httr::timeout(default_timeout()))
	r <- handle_errors(r, url, "DELETE", NULL)
	refresh_token_before_expired()
	return(r)
}

# HEAD fxn
sbtools_HEAD <- function(url, ...) {
	session_val()
	r <- tryCatch(RETRY("HEAD", url = url, ..., httrUserAgent(), get_token_header(),
										 timeout = httr::timeout(default_timeout())),
								error = function(e) {
									warning(paste("Something went wrong with request: \n",
																e))
									return(list(status_code = 400))
								})
	log <- if (r$status_code == 200) TRUE else FALSE
	refresh_token_before_expired()
	return(log)
}

# helpers -------------
handle_errors <- function(x, url, method, types) {
	tryCatch({
	if(is(x, "list")) {
		if(x$status == 404) warning("Could not access sciencebase")
		return(NULL)
	}
		
	if(x$status_code == 404) {
		warning("Sciencebase returned '404' -- item doesn't exist or is secured")
		return(NULL)
	}
		
	if(x$status_code == 403) {
		warning("Sciencebase returned '403 Forbidden'")
		return(NULL)
	}
		
	if(x$status_code == 405) {
		warning("Sciencebase returned '405 not allowed'")
	}
	
	if (!is.null(types)) {
		if (!strsplit(headers(x)[['content-type']], '[;]')[[1]][1] %in% types) {
			message(method, ' failed to ', url, '. check authorization and/or content', call. = FALSE)
			return(NULL)
		}
	}
	
	if ('errors' %in% names(content(x))) {
		
		errors <- as.character(content(x)$errors)
		
		if("mesage" %in% names(errors)) {
		
			message(paste(sapply(errors, function (x) x$message), collapse = "\n"))

		} else {
			
			message(paste(errors, collapse = "\n"))
			
		}
		
		return(NULL)
	}
		
		if(grepl("123456789", rawToChar(x$content))) {
			message("The request was blocked by a firewall. Please contact the sbtools maintainer.")
			message()
		} 
	
	return(x)
	}, error = function(e) {
		
		message(paste("Error when calling sciencebase,", e))
		return(NULL)
		
	})
}

#' @importFrom curl curl_version
#' @importFrom utils packageVersion
#' @import httr
httrUserAgent <- function() {
	versions <- c(
		libcurl = curl::curl_version()$version,
		`r-curl` = as.character(utils::packageVersion("curl")),
		httr = as.character(utils::packageVersion("httr")),
		sbtools = as.character(utils::packageVersion("sbtools"))
	)
	user_agent(paste0(names(versions), "/", versions, collapse = " "))
}
