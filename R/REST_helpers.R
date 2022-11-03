#' generic POSTs for sbtools package
#' package wrapped for generic POSTs that test sessions internally and wrap some errors
#' 
#' @param url a base url for the POST
#' @param body a POST body
#' @param session a sbtools session object
#' @param ... additional params passed to \code{\link[httr]{POST}}
#' @import httr
#' @export
#' @keywords internal
sbtools_POST <- function(url, body, ..., session){
	
	supported_types <- c('text/plain', 'application/json')
	if(!check_session(session))
		return(NULL)
	
	r = RETRY(verb = "POST", url=url, ..., httrUserAgent(), accept_json(), body=body, handle=session, 
					 timeout = httr::timeout(default_timeout())) 
	r <- handle_errors(r, url, "POST", supported_types)	
	# if (!strsplit(headers(r)[['content-type']], '[;]')[[1]][1] %in% supported_types)
	# 	stop('POST failed to ',url,'. check authorization and/or content')
	
	session_age_reset()
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
#' @param session a sbtools session object
#' @import httr
#' @export
#' @keywords internal
sbtools_GET <- function(url, ..., session = NULL) {
	supported_types <- c('text/plain','text/csv','text/tab-separated-values','application/json','application/x-gzip', 'application/pdf')
	r <- tryCatch({
		RETRY(verb = "GET", url = url, ..., httrUserAgent(), handle = session, 
								timeout = httr::timeout(default_timeout()))
	}, error = function(e) {
		if(grepl("Item not found", e))  {
			warning(e)
			return(list(status = 404))
		}
		
		if(!is.null(session) && !(inherits(session, "curl_handle") | inherits(session, "handle"))) stop("Session is not valid.")
		
		warning(paste("Error when calling ScienceBase,", 
																		"internet or server down? Original", 
																		"error was:\n", e))
																 return(list(status = 404))
	})
	r <- handle_errors(r, url, "GET", supported_types)
	session_age_reset()
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
#' @param session a sbtools session object
#' @import httr
#' @export
#' @keywords internal
sbtools_PUT <- function(url, body, ..., session) {
	
	if(!check_session(session))
		return(NULL)
	
	r <- RETRY(verb = "PUT", url = url, ..., httrUserAgent(), body = body, 
						 handle = session, timeout = httr::timeout(default_timeout()))
	r <- handle_errors(r, url, "PUT", NULL)
	session_age_reset()
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
#' @param session a sbtools session object
#' @import httr
#' @export
#' @keywords internal
sbtools_DELETE <- function(url, ..., session) {
	
	if(!check_session(session))
		return(NULL)
	
	uid <- tryCatch(user_id(session = session), 
									error = function(e) "0")
	
	if(uid != 0 && grepl(uid, url)) {
		stop("Deleting a user id is not supported.") #notest
	}
	
	r = RETRY(verb = "DELETE", url = url, ..., httrUserAgent(), accept_json(), 
						 handle = session, timeout = httr::timeout(default_timeout()))
	r <- handle_errors(r, url, "DELETE", NULL)
	session_age_reset()
	return(r)
}

# HEAD fxn
sbtools_HEAD <- function(url, ..., session) {
	session_val(session)
	r <- tryCatch(RETRY("HEAD", url = url, ..., httrUserAgent(), handle = session,
										 timeout = httr::timeout(default_timeout())),
								error = function(e) {
									warning(paste("Something went wrong with request: \n",
																e))
									return(list(status_code = 400))
								})
	log <- if (r$status_code == 200) TRUE else FALSE
	session_age_reset()
	return(log)
}

# helpers -------------
handle_errors <- function(x, url, method, types) {
	tryCatch({
	if(is(x, "list")) {
		if(x$status == 404) warning("Could not access sciencebase")
		return(NULL)
	}
		
	if(x$status_code == 403) {
		warning("Sciencebase returned '403 Forbidden'")
		return(NULL)
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
		
			message(paste(sapply(errors, function (x) x$message), collapse = "\n"), call. = FALSE)

		} else {
			
			message(paste(errors, collapse = "\n"), call. = FALSE)
			
		}
		
		return(NULL)
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
