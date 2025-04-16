try_auth <- function() {
	skipit <- TRUE
	
	user <- try(get_username())
	
	if(!inherits(user, "try-error")) {
		skipit <- !initialize_sciencebase_session()
	}
	
	if(skipit) {
		skip("Authenticated tests skipped due to lack of login info")
	}
}
