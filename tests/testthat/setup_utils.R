try_auth <- function() {
	skipit <- TRUE
	
	user <- try(sbtools:::get_username())
	
	if(!inherits(user, "try-error")) {
		skipit <- !initialize_sciencebase_session()
	}
	
	if(skipit) {
		skip("Authenticated tests skipped due to lack of login info")
	}
}
