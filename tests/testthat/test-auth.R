user <- Sys.getenv("sb_user", unset=NA)
token_text <- Sys.getenv("token_text", unset = NA)

test_that("not_logged in tests", {
	expect_error(sbtools:::get_access_token(), "no token found, must call authenticate_sb()")
	expect_error(sbtools:::get_refresh_token(), "no token found, must call authenticate_sb()")
	
	if(!interactive())
		expect_error(authenticate_sb(), 'username required for authentication')
	
	if(!interactive())
		expect_error(authenticate_sb("dummy"), 'No password supplied to authenticate_sciencebase in a non-interactive session.')
})

test_that("authenticate_sb errors", {
	skip_on_cran()
	
	if(is.na(user)){
		skip("Authenticated tests skipped due to lack of login info")
	}
	
	keyring::key_set_with_value("sciencebase", "test-user@usgs.gov", "broken")
	
	on.exit(keyring::key_delete("sciencebase", "test-user@usgs.gov"))
	
	expect_error(authenticate_sb("test-user@usgs.gov"), 
							 "Sciencebase login failed with stored password?")

})

test_that("authenticate_sb login results in valid session and renew works", {
	skip_on_cran()
	
	if(is.na(Sys.getenv("sb_user", unset=NA))){
		skip("Authenticated tests skipped due to lack of login info")
	}

	expect_silent(authenticate_sb(Sys.getenv("sb_user", unset=""), 
																Sys.getenv("sb_pass", unset="")))
	
	on.exit(sbtools:::clean_session())
	
	expect_true(session_validate())

	expect_equal(nchar(user_id()), 24)
	
	expect_type(sbtools:::get_refresh_token(), "character")
	expect_type(sbtools:::get_access_token(), "character")
	
	expect_s3_class(sbtools:::pkg.env$keycloak_expire, "POSIXct")
	
	old <- sbtools:::pkg.env$keycloak_expire
	
	check <- sbtools:::refresh_token_before_expired(5000)
	
	expect_true(check)

	new <- sbtools:::pkg.env$keycloak_expire		
	
	expect_true(old < new)

})


test_that("authenticate_sb login results in valid session and renew works (new)", {
	skip_on_cran()
	
	if(is.na(Sys.getenv("token_text", unset=NA)) | is.na(Sys.getenv("sb_user"))){
		skip("Authenticated tests skipped due to lack of login info")
	}
	
	if(!initialize_sciencebase_session(Sys.getenv("sb_user"), Sys.getenv("token_text"))) {
		sbtools:::clean_session()
		
		skip("token didn't work, refresh it?")
		
	}
	
	on.exit(sbtools:::clean_session())
	
	expect_true(session_validate())
	
	expect_equal(nchar(user_id()), 24)
	
	expect_type(sbtools:::get_refresh_token(), "character")
	expect_type(sbtools:::get_access_token(), "character")
	
	expect_s3_class(sbtools:::pkg.env$keycloak_expire, "POSIXct")
	
	old <- sbtools:::pkg.env$keycloak_expire
	
	check <- sbtools:::refresh_token_before_expired(5000)
	
	expect_true(check)
	
	new <- sbtools:::pkg.env$keycloak_expire		
	
	expect_true(old < new)
	
})
