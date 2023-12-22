user <- Sys.getenv("sb_user", unset=NA)

test_that("not_logged in tests", {
	skip("retired pattern")
	expect_error(sbtools:::get_access_token(), "no token found, must call authenticate_sb()")
	expect_error(sbtools:::get_refresh_token(), "no token found, must call authenticate_sb()")
	
	if(!interactive())
		expect_error(authenticate_sb(), 'username required for authentication')
	
	if(!interactive())
		expect_error(authenticate_sb("dummy"), 'No password supplied to authenticate_sciencebase in a non-interactive session.')
})

test_that("authenticate_sb errors", {
	skip("retired pattern")
	
	if(is.na(user)){
		skip("Authenticated tests skipped due to lack of login info")
	}
	
	keyring::key_set_with_value("sciencebase", "test-user@usgs.gov", "broken")
	
	on.exit(keyring::key_delete("sciencebase", "test-user@usgs.gov"))
	
	expect_error(authenticate_sb("test-user@usgs.gov"), 
							 "Sciencebase login failed with stored password?")

})

test_that("initialize_scienbase_session login results in valid session and renew works", {
	skip_on_cran()
	
	if(is.na(Sys.getenv("sb_user", unset=NA))){
		skip("Authenticated tests skipped due to lack of login info")
	}

	initialize_sciencebase_session()
	
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


test_that("login results in valid session and renew works (new)", {
	skip_on_cran()
	
	sbtools:::clean_session()
	
	token <- sbtools:::grab_token()
	
	if(token == "" | is.na(Sys.getenv("sb_user"))){
		skip("Authenticated tests skipped due to lack of login info")
	}
	
	unlink(sbtools:::token_stache_path(), force = TRUE)
	
	if(!initialize_sciencebase_session(Sys.getenv("sb_user"), token)) {
		sbtools:::clean_session()
		
		skip("token didn't work, refresh it?")
		
	}
	
	on.exit(sbtools:::clean_session())
	
	expect_true(file.exists(sbtools:::token_stache_path()))
	
	expect_true(initialize_sciencebase_session())
	
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
