
test_that("initialize_scienbase_session login results in valid session and renew works", {
	skip_on_cran()
	
	try_auth()

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
	
	token <- get_cached_token()
	user <- try(get_username())
	
	if(token == "" | inherits(user, "try-error")) {
		skip("Authenticated tests skipped due to lack of login info")
	}
	
	unlink(sbtools:::token_stache_path(), force = TRUE)
	
	if(!initialize_sciencebase_session(user, token)) {
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
