
context("simulate successful request and reset session expiration")

test_that("session age reset works as expected",{
	skip_on_cran()
	
	session <- httr::handle("https://google.com")
	attributes(session) <- c(attributes(session), list(birthdate=Sys.time()))
	set_expiration(as.difftime("00:00:02"))
	Sys.sleep(3)
	expect_true(session_expired(session))
	session <- session_age_reset(session)
	expect_false(session_expired(session))
})

test_that("session age reset ignores NULL session",{
	skip_on_cran()
	
	expect_null(session_age_reset(NULL))
})

test_that("session age works on internal session",{
	skip_on_cran()
	
	sbtools:::set_expiration(as.difftime("00:00:05"))
	session <- httr::handle("https://google.com")
	attributes(session) <- c(attributes(session), list(birthdate=Sys.time()))
	sbtools:::session_set(session)
	sbtools:::set_expiration(as.difftime("00:00:02"))
	Sys.sleep(3)
	expect_true(suppressWarnings(session_expired()))
	suppressWarnings(session_age_reset())
	expect_false(suppressWarnings(session_expired()))
	
	sbtools:::session_set(NULL)
	sbtools:::set_expiration(as.difftime("00:59:00"))
})
