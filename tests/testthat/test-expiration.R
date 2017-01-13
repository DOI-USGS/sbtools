
context("simulate successful request and reset session expiration")

test_that("session age reset works as expected",{
	session <- httr::handle("https://google.com")
	attributes(session) <- c(attributes(session), list(birthdate=Sys.time()))
	set_expiration(as.difftime("00:00:02"))
	Sys.sleep(3)
	expect_true(session_expired(session))
	session <- session_age_reset(session)
	expect_false(session_expired(session))
})

test_that("session age reset ignores NULL session",{
	expect_null(session_age_reset(NULL))
})

test_that("session age works on internal session",{
	sbtools:::set_expiration(as.difftime("00:00:05"))
	session <- httr::handle("https://google.com")
	attributes(session) <- c(attributes(session), list(birthdate=Sys.time()))
	sbtools:::session_set(session)
	sbtools:::set_expiration(as.difftime("00:00:02"))
	Sys.sleep(3)
	expect_true(session_expired())
	session_age_reset()
	expect_false(session_expired())
})
