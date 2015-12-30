context("authentication")

test_that("session validation works appropriately", {
  expect_false(session_validate(5))
	expect_null(current_session())
	session <- httr::handle("http://google.com")
	attributes(session) <- c(attributes(session), list(birthdate=Sys.time()))
	expect_true(session_validate(session))
	expect_is(session_check_reauth(session), 'handle')
	set_expiration(as.difftime("00:00:01"))
	Sys.sleep(2)
	expect_false(session_validate(session))
	
})


