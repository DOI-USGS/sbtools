
context("simulate successful request and reset session expiration")

test_that("session age works on internal session",{
	skip_on_cran()
	
	sbtools:::set_expiration(as.difftime("00:00:05"))
	session <- httr::handle("https://google.com")
	attributes(session) <- c(attributes(session), list(birthdate=Sys.time()))
	sbtools:::set_expiration(as.difftime("00:00:02"))
	Sys.sleep(3)
	expect_true(suppressWarnings(session_expired()))
	suppressWarnings(token_refresh())
	expect_false(suppressWarnings(session_expired()))
	
	sbtools:::set_expiration(as.difftime("00:59:00"))
})
