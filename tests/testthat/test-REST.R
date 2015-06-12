context("sbtools_POST")

test_that("generic post fails w/o auth", {
	
	# auth fails locally:
	expect_error(item_append_files("54e265a4e4b08de9379b4dfb", '/foo/bar/baz.zip'),'session is not authorized')
	session <- httr::handle("http://google.com", cookies = FALSE)
	attributes(session) <- c(attributes(session), list(birthdate=Sys.time()))
	
	# auth passes locally, but POST fails due to files not existing:
	expect_error(item_append_files("54e265a4e4b08de9379b4dfb", '/foo/bar/baz.zip', session), 'This file does not exist')
	
	# auth passes, and file is there, but auth fails on sciencebase.gov
	files <- system.file('extdata',"This_works_new_extension.zip", package='sbtools')
	expect_error(item_append_files("54e265a4e4b08de9379b4dfb", files, session), 'POST failed to')
	
})


