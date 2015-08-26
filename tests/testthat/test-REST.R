context("sbtools_POST")

test_that("generic post fails w/o auth", {
	# auth fails locally:
	expect_error(item_append_files("54e265a4e4b08de9379b4dfb", '/foo/bar/baz.zip'),'session is not authorized')
	session <- httr::handle("http://google.com", cookies = FALSE)
	attributes(session) <- c(attributes(session), list(birthdate=Sys.time()))
	
	# auth passes locally, but POST fails due to files not existing:
	expect_error(item_append_files("54e265a4e4b08de9379b4dfb", '/foo/bar/baz.zip', session=session), 'This file does not exist*')
	
	# auth passes, and file is there, but auth fails on sciencebase.gov
	files <- system.file('extdata',"This_works_new_extension.zip", package='sbtools')
	expect_error(item_append_files("54e265a4e4b08de9379b4dfb", files, session=session), 'Error in*')
	
})

context("sbtools_GET")
public_item <- '4f4e4a62e4b07f02db636b68' # public read access
private_item <- '55569325e4b0a92fa7e9cf36'

test_that("generic get w/ and w/o auth", {
	expect_is(item_get(public_item, session = NULL), 'list')
	expect_error(item_get(private_item, session = NULL), 'Item not found')
	
	session <- httr::handle("http://google.com", cookies = FALSE)
	attributes(session) <- c(attributes(session), list(birthdate=Sys.time()))
	expect_is(item_get(public_item, session = session), 'list') # public get with 'valid' session
	
	expect_error(item_get(private_item, session = session))
	set_expiration(as.difftime("00:00:01"))
	Sys.sleep(2)
	expect_error(item_get(public_item, session = session), 'session is not valid')
	
})

