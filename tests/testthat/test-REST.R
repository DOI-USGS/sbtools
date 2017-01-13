context("sbtools_POST")

test_that("generic post fails w/o auth", {
	# auth fails locally:
	expect_error(item_append_files("54e265a4e4b08de9379b4dfb", '/foo/bar/baz.zip'),'Item not found for')
	session <- httr::handle("https://google.com")
	attributes(session) <- c(attributes(session), list(birthdate=Sys.time()))
	
	# auth passes locally, but POST fails due to files not existing:
	expect_error(item_append_files("54e265a4e4b08de9379b4dfb", '/foo/bar/baz.zip', session=session), 'Item not found for')
	
	# auth passes, and file is there, but auth fails on sciencebase.gov
	files <- system.file('extdata',"This_works_new_extension.zip", package='sbtools')
	expect_error(item_append_files("54e265a4e4b08de9379b4dfb", files, session=session), 
							 'Item not found for')
})

context("sbtools_GET")
public_item <- '4f4e4a62e4b07f02db636b68' # public read access
non_item <-     '4f4e4a62e4b0a92fa7e9cf36' # made-up ID
#private_item <- '55569325e4b0a92fa7e9cf36' # private to whom? can we test with a session that has access and yet is Travis-runable?

test_that("generic get w/ and w/o auth", {

	session <- httr::handle("https://google.com")
	attributes(session) <- c(attributes(session), list(birthdate=Sys.time()))

	# public access to public items, with or without login
	expect_is(item_get(public_item, session = NULL), 'sbitem')
	expect_is(item_get(public_item, session = session), 'sbitem')
	
	# 'not found' error for missing items, with or without login
	expect_error(item_get(non_item, session = NULL), 'Item not found')
	expect_error(item_get(non_item, session = session), 'Item not found')
	
})

