context("sbtools_POST")

test_that("generic post fails w/o auth", {
	skip_on_cran()
	
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

test_that("generic get w/ and w/o auth", {
	skip_on_cran()
	
	context("sbtools_GET")
	public_item <- '5c081d14e4b0815414d0346c' # public read access
	non_item <-     '4f4e4a62e4b0a92fa7e9cf36' # made-up ID
	#private_item <- '55569325e4b0a92fa7e9cf36' # private to whom? can we test with a session that has access and yet is Travis-runable?
	
	session <- httr::handle("https://google.com")
	attributes(session) <- c(attributes(session), list(birthdate=Sys.time()))

	# public access to public items, with or without login
	expect_is(item_get(public_item, session = NULL), 'sbitem')
	expect_is(item_get(public_item, session = session), 'sbitem')
	
	# 'not found' error for missing items, with or without login
	expect_error(item_get(non_item, session = NULL), 'Item not found')
	expect_error(item_get(non_item, session = session), 'Item not found')
	
})

test_that("REST_helpers tests", {
	skip_on_cran()
	
	item <- sbtools_GET("https://www.sciencebase.gov/catalog/item/5c4f4a04e4b0708288f78e07")

	expect_error(sbtools_GET("https://www.sciencebase.gov/catalog/item/a04e4b0708288f78e07"), 
							 "Invalid Item ID")
	
	expect_error(sbtools_GET("https://www.sciencebase.gov/catalog/item/a04e4b0708288f78e07", session = list(session = "borked")),
							 "Session is not valid.")
	
	session <- httr::handle("https://google.com")
	attributes(session) <- c(attributes(session), list(birthdate=Sys.time()))
	
	expect_warning(sbtools_GET("https://www.sciencebase.go/catalog/item/5c4f4a04e4b0708288f78e07", session = NULL))
	
	put_test <- sbtools_PUT("https://www.sciencebase.gov/catalog/item/5c4f4a04e4b0708288f78e07", "test", session = session)
	
	expect_equal(put_test$status_code, 405)
	
	delete_test <- sbtools_DELETE("https://www.sciencebase.gov/catalog/item/5c4f4a04e4b0708288f78e07", session = session)

	expect_equal(delete_test$status_code, 405)
	
	expect_error(post_test <- sbtools_POST("https://www.sciencebase.gov/catalog/item/5c4f4a04e4b0708288f78e07", "test", session = session))
})
