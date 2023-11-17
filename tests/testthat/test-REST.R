test_that("generic post fails w/o auth", {
	skip_on_cran()
	
	assign("keycloak_expire", NULL, envir = sbtools:::pkg.env)
	assign("keycloak_token", NULL, envir = sbtools:::pkg.env)
	
	# auth passes, and file is there, but auth fails on sciencebase.gov
	files <- system.file('extdata',"This_works_new_extension.zip", package='sbtools')
	expect_warning(item_append_files("54e265a4e4b08de9379b4dfb", files))
})

test_that("generic get w/ and w/o auth", {
	skip_on_cran()
	
	public_item <- '5c081d14e4b0815414d0346c' # public read access
	non_item <-     '4f4e4a62e4b0a92fa7e9cf36' # made-up ID
	#private_item <- '55569325e4b0a92fa7e9cf36' # private to whom? can we test with a session that has access and yet is Travis-runable?
	
	# public access to public items, with or without login
	expect_s3_class(item_get(public_item), 'sbitem')
	
	# 'not found' error for missing items, with or without login
	expect_warning(expect_warning(item_get(non_item)))
	
})
