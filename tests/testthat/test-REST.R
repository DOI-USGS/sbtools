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

	# public access to public items, with or without login
	expect_s3_class(item_get(public_item), 'sbitem')
	
})
