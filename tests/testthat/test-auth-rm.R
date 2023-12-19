
test_that("Test that surgical item rm", {
	skip_on_cran()
	
	if(is.na(Sys.getenv("sb_user", unset=NA))){
		skip("Authenticated tests skipped due to lack of login info")
	}
	
	initialize_sciencebase_session()
	
	on.exit(sbtools:::clean_session())
	
	item = item_create(title="file add rm test item")
	
	expect_s3_class(item_append_files(item, system.file("examples/data.csv", package="sbtools")), "sbitem")
	expect_s3_class(item_append_files(item, system.file("extdata/This_works_new_extension.zip", package="sbtools")), 'sbitem')
	
	#should be two files
	expect_equal(nrow(item_list_files(item)), 2)
	
	#this should replace files, but not remove them all (as per all switch)
	item_replace_files(item, system.file("extdata/This_works_new_extension.zip", package="sbtools"), all=FALSE)
	
	#should still be just two files
	expect_equal(nrow(item_list_files(item)), 2)
	
	#should only be one file now
	item_replace_files(item, system.file("extdata/This_works_new_extension.zip", package="sbtools"), all=TRUE)
	expect_equal(nrow(item_list_files(item)), 1)
	
	#back to two
	expect_s3_class(item_append_files(item, system.file("examples/data.csv", package="sbtools")), "sbitem")
	expect_equal(nrow(item_list_files(item)), 2)
	
	#should delete the data.csv file
	item_rm_files(item, 'data.csv')
	expect_equal(nrow(item_list_files(item)), 1)
	
	#should do nothing
	item_rm_files(item, 'data.csv')
	expect_equal(nrow(item_list_files(item)), 1)
	
	#should delete all files regardless
	item_rm_files(item)
	
	expect_equal(nrow(item_list_files(item)), 0)
	
	item_rm(item)
	
	f <- file.path(tempdir(check = TRUE),
								 "metadata6644450227216673613.xml")
	cat("1234", file = f)
	expect_warning(item_replace_files("4f4e4b24e4b07f02db6aea14", 
																		files = f),
								 "item doesn't exist or is secured")
	
	assign("keycloak_expire", NULL, envir = sbtools:::pkg.env)
	
	expect_warning(item_replace_files("4f4e4b24e4b07f02db6aea14", 
																		files = f),
								 "item doesn't exist or is secured")
})
