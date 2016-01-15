


context("test sb functionality requiring authentication")

test_that("authenticate_sb login results in valid session", {
	
	if(is.na(Sys.getenv("sb_user", unset=NA))){
		skip("Authenticated tests skipped due to lack of login info")
	}
	
	
	expect_silent(authenticate_sb(Sys.getenv("sb_user", unset=""), Sys.getenv("sb_pass", unset="")))
	expect_true(session_validate())
	
	expect_equal(nchar(user_id()), 24)
	
})

test_that("item creation, identifiers, and file upload works", {
	
	if(is.na(Sys.getenv("sb_user", unset=NA))){
		skip("Authenticated tests skipped due to lack of login info")
	}
	
	authenticate_sb(Sys.getenv("sb_user", unset=""), Sys.getenv("sb_pass", unset=""))
	
	#create an item
	item = item_create(title="automated testing item")
	expect_is(item, 'sbitem')
	
	#add item identifier
	rand_key = as.character(floor(runif(1, 0, 10^12)))
	expect_equal(item_update_identifier(item, 'test_scheme', 'test_type', rand_key)$status, 200)
	
	#upload file
	expect_is(item_append_files(item, system.file("examples/data.csv", package="sbtools")), 'sbitem')
	
	#download file
	dir_name = tempdir()
	dl_files = item_file_download(item, dest_dir=dir_name)
	
	expect_equal(file.info(dl_files)$size, file.info(system.file("examples/data.csv", package="sbtools"))$size)
	
	#check item identifier (checking issue #74)
	ident = item_get(item)$identifier
	
	expect_equal(ident[[1]]$type, "test_type")
	expect_equal(ident[[1]]$scheme, "test_scheme")
	expect_equal(ident[[1]]$key, rand_key)
	
	#remove the test item when done
	item_rm(item)
	
	expect_error(item_get(item), 'Item not found*.')
	
	expect_silent(session_logout())
})


test_that("Test that surgical item rm", {
	
	if(is.na(Sys.getenv("sb_user", unset=NA))){
		skip("Authenticated tests skipped due to lack of login info")
	}
	
	authenticate_sb(Sys.getenv("sb_user", unset=""), Sys.getenv("sb_pass", unset=""))
	item = item_create(title="file add rm test item")
	
	expect_is(item_append_files(item, system.file("examples/data.csv", package="sbtools")), "sbitem")
	expect_is(item_append_files(item, system.file("extdata/This_works_new_extension.zip", package="sbtools")), 'sbitem')
	
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
	expect_is(item_append_files(item, system.file("examples/data.csv", package="sbtools")), "sbitem")
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
	
	expect_silent(session_logout())
	
})



