


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




