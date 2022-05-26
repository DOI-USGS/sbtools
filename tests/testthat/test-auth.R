context("test sb functionality requiring authentication")

test_that("not_logged in tests", {
	expect_error(sbtools:::get_access_token(), "no token found, must call athenticate_sb()")
	expect_error(sbtools:::get_refresh_token(), "no token found, must call athenticate_sb()")
	
	expect_error(authenticate_sb(), 'username required for authentication')
	
	if(!interactive())
		expect_error(authenticate_sb("dummy"), 'No password supplied to authenticate_sciencebase in a non-interactive session.')
})

test_that("authenticate_sb login results in valid session and renew works", {
	skip_on_cran()
	
	if(is.na(Sys.getenv("sb_user", unset=NA))){
		skip("Authenticated tests skipped due to lack of login info")
	}

	expect_silent(authenticate_sb(Sys.getenv("sb_user", unset=""), Sys.getenv("sb_pass", unset="")))
	expect_true(session_validate())
	
	expect_equal(nchar(user_id()), 24)
	
	expect_is(sbtools:::get_refresh_token(), "character")
	expect_is(sbtools:::get_access_token(), "character")
	
	expect_is(sbtools:::pkg.env$keycloak_expire, "POSIXct")
	
	old <- sbtools:::pkg.env$keycloak_expire
	
	check <- sbtools:::refresh_token_before_expired(5000)
	
	expect_true(check)

	new <- sbtools:::pkg.env$keycloak_expire		
	
	expect_true(old < new)

})

test_that("item creation, identifiers, and file upload works", {
	skip_on_cran()
	
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
	
	item_rm(item)
	
	item = item_create(title="automated testing item")
	
	expect_message(
	output <- capture_output(cloud_file <- item_upload_cloud(item, system.file("examples/data.csv", package="sbtools"), status = TRUE)),
	"Uploading.*")
	
	expect_true(grepl("100%", output))
	
	expect_equal(cloud_file, "Success")
	
	found <- FALSE
	w <- 1
	
	while(!found) {
		
		files <- item_list_files(item)
		
		if(nrow(files > 1) && grepl("data.csv", attr(files, "cloud")[[1]]$key)) {
			found <- TRUE
		} else {
			Sys.sleep(5)
		}
		
		w <- w + 1
		
		# 12 is arbitrary
		if(w > 12) stop("cloud upload failed?")
		
	}
	
	expect_true(attr(files, "cloud")[[1]]$cuid != "")
	
	expect_true(grepl("https://prod-is-usgs-sb-prod-content.s3.us-west-2.amazonaws.com",
										files$url))
	
	unlink(dl_files)
	
	mess <- capture_messages(dl_files <- item_file_download(item, dest_dir = dir_name))
	
	expect_equal(length(mess), 3)
	
	expect_true(file.exists(file.path(dir_name, "data.csv")))
	
	#remove the test item when done
	item_rm(item)
	
	expect_message(item_get(item), 'Item not found*.')
	
	expect_silent(session_logout())
})


test_that("Test that surgical item rm", {
	skip_on_cran()
	
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
	
	f <- file.path(tempdir(check = TRUE),
								 "metadata6644450227216673613.xml")
	cat("1234", file = f)
	expect_warning(item_replace_files("4f4e4b24e4b07f02db6aea14", 
																	files = f),
								 "Sciencebase returned '403 Forbidden'")
	
	expect_silent(session_logout())
	
	expect_error(item_replace_files("4f4e4b24e4b07f02db6aea14", 
																		files = f),
							 "session is not authorized")
})


