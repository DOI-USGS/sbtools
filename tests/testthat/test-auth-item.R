test_that("item creation, identifiers, and file upload works", {
	skip_on_cran()
	
	if(is.na(Sys.getenv("sb_user", unset=NA))){
		skip("Authenticated tests skipped due to lack of login info")
	}
	
	initialize_sciencebase_session()
	
	on.exit(sbtools:::clean_session())
	
	#create an item
	item = item_create(title="automated testing item")
	expect_s3_class(item, 'sbitem')
	
	#add item identifier
	rand_key = as.character(floor(runif(1, 0, 10^12)))
	expect_equal(item_update_identifier(item, 'test_scheme', 'test_type', rand_key)$status, 200)
	
	#upload file
	expect_s3_class(item_append_files(item, system.file("examples/data.csv", package="sbtools")), 'sbitem')
	
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
	
	test_file <- system.file("examples/data.csv", package="sbtools")
	
	test_2 <- file.path(tempdir(check = TRUE), "test.csv")
	
	file.copy(test_file, test_2)	
	
	item <- item_append_files(item, test_file)
	
	expect_equal(unname(tools::md5sum(test_file)), item$files[[1]]$checksum$value)
	
	item <- item_append_files(item, test_2)
	
	expect_equal(unname(tools::md5sum(test_2)), item$files[[2]]$checksum$value)
	
	item <- item_rm_files(item)
	
	expect_true(is.null(item$files))
	
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
	
	item <- item_get(item$id)
	
	expect_equal(item$files[[1]]$checksum$value, as.character(tools::md5sum(system.file("examples/data.csv", package="sbtools"))))
	
	unlink(dl_files)
	
	mess <- capture_messages(dl_files <- item_file_download(item, dest_dir = dir_name))
	
	expect_equal(length(mess), 2)
	
	expect_true(file.exists(file.path(dir_name, "data.csv")))
	
	item_publish_cloud(item$id, "data.csv")
	
	item <- item_get(item$id)
	
	expect_true(grepl("prod-is-usgs-sb-prod-publish", item$files[[1]]$publishedS3Ur))
	
	#remove the test item when done
	item_rm(item)
	
	expect_warning(item_get(item), "Sciencebase returned '404' -- item doesn't exist or is secured")
	
	assign("keycloak_expire", NULL, envir = sbtools:::pkg.env)
	assign("keycloak_token", NULL, envir = sbtools:::pkg.env)
	
	warnings <- testthat::capture_warnings(item_list_files("57054bf2e4b0d4e2b756d364"))
	
	expect_true(grepl("fetch cloud URLs", warnings[1]))
})
