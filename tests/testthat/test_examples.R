context("basics")

test_that("basic examples work", {
	skip_on_cran()
	
	item <- "4f4e4b24e4b07f02db6aea14"
	
	item_files <- item_list_files(item)
	
	file_name <- "metadata6644450227216673613.xml"
	
	expect_equal(item_files$fname, file_name)
	
	f <- item_file_download(item, dest_dir = tempdir(), overwrite_file = TRUE)
	
	expect_equal(basename(f), file_name)
	
	expect_error(item_file_download(item, dest_dir = tempdir(), overwrite_file = FALSE),
							 "Path exists and overwrite is FALSE")
})