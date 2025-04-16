test_that("items", {
	skip_on_cran()
	
	try_auth()
	
	on.exit(sbtools:::clean_session())
	
	aname <- function() paste0(sample(letters, size = 5, replace = TRUE), collapse = "")
	
	name1 <- aname()
	
	item <- item_upsert(title = name1)
	
	expect_equal(item$title, name1)
	
	item <- suppressMessages(item_upsert(item, info = list(contacts = list(list(name = "Suzy")))))
	
	expect_equal(item$contacts[[1]]$name, "Suzy")
	
	name2 <- aname()
	
	item2 <- item_create(title = name2)
	
	expect_equal(item2$title, name2)
	
	name3 <- aname()
	name4 <- aname()
	
	item_rm(item)
	item_rm(item2)
	
	# Pass an object of class sbitem
	folder <- folder_create(user_id(), "test-folder")
	
	expect_equal(folder$title, "test-folder")	
	
	item_rm(folder)
	
	testthat::skip("upload_create not working")
	# You'll need a parent id for a folder/item
	## here, using your highest level parent folder
	file <- system.file("examples", "books.json", package = "sbtools")
	item <- item_upload_create(user_id(), file)

	expect_equal(item$files[[1]]$name, "books.json")
	
	item <- item_rename_files(item, "books.json", 'book.json')
	
	expect_equal(item$files[[1]]$name, 'book.json')
	
	item_rm(item)
	
})
