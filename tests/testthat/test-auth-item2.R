test_that("items", {
	skip_on_cran()
	
	if(is.na(Sys.getenv("sb_user", unset=NA))){
		skip("Authenticated tests skipped due to lack of login info")
	}
	
	initialize_sciencebase_session()
	
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
	
	items <- items_upsert(list(item,item2), 
												title = c(name3, name4))
	
	expect_equal(items[[1]]$title, name3)
	expect_equal(items[[2]]$title, name4)
	
	items <- items_upsert(items, title = c(name3, name4), info = list(list(contacts = list(list(name = "Suzy"))),
																																		list(contacts = list(list(name = "Dave")))))
	
	expect_equal(items[[1]]$contacts[[1]]$name, "Suzy")
	expect_equal(items[[2]]$contacts[[1]]$name, "Dave")
	
	done <- sapply(items, item_rm)
	
	# Pass an object of class sbitem
	folder <- folder_create(user_id(), "test-folder")
	items <- items_create(folder$id, title = c("name-1", "name-2"))
	
	expect_equal(folder$title, "test-folder")	
	expect_equal(items[[1]]$parentId, folder$id)
	
	item <- item_move(items[[1]], user_id())
	
	expect_equal(item$parentId, user_id())
	
	done <- sapply(items, item_rm)
	
	# You'll need a parent id for a folder/item
	## here, using your highest level parent folder
	file <- system.file("examples", "books.json", package = "sbtools")
	item <- item_upload_create(user_id(), file)

	expect_equal(item$files[[1]]$name, "books.json")
	
	item <- item_rename_files(item, "books.json", 'book.json')
	
	expect_equal(item$files[[1]]$name, 'book.json')
	
	item_rm(item)
	
	res <- items_create(user_id(), title = c('name_1', 'name_2'))
	
	out <- items_update(res, info = list( list(title = 'name1'), list(title = 'name2') ) )
	expect_equal(vapply(res, "[[", "", "title"), c('name_1', 'name_2'))
	expect_equal(vapply(out, "[[", "", "title"), c('name1', 'name2'))
	
	done <- sapply(out, item_rm)
})
