context("recursive")
test_that("recursive file list works right", {
	skip_on_cran()
	
	if(!sb_ping()) stop("sciencebase unavailable, tests won't work")
	
	sb_id <- "59cadcffe4b017cf314095a7"
	
	item <- item_get(sb_id)
	
	files <- item_list_files(item, recursive = TRUE)
	
	expect_true(nrow(files) > 100)
})