
test_that("recursive file list works right", {
	skip_on_cran()
	
	if(!sb_ping()) skip("sciencebase unavailable, tests won't work")
	
	sb_id <- "59cadcffe4b017cf314095a7"
	
	item <- item_get(sb_id)
	
	files <- item_list_files(item, recursive = TRUE)
	
	expect_true(nrow(files) > 100)
})

test_that("cloud facets", {
	
	skip_on_cran()
	
	if(!sb_ping()) skip("sciencebase unavailable, tests won't work")
	
	sbid <- "69330512d4be02765ea81805"
	
	item <- item_get(sbid)
	
	files <- item_list_files(sbid)
	
	# verify that a cloud shapefile is in here
	expect_true(any(sapply(attr(files, "cloud"), \(x) x$cuid != "" & grepl("shp", x$key))))
	
})