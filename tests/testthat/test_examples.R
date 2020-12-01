context("basics")

test_that("basic examples work", {
	skip_on_cran()
	
	ping <- sb_ping()
	
	expect_true(ping)
	
	if(!ping) stop("sciencebase down, tests won't work")
	 
	item <- "4f4e4b24e4b07f02db6aea14"
	
	expect_true(identifier_exists(item))
	
	expect_false(identifier_exists("1234567890lkjhgfds"))
	
	expect_true(item_exists("https://www.sciencebase.gov/vocab/category/item/identifier", "DOI", "doi:10.5066/P9ZRX948"))
	
	q <- query_sb_doi("doi:10.5066/P9ZRX948")
	
	expect_equal(q[[1]]$id, "5c081d14e4b0815414d0346c")

	i <- item_get_parent(q[[1]]$id)
	
	expect_equal(i$id, "5474ec49e4b04d7459a7eab2")
	
	# test print method
	expect_equal(length(capture.output(i)), 7)

	w <- query_item_in_folder("Water", i$id, limit = 3)
	
	expect_equal(length(w), 3)
	
	q <- query_sb_date(Sys.time(), Sys.time(), limit = 1)
		
	expect_equal(length(q), 1)
	
	item_files <- item_list_files(item)
	
	file_name <- "metadata6644450227216673613.xml"
	
	expect_equal(item_files$fname, file_name)
	
	f <- item_file_download(item, dest_dir = tempdir(), overwrite_file = TRUE)
	
	expect_equal(basename(f), file_name)
	
	expect_error(item_file_download(item, dest_dir = tempdir(), overwrite_file = FALSE),
							 "Path exists and overwrite is FALSE")

	f <- item_file_download(item, names = file_name, 
													destinations = file.path(tempdir(), file_name), 
													overwrite_file = TRUE)
	
	expect_equal(basename(f), file_name)
	
	expect_error(item_file_download(item, names = c("test", file_name), 
													destinations = file.path(tempdir(), file_name), 
													overwrite_file = TRUE),
							 "Length of names and destinations must be identical")
	
	expect_error(item_file_download(item, 
																	destinations = file.path(tempdir(), file_name), 
																	overwrite_file = TRUE),
							 "Must have either names & destinations, or dest_dir for all files")
	
	set_endpoint('dev')
	
	expect_equal(sbtools:::pkg.env$domain, "https://beta.sciencebase.gov/")
	
	set_endpoint()
	
	expect_equal(sbtools:::pkg.env$domain, "https://www.sciencebase.gov/")

	types <- sb_datatypes()
	
	expect_equal(class(types), "character")
	
	search_result <- query_sb_datatype('Map Service')
	
	expect_equal(length(search_result), 20)
	
	search_result <- query_sb_datatype('Map Service', limit = 5)
	
	expect_equal(length(search_result), 5)
	
	get_f <- c('title', 'citation', 'contacts')
	
	fields <- item_get_fields("4f4e4b24e4b07f02db6aea14", get_f)

	expect_equal(names(fields), get_f)
	
	expect_warning(
	wfs_data <- item_get_wfs("58c988bce4b0849ce97b4845"),
	"item_get_wfs is going to be removed in a future version of sbtools")
	
	expect_equal(as.character(class(wfs_data)), "SpatialPointsDataFrame")
	
	suppressWarnings(wfs_data <- item_get_wfs("58c988bce4b0849ce97b4845", as_sf = TRUE))
	
	expect_equal(as.character(class(wfs_data))[1], "sf")
	
	qs <- query_sb_spatial(long=c(-104.4, -95.1), lat=c(37.5, 41.0), limit=3)
	
	expect_equal(length(qs), 3)
	
	qs2 <- query_sb_spatial(bb_wkt="POLYGON((-104.4 41.0,-95.1 41.0,-95.1 37.5,-104.4 37.5,-104.4 41.0))", limit = 3)
	
	expect_equal(length(qs2), 3)
	
	expect_equal(length(query_sb_text('Lees Ferry')), 20)
	
	expect_equal(length(item_list_children(item_get('5060b03ae4b00fc20c4f3c8b'))), 20)
	
	expect_equal(length(item_list_children(item_get('5060b03ae4b00fc20c4f3c8b'), limit = 5)), 5)
	
	expect_error(query_sb("test"), "query_list must be a list of query parameters")
	
	res <- query_sb(list(parentId = "5474ec49e4b04d7459a7eab2"), limit = 1010)
	
	expect_equal(length(res), 1010)
	
	expect_error(user_id())
})
	