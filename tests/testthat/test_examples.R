context("basics")

test_that("basic examples work", {
	skip_on_cran()

	ping <- sb_ping()
	
	expect_true(ping)
	
	if(!ping) stop("sciencebase down, tests won't work")
	 
	item <- "5a83025ce4b00f54eb32956b"
	
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
	
	q <- query_sb_date(start = as.POSIXct("2020-01-01"), 
										 end = as.POSIXct("2021-01-01"), 
										 limit = 1)
		
	expect_equal(length(q), 1)
	
	item_files <- item_list_files(item)
	
	file_name <- "huc8_05010007_example.zip"
	
	expect_equal(item_files$fname, file_name)
	
	f <- item_file_download(item, dest_dir = tempdir(), overwrite_file = TRUE)
	
	expect_equal(basename(f), file_name)
	
	expect_warning(item_file_download(item, dest_dir = tempdir(), overwrite_file = FALSE),
								 "exists, and overwrite is false. Skipping.")

	f <- item_file_download(item, names = file_name, 
													destinations = file.path(tempdir(), file_name), 
													overwrite_file = TRUE)
	
	expect_equal(basename(f), file_name)
	
	expect_error(item_file_download(item, names = c("test", file_name), 
													destinations = file.path(tempdir(), file_name), 
													overwrite_file = TRUE),
							 "Length of names and destinations must be identical")
	
	set_endpoint("dev")
	
	expect_equal(sbtools:::pkg.env$domain, "https://beta.sciencebase.gov/")
	
	expect_equal(sbtools:::pkg.env$graphql_url, 
							 "https://api-beta.staging.sciencebase.gov/graphql")
	
	set_endpoint()
	
	expect_equal(sbtools:::pkg.env$domain, 
							 "https://www.sciencebase.gov/")

	expect_equal(sbtools:::pkg.env$graphql_url, 
							 "https://api.sciencebase.gov/graphql")
	
	expect_equal(sbtools:::pkg.env$auth_server_url,
							 "https://www.sciencebase.gov/auth")
	
	types <- sb_datatypes()
	
	expect_equal(class(types), "character")
	
	search_result <- query_sb_datatype('Map Service')
	
	expect_equal(length(search_result), 20)
	
	search_result <- query_sb_datatype('Map Service', limit = 5)
	
	expect_equal(length(search_result), 5)
	
	get_f <- c('title', 'citation', 'contacts')
	
	fields <- item_get_fields("651346cad34eeedefc139ec9", get_f)

	expect_equal(names(fields), get_f)
	
	qs <- query_sb_spatial(long=c(-104.4, -95.1), lat=c(37.5, 41.0), limit=3)
	
	expect_equal(length(qs), 3)
	
	qs2 <- query_sb_spatial(bb_wkt="POLYGON((-104.4 41.0,-95.1 41.0,-95.1 37.5,-104.4 37.5,-104.4 41.0))", limit = 3)
	
	expect_equal(length(qs2), 3)
	
	conus <- data.frame(
		lat = c(49.078148, 47.575022, 32.914614, 25.000481),
		long = c(-124.722111, -67.996898, -118.270335, -80.125804))
	
	bbox <- sf::st_as_sf(conus, coords = c("long", "lat"), crs = 4326)
	
	qs3 <- query_sb_spatial(
		bbox = bbox, 
		limit = 3)
	
	expect_equal(length(qs3), 3)
	
	expect_equal(length(query_sb_text('Lees Ferry')), 20)
	
	expect_equal(length(item_list_children(item_get('5060b03ae4b00fc20c4f3c8b'))), 20)
	
	expect_equal(length(item_list_children(item_get('5060b03ae4b00fc20c4f3c8b'), limit = 5)), 5)
	
	expect_warning(query_sb("test"))
	
	res <- query_sb(list(parentId = "5474ec49e4b04d7459a7eab2"), limit = 1010)
	
	expect_equal(length(res), 1010)
	
	expect_error(user_id())
	
	expect_error(as.sbitem(matrix(1,2,3)), "No 'as.sbitem' method for class  matrix, array")
	
	expect_equal(as.sbitem(NULL), NULL)
	
	expect_false(is.sbitem(NULL))

	# TODO: bring back session_details()	
	# d <- session_details()
	# 
	# expect_true(is.list(d))
})
	