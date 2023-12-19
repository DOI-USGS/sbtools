if(Sys.getenv("sbtools_test_env") != "") {
	try(sbtools::set_endpoint(Sys.getenv("sbtools_test_env")))
}
