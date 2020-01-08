#' @title Download and load from SB WFS service (Deprecated)
#' 
#' @template manipulate_item
#' @param as_sf boolean, return data in sf format
#' 
#' @import stringr
#' @importFrom utils unzip
#' 
#' @description 
#' This function attempts to download the spatial layer data attached to the
#' requested SB item. SB exposes discrete spatial objects (points, polygons) as 
#' web services based on the Open Geospatial Consortium, 
#' \href{https://www.opengeospatial.org/standards/wfs}{Web Feature Service (WFS)}
#' standardized interface. This requires the following libraries not by default
#' installed with sbtools: \code{sf}, \code{httr}, and \code{xml2}. 
#' You can install them simply by running \code{install.packages(c("xml2", "httr", "sf"))}
#' 
#' @export
#' 
item_get_wfs = function(sb_id, as_sf = FALSE, ..., session){
	
	warning("item_get_wfs is going to be removed in a future version of sbtools")
	
	if(!requireNamespace("httr") || !requireNamespace("sf") || !requireNamespace("xml2")){
		stop('
httr, xml2, and sf packages not installed. 
Both are required to interact with WFS services. 
Please run: install.packages(c(\'xml2\',\'httr\', \'sf\'))')
	}
	
	wfs_url = item_get_wfs_url(sb_id)
	
	caps = xml2::read_xml(wfs_url)
	
	layer_names = xml2::xml_text(xml2::xml_find_all(caps, '//d1:FeatureType/d1:Name', xml2::xml_ns(caps)))
	
	layer_names = layer_names[!is.na(layer_names) & !layer_names %in% c('sb:boundingBox', 'sb:footprint')]
	
	if(length(layer_names) > 1){
		stop('SB Item WFS has > 1 layer. item_download_sp currently cannot handle more than one layer')
	}
	
	wfs_request = sub('request=GetCapabilities', 'request=GetFeature', wfs_url, ignore.case = TRUE)
	wfs_request = paste0(wfs_request, '&outputformat=application/json&typename=', layer_names)
	
	dat <- httr::RETRY("GET", wfs_request, times = 3, pause_min = 10, pause_base = 10, pause_cap = 40)
	
	layer = sf::read_sf(rawToChar(dat$content))
	
	if(!as_sf) {
		sf::as_Spatial(layer)
	} else {
		layer
	}
}


item_get_wfs_url = function(sb_id){
	
	WFS_url <- match_url_distro(as.sbitem(sb_id), "ScienceBase WFS Service")
	return(WFS_url)
}


match_url_distro = function(item, title_to_match){
	
	#check that it has a WFS service, throw error if missing
	if('distributionLinks' %in% names(item)){
		if(!any(sapply(item[['distributionLinks']], function(x) x$title) == title_to_match)){
			stop('Item ', item$id, ' has no ', title_to_match, ' available')
		}
	}
	
	num_links <- length(item[["distributionLinks"]])
	
	url = NULL
	for (i in seq_len(num_links)){
		if (item[["distributionLinks"]][[i]][['title']] == title_to_match){
			url <- item[["distributionLinks"]][[i]][['uri']]
			break
		}
	}
	
	return(url)
}