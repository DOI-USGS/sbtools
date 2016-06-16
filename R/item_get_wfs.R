#' @title Download and load from SB WFS service
#' 
#' @template manipulate_item
#' 
#' @import stringr
#' 
#' @description 
#' This function attempts to download the spatial layer data attached to the
#' requested SB item. SB exposes discrete spatial objects (points, polygons) as 
#' web services based on the Open Geospatial Consortium, Web Feature Service (WFS)
#' standardized interface. This requires the following libraries not by default
#' installed with sbtools: \code{rgdal}, \code{httr}, and \code{xml2}. 
#' You can install them simply by running \code{install.packages(c("xml2", "httr", "rgdal"))}
#' 
#' @examples 
#' \dontrun{
#' library(sp)
#' layer = item_get_wfs('55e372b9e4b05561fa208212')
#' 
#' plot(layer)
#' 
#' }
#' 
#' @export
item_get_wfs = function(sb_id, ..., session){
	
	if(!requireNamespace("httr") || !requireNamespace("rgdal") || !requireNamespace("xml2")){
		stop('
httr, xml2, and rgdal packages not installed. 
Both are required to interact with WFS services. 
Please run: install.packages(c(\'xml2\',\'httr\', \'rgdal\'))')
	}
	
	wfs_url = item_get_wfs_url(sb_id)
	
	caps = xml2::read_xml(wfs_url)
	
	layer_names = xml2::xml_text(xml2::xml_find_all(caps, '//d1:FeatureType/d1:Name', xml2::xml_ns(caps)))
	
	layer_names = layer_names[!is.na(layer_names) & !layer_names %in% c('sb:boundingBox', 'sb:footprint')]
	
	if(length(layer_names) > 1){
		stop('SB Item WFS has > 1 layer. item_download_sp currently cannot handle more than one layer')
	}
	
	fname = tempfile(fileext = '.shp')
	
	wfs_request = sub('request=GetCapabilities', 'request=GetFeature', wfs_url, ignore.case = TRUE)
	wfs_request = paste0(wfs_request, '&outputformat=shape-zip&format_options=filename:shapedl.zip&typename=', layer_names)
	fname = tempfile(fileext = '.zip')
	dirname = file.path(tempdir(), basename(tempfile()))
	
	httr::GET(wfs_request, httr::write_disk(fname))
	
	unzip(fname, exdir = dirname)
	
	layer_sp = rgdal::readOGR(dirname, strsplit(layer_names, ':')[[1]][2])
	return(layer_sp)
	
}


item_get_wfs_url = function(sb_id){
	
	WFS_url <- match_url_distro(as.sbitem(sb_id), "ScienceBase WFS Service")
	return(WFS_url)
}


match_url_distro = function(item, title_to_match){
	
	#'check that it has a WFS service, throw error if missing
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