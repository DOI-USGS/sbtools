#' @title Download and load from SB WFS service
#' 
#' @template manipulate_item
#' 
#' @import stringr
#' 
#' @description 
#' This function attempts to download the spatial layer data attached to the
#' requested SB item. SB exposes discrete spatial objects (points, polygons) as 
#' WFS web services. This requires the GDAL libraries \code{rgdal} and \code{gdalUtils} 
#' to be installed, which may require the external GDAL libraries as well.
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
	
	if(!requireNamespace("gdalUtils") || !requireNamespace("rgdal")){
		stop('gdalUtils and rgdal packages not installed. 
					Both are required to interact with WFS services. 
				 	Please run: install.packages(c(\'rgdal\',\'gdalUtils\'))')
	}
	
	wfs_url = item_get_wfs_url(sb_id)
	
	layer_info = gdalUtils::ogrinfo(paste0('WFS:', wfs_url))
	
	#from this layer info, we need to extract layer name
	layer_raw = str_match_all(layer_info, '[[:digit:]]+: (sb:[[:alpha:]_]+) \\([Polygon|Multi Polygon|Points|Point]+\\)')
	
	layer_names = sapply(layer_raw, function(x)x[2])
	
	#drop the bounding box and footprint layers, which we don't want
	layer_names = layer_names[!is.na(layer_names) & !layer_names %in% c('sb:boundingBox', 'sb:footprint')]
	if(length(layer_names) > 1){
		stop('SB Item WFS has > 1 layer. item_download_sp currently cannot handle more than one layer')
	}
	
	fname = tempfile(fileext = '.shp')
	
	gdalUtils::ogr2ogr(wfs_url, fname, layer_names)
	
	layer_sp <- rgdal::readOGR(fname, substr(basename(fname), 1, nchar(basename(fname))-4), stringsAsFactors=FALSE)
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