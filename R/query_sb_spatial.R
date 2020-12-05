#' @title Query SB based on spatial extent
#' 
#' @inheritParams query_sb
#' @param bbox An sp spatial data object. The bounding box of the object is used for the query.
#' @param long A vector of longitude values that will define the boundaries of a bounding box. Min and Max of supplied longitudes are used. (alternate option to bbox).
#' @param lat A vector of latitude values that will define the boundaries of a bounding box. Min and Max of supplied latitude are used. (alternate option to bbox).
#' @param bb_wkt A character string using the Well Known Text (WKT) standard for defining spatial data. Must be a POLYGON WKT object. 
#' 
#' @description 
#' Queries ScienceBase based on a spatial bounding box. Accepts either an sp spatial data object
#' (uses the spatial object's bounding box) or long/lat coordinates defining the bounding box limits. 
#' 
#' 
#' @examples
#' 
#' #specify the latitude and longitude points to define the bounding box range. 
#' # This is simply bottom left and top right points
#' query_sb_spatial(long=c(-104.4, -95.1), lat=c(37.5, 41.0), limit=3)
#' 
#' #use a pre-formatted WKT polygon to grab data
#' query_sb_spatial(bb_wkt="POLYGON((-104.4 41.0,-95.1 41.0,-95.1 37.5,-104.4 37.5,-104.4 41.0))", 
#' 	                limit=3)
#' 
#' @export
#' 
query_sb_spatial = function(bbox, long, lat, bb_wkt, ..., limit=20, session=current_session()){
	
	if(!missing(bbox)){
		message("bbox must be from an sp object in WGS84 Lon/Lat")
		bb_wkt = make_wkt(bbox@bbox)
	}else if(!missing(long) & !missing(lat)){
		bb = matrix(nrow=2, ncol=2)
		bb[1,1] = min(long)
		bb[2,1] = min(lat)
		bb[1,2] = max(long)
		bb[2,2] = max(lat)
		
		bb_wkt = make_wkt(bb)
	}else if(!missing(bb_wkt)){
		#We'll just assume this is fine then and will be passed to query_sb
		
	}else{
		stop('Must supply either bbox sp object or bb_min and bb_max vectors')
	}
  
	query_sb(list(spatialQuery = bb_wkt), limit=limit)
}

make_wkt = function(bb){
	paste0('POLYGON((', bb[1,1], ' ', bb[2,2], ',',
				 bb[1,2], ' ', bb[2,2], ',', 
				 bb[1,2], ' ', bb[2,1], ',',
				 bb[1,1], ' ', bb[2,1], ',',
				 bb[1,1], ' ', bb[2,2], '))')
}
