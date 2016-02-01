#' @title Query SB based on spatial extent
#' 
#' 
#' @param bbox An object of class \code{\link{sp}}. The bounding box of the object is used for the query.
#' @param bb_min Alternate to bbox param. A vector length 2 defining the c(long, lat) of lower left corner of the bounding box.
#' @param bb_max Alternate to bbox param. A vector length 2 defining the c(long, lat) of upper right corner of the bounding box.
#' @param bb_wkt A character string using the Well Known Text (WKT) standard for defining spatial data. Must be a POLYGON WKT object. 
#' 
#' @description 
#' Queries ScienceBase based on a spatial bounding box. Accepts either an object of class \code{\link{sp}} 
#' (uses the spatial object's bounding box) or two long/lat coordinates defining the lower left and upper 
#' right corners. 
#' 
#' 
#' @examples
#' \dontrun{
#' #specify the corners of the bounding box
#' query_sb_spatial(bb_min=c(-104.4, 37.5), bb_max=c(-95.1, 41.0))
#' 
#' #use a pre-formatted WKT polygon to grab data
#' query_sb_spatial(bb_wkt="POLYGON((-104.4 41.0,-95.1 41.0,-95.1 37.5,-104.4 37.5,-104.4 41.0))")
#' 
#' ###Use the bounding box of an sp object 
#' #grab an sp object from a pre-determined ScienceBase Item
#' layer = item_get_wfs('55e372b9e4b05561fa208212')
#' 
#' #get items in that BB
#' query_sb_spatial(layer)
#' }
#' 
#' @export
query_sb_spatial = function(bbox, bb_min, bb_max, bb_wkt, ..., limit=20, session=current_session()){
	
	if(!missing(bbox)){
		if(requireNamespace("sp")){
			#project first into longlat projection so bounding box is in degrees
			bb = sp::spTransform(bbox, sp::CRS("+proj=longlat +datum=WGS84"))@bbox
			bb_wkt = make_wkt(bb)
		}else{
			stop('sp package required to use bbox parameter. Please: install.packages("sp")')
		}
	}else if(!missing(bb_min) & !missing(bb_max)){
		bb = matrix(nrow=2, ncol=2)
		bb[,1] = bb_min
		bb[,2] = bb_max
		
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
