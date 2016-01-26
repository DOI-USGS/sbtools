#' @title Query SB based on spatial extent
#' 
#' 
#' @param bbox An object of class \code{\link{sp}}. The bounding box of the object is used for the query.
#' @param bb_min Alternate to bbox param. A vector length 2 defining the c(long, lat) of lower left corner of the bounding box.
#' @param bb_max Alternate to bbox param. A vector length 2 defining the c(long, lat) of upper right corner of the bounding box.
#' 
#' @description 
#' Queries ScienceBase based on a spatial bounding box. Accepts either an object of class \code{\link{sp}} 
#' (uses the spatial object's bounding box) or two long/lat coordinates defining the lower left and upper 
#' right corners. 
#' 
#' 
#' @examples
#' 
#' query_sb_spatial(bb_min=c(-104.4, 37.5), bb_max=c(-95.1, 41.0))
#' 
#' 
#' @export
query_sb_spatial = function(bbox, bb_min, bb_max, ..., limit=20, session=current_session()){
	
	if(!missing(bbox)){
		if(!requireNamespace("sp")){
			#project first into longlat projection so bounding box is in degrees
			bb = spTransform(bbox, CRS("+proj=longlat +datum=WGS84"))@bbox
		}else{
			stop('sp package required to use bbox parameter. Please: install.packages("sp")')
		}
	}else if(!missing(bb_min) & !missing(bb_max)){
		bb = matrix(nrow=2, ncol=2)
		bb[,1] = bb_min
		bb[,2] = bb_max
		
	}else{
		stop('Must supply either bbox sp object or bb_min and bb_max vectors')
	}

	#I don't know of a better way than this.
	bb_wkt = paste0('POLYGON((', bb[1,1], ' ', bb[2,2], ',',
				 											 bb[1,2], ' ', bb[2,2], ',', 
				 											 bb[1,2], ' ', bb[2,1], ',',
				 											 bb[1,1], ' ', bb[2,1], ',',
															 bb[1,1], ' ', bb[2,2], '))')
	
	
	query_sb(list(spatialQuery = bb_wkt), limt=limit)
}
