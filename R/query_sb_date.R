#' @title Query SB for items within a date range
#' @param start Start date as \code{\link{POSIXct}} object. Defaults to 1970-01-01
#' @param end   End date as \code{\link{POSIXct}} object. Defaults to today.
#' @param date_type Which object timestamp to query against. Options are (case sensitive): 
#' 'Acquisition', 'Award', 'Collected', 'dateCreated', 'Received', 'Reported', 
#' 'Transmitted', 'Due', 'End', 'Info', 'lastUpdated', 'Publication', 'Release', 
#' 'Repository Created', 'Repository Updated', 'Start'.
#' 
#' 
#' @inheritParams query_sb
#' 
#' @description 
#' Queries ScienceBase for items with timestamps within a certain date/time range.
#' 
#' @examples \dontrun{
#' # find items updated today
#' query_sb_date(Sys.time(), Sys.time())
#' 
#' # find items with publications from the 1970's
#' query_sb_date(as.POSIXct('1970-01-01'), as.POSIXct('1980-01-01'), 
#'   date_type='Publication', limit=1000)
#' 
#' }
#' 
#' @export
query_sb_date = function(start=as.POSIXct('1970-01-01'), end=Sys.time(), date_type='lastUpdated', ..., limit=20, session=current_session()){
	
	if(!date_type %in% valid_date_types){
		stop('date_type choice must be identical (including case) to listed options in documentation. See ?query_sb_date')
	}
	
	date_range = paste0('{"dateType":"', date_type, '","choice":"range","start":"', format(start, '%Y-%m-%d'),
											'","end":"', format(end, '%Y-%m-%d'), '"}')
	
	query_sb(list(dateRange = date_range), ..., limit=limit, session=session)
	
}

valid_date_types = c('Acquisition', 'Award', 'Collected', 'dateCreated', 'Received', 'Reported',
										 'Transmitted', 'Due', 'End', 'Info', 'lastUpdated', 'Publication', 'Release', 
										  'Repository Created', 'Repository Updated', 'Start')

