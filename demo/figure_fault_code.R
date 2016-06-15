#figure_faultline_code

library(sbtools)
#Source non-sbtools-required but useful mapping packages
library(sp)
library(maps)

faults = query_sb(list(q="faults", browseType = "OGC WFS Layer"), limit=20)

png('faultlinefig.png', res=300, width=1700, height=1400)
par(mar=c(5,5,1,1), oma=c(0,2,0,0))
map('usa')
for(i in 1:length(faults)){
	#just to finish fig if there's an HTTP error
	tryCatch({
		layer = item_get_wfs(faults[[i]]$id)
		layer = spTransform(layer, CRS('+proj=longlat +datum=WGS84'))
		plot(layer, add=TRUE, col='red')
	}, error=function(e){})
}
map.axes()
mtext('lon', 1, line=2.2, mex=2)
mtext('lat', 2, line=2.2, mex=2)
dev.off()
