#figure_faultline_code

library(sbtools)
#Source non-sbtools-required but useful mapping packages
library(sp)
library(maps)

faults = query_sb(list(q="faults", browseType = "OGC WFS Layer"), limit=20)

png('manuscript/faultlinefig.png', res=300, width=1200, height=1200)
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
dev.off()
