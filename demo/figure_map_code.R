
library(sbtools)

#Source non-sbtools-required but useful mapping packages
library(maps)
library(sp)
#an item with an included OGC WFS service
layer = item_get_wfs('55e372b9e4b05561fa208212')

#png('manuscript/mapfig.png', res=300, width=1700, height=1400)
par(mar=c(5,5,1,1), oma=c(0,2,0,0))
map('state', regions='iowa', ylab='Lat', xlab='Lon')

plot(spTransform(layer, CRS("+proj=longlat +datum=WGS84")), add=TRUE)
map.axes(mex=0.2)
mtext('lon', 1, line=2.2, mex=2)
mtext('lat', 2, line=2.2, mex=2)
#dev.off()
