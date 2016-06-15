
library(sbtools)

#Source non-sbtools-required but useful mapping packages
library(maps)
library(sp)
#an item with an included OGC WFS service
layer = item_get_wfs('55e372b9e4b05561fa208212')

png('manuscript/mapfig.png', res=300, width=1200, height=1200)
map('state', regions='iowa')

plot(spTransform(layer, CRS("+proj=longlat +datum=WGS84")), add=TRUE)
map.axes()
dev.off()
