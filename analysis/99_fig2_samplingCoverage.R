rm(list=ls())
library(oce)
library(ocedata)
data(coastlineWorldFine)
library(csasAtlPhys)
source('00_setupFile.R')
# get bathymetry
topo <- download.topo(west = -75, east = -50,
                      south = 38, north = 50,
                      resolution = 1)
ocetopo <- read.topo(topo)
# load data
load(paste(destDirData, 'climateAndArchiveCTD.rda', sep = '/'))
# get some information out of the data
startTime <- as.POSIXct(unlist(lapply(allctd, function(k) k[['startTime']])),
                        origin = '1970-01-01',
                        tz = 'UTC')
dataType <- unlist(lapply(allctd, '[[', 'dataType'))
lon <- unlist(lapply(allctd, function(k) k[['longitude']][1]))
lat <- unlist(lapply(allctd, function(k) k[['latitude']][1]))
# define pch and col for dataType
utype <- unique(dataType)
dataTypePch <- data.frame(dataType = levels(factor(utype)),
                          pch = 1:nlevels(factor(utype)) + 14,
                          col = 1:nlevels(factor(utype)))
dataTypePch$col[1] <- 'grey'
dataTypePch$pch[1] <- 0
# plot
par(mfrow=c(1,3))
## map
stn2lonlim <- range(station2Polygon[['longitude']]) + c(-0.25, 0.25)
stn2latlim <- range(station2Polygon[['latitude']]) + c(-0.25, 0.25)
mapPlot(coastlineWorldFine,
        longitudelim = stn2lonlim,
        latitudelim = stn2latlim,
        col = fillcol,
        proj = proj)
levels <- c(-2000, -1000, -200, -100)
bathycol <- gray.colors(n = length(levels))
mapContour(longitude = ocetopo[['longitude']],
           latitude = ocetopo[['latitude']],
           z = ocetopo[['z']],
           levels = levels,
           lwd = 0.8, col = bathycol)
## Halifax line
lapply(halifaxStationPolygons, function(k) mapPoints(k[['longitude']], k[['latitude']], pch = 20, col = 'black', cex = 1.4))
## occupations from data pull
typecol <- dataTypePch$col[match(dataType, dataTypePch$dataType)]
mapPoints(longitude = lon,
          latitude = lat,
          pch = 20,
          col = typecol)
## Station 2
mapPoints(longitude = stn2lon,
          latitude = stn2lat,
          pch = 20, col = 'black',
          cex = 1.4)
mapText(longitude = stn2lon,
        latitude = stn2lat,
        labels = 'Station 2',
        pos = 4,
        col = 'black')
## Station 2 bounding box
mapPolygon(longitude = station2Polygon[['longitude']],
           latitude = station2Polygon[['latitude']],
           lwd = 1.4)

## year versus yearDay, color coded by type
year <- as.POSIXlt(startTime)$year + 1900
month <- as.POSIXlt(startTime)$mon + 1
yday <- as.POSIXlt(startTime)$yday
par(mar = c(3.5, 3.5, 1, 1))
plot(yday, year, pch = 21, bg = typecol, ylab = '', xlab = '', ylim = range(year) + c(-4, 0)) # set ylim to have room for legend
mtext(text = 'Year', side = 2, line = 2)
mtext(text = 'Year day', side = 1, line = 2)
legend('bottomleft', pch = 21, pt.bg = dataTypePch$col, legend = dataTypePch$dataType, ncol = 2)
## bar plot of total number of stations per year, color coded by type
yearseq <- min(unlist(climatologyYears)):max(unlist(climatologyYears))
yearall <- lapply(yearseq, function(k) {ok <- year %in% k;
yt <- factor(dataType[ok], levels = utype);
table(yt)})
yearallt <- do.call("rbind", yearall)
rownames(yearallt) <- yearseq
yearallt <- yearallt[,order(colnames(yearallt))]

bp <- barplot(t(yearallt), legend = TRUE,
              args.legend = list(x = 'topright', ncol = 2),
              ylim = c(0, max(apply(yearallt, 1, sum) + 10)),
              col = dataTypePch$col,
              xlab = '', xaxt = 'n')
xat <- pretty(yearseq, n = 10)
xat <- xat[xat %in% yearseq]
axis(side = 1, at = bp[yearseq %in% xat], labels = xat)
mtext(text = 'Year', side = 1, line = 2)
mtext(text = 'Number of stations', side = 2, line = 2)
box()
dev.off()