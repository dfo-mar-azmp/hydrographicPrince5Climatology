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
                          pch = 1:nlevels(factor(utype)), #+ 20,
                          col = palette.colors(n = nlevels(factor(utype)) + 1)[2:(nlevels(factor(utype))+1)])
# set map plotting parameters
lonlim <- c(-67, -56)
latlim <- c(41.5, 48.5)
proj <- '+proj=merc'
fillcol <- 'bisque2'
png(filename = paste(destDirFigures, '99_fig1_generalMap.png', sep = '/'),
    width = 7, height = 7, units = 'in',
    pointsize = 13,res = 250
    )
m <- matrix(c(1,1,1,1,2,2,
           3,3,3,4,4,4),
           nrow = 2,
          byrow = TRUE)
layout(m)
par(mar = c(2, 3.5, 1, 0.5))
par(oma = c(0, 0, 0.7, 0))
mapPlot(coastlineWorldFine,
        longitudelim = lonlim,
        latitudelim = latlim,
        col = fillcol,
        proj = proj)
levels <- c(-2000, -1000, -200, -100)
bathycol <- gray.colors(n = length(levels))
mapContour(longitude = ocetopo[['longitude']],
           latitude = ocetopo[['latitude']],
           z = ocetopo[['z']],
           levels = levels,
           lwd = 0.8, col = bathycol)
mapScalebar('topleft',
            length = 100)
## add AZMP lines
azmpstns <- c(cabotStraitStationPolygons,
              louisbourgStationPolygons,
              halifaxStationPolygons,
              brownsBankStationPolygons)
lapply(azmpstns, function(k) mapPoints(k[['longitude']], k[['latitude']], pch = 20, col = 'black'))
mapText(-64.2, 42.65, labels = 'Browns Bank', cex = 1)
mapText(-61.5, 43.4, labels = 'Halifax', cex = 1)
mapText(-57.95, 45.1, labels = 'Louisbourg', cex = 1)
mapText(-60.3, 47.6, labels = 'Cabot \n Strait', cex = 1)
## add Prince 5 point
p5col <- 'black'
mapPoints(longitude = p5lon,
          latitude = p5lat,
          pch = 20, col = p5col,
          cex = 2)
mapText(longitude = p5lon,
        latitude = p5lat,
        labels = 'Prince 5',
        pos = 4,
        col = p5col,
        font = 2,
        cex = 1)
# label
mtext('A.)', side = 3, adj = 0, cex = 4/5, line = 0.3, font = 2)
# Station 2 close up
par(mar = c(2, 1.5, 1, 0.5))
p5lonlim <- p5lon + c(-0.25, 0.25)
p5latlim <- p5lat + c(-0.25, 0.25)
mapPlot(coastlineWorldFine,
        longitudelim = p5lonlim,
        latitudelim = p5latlim,
        col = fillcol,
        proj = proj)
levels <- c(-2000, -1000, -200, -100)
bathycol <- gray.colors(n = length(levels))
mapContour(longitude = ocetopo[['longitude']],
           latitude = ocetopo[['latitude']],
           z = ocetopo[['z']],
           levels = levels,
           lwd = 0.8, col = bathycol)
mapScalebar('topleft',
            length = 25)
## occupations from data pull
typecol <- dataTypePch$col[match(dataType, dataTypePch$dataType)]
typepch <- dataTypePch$pch[match(dataType, dataTypePch$dataType)]
mapPoints(longitude = lon,
          latitude = lat,
          col = typecol,
          pch = typepch)
## Prince 5 bounding box
mapPolygon(longitude = prince5Polygon[['longitude']],
           latitude = prince5Polygon[['latitude']],
           lwd = 1.4)
# label
mtext('B.)', side = 3, adj = 0, cex = 4/5, line = 0.3, font = 2)
## bar plot of total number of stations per year, color coded by type
par(mar = c(3.5, 3.5, 1, 0.5))
year <- as.POSIXlt(startTime)$year + 1900
month <- as.POSIXlt(startTime)$mon + 1
yday <- as.POSIXlt(startTime)$yday
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
mtext(text = 'Year', side = 1, line = 2, cex = 4/5)
mtext(text = 'Number of occupations', side = 2, line = 2, cex = 4/5)
box()
# label
mtext('C.)', side = 3, adj = 0, cex = 4/5, line = 0.3, font = 2)
## year versus yearDay, color coded by type
plot(yday, year, pch = typepch, col = typecol, ylab = '', xlab = '', ylim = range(year) + c(-5, 0)) # set ylim to have room for legend
## y-axis label
mtext(text = 'Year', side = 2, line = 2, cex = 4/5)
## x-axis label
mtext(text = 'Year day', side = 1, line = 2, cex = 4/5)
## grid
abline(h = pretty(year), lty = 3, col = 'lightgrey')
abline(v = pretty(yday), lty = 3, col = 'lightgrey')
points(yday, year, pch = typepch, col = typecol)
## legend
legend('bottomleft', pch = dataTypePch$pch, col = dataTypePch$col, legend = dataTypePch$dataType, ncol = 2)
# label
mtext('D.)', side = 3, adj = 0, cex = 4/5, line = 0.3, font = 2)
dev.off()
