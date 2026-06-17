rm(list=ls())
library(oce)
library(cmocean)
library(csasAtlPhys)
data("station2PlotLimits")
source('00_setupFile.R')
# load data
load(paste(destDirData, 'dailyClimatologyWAnomaly.rda', sep = '/'))
# define variables
vars <- list(c('temperature', 'salinity', 'sigmaTheta'),
             c('temperature', 'salinity', 'sigmaTheta'),
             c('temperatureAnomaly', 'salinityAnomaly', 'sigmaThetaAnomaly'))
# initiate png output
png(filename = paste(destDirFigures,
                     '03_sectionDailyClimatologyAndDifference.png',
                     sep = '/'),
    width = 11, height = 6, units = 'in',
    res = 200, pointsize = 10)
# assuming that loaded `climatology` data is in order
#   or 1981, 1991, and 1991wAnomaly
# define graphical parameters
mfrow <- c(3,3)
oma <- c(2, 2, 2, 0)
mar <- c(1.5, 3.5, 1.5, 2)
par(mfrow = mfrow, mar = mar, oma = oma)
par(cex = 0.8) # for nice palette font size
# define general plotting parameters
xlim <- as.POSIXct(range(unlist(lapply(climatology[[1]][['dailyClimatologyCtd']], '[[', 'startTime'))),
                   origin = '1970-01-01',
                   tz = 'UTC')
ylim <- rev(range(climatology[[1]][['dailyClimatologyCtd']][[1]][['pressure']]))
for(ic in 1:2){
  cctd <- climatology[[ic]][['dailyClimatologyCtd']]
  # create section
  s <- as.section(cctd)
  # create gridded section (though, it's basically gridded, but for completeness)
  p <- s[['pressure', 'byStation']][[1]]
  sg <- sectionGrid(section = s,
                    p = p)
  # plot
  ## get plotting variables for given list index
  pvars <- vars[[ic]]
  for(var in pvars){
    # set up various plotting parameters
    zlim <- station2PlotLimits[['limits']][[var]]
    levels <- station2PlotLimits[['contourLevels']][[var]]
    levelLimits <- station2PlotLimits[['contourLevelLimits']][[var]]
    axes <- ifelse(i == length(climatology), TRUE, FALSE)
    zcol <- switch(var,
                   'temperature' = cmocean::cmocean("thermal"),
                   'temperatureAnomaly'= cmocean::cmocean("balance"),
                   'salinity' = cmocean::cmocean("haline"),
                   'salinityAnomaly' = cmocean::cmocean("balance"),
                   'sigmaTheta' = cmocean::cmocean("dense"),
                   'sigmaThetaAnomaly' = cmocean::cmocean("balance"))
    R <- ']'
    L <- '['
    zlab <- switch(var,
                   'temperature'= bquote(bold(.(gettext('Temperature', domain = 'R-oce')) * .(L) * degree * "C" * .(R))),
                   'temperatureAnomaly' = getAnomalyLabel('temperatureAnomaly', bold = TRUE),
                   'salinity' = bquote(bold(.(gettext('Practical Salinity', domain = 'R-oce')))),
                   'salinityAnomaly' = getAnomalyLabel('salinityAnomaly', bold = TRUE),
                   'sigmaTheta' = bquote(bold(sigma[theta] *' '* .(L) * kg/m^3 * .(R))),
                   'sigmaThetaAnomaly' = getAnomalyLabel('sigmaThetaAnomaly', bold = TRUE))
    plot(sg, which = var, ztype = 'image',
         zlim = zlim, ylim = ylim, xlim = xlim,
         xtype = 'time', zcol = zcol, zbreaks = NULL,
         legend.loc = '', ylab = '', zlab = '',
         axes = FALSE, xlab = '', mar = mar,
         stationTicks = TRUE, showBottom = FALSE, drawPalette = TRUE)
    # add contours
    clx <- sg[['time', 'byStation']]
    cly <- sg[['station',1]][['pressure']]
    clz <- matrix(sg[[var]], byrow = TRUE, nrow = length(sg[['station']]))
    contour(clx, cly, clz, levels = levels[levels > levelLimits[1] & levels < levelLimits[2] ],
            col = 'black', add = TRUE, #labcex = 1,
            vfont = c('sans serif', 'bold'),
            xlim = xlims, ylim = ylim)
    contour(clx, cly, clz, levels = levels[levels <= levelLimits[1] | levels >= levelLimits[2]],
            col = 'white', add = TRUE, #labcex = 1,
            vfont = c('sans serif', 'bold'),
            xlim = xlims, ylim = ylim)
    # add y-axis
    axis(side = 2)
    axis(side = 4, labels = FALSE, line = -3.45)
    mtext(text = resizableLabel('depth'), side = 2, line = 2, cex = 4/5)
    # add x-axis
    xat <- seq.POSIXt(from = xlim[1] + (60*60*24), # add a day to avoid weird labelling ?
                      to = xlim[2],
                      by = 'month')
    axis.POSIXct(side = 1,
                 at = xat,
                 format = '%b')
    # label variable
    if(ic == 1){
      mtext(text = zlab, side = 3, line = 1.3)

    }
    # label climatology reference period
    if(var == 'temperature'){
      mtext(text = paste(range(climatology[[ic]][['climatologyYears']]), collapse = ' to '),
            side = 2, line = 3.5, font = 2)
    }
  }
}
# plot differences as an imagep
ic <- 3
cctd <- climatology[[ic]][['dailyClimatologyCtd']]
# define y, unique pressure values
allP <- unlist(lapply(cctd, '[[', 'pressure'))
y <- unique(allP)
# get the time
x <- as.POSIXct(unlist(lapply(cctd, '[[', 'startTime')), tz = 'UTC')
# now iterate through each variable
for(var in vars[[ic]]){
  # define z and fill matrix with data
  z <- matrix(data = NA, nrow = length(y), ncol = length(x))
  for(is in 1:length(cctd)){
    pidx <- unlist(lapply(cctd[[is]][['pressure']], function(k) which(y == k)))
    z[pidx, is] <- cctd[[is]][[var]]
  }
  cm <- colormap(z = z,
                 zlim = max(abs(z), na.rm = TRUE) * c(-1, 1),
                 col = cmocean('balance'),
                 missingColor = 'grey49')
  imagep(x = x, y = y, z = t(z),
         colormap = cm,
         ylim = ylim, xlim = xlim,
         axes = FALSE, drawPalette=TRUE, mar = mar)
  box()
  # add y-axis
  axis(2)
  mtext(text = resizableLabel('depth'), side = 2, line = 2, cex = 4/5)
  axis(side = 4, labels = FALSE)
  # add x-axis
  xat <- seq.POSIXt(from = xlim[1] + (60*60*24), # add a day to avoid weird labelling ?
                    to = xlim[2],
                    by = 'month')
  axis.POSIXct(side = 1,
               at = xat,
               format = '%b')
}
dev.off()