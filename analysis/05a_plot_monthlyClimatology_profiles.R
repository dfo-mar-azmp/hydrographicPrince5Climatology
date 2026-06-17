rm(list=ls())
library(oce)
data("ctd")
ghostctd <- ctd
source('00_setupFile.R')
# load data
load(paste(destDirData, 'monthlyClimatologyWAnomaly.rda', sep = '/'))
# function to plot line and polygon to keep code DRY
plotLineAndPolygon <- function(dd, var, climYear){
  for(p in 1:length(dd)){
    polyCol <- switch(as.character(climYear[p]),
                      '1981' = hcl.colors(n=5, palette = 'Burg')[3],
                      '1991' = hcl.colors(n=5, palette = 'Blues')[3])
    alpha <- switch(as.character(climYear[p]),
                    '1981' = 150,
                    '1991' = 100)
    profile <- dd[[p]]
    ok <- !is.na(profile[[var]])
    polygon(c(profile[[var]][ok] - profile[[paste0(var,'SD')]][ok],
              rev(profile[[var]][ok] + profile[[paste0(var,'SD')]][ok])),
            c(profile[['pressure']][ok],
              rev(profile[['pressure']][ok])),
            border = NA, col = rgb(t(col2rgb(polyCol)), alpha = alpha, max = 255))
  }
  for(p in 1:length(dd)){
    linCol <- switch(as.character(climYear[p]),
                     '1981' = hcl.colors(n=5, palette = 'Burg')[1],
                     '1991' = hcl.colors(n=5, palette = 'Blues 2')[1])
    profile <- dd[[p]]
    ok <- !is.na(profile[[var]])
    lines(profile[[var]], profile[['pressure']], col = linCol, lwd = 2)
  }
}

vars <- c('temperature', 'salinity', 'sigmaTheta')
# get limits for variables
limits <- vector(mode = 'list', length = length(vars))
for(iv in 1:length(vars)){
  limits[[iv]] <- range(c(unlist(lapply(climatology, function(k) unlist(lapply(k[['allctd']], '[[', vars[iv])))), # ctd
                unlist(lapply(climatology, function(k) k[['monthlyClimatologyDf']][[vars[iv]]] - k[['monthlyClimatologyDf']][[paste0(vars[iv], 'SD')]])), # climatology - SD
                unlist(lapply(climatology, function(k) k[['monthlyClimatologyDf']][[vars[iv]]] + k[['monthlyClimatologyDf']][[paste0(vars[iv], 'SD')]])) # climatology + SD
                ),
              na.rm = TRUE)
}
names(limits) <- vars
breaks <- names(climatology[[1]][['monthlyData']][[1]])
ubreaksplit <- strsplit(breaks, split = ',')
minbreakdepth <- as.numeric(unlist(lapply(ubreaksplit, function(k) gsub('\\((.*)', '\\1', k[1]))))
maxbreakdepth <- as.numeric(unlist(lapply(ubreaksplit, function(k) gsub('(.*)\\]', '\\1', k[2]))))
plim <- c(min(minbreakdepth), max(maxbreakdepth))
yearlim <- c(1981, 2020)
yearseq <- seq(yearlim[1], yearlim[2], 1)
startYear <- min(yearseq) + c(0, 10, 30)
endYear <- startYear + c(9, 19, 9)
# define colors for barplot
cmcol <- hcl.colors(5, palette = 'RdYlGn')[c(1, 2, 5)]
cmcoladj <- colorspace::lighten(cmcol, amount = 0.5)
# get limit for barplot,
bpxlim <- c(-1, 1) * max(unlist(lapply(climatology, function(k) lapply(k[['monthlyData']], function(kk) lapply(kk, function(kkk) apply(kkk[, names(kkk) %in% vars], 2, function(kkkk) length(which(!is.na(kkkk)))))))))
months <- 1:12
for(im in 1:length(months)){
  lookmonth <- months[im]
  # initiate png output
  png(filename = paste(destDirFigures,
                       paste0(paste('06_monthlyClimatologyProfiles', lookmonth, sep = '_'), '.png'),
                       sep = '/'),
      width = 6, height = 8, units = 'in',
      res = 250, pointsize = 10)
  # plot nobs per variable, left side 1981 to 2010, right side 1991 to 2020
  mlay <- matrix(data = c(1, 2, 3, 10,
                          4, 5, 6, 11,
                          7, 8, 9, 0),
                 nrow = 3,
                 ncol = 4,
                 byrow = TRUE)
  palwid <- 0.18
  layout(mlay, widths = c(rep(1-palwid, 3), palwid))
  par(oma = c(0, 3, 4, 1))
  ymar1 <- 0.5
  ymar2 <- 1.5
  barplotMar <- c(4, ymar1, 0, ymar2)
  profileMar <- c(0, ymar1, 4, ymar2)
  profileMar2 <- c(1.5, ymar1, 2.5, ymar2)
  par(cex = 4/5)
  par(mar = barplotMar)
  for(var in vars){
    bpdout <- vector(mode = 'list', length = 2)
    for(ic in 1:2){
      cmd <- climatology[[ic]][['monthlyData']]
      dmon <- as.numeric(names(cmd))
      okcmd <- which(dmon == lookmonth)
      dd <- cmd[[okcmd]]
      # define output matrix
      noutname <- paste0('n', min(climatology[[ic]][['climatologyYears']]) - 1900)
      noutsubname <- paste(startYear, endYear, sep = '-')
      bpd <- matrix(data = NA, ncol = length(dd), nrow = 5, dimnames = list(c('depth', noutname, noutsubname)))
      for(id in 1:length(dd)){
        d <- dd[[id]]
        year <- d[['year']][!is.na(d[[var]])]
        yearDay <- d[['yearDay']][!is.na(d[[var]])]
        dfdecade <- data.frame(year = factor(year, levels = yearseq))
        ydayyeardist <- table(dfdecade)
        decadalTable <- mapply(function(start, end) {ok <- as.numeric(rownames(ydayyeardist)) %in% start:end;
        sum(ydayyeardist[ok])},
        startYear, endYear)
        ## add value to bpd
        bpd[2, id] <- sum(decadalTable)
        bpd[3:5, id] <- decadalTable
        # get the depth ranges
        dbreak <- names(dd)[id]
        dbreaks <- strsplit(dbreak, split = ',')[[1]]
        minbreak <- as.numeric(gsub('\\((.*)', '\\1', dbreaks[1]))
        maxbreak <- as.numeric(gsub('(.*)\\]', '\\1', dbreaks[2]))
        ## add value to bpd
        bpd[1, id] <- mean(c(minbreak, maxbreak))
      } # closes id
      # plot barplot
      if(ic == 1){
        o <- order(bpd[rownames(bpd) == 'depth', ], decreasing = TRUE)
        bp <- barplot(bpd[!rownames(bpd) %in% c('depth', noutname), o] * -1,
                      col = cmcoladj,
                      axes = FALSE,
                      horiz = TRUE,
                      xlim = bpxlim)
        box()
        # y-axis
        yat <- seq(1, 30, 10) # manually set after outputting plot, really terrible way
        axis(2, at = bp[yat], labels = bpd[rownames(bpd) == 'depth', o][yat])
        axis(2, at = max(bp) + diff(bp)[1], labels = 0)
        if(var == vars[1]){
          mtext(text = resizableLabel(item = 'p',
                                      axis = 'y'),
                side = 2,
                line = 2,
                cex = 4/5)
        }
        # x-axis
        xat <- axis(1, labels = FALSE)
        axis(1, at = xat, labels = abs(xat))
        mtext(text = 'Number of observations',
              side = 1,
              line = 2,
              cex = 4/5)
        # add grid
        abline(h = bp[yat], lty = 3, col = 'lightgrey')
        abline(h = max(bp) + diff(bp)[1], lty = 3, col = 'lightgrey') # manually add 0
        abline(v = xat, lty = 3, col = 'lightgrey')
        # add barplot back on top
        bp <- barplot(bpd[!rownames(bpd) %in% c('depth', noutname), o] * -1,
                      col = cmcoladj,
                      horiz = TRUE,
                      axes = FALSE,
                      xlim = bpxlim,
                      add = TRUE)
        # add variable label
        R <- ']'
        L <- '['
        zlab <- switch(var,
                       'temperature'= bquote(bold(.(gettext('Temperature', domain = 'R-oce')) * .(L) * degree * "C" * .(R))),
                       'salinity' = bquote(bold(.(gettext('Practical Salinity', domain = 'R-oce')))),
                       'sigmaTheta' = bquote(bold(sigma[theta] *' '* .(L) * kg/m^3 * .(R))))
        mtext(text = zlab, side = 3, line = 1.3, cex = 1)
        # add climatology reference period label
        mtext(text = paste(range(climatology[[ic]][['climatologyYears']]), collapse = ' to '),
              side = 3, line = 0.2, font = 2, adj = 0.15, cex = 2/3)

      }
      else {
        o <- order(bpd[rownames(bpd) == 'depth', ], decreasing = TRUE)
        bp <- barplot(bpd[!rownames(bpd) %in% c('depth', noutname), o],
                      col = cmcoladj,
                      horiz = TRUE,
                      axes = FALSE,
                      xlim = bpxlim,
                      add = TRUE)
        # thick vertical line at zero
        abline(v = 0, lwd = 2)
        axis(side = 3, at = 0, labels = FALSE, lwd.ticks = 2, tcl = -1)
        # add climatology reference period label
        mtext(text = paste(range(climatology[[ic]][['climatologyYears']]), collapse = ' to '),
              side = 3, line = 0.2, font = 2, adj = 0.85, cex = 2/3)
      }
    } # closes ic
  } # closes var
  # plot all data with mean profile +/- 0.5 SD
  for(var in vars){
    nctd <- NULL
    for(ic in 1:2){
      dctd <- climatology[[ic]][['allctd']]
      startTime <- as.POSIXlt(unlist(lapply(dctd, '[[', 'startTime')), tz = 'UTC')
      ctdmonth <- startTime$mon + 1
      okctd <- ctdmonth == lookmonth
      mctd <- dctd[okctd]
      # save nctd
      nctd <- c(nctd, length(mctd))
      ctdyear <- startTime[okctd]$year + 1900
      cm <- colormap(z = ctdyear, breaks = c(1981, 1991, 2011, 2020), col = cmcoladj)
      # initiate profile plot
      if(ic == 1){
        ylab <- switch(var,
                       'temperature' = NULL,
                       'salinity' = '',
                       'sigmaTheta' = '')
        # initialize profile plot
        plotProfile(ghostctd, xtype = var,
                    Tlim = limits[['temperature']],
                    Slim = limits[['salinity']],
                    densitylim = limits[['sigmaTheta']],
                    plim = rev(plim),
                    ylab = ylab,
                    col = 'white',
                    mar = profileMar)
      }
      # add lines of all profiles
      for(istn in 1:length(mctd)){
        lines(mctd[[istn]][[var]], mctd[[istn]][['pressure']], col = cm$zcol[istn])
      }
    } # closes ic
    # add monthly climatology profile +/- SD
    ## get the monthly climatology profiles
    mclim <- lapply(climatology[1:2], function(k) k[['monthlyClimatologyCtd']][[which(as.POSIXlt(unlist(lapply(k[['monthlyClimatologyCtd']], '[[', 'startTime')), tz = 'UTC')$mon + 1 == lookmonth)]])
    mclimyear <- unlist(lapply(climatology[1:2], function(k) min(k[['climatologyYears']])))
    plotLineAndPolygon(dd = mclim, var = var, climYear = mclimyear)
    # add legend with number of profile for each climatology
    if(var == 'temperature'){
      climnum <- unlist(lapply(climatology[1:2], function(k) substr(x = min(k[['climatologyYears']]), 3, 4)))
      legendText <- as.expression(mapply(FUN = function(x, y) bquote('n'[.(x)] * '=' * .(y)), x = climnum, y = nctd))
      legend('bottomright', legend = legendText, bty = 'n')
    }
  } # closes var
  # plot difference profile
  diffd <- climatology[[3]][['monthlyClimatologyCtd']][[which(as.POSIXlt(unlist(lapply(climatology[[3]][['monthlyClimatologyCtd']], '[[', 'startTime')),tz = 'UTC')$mon + 1 == lookmonth)]]
  mlimits <- vector(mode = 'list', length = length(vars))
  for(iv in 1:length(vars)){
    lookvar <- vars[iv]
    mlimits[[iv]] <- range(c(diffd[[paste0(lookvar, 'Anomaly')]] - (diffd[[paste0(lookvar, 'AnomalySD')]]/2),
                            diffd[[paste0(lookvar, 'Anomaly')]] + (diffd[[paste0(lookvar, 'AnomalySD')]]/2)),
                          na.rm = TRUE)
  }
  names(mlimits) <- vars
  for(var in vars){
    # initiate plot
    ylab <- switch(var,
                   'temperature' = NULL,
                   'salinity' = '',
                   'sigmaTheta' = '')
    yaxt <- switch(var,
                   'temperature' = 's',
                   'salinity' = 'n',
                   'sigmaTheta' = 'n')
    plotProfile(ghostctd, xtype = var,
                Tlim = mlimits[['temperature']],
                Slim = mlimits[['salinity']],
                densitylim = mlimits[['sigmaTheta']],
                plim = rev(plim),
                col = 'white',
                mar = profileMar2,
                xlab = '',
                ylab = ylab,
                yaxt = yaxt)
    polyCol <- hcl.colors(n=5, palette = 'Grays')[3]
    linCol <- hcl.colors(n=5, palette = 'Grays')[1]
    alpha <- 100
    # +/- 0.5 sd
    varanomaly <- paste0(var, 'Anomaly')
    varsd <- paste0(var, 'AnomalySD')
    x <- diffd[[varanomaly]]
    xsd <- diffd[[varsd]]
    ok <- !is.na(x)
    polygon(c(x[ok] - (xsd[ok]/2),
              rev(x[ok] + (xsd[ok]/2))),
            c(diffd[['pressure']][ok],
              rev(diffd[['pressure']][ok])),
            border = NA, col = rgb(t(col2rgb(polyCol)), alpha = alpha, max = 255))
    # difference
    lines(x, diffd[['pressure']], col = linCol, lwd = 2)
    # vertical line at 0
    abline(v = 0)
  } # closes var
  # draw palettes
  par(mfg = c(3,2)) # plot area 10
  par(mar = barplotMar + c(0, 0, 0, 6 - barplotMar[4]))
  cmcol <- hcl.colors(5, palette = 'RdYlGn')[c(1, 2, 5)]
  cmcoladj <- colorspace::lighten(cmcol, amount = 0.5)
  drawPalette(breaks = c(1981, 1991, 2011, 2020),
              at = c(1981, 1991, 2010, 2020),
              col = cmcoladj,
              zlab = '')
  mtext(text = 'Year', side = 4, line = 9, cex = 4/5)
  par(mfg = c(3,3)) # plot area 11
  par(mar = profileMar + c(0, 0, 0, 6 - profileMar[4]))
  cmcol <- hcl.colors(5, palette = 'RdYlGn')[c(1, 2, 5)]
  cmcoladj <- colorspace::lighten(cmcol, amount = 0.5)
  drawPalette(breaks = c(1981, 1991, 2011, 2020),
              at = c(1981, 1991, 2010, 2020),
              col = cmcoladj,
              zlab = '')
  mtext(text = 'Year', side = 4, line = 9, cex = 4/5)
  dev.off()
} # closes im
