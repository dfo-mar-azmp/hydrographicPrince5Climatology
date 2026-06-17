rm(list=ls())
source('00_setupFile.R')
# some things are plotting for checking results.
# an attempt was made to place the plotting code after analysis to make code cleaner.
plot <- TRUE # toggle if some things should be plotted (FALSE for speed)
data("ctd")
ghostctd <- ctd
source('../R/gridClimatologyFn.R')
source('../R/gridClimatologyDailyFn.R')
source('../R/plotClimatologyFn.R')
load(paste(destDirData, 'climateAndArchiveCTD.rda', sep = '/'))
# get information out of ctd data
startTime <- as.POSIXct(unlist(lapply(allctd, function(k) k[['startTime']])), origin = '1970-01-01', tz = 'UTC')
startMonths <- as.POSIXlt(startTime)$mon + 1
startYears <- as.POSIXlt(startTime)$year + 1900
# define variables to do analysis
vars <- c('temperature', 'salinity', 'sigmaTheta')
# define list to output results
climatology <- vector(mode = 'list', length = length(climatologyYears))
for(ic in 1:length(climatologyYears)){
  climyears <- climatologyYears[[ic]]
  okclim <- startYears %in% climyears
  climctd <- allctd[okclim]
  # get all p, T, S, ST data
  time <- as.POSIXct(unlist(lapply(climctd, function(k) rep(k[['startTime']], length = length(k[['pressure']])))), origin = '1970-01-01', tz = 'UTC')
  p <- unlist(lapply(climctd, function(k) k[['pressure']]))
  T <- unlist(lapply(climctd, function(k) k[['temperature']]))
  S <- unlist(lapply(climctd, function(k) if('salinity' %in% names(k@data)) k[['salinity']] else rep(NA, length(k[['pressure']]))))
  ST <- unlist(lapply(climctd, function(k) if('salinity' %in% names(k@data)) k[['sigmaTheta']] else rep(NA, length(k[['pressure']]))))
  yday <- as.POSIXlt(time)$yday + 1
  df <- data.frame(time = time,
                   year = as.POSIXlt(time)$year + 1900,
                   yearDay = yday,
                   pressure = p,
                   temperature = T,
                   salinity = S,
                   sigmaTheta = ST)
  # set deltaz and breaks for cutting
  deltaz <- 5
  breaks1 <- seq(2.5, 92.5, deltaz) # set start to 2.5 to give first point at 5dbar
  sdf <- split(df, cut(df[['pressure']], breaks1, dig.lab = max(nchar(breaks1) - 1)))
  # define yday to predict climatology
  ydaypredict <- 1:365
  # define output for variables
  varoutput <- lapply(c(vars, paste0(vars, 'SD')), function(k) matrix(data = NA, nrow = length(sdf), ncol = length(ydaypredict), byrow = FALSE, dimnames = list(head(breaks1,-1) + (deltaz/2), ydaypredict)))
  names(varoutput) <- c(vars, paste0(vars, 'SD'))
  if(plot) pdf(file = paste(destDirSuppFigures, paste0('dailyClimatologyCheck', min(climyears), 'to', max(climyears), '.pdf'), sep = '/'), width = 6, height = 4, pointsize = 8)
  for(k in 1:length(sdf)){
    # do the analysis
    subdf <- sdf[[k]]
    # pad the timeseries to help with timeseries boundaries
    #   use 28 days
    subdays <- 28
    startadd <- subdf[subdf[['yearDay']] >= (365 - subdays), ]
    startadd[['yearDay']] <- startadd[['yearDay']] - 365
    endadd <- subdf[subdf[['yearDay']] <= subdays, ]
    endadd[['yearDay']] <- endadd[['yearDay']] + 365
    subdffull <- rbind(subdf,
                       startadd,
                       endadd)
    for(var in vars){
      equation <- paste(var, '~ yearDay')
      loe <- loess(formula = equation,
                   data = subdffull,
                   span = 0.5)
      ploe <- predict(loe, newdata = data.frame(yearDay = ydaypredict), se = TRUE)
      # variance predicted function modified from cran msir function loess.sd
      r <- residuals(loe)
      yearDayResid <- subdffull[['yearDay']][!is.na(subdffull[[var]])]
      modr <- loess(I(r^2) ~ yearDayResid, span = 0.5)
      sd <- sqrt(pmax(0, predict(modr, data.frame(yearDayResid = ydaypredict))))
      # save results
      varoutput[[var]][k, ] <- ploe$fit
      varoutput[[paste0(var, 'SD')]][k, ] <- sd
      # plot data with fit and +/- SD
      if (plot){
        Tlim <- range(df[['temperature']], na.rm = TRUE)
        Slim <- range(df[['salinity']], na.rm = TRUE)
        STlim <- range(df[['sigmaTheta']], na.rm = TRUE)
        cm <- colormap(z = subdffull[['year']], breaks = climyears - 0.5)
        par(mar = c(3.5, 3.5, 1, 1), oma = c(0, 1, 1, 0))
        if(var == vars[1]){
          d <- 0.24
          layout(matrix(1:(length(vars)+1), nrow = 1), widths = c(rep(1-d, length(vars)), d))
        }
        plot(subdffull[['yearDay']], subdffull[[var]],
             ylim = Tlim, pch = 20, col = cm$zcol,
             ylab = ' ', xlab = ' ')
        lines(ydaypredict, ploe$fit, col = 'red')
        lines(ydaypredict, ploe$fit + 0.5*sd, col = 'red', lty = 2)
        lines(ydaypredict, ploe$fit - 0.5*sd, col = 'red', lty = 2)
        grid()
        mtext(text = 'Year day', side = 1, line = 2.3)
        resizableLabelVar <- switch(var,
                                    'temperature' = 'T',
                                    'salinity' = 'S',
                                    'sigmaTheta' = 'sigmaTheta')
        mtext(text = resizableLabel(resizableLabelVar), side = 2, line = 2.3)
      }
    }
    if(plot) mtext(paste('depth =', names(sdf)[k]), outer = TRUE, line = -1)
    if(plot) {drawPalette(colormap = cm, zlab = '', cex = 1) ; mtext(text = 'Year', side = 4, line = 4.5, cex = 0.75)}
    # check out the data spread
    if(plot){
      par(mar = c(3.5, 3.5, 1, 1))
      year <- sdf[[k]][['year']]
      yearDay <- sdf[[k]][['yearDay']]
      dfdecade <- data.frame(year = factor(year, levels = climyears),
                             yearDay = factor(yearDay, levels = ydaypredict))
      ydayyeardist <- table(dfdecade)
      startYear <- min(climyears) + c(0, 10, 20)
      endYear <- min(climyears) + 9 * c(1, 2, 3)
      decadalTable <- mapply(function(start, end) {ok <- as.numeric(rownames(ydayyeardist)) %in% start:end;
      apply(ydayyeardist[ok, ], 2, sum)},
      startYear, endYear)
      colnames(decadalTable) <- paste(startYear, endYear, sep = '-')
      decadalTable <- t(decadalTable)
      layout(c(1))
      bp <- barplot(decadalTable, col = 1:3,
                    ylim = c(0, max(apply(decadalTable, 2, sum))),
                    legend = FALSE, xaxt = 'n')
      box()
      label <- c(1, seq(25,365,25))
      at <- bp[label]
      axis(side=1, at = at, label = label)
      mtext(text = 'Year day', side = 1, line = 2.3)
      mtext(text = 'Frequency', side = 2, line = 2.3)
    }
  } # closes iteration through each yearday, k
  if(plot) dev.off()
  # create ctd-objects for each day
  scatterDailyClimatologyCtd <- vector(mode = 'list', length = length(ydaypredict))
  # define fake date for output
  fakeDate <- seq(as.POSIXct('2020-01-01', tz = 'UTC'), as.POSIXct('2020-12-31', tz = 'UTC'), by = 'day')
  fakeDateDf <- data.frame(month = as.POSIXlt(fakeDate)$mon + 1,
                           monthDay =as.POSIXlt(fakeDate)$mday,
                           yearDay = as.POSIXlt(fakeDate)$yday + 1)
  for (iyday in 1:length(ydaypredict)){
    okdate <- fakeDateDf[['yearDay']] %in% ydaypredict[iyday]
    scatterDailyClimatologyCtd[[iyday]] <- as.ctd(salinity = varoutput[['salinity']][,iyday],
                                               temperature = varoutput[['temperature']][,iyday],
                                               pressure = head(breaks1,-1) + (deltaz/2),
                                               startTime = as.POSIXct(paste(2020, fakeDateDf[['month']][okdate], fakeDateDf[['monthDay']][okdate], sep = '-'), tz = 'UTC'))
  }
  # make a data frame of entire climatology
  dailyClimatologyDf <- NULL
  for(iv in 1:length(varoutput)){
    vo <- varoutput[[iv]]
    vodf <- as.data.frame(as.table(vo))
    colnames(vodf) <- c('depth', 'yearDay', names(varoutput)[iv])
    if(is.null(dailyClimatologyDf)){
      dailyClimatologyDf <- vodf
    } else {
      dailyClimatologyDf <- merge(dailyClimatologyDf, vodf, all = TRUE)
    }
  }
  # plot resulting daily profiles with profiles +/- daily threshold
  if(plot){
    pdf(file = paste(destDirSuppFigures, paste0('dailyClimatologyProfilePlots_', deltaz, 'dbar_', min(climyears), 'to', max(climyears),'.pdf'), sep = '/'), width = 7, height = 4, pointsize = 9)
    climyday <- as.POSIXlt(unlist(lapply(climctd, function(k) k[['startTime']])), origin = '1970-01-01', tz = 'UTC')$yday
    threshold <- 14 # number of days to check for data to plot on daily climatology profile
    Tlim <- range(unlist(lapply(scatterDailyClimatologyCtd, function(k) k[['temperature']])), na.rm = TRUE)
    Slim <- range(unlist(lapply(scatterDailyClimatologyCtd, function(k) k[['salinity']])), na.rm = TRUE)
    STlim <- range(unlist(lapply(scatterDailyClimatologyCtd, function(k) k[['sigmaTheta']])), na.rm = TRUE)
    plim <- rev(range(unlist(lapply(scatterDailyClimatologyCtd, function(k) k[['pressure']])), na.rm = TRUE))
    par(mfrow = c(1,4))
    mar <- c(3.5, 3.5, 4.5, 1.5)
    for(i in 1:length(scatterDailyClimatologyCtd)){
      pctd <- scatterDailyClimatologyCtd[[i]]
      pctdyday <- as.POSIXlt(pctd[['startTime']])$yday
      # check three conditions, for the beginning of the timeseries (ydays 0:14), inner year, and end (ydays 351:365)
      ydayrange <- (pctdyday - threshold):(pctdyday + threshold)
      ok <- (climyday - 365) %in% ydayrange  | climyday %in% ydayrange | (climyday + 365) %in% ydayrange
      # temperature
      plotProfile(pctd, xtype = 'temperature',
                  Tlim = Tlim,
                  plim = plim,
                  mar = mar)
      lapply(climctd[ok], function(k) lines(k[['temperature']], k[['pressure']], col = 'lightgrey'))
      lines(pctd[['temperature']], pctd[['pressure']], lwd = 1.4)
      # salinity
      plotProfile(pctd, xtype = 'salinity',
                  Slim = Slim,
                  plim = plim,
                  mar = mar)
      lapply(climctd[ok], function(k) if('salinity' %in% names(k@data)) {lines(k[['salinity']], k[['pressure']], col = 'lightgrey')})
      lines(pctd[['salinity']], pctd[['pressure']], lwd = 1.4)
      # sigmaTheta
      plotProfile(pctd, xtype = 'sigmaTheta',
                  densitylim = STlim,
                  plim = plim,
                  mar = mar)
      lapply(climctd[ok], function(k) if('salinity' %in% names(k@data)) {lines(k[['sigmaTheta']], k[['pressure']], col = 'lightgrey')})
      lines(pctd[['sigmaTheta']], pctd[['pressure']], lwd = 1.4)
      # T-S
      plotTS(pctd, Tlim = Tlim, Slim = Slim, mar = mar)
      lapply(climctd[ok], function(k) if('salinity' %in% names(k@data)) {points(k[['salinity']], k[['temperature']], col = 'lightgrey', pch = 20)})
      points(pctd[['salinity']], pctd[['temperature']], pch = 20)
      mtext(text = format(pctd[['startTime']], '%m-%d'), side = 3, outer = TRUE, line = -1.5)
    }
    dev.off()
  } # closes plotting daily climatology with data
  # save output
  climatology[[ic]][['dailyData']] <- sdf
  climatology[[ic]][['dailyClimatologyCtd']] <- scatterDailyClimatologyCtd
  climatology[[ic]][['dailyClimatologyDf']] <- dailyClimatologyDf
  climatology[[ic]][['climatologyYears']] <- climyears
}
save(climatology, file = paste(destDirData, 'dailyClimatology.rda', sep = '/'))