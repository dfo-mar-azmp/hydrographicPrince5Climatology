# note this isn't a formal function, it was written to save space in script and to avoid drift
plotClimatology <- function(sg, ylim = NULL){
  if(is.null(ylim)){
    ylim <- range(sg[['pressure', 'byStation']])[[1]]
  }
  year <- as.POSIXlt(sg[['startTime']])$year + 1900
  # need to find most frequent year
  dfyear <- data.frame(table(year))
  xlimyear <- as.numeric(paste(dfyear[['year']][which.max(dfyear[['Freq']])]))
  

  xlim <- c(as.POSIXct(paste(xlimyear, '01', '01', sep = '-'), tz = 'UTC'),
            as.POSIXct(paste(xlimyear, '12', '31', sep = '-'), tz = 'UTC'))


  vars <- c('temperature',
            'salinity',
            'sigmaTheta')
  par(mfrow=c(3,1), oma = c(1, 0, 2, 0))
  for(var in vars){
    zlim <- station2PlotLimits[['limits']][[var]]
    levels <- station2PlotLimits[['contourLevels']][[var]]
    levelLimits <- station2PlotLimits[['contourLevelLimits']][[var]]
    R <- ']'
    L <- '['
    zlab <- switch(var,
                   'temperature'= bquote(bold(.(gettext('Temperature', domain = 'R-oce')) * .(L) * degree * "C" * .(R))),
                   'temperatureAnomaly' = getAnomalyLabel('temperatureAnomaly', bold = TRUE),
                   'salinity' = bquote(bold(.(gettext('Practical Salinity', domain = 'R-oce')))),
                   'salinityAnomaly' = getAnomalyLabel('salinityAnomaly', bold = TRUE),
                   'sigmaTheta' = bquote(bold(sigma[theta] *' '* .(L) * kg/m^3 * .(R))),
                   'sigmaThetaAnomaly' = getAnomalyLabel('sigmaThetaAnomaly', bold = TRUE))
    # mar <- switch(var,
    #               'temperature' = c(1.5, 3.5, 2, 1.5),
    #               'salinity' = c(2.5, 3.5, 1, 1.5))
    axes <- switch(var,
                   'temperature' = FALSE,
                   'salinity' = FALSE,
                   'sigmaTheta' = TRUE)
    ylab <- switch(var,
                   'temperature' = TRUE,
                   'temperatureAnomaly' = FALSE,
                   'salinity' = TRUE,
                   'salinityAnomaly' = FALSE,
                   'sigmaTheta' = TRUE,
                   'sigmaThetaAnomaly' = FALSE)
    zcol <- switch(var,
                   'temperature' = oceColorsJet,
                   'temperatureAnomaly'= anomCol,
                   'salinity' = oceColorsJet,
                   'salinityAnomaly' = anomCol,
                   'sigmaTheta' = oceColorsJet,
                   'sigmaThetaAnomaly' = anomCol)
    zbreaks <- switch(var,
                      'temperature'= NULL, #seq(Tlim[1], Tlim[2],1),
                      'temperatureAnomaly'= seq(-7,7,1),
                      'salinity'= NULL, #seq(Slim[1], Slim[2]),
                      'salinityAnomaly' = c(anomcsv$LowerRange, anomcsv$UpperRange[length(anomcsv$UpperRange)]),
                      'sigmaTheta' = NULL, #seq(STlim[1], STlim[2],1),
                      'sigmaThetaAnomaly' = c(anomcsv$LowerRange, anomcsv$UpperRange[length(anomcsv$UpperRange)]))
    
    mar <- c(2, 3.5, 1, 2)
    plot(sg, which = var, ztype = 'image', 
         zlim = zlim, ylim = ylim, xlim = xlim,
         xtype = 'time', zcol = zcol, zbreaks = zbreaks,
         legend.loc = '', ylab = '',
         axes = FALSE, xlab = '', mar = mar,
         stationTicks = FALSE, drawPalette = FALSE)
    
    clx <- sg[['startTime', 'byStation']]
    
    clxlab <- as.POSIXct(paste(xlimyear, 1:12, '01', sep = '-'), tz = 'UTC')
    
    
    cly <- sg[['station',1]][['pressure']]
    clz <- matrix(sg[[var]], byrow = TRUE, nrow = length(sg[['station']]))
    contour(clx, cly, clz, levels = levels[levels > levelLimits[1] & levels < levelLimits[2] ], 
            col = 'black', add = TRUE, labcex = 0.8, 
            vfont = c('sans serif', 'bold'))
    contour(clx, cly, clz, levels = levels[levels <= levelLimits[1] | levels >= levelLimits[2]],
            col = 'white', add = TRUE, labcex = 0.8, 
            vfont = c('sans serif', 'bold'))
    pylim <- pretty(ylim)
    aty <- pylim[pylim >= min(ylim) & pylim <= max(ylim)]
    if(ylab){
      axis(side = 2, at = aty)
      mtext(side = 2, text = resizableLabel('depth'), line = 2, cex = 1)
    } else {
      axis(side = 2, at = aty, labels = FALSE)
    }
    axis(side = 4, at = aty, labels = FALSE)
    
    {if(!axes){
      axis(side = 1, at = clxlab, labels = FALSE)
    }else{
      mlab <- substr(month.abb, 1, 1)
      axis.POSIXct(side = 1, at = clxlab, labels = mlab)
      }}
  
    mtext(side = 3, text = zlab, cex = 1, line = 0.6)
  }
}
