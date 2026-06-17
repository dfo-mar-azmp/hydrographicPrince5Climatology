rm(list=ls())
source('00_setupFile.R')
# load data
load(paste(destDirData, 'dailyClimatology.rda', sep = '/'))
# get the breaks in the daily data
breaks <- lapply(climatology, function(k) names(k[['dailyData']]))
ubreaks <- unique(unlist(breaks))
# define lookdepths to pull out the fitted values
#     it's going to be the mean of each break
ubreaksplit <- strsplit(ubreaks, split = ',')
minbreakdepth <- as.numeric(unlist(lapply(ubreaksplit, function(k) gsub('\\((.*)', '\\1', k[1]))))
maxbreakdepth <- as.numeric(unlist(lapply(ubreaksplit, function(k) gsub('(.*)\\]', '\\1', k[2]))))
breakdepthsm <- matrix(data = c(minbreakdepth, maxbreakdepth), nrow = length(minbreakdepth), ncol = 2)
lookdepths <- apply(breakdepthsm, 1, mean)
# define variables
vars <- c('temperature', 'salinity', 'sigmaTheta')
# define some limits
yearlim <- c(1981, 2020)
yearseq <- seq(yearlim[1], yearlim[2], 1)
yeardaylim <- c(1, 365)
yeardayseq <- seq(yeardaylim[1], yeardaylim[2], 1)
Tlim <- range(unlist(lapply(climatology, function(k) unlist(lapply(k[['dailyData']], '[[', 'temperature')))), na.rm = TRUE)
Slim <- range(unlist(lapply(climatology, function(k) unlist(lapply(k[['dailyData']], '[[', 'salinity')))), na.rm = TRUE)
STlim <- range(unlist(lapply(climatology, function(k) unlist(lapply(k[['dailyData']], '[[', 'sigmaTheta')))), na.rm = TRUE)
nlim <- c(0,
          max(unlist(lapply(climatology, function(k) unlist(lapply(k[['dailyData']], function(kk) unlist(lapply(vars, function(kkk) as.vector(table(kk[['yearDay']][!is.na(kk[[kkk]])]))))))))))
# define other parameters
mtextcex <- 4/5
for(ib in 1:length(ubreaks)){
  lookbreak <- ubreaks[ib]
  lookdepth <- lookdepths[ib]
  # initiate png output
  lookbreaks <- strsplit(lookbreak, split = ',')[[1]]
  minbreak <- as.numeric(gsub('\\((.*)', '\\1', lookbreaks[1]))
  maxbreak <- as.numeric(gsub('(.*)\\]', '\\1', lookbreaks[2]))
  filename <- paste0('04a_dailyClimatologyFit_',
                     paste(minbreak, maxbreak, sep = 'to'),
                     '.png')
  png(filename = paste(destDirFigures, filename, sep = '/'),
      width = 8.5, height = 6, units = 'in',
      res = 250, pointsize = 9)
  # initialize plotting area
  mlay <- matrix(c(1,3,5,0,
            2, 4, 6,13,
            7,9,11,0,
            8, 10, 12, 14), nrow = 4, ncol = 4, byrow = TRUE)
  palwid <- 0.18
  layout(mlay, widths = c(rep(1-palwid, 3), palwid))
  # set graphical parameters
  oma <- c(3.5, 1.5, 3, 0.75)
  mar <- c(0.5, 4.5, 0.5, 0)
  palmar <- c(0.5, 0, 0.5, 6)
  par(mar = mar, oma = oma)
  # set cex for nice palette font size
  par(cex = 0.8)
  for(ic in 1:length(climatology)){
    d <- climatology[[ic]]
    # get the daily data
    okc <- which(names(d[['dailyData']]) == lookbreak)
    dd <- d[['dailyData']][[okc]]
    # get the fitted data
    okf <- which(d[['dailyClimatologyDf']][['depth']] == lookdepth)
    f <- d[['dailyClimatologyDf']][okf, ]
    ## order fitted data by yearDay
    of <- order(as.numeric(as.character(f[['yearDay']])))
    f <- f[of, ]
    for(var in vars){
      R <- ']'
      L <- '['
      zlab <- switch(var,
                     'temperature'= bquote(bold(.(gettext('Temperature', domain = 'R-oce')) * .(L) * degree * "C" * .(R))),
                     'salinity' = bquote(bold(.(gettext('Practical Salinity', domain = 'R-oce')))),
                     'sigmaTheta' = bquote(bold(sigma[theta] *' '* .(L) * kg/m^3 * .(R))))
      # plot barplot
      year <- dd[['year']][!is.na(dd[[var]])]
      yearDay <- dd[['yearDay']][!is.na(dd[[var]])]
      dfdecade <- data.frame(year = factor(year, levels = yearseq),
                             yearDay = factor(yearDay, levels = yeardayseq))
      ydayyeardist <- table(dfdecade)
      startYear <- min(yearseq) + c(0, 10, 20, 30)
      endYear <- startYear + 9
      decadalTable <- mapply(function(start, end) {ok <- as.numeric(rownames(ydayyeardist)) %in% start:end;
      apply(ydayyeardist[ok, ], 2, sum)},
      startYear, endYear)
      colnames(decadalTable) <- paste(startYear, endYear, sep = '-')
      decadalTable <- t(decadalTable)
      bp <- barplot(decadalTable, col = 1:3,
                    ylim = nlim,
                    legend = FALSE, xaxt = 'n')
      box()
      label <- c(1, seq(25,365,25))
      at <- bp[label]
      axis(side=1, at = at, label = FALSE)
      #mtext(text = 'Year day', side = 1, line = 2.3, cex = mtextcex)
      mtext(text = 'Number of observations', side = 2, line = 2.3, cex = mtextcex)
      # label variable
      if(ic == 1){
        mtext(text = zlab, side = 3, line = 1.3)

      }
      # label climatology reference period
      if(var == 'temperature'){
        mtext(text = paste(range(climatology[[ic]][['climatologyYears']]), collapse = ' to '),
              side = 2, line = 3.9, font = 2)
      }
      # plot data with fits
      ylim <- switch(var,
                     'temperature' = Tlim,
                     'salinity' = Slim,
                     'sigmaTheta' = STlim)
      # colormap
      ## viridis
      # cm <- colormap(z = dd[['year']], breaks = yearseq - 0.5, col = hcl.colors(n = length(yearseq), palette = 'Viridis'))
      ## by overlapping decades
      cmcol <- hcl.colors(5, palette = 'RdYlGn')[c(1, 2, 5)]
      cmcoladj <- colorspace::lighten(cmcol, amount = 0.5)
      cm <- colormap(z = dd[['year']], breaks = c(1981, 1991, 2011, 2020), col = cmcoladj)
      plot(dd[['yearDay']], dd[[var]],
           ylim = ylim, xlim = yeardaylim,
           pch = 20, col = cm$zcol,
           ylab = ' ', xlab = ' ',
           xaxt = 'n', yaxt = 'n')
      lines(f[['yearDay']], f[[var]], col = 'black', lwd = 1.4)
      lines(f[['yearDay']], f[[var]] + 0.5*f[[paste0(var, 'SD')]], col = 'black', lty = 2, lwd = 1.4)
      lines(f[['yearDay']], f[[var]] - 0.5*f[[paste0(var, 'SD')]], col = 'black', lty = 2, lwd = 1.4)
      grid()
      # x-axis
      ## same as barplot
      axis(side = 1, at = label, labels = ifelse(ic == length(climatology), TRUE, FALSE))
      ## label
      if(ic == length(climatology)){
        mtext(text = 'Year day', side = 1, line = 2.3, cex = mtextcex)
      }
      # y-axis
      ## axis
      axis(2)
      ## label
      resizableLabelVar <- switch(var,
                                  'temperature' = 'T',
                                  'salinity' = 'S',
                                  'sigmaTheta' = 'sigmaTheta')
      mtext(text = resizableLabel(item = resizableLabelVar,
                                  axis = 'y'),
            side = 2, line = 2.3,
            cex = mtextcex)
      # label climatology reference period
      if(var == 'temperature'){
        mtext(text = paste(range(climatology[[ic]][['climatologyYears']]), collapse = ' to '),
              side = 2, line = 3.9, font = 2)
      }
    }
  }
  # draw palettes
  par(mfg = c(4,1)) # plot window 11
  par(mar = palmar)

  # drawPalette(zlim = yearlim,
  #             breaks = c(yearseq, yearseq[length(yearseq)] + 1) - 0.5,
  #             col = hcl.colors(n = length(yearseq), palette = 'Viridis'),
  #             zlab = '')
  cmcol <- hcl.colors(5, palette = 'RdYlGn')[c(1, 2, 5)]
  cmcoladj <- colorspace::lighten(cmcol, amount = 0.5)
  drawPalette(zlim = yearlim,
              breaks = c(1981, 1991, 2011, 2020),
              at = c(1981, 1991, 2010, 2020),
              col = cmcoladj,
              zlab = '')
  mtext(text = 'Year', side = 4, line = 9, cex = 4/5)
  par(mfg = c(4,2)) # plot window 12
  par(mar= palmar)
  # drawPalette(zlim = yearlim,
  #             breaks = c(yearseq, yearseq[length(yearseq)] + 1) - 0.5,
  #             col = hcl.colors(n = length(yearseq), palette = 'Viridis'),
  #             zlab = '')
  cmcol <- hcl.colors(5, palette = 'RdYlGn')[c(1, 2, 5)]
  cmcoladj <- colorspace::lighten(cmcol, amount = 0.5)
  drawPalette(zlim = yearlim,
              breaks = c(1981, 1991, 2011, 2020),
              at = c(1981, 1991, 2010, 2020),
              col = cmcoladj,
              zlab = '')
  mtext(text = 'Year', side = 4, line = 9, cex = 4/5)
  dev.off()
}
