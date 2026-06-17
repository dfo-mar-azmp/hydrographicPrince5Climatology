rm(list=ls())
source('00_setupFile.R')
# get files
climFiles <- list.files(path = destDirData,
                        pattern = 'depthAverageClimatologyDailyScatter.*',
                        full.names = TRUE)
dailyIntLimits <- unlist(lapply(climFiles, function(k) gsub("depthAverageClimatologyDailyScatter_(.*)\\.rda", '\\1', basename(k))))
climMonthlyFiles <- list.files(path = destDirData,
                               pattern = 'depthAverageClimatologyMonthly.*',
                               full.names = TRUE)
monthlyIntLimits <- unlist(lapply(climMonthlyFiles, function(k) gsub("depthAverageClimatologyMonthly_(.*)\\.rda", '\\1', basename(k))))
# define variables
vars <- c('integratedTemperature', 'integratedSalinity',
          'stratification', 'stratificationIndex')
# define some limits
yearlim <- c(1981, 2020)
yearseq <- seq(yearlim[1], yearlim[2], 1)
yeardaylim <- c(1, 365)
yeardayseq <- seq(yeardaylim[1], yeardaylim[2], 1)
# define other parameters
mtextcex <- 4/5
for(icf in 1:length(climFiles)){
  # get depth min/max limits used for calculations from filename
  integrationLimits <- dailyIntLimits[icf]
  # match monthly file
  okmonth <- which(monthlyIntLimits == integrationLimits)
  # load monthly file
  ## have to do this first since the saved variable is the same as daily
  cat(paste("loading", climMonthlyFiles[okmonth]), sep = '\n')
  load(climMonthlyFiles[okmonth])
  ## re-name monthly climatology var
  depthAverageMonthlyClimatology <- depthAverageClimatology
  # load daily file
  cat(paste("loading", climFiles[icf]), sep = '\n')
  load(climFiles[icf])
  # get limits for each variable for both climatology
  #   only need to use daily climatology since it's the same data
  ylims <- lapply(vars, function(k)
    range(unlist(lapply(depthAverageClimatology, function(kk)
      kk[['dailyData']][k])), na.rm = TRUE))
  names(ylims) <- vars
  # get number of observation limit
  nlim <- c(0,
            max(unlist(lapply(depthAverageClimatology, function(k) unlist(lapply(vars, function(kkk) as.vector(table(k[['dailyData']][['yearDay']][!is.na(k[['dailyData']][[kkk]])]))))))))
  nlimmonth <- c(0,
                 max(unlist(lapply(depthAverageClimatology, function(k) unlist(lapply(vars, function(kkk) as.vector(table(k[['dailyData']][['month']][!is.na(k[['dailyData']][[kkk]])]))))))))
  # calculate difference between climatology periods
  ## daily
  climMinYear <- unlist(lapply(depthAverageClimatology, function(k) min(k[['climatologyYears']])))
  ## want the max year minus the min year
  ok1 <- which.max(climMinYear)
  ok2 <- which.min(climMinYear)
  df1 <- depthAverageClimatology[[ok1]][['dailyClimatologyDf']]
  df2 <- depthAverageClimatology[[ok2]][['dailyClimatologyDf']]
  # merging by yearday, so re-name everything
  ## for ease i'll name variables .*1 and .*2
  names(df1)[!names(df1) %in% 'yearDay'] <- paste0(names(df1)[!names(df1) %in% 'yearDay'], '1')
  names(df2)[!names(df2) %in% 'yearDay'] <- paste0(names(df2)[!names(df2) %in% 'yearDay'], '2')
  ## merge
  dfdc <- merge(df1, df2, by = 'yearDay', all = TRUE)
  ## order by day
  dfdc <- dfdc[order(dfdc[['yearDay']]), ]
  ## monthly
  climMinYear <- unlist(lapply(depthAverageMonthlyClimatology, function(k) min(k[['climatologyYears']])))
  ## want the max year minus the min year
  ok1 <- which.max(climMinYear)
  ok2 <- which.min(climMinYear)
  df1 <- depthAverageMonthlyClimatology[[ok1]][['monthlyClimatologyDf']]
  df2 <- depthAverageMonthlyClimatology[[ok2]][['monthlyClimatologyDf']]
  # merging by month, so re-name everything
  ## for ease i'll name variables .*1 and .*2
  names(df1)[!names(df1) %in% 'month'] <- paste0(names(df1)[!names(df1) %in% 'month'], '1')
  names(df2)[!names(df2) %in% 'month'] <- paste0(names(df2)[!names(df2) %in% 'month'], '2')
  ## merge
  dfmc <- merge(df1, df2, by = 'month', all = TRUE)
  ## order by month
  dfmc <- dfmc[order(dfmc[['month']]), ]
  for(var in vars){
    # calculate difference and standard deviation
    ## daily
    vardiff <- dfdc[[paste0(var, 1)]] - dfdc[[paste0(var, 2)]]
    varsumsqsd <- (dfdc[[paste0(var, 'SD1')]])^2 + (dfdc[[paste0(var, 'SD2')]])^2
    vardiffsd <- sqrt(varsumsqsd)
    dailyDifference <- data.frame(yearDay = dfdc[['yearDay']],
                                  mean = vardiff,
                                  sd = vardiffsd)
    ## monthly
    vardiff <- dfmc[[paste0(var, 1)]] - dfmc[[paste0(var, 2)]]
    varsumsqsd <- (dfmc[[paste0(var, 'SD1')]])^2 + (dfmc[[paste0(var, 'SD2')]])^2
    vardiffsd <- sqrt(varsumsqsd)
    monthlyDifference <- data.frame(month = dfmc[['month']],
                                  mean = vardiff,
                                  sd = vardiffsd)
    ## get difference limit
    difflim <- max(abs(c(unlist(lapply(c(0.5, -0.5), function(k) dailyDifference[['mean']] + (k*dailyDifference[['sd']]))),
                       unlist(lapply(c(0.5, -0.5), function(k) monthlyDifference[['mean']] + (k*monthlyDifference[['sd']]))))),
                     na.rm = TRUE) * c(-1, 1)
    # initiate png output
    filename <- paste0('07a_depthAverageClimatology_',
                        var,
                        '_',
                       integrationLimits,
                       '.png')
    png(filename = paste(destDirFigures, filename, sep = '/'),
        width = 11, height = 6, units = 'in',
        res = 250, pointsize = 9)
    # initialize plotting area
    mlay <- matrix(c(1, 2, 11, 7, 6,
                     3, 4, 12, 9, 8,
                     0, 5, 0, 10, 0),
                   nrow = 3, ncol = 5, byrow = TRUE)
    palwid <- 0.20
    layout(mlay, widths = c(rep(1-palwid, 2), palwid, rep(1-palwid, 2)))
    # set graphical parameters
    oma <- c(3.5, 1.5, 3, 0.75)
    mar <- c(0.5, 4.5, 0.5, 0)
    palmar <- c(1, 12, 1, 0) # tighter x-axis lims so labels don't overlap
    par(mar = mar, oma = oma)
    # set cex for nice palette font size
    par(cex = 0.8)
    # define labels
    varlabel <- switch(var,
                       'integratedTemperature' = resizableLabel(item = 'T', axis = 'y'),
                       'integratedSalinity' = resizableLabel(item = 'S', axis = 'y'),
                       'stratification' = bquote('Stratification' *' '* .(L) * kg/m^3 * .(R)),
                       'stratificationIndex' = bquote('Stratification Index' *' '* .(L) * kg/m^4 * .(R)))
    # plot daily nobs and fits
    for(ic in 1:length(depthAverageClimatology)){
      d <- depthAverageClimatology[[ic]]
      # get the daily data
      dd <- d[['dailyData']]
      # get the fitted data
      f <- d[['dailyClimatologyDf']]
      ## order fitted data by yearDay (should be already, but for safety)
      of <- order(as.numeric(as.character(f[['yearDay']])))
      f <- f[of, ]
      R <- ']'
      L <- '['
      zlab <- switch(var,
                     'integratedTemperature'= bquote(bold(.(gettext('Temperature', domain = 'R-oce')) * .(L) * degree * "C" * .(R))),
                     'integratedSalinity' = bquote(bold(.(gettext('Practical Salinity', domain = 'R-oce')))),
                     'stratification' = bquote(bold('Stratification' *' '* .(L) * kg/m^3 * .(R))),
                     'stratificationIndex' = bquote(bold('Stratification Index' *' '* .(L) * kg/m^4 * .(R))))
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
      # x-axis
      ## axis
      axis(side = 1, at = at, labels = FALSE)
      ## label
      if(ic == length(depthAverageClimatology)){
        axis(side = 1, at = at, labels = label)
        mtext(text = 'Year day', side = 1, line = 2.3, cex = mtextcex)
      }
      #mtext(text = 'Year day', side = 1, line = 2.3, cex = mtextcex)
      mtext(text = 'Number of observations', side = 2, line = 2.3, cex = mtextcex)
      # label climatology reference period
      mtext(text = paste(range(depthAverageClimatology[[ic]][['climatologyYears']]), collapse = ' to '),
            side = 2, line = 3.9, font = 2)
      # plot data with fits
      # colormap
      ## by overlapping decades
      cmcol <- hcl.colors(5, palette = 'RdYlGn')[c(1, 2, 5)]
      cmcoladj <- colorspace::lighten(cmcol, amount = 0.5)
      cm <- colormap(z = dd[['year']], breaks = c(1981, 1991, 2011, 2020), col = cmcoladj)
      plot(dd[['yearDay']], dd[[var]],
           ylim = ylims[[var]], xlim = yeardaylim,
           pch = 20, col = cm$zcol,
           ylab = ' ', xlab = ' ',
           xaxt = 'n', yaxt = 'n')
      lines(f[['yearDay']], f[[var]], col = 'black', lwd = 1.4)
      lines(f[['yearDay']], f[[var]] + 0.5*f[[paste0(var, 'SD')]], col = 'black', lty = 2, lwd = 1.4)
      lines(f[['yearDay']], f[[var]] - 0.5*f[[paste0(var, 'SD')]], col = 'black', lty = 2, lwd = 1.4)
      grid()
      # x-axis
      ## same as barplot
      #axis(side = 1, at = label, labels = ifelse(ic == length(depthAverageClimatology), TRUE, FALSE))
      axis(side = 1, at = label, labels = FALSE)
      ## label
      # if(ic == length(depthAverageClimatology)){
      #   mtext(text = 'Year day', side = 1, line = 2.3, cex = mtextcex)
      # }
      # y-axis
      ## axis
      axis(2)
      ## label
      mtext(text = varlabel,
            side = 2, line = 2.3,
            cex = mtextcex)
    } # closes depthAverageClimatology (daily)
    # plot climatology difference (daily)
    plot(dailyDifference[['yearDay']], dailyDifference[['mean']],
         ylim = difflim, xlim = yeardaylim,
         type = 'l', col = cm$zcol, lwd = 1.4,
         ylab = ' ', xlab = ' ',
         xaxt = 'n', yaxt = 'n')
    grid()
    lines(dailyDifference[['yearDay']], dailyDifference[['mean']], col = 'black', lwd = 1.4)
    lines(dailyDifference[['yearDay']], dailyDifference[['mean']] + 0.5*dailyDifference[['sd']], col = 'black', lty = 2, lwd = 1.4)
    lines(dailyDifference[['yearDay']], dailyDifference[['mean']] - 0.5*dailyDifference[['sd']], col = 'black', lty = 2, lwd = 1.4)
    # x-axis
    ## same as barplot
    axis(side = 1, at = label)
    ## label
    mtext(text = 'Year day', side = 1, line = 2.3, cex = mtextcex)
    # y-axis
    ## axis
    axis(2)
    ## label
    mtext(text = varlabel,
          side = 2, line = 2.3,
          cex = mtextcex)
    # plot monthly with nobs
    for(imc in 1:length(depthAverageMonthlyClimatology)){
      d <- depthAverageMonthlyClimatology[[imc]]
      # get the daily data
      dd <- d[['dailyData']]
      # get the fitted data
      f <- d[['monthlyClimatologyDf']]
      ## order fitted data by month (should be already, but for safety)
      of <- order(as.numeric(as.character(f[['month']])))
      f <- f[of, ]
      R <- ']'
      L <- '['
      # plot line plot of nobs per month
      varmonth <- dd[['month']][!is.na(dd[[var]])]
      monthTable <- table(varmonth)
      # define fake-x
      fakeYear <- 1990
      fakeDay <- 15
      fakeX <- as.POSIXct(paste(fakeYear, as.numeric(names(monthTable)), fakeDay, sep = '-'), tz = 'UTC')
      fakeXlim <- c(as.POSIXct(paste(fakeYear, 1, 1, sep = '-'), tz = 'UTC'),
                    as.POSIXct(paste(fakeYear, 12, 31, sep = '-'), tz = 'UTC'))
      # plot
      plot(x = fakeX,
           y = as.vector(monthTable),
           type = 'o',
           lty = 1,
           pch = 21,
           bg = 'white',
           xlim = fakeXlim,
           ylim = nlimmonth,
           xlab = '',
           ylab = '',
           xaxt = 'n')
      # add grid
      abline(v = fakeX, lty = 3, col = 'lightgrey')
      abline(h = pretty(nlimmonth), lty = 3, col = 'lightgrey')
      ## re-add data
      lines(x = fakeX,
            y = as.vector(monthTable),
            type = 'o',
            lty = 1,
            pch = 21,
            bg = 'white')
      # x-axis
      ## add axis
      axis.POSIXct(side = 1, at = fakeX, format = '%b', labels = ifelse(imc == length(depthAverageMonthlyClimatology), TRUE, FALSE))
      if(imc == length(depthAverageMonthlyClimatology)){
        mtext(text = 'Month',
              side = 1, line = 2.3,
              cex = mtextcex)
      }
      # y-axis
      ## add label
      mtext("Number of profiles",
            side = 2,
            line = 2.3,
            cex = mtextcex)
      label <- c(1, seq(25,365,25))
      at <- bp[label]
      axis(side=1, at = at, label = FALSE)
      # plot data with fits
      # colormap
      ## by overlapping decades
      cmcol <- hcl.colors(5, palette = 'RdYlGn')[c(1, 2, 5)]
      cmcoladj <- colorspace::lighten(cmcol, amount = 0.5)
      cm <- colormap(z = dd[['year']], breaks = c(1981, 1991, 2011, 2020), col = cmcoladj)
      fakeddDate <- as.POSIXct(as.Date(dd[['yearDay']], origin = paste0(fakeYear, '-01-01')))
      plot(fakeddDate, dd[[var]],
           ylim = ylims[[var]], xlim = fakeXlim,
           pch = 20, col = cm$zcol,
           ylab = ' ', xlab = ' ',
           xaxt = 'n', yaxt = 'n')
      fakeXf <- as.POSIXct(paste(fakeYear, f[['month']], fakeDay, sep = '-'), tz = 'UTC')
      # add grid
      abline(v = fakeXf, lty = 3, col = 'lightgrey')
      abline(h = pretty(ylims[[var]]), lty = 3, col = 'lightgrey')
      ## re-add data
      points(x = fakeddDate,
            y = dd[[var]],
            pch = 20,
            col = cm$zcol)
      # add climatology
      lines(fakeXf, f[[var]], col = 'black', lwd = 1.4)
      lines(fakeXf, f[[var]] + 0.5*f[[paste0(var, 'SD')]], col = 'black', lty = 2, lwd = 1.4)
      lines(fakeXf, f[[var]] - 0.5*f[[paste0(var, 'SD')]], col = 'black', lty = 2, lwd = 1.4)
      # x-axis
      ## add axis
      axis.POSIXct(side = 1, at = fakeX, format = '%b', labels = FALSE)
      # y-axis
      ## axis
      axis(2)
      ## label
      mtext(text = varlabel,
            side = 2, line = 2.3,
            cex = mtextcex)
    } # closes depthAverageMonthlyClimatology
    # plot climatology difference (monthly)
    fakeXdiff <- as.POSIXct(paste(fakeYear, monthlyDifference[['month']], fakeDay, sep = '-'), tz = 'UTC')
    plot(fakeXdiff, monthlyDifference[['mean']],
         ylim = difflim, xlim = fakeXlim,
         type = 'l', col = 'black', lwd = 1.4,
         ylab = ' ', xlab = ' ',
         xaxt = 'n', yaxt = 'n')
    # add grid
    abline(v = fakeXdiff, lty = 3, col = 'lightgrey')
    abline(h = pretty(difflim), lty = 3, col = 'lightgrey')
    box()
    # add data
    lines(fakeXdiff, monthlyDifference[['mean']], col = 'black', lwd = 1.4)
    lines(fakeXdiff, monthlyDifference[['mean']] + 0.5*monthlyDifference[['sd']], col = 'black', lty = 2, lwd = 1.4)
    lines(fakeXdiff, monthlyDifference[['mean']] - 0.5*monthlyDifference[['sd']], col = 'black', lty = 2, lwd = 1.4)
    # x-axis
    ## axis
    axis.POSIXct(side = 1, at = fakeX, format = '%b')
    ## label
    mtext(text = 'Month',
          side = 1, line = 2.3,
          cex = mtextcex)
    # y-axis
    ## axis
    axis(2)
    ## label
    mtext(text = varlabel,
          side = 2, line = 2.3,
          cex = mtextcex)
    # draw palettes
    par(mfg = c(3,1)) # plot window 17
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
    mtext(text = 'Year', side = 4, line = -1.2, cex = 4/5)
    par(mfg = c(3,2)) # plot window 18
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
    mtext(text = 'Year', side = 4, line = -1.2, cex = 4/5)
    dev.off()
  } # closes var
}
