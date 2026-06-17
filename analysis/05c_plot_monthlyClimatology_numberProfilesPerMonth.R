rm(list=ls())
library(oce)
source('00_setupFile.R')
# load data
load(paste(destDirData, 'monthlyClimatologyWAnomaly.rda', sep = '/'))
# get number of profiles per month and variable included in climatology calculations
## define variables
vars <- c('temperature', 'salinity', 'sigmaTheta')
## define months
months <- 1:12
## iterate through each variable
varnctd <- vector(mode = 'list', length = length(vars))
for(iv in 1:length(vars)){
  var <- vars[iv]
  ### define output
  output <- matrix(data = NA, nrow = 2, ncol = length(months))
  ### iterate through each month
  for(im in 1:length(months)){
    lookmonth <- months[im]
    #### iterate through each climatology period
    for(ic in 1:2){
      dctd <- climatology[[ic]][['allctd']]
      startTime <- as.POSIXlt(unlist(lapply(dctd, '[[', 'startTime')), tz = 'UTC')
      ctdmonth <- startTime$mon + 1
      okctd <- ctdmonth == lookmonth
      mctd <- dctd[okctd]
      hasvar <- unlist(lapply(mctd, function(k) all(!is.na(k[[var]]))))
      ###### save nctd
      output[ic, im] <- length(mctd[hasvar])
    }
  }
  ### define row and column names
  rownames <- unlist(lapply(climatology[1:2], function(k) paste(range(k[['climatologyYears']]), collapse = '-')))
  colnames <- months
  rownames(output) <- rownames
  colnames(output) <- colnames
  ### save output
  varnctd[[iv]] <- output
}
names(varnctd) <- vars
# initiate png output
png(filename = paste(destDirFigures,
                     '06c_numberProfilesPerMonth.png',
                     sep = '/'),
    width = 11, height = 3, units = 'in',
    res = 200, pointsize = 10)
# iterate through each variable and plot the number of profiles for each climatology period
## define graphical parameters
mfrow <- c(1,3)
oma <- c(2, 2, 2, 0)
mar <- c(1.5, 3.5, 1.5, 2)
par(mfrow = mfrow, mar = mar, oma = oma)
par(cex = 0.8) # for nice font size
ylim <- range(unlist(varnctd), na.rm = TRUE)
for(i in 1:length(varnctd)){
  d <- varnctd[[i]]
  # define fake-x
  fakeYear <- 1990
  fakeDay <- 15
  fakeX <- as.POSIXct(paste(fakeYear, colnames(d), fakeDay, sep = '-'), tz = 'UTC')
  fakeXlim <- c(as.POSIXct(paste(fakeYear, 1, 1, sep = '-'), tz = 'UTC'),
                as.POSIXct(paste(fakeYear, 12, 31, sep = '-'), tz = 'UTC'))
  for(ic in 1:dim(d)[1]){
    if(ic == 1){
      plot(x = fakeX,
           y = as.vector(d[ic, ]),
           type = 'o',
           lty = ic,
           pch = 21,
           bg = 'white',
           xlim = fakeXlim,
           ylim = ylim,
           xlab = '',
           ylab = '',
           xaxt = 'n')
      # add grid
      abline(v = fakeX, lty = 3, col = 'lightgrey')
      abline(h = pretty(ylim), lty = 3, col = 'lightgrey')
      ## re-add data
      lines(x = fakeX,
            y = as.vector(d[ic, ]),
            type = 'o',
            lty = ic,
            pch = 21,
            bg = 'white')
      # x-axis
      ## add axis
      axis.POSIXct(side = 1, at = fakeX, format = '%b')
      ## add label
      mtext("Month",
            side = 1,
            line = 2.3)
      # y-axis
      ## add label
      mtext("Number of Profiles",
            side = 2,
            line = 2.3)
      # label variable
      R <- ']'
      L <- '['
      varlab <- switch(names(varnctd)[i],
                     'temperature'= bquote(bold(.(gettext('Temperature', domain = 'R-oce')) * .(L) * degree * "C" * .(R))),
                     'salinity' = bquote(bold(.(gettext('Practical Salinity', domain = 'R-oce')))),
                     'sigmaTheta' = bquote(bold(sigma[theta] *' '* .(L) * kg/m^3 * .(R))))
      mtext(text = varlab, side = 3, line = 1.3)
    } else {
      lines(x = fakeX,
            y = as.vector(d[ic, ]),
            type = 'o',
            lty = ic,
            pch = 21,
            bg = 'white')
    }
  }
  legend("topright",
         lty = 1:dim(d)[1],
         pch = 21,
         col = 'black',
         pt.bg = 'white',
         legend = rownames(d))
}
dev.off()