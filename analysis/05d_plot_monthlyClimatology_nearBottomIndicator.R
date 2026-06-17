rm(list=ls())
library(oce)
data("ctd")
ghostctd <- ctd
source('00_setupFile.R')
# function to plot line and polygon to keep code DRY
getLinCol <- function(x){
  switch(as.character(x),
         '1981' = hcl.colors(n=5, palette = 'Burg')[1],
         '1991' = hcl.colors(n=5, palette = 'Blues 2')[1])
}
getPolyCol <- function(x){
  switch(as.character(x),
         '1981' = hcl.colors(n=5, palette = 'Burg')[3],
         '1991' = hcl.colors(n=5, palette = 'Blues')[3])
}
getAlpha <- function(x){
  switch(as.character(x),
         '1981' = 150,
         '1991' = 100)
}
plotLineAndPolygon <- function(df, var, climYear){
  polyCol <- getPolyCol(x = climYear)
  alpha <- getAlpha(x= climYear)
  polygon(x = c(df[['time']],
                rev(df[['time']])),
          y = c(df[[var]] - (0.5 * df[[paste0(var,'SD')]]),
                rev(df[[var]] + (0.5 * df[[paste0(var,'SD')]]))),
          border = NA, col = rgb(t(col2rgb(polyCol)), alpha = alpha, max = 255))
  linCol <- getLinCol(x = climYear)
  lines(x = df[['time']], y = df[[var]], , col = linCol, lwd = 2)
}
# load data
load(paste(destDirData, 'monthlyClimatology.rda', sep = '/'))
# define variables
vars <- c('temperature', 'salinity')
# initiate plot
png(filename = paste(destDirFigures,
                     '06d_monthlyNearBottomIndicator.png',
                     sep = '/'),
    width = 5, height = 4 * 1.5, units = 'in',
    pointsize = 9, res = 300)
## define graphical parameters
mfrow <- c(2,1)
oma <- c(2, 0.5, 0, 0)
mar <- c(1.5, 3.5, 1.5, 2)
par(mfrow = mfrow, mar = mar, oma = oma)
par(cex = 0.8) # for nice font size
# define depth bin
lookdepth <- 150
# iterate through each variable
for(var in vars){
  # get ylim for the variable
  ylim <- range(unlist(lapply(climatology, function(k) {ok <- k[['monthlyClimatologyDf']][['pressure']] == lookdepth;
                                                       c(k[['monthlyClimatologyDf']][[var]][ok] - (0.5 * k[['monthlyClimatologyDf']][[paste0(var, 'SD')]][ok]),
                                                         k[['monthlyClimatologyDf']][[var]][ok] + (0.5 * k[['monthlyClimatologyDf']][[paste0(var, 'SD')]][ok]))})),
                na.rm = TRUE)
  # define y-axis label
  # label variable
  R <- ']'
  L <- '['
  varlab <- switch(var,
                   'temperature'= bquote(.(gettext('Temperature', domain = 'R-oce')) ~ .(L) * degree * "C" * .(R)),
                   'salinity' = bquote(.(gettext('Practical Salinity', domain = 'R-oce'))),
                   'sigmaTheta' = bquote(sigma[theta] *' '* .(L) * kg/m^3 * .(R)))
  for(ic in 1:length(climatology)){
    d <- climatology[[ic]][['monthlyClimatologyDf']]
    # subset the monthly data frame to the bin value
    okrow <- d[['pressure']] == lookdepth
    keepcol <- c('month', 'pressure', var, paste0(var, 'SD'))
    okcol <- names(d) %in% keepcol
    df <- d[okrow, okcol]
    # define fake-x
    fakeYear <- 1990
    fakeDay <- 15
    fakeX <- as.POSIXct(paste(fakeYear, df[['month']], fakeDay, sep = '-'), tz = 'UTC')
    fakeXlim <- c(as.POSIXct(paste(fakeYear, 1, fakeDay, sep = '-'), tz = 'UTC'),
                  as.POSIXct(paste(fakeYear, 12, fakeDay, sep = '-'), tz = 'UTC'))
    ## add to data.frame
    df <- data.frame(df,
                     time = fakeX)
    # get climatology year
    climYear <- min(climatology[[ic]][['climatologyYears']])
    if(ic == 1){
      # initialize the profile
      plot(x = df[['time']],
           y = df[[var]],
           col = 'white',
           xlim = fakeXlim,
           ylim = ylim,
           ylab = '',
           xlab = '',
           xaxt = 'n',
           xaxs = 'i')
      # add grid
      abline(v = fakeX, lty = 3, col = 'lightgrey')
      abline(h = pretty(ylim), lty = 3, col = 'lightgrey')
      box()
      # add data
      plotLineAndPolygon(df = df, var = var, climYear = climYear)
      # x-axis
      ## add axis
      axis.POSIXct(side = 1, at = fakeX, format = '%b')
      ## add label
      if(var == vars[length(vars)]){
        mtext("Month",
              side = 1,
              line = 2.3)
      }
      # y-axis
      mtext(text = varlab, side = 2, line = 2.3)
    } else {
      plotLineAndPolygon(df = df, var = var, climYear = climYear)
    }
    # legend
    legend("topright",
           lty = 1,
           lwd = 2,
           col = unlist(lapply(climatology, function(k) getLinCol(min(k[['climatologyYears']])))),
           legend = unlist(lapply(climatology, function(k) paste(range(k[['climatologyYears']]), collapse = '-'))))
  } # closes ic, climatology
} # closes var
dev.off()