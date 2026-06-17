rm(list=ls())
library(oce)
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
# breakdepthsm <- matrix(data = c(minbreakdepth, maxbreakdepth), nrow = length(minbreakdepth), ncol = 2)
# lookdepths <- apply(breakdepthsm, 1, mean)
# define vars
vars <- c('temperature', 'salinity', 'sigmaTheta')
# define some limits
ylim <- c(min(minbreakdepth), max(maxbreakdepth))
xlim <- c(0,
          max(unlist(lapply(climatology, function(k) lapply(k[['dailyData']], function(kk) apply(kk[, names(kk) %in% vars], 2, function(kkk) length(which(!is.na(kkk)))))))))
yearlim <- c(1981, 2020)
yearseq <- seq(yearlim[1], yearlim[2], 1)
yeardaylim <- c(1, 365)
yeardayseq <- seq(yeardaylim[1], yeardaylim[2], 1)
# define colors for barplot
cmcol <- hcl.colors(5, palette = 'RdYlGn')[c(1, 2, 5)]
cmcoladj <- colorspace::lighten(cmcol, amount = 0.5)
# initiate png output
png(filename = paste(destDirFigures,
                     '99_nObsPerDepthBin_b.png',
                     sep = '/'),
    width = 6.5, height = 8.5, units = 'in',
    res = 200, pointsize = 10)
# define graphical parameters
mlay <- matrix(c(1, 2, 3, 10,
                 4, 5, 6, 11,
                 7, 8, 9, 0),
               nrow = 3,
               ncol = 4,
               byrow = TRUE)
palwid <- 0.18
layout(mat = mlay,
       widths = c(rep(1-palwid, 3), palwid))
oma <- c(3.5, 4.5, 2, 1.5)
mar <- c(1.5, 1, 1.5, 2)
palmar <- c(1.5, 0, 1.5, 6)
par(mar = mar,
    oma = oma,
    cex = 0.8 # for nice palette labels
    )
# define output to plot ratio
nout <- vector(mode = 'list',
               length = length(vars) * length(climatology)
)
ncnt <- 1
# iterate through each climatology, make a barplot of nobs per depth bin
startYear <- min(yearseq) + c(0, 10, 30)
endYear <- startYear + c(9, 19, 9)
for(ic in 1:length(climatology)){
  dd <- climatology[[ic]][['dailyData']]
  for(var in vars){
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
    o <- order(bpd[rownames(bpd) == 'depth', ], decreasing = TRUE)
    bp <- barplot(bpd[!rownames(bpd) %in% c('depth', noutname), o],
                  col = cmcoladj,
                  horiz = TRUE,
                  xlim = xlim)
    box()
    # y-axis
    yat <- seq(3, 27, 4)
    axis(2, at = bp[yat], labels = bpd[rownames(bpd) == 'depth', o][yat])
    if(var == vars[1]){
      mtext(text = resizableLabel(item = 'depth',
                                  axis = 'y'),
            side = 2,
            line = 2.3,
            cex = 4/5)
    }
    # x-axis
    xat <- axis(1)
    if(ic == length(climatology)){
      mtext(text = 'Number of observations',
            side = 1,
            line = 2.3,
            cex = 4/5)
    }
    # add grid
    abline(h = bp[yat], lty = 3, col = 'lightgrey')
    abline(v = xat, lty = 3, col = 'lightgrey')
    # add barplot back on top
    bp <- barplot(bpd[!rownames(bpd) %in% c('depth', noutname), o],
                  col = cmcoladj,
                  horiz = TRUE,
                  xlim = xlim,
                  add = TRUE)
    # add variable label
    if(ic == 1){
      R <- ']'
      L <- '['
      zlab <- switch(var,
                     'temperature'= bquote(bold(.(gettext('Temperature', domain = 'R-oce')) * .(L) * degree * "C" * .(R))),
                     'salinity' = bquote(bold(.(gettext('Practical Salinity', domain = 'R-oce')))),
                     'sigmaTheta' = bquote(bold(sigma[theta] *' '* .(L) * kg/m^3 * .(R))))
      mtext(text = zlab, side = 3, line = 1.3)
    }
    # add climatology reference period label
    if(var == 'temperature'){
      mtext(text = paste(range(climatology[[ic]][['climatologyYears']]), collapse = ' to '),
            side = 2, line = 4, font = 2)
    }
    print(par('mfg'))
    # output
    bpdf <- as.data.frame(t(bpd))
    nout[[ncnt]][['data']] <- bpdf
    nout[[ncnt]][['variable']] <- var
    nout[[ncnt]][['climatologyYears']] <- climatology[[ic]][['climatologyYears']]
    ncnt <- ncnt + 1
  } # closes var
} # closes ic
# plot proportion of observations
## get variables
noutvars <- unlist(lapply(nout, '[[', 'variable'))
## re-define graphical parameters to allow for x-axis labels and space between plots
mar <- c(0, 1, 3, 2)
par(mar = mar)
## define variable output
ratout <- vector(mode = 'list', length = length(vars))
rcnt <- 1
for(var in vars){
  ok <- which(noutvars %in% var)
  # always going to assume 2
  d1 <- nout[[ok[1]]][['data']]
  d2 <- nout[[ok[2]]][['data']]
  dm <- merge(x = d1,
              y = d2,
              by = 'depth',
              all = TRUE)
  rat <- dm[['n91']] / dm[['n81']]
  print(range(rat))
  plot(x = rat,
       y = dm[['depth']],
       type = 'o', pch = 21, bg = 'black',
       xlim = c(1.30, 1.95), ylim = rev(ylim),
       xaxt = 'n', yaxt = 'n', # no axes, manually add
       xlab = '', ylab = '', # no labels, manually add
       yaxs = 'i' # tight limits
  )
  # x-axis
  xat <- axis(1)
  mtext(text = 'Number of observations ratio',
        side = 1,
        line = 2.3,
        cex = 4/5)
  # y-axis
  yat <- axis(2)
  if(var == vars[1]){
    mtext(text = resizableLabel(item = 'depth',
                                axis = 'y'),
          side = 2,
          line = 2.3,
          cex = 4/5)
  }
  # add grid
  abline(h = yat, lty = 3, col = 'lightgrey')
  abline(v = xat, lty = 3, col = 'lightgrey')
  # re-add data
  lines(x = rat,
        y = dm[['depth']],
        type = 'o', pch = 21, bg = 'black')
  ## output
  dm <- data.frame(dm,
                   ratio = rat)
  ratout[[rcnt]][['data']] <- dm
  ratout[[rcnt]][['variable']] <- var
  rcnt <- rcnt + 1
}
# add palettes
par(mfg = c(3,2)) # plot area 10
par(mar = palmar)
cmcol <- hcl.colors(5, palette = 'RdYlGn')[c(1, 2, 5)]
cmcoladj <- colorspace::lighten(cmcol, amount = 0.5)
drawPalette(breaks = c(1981, 1991, 2011, 2020),
            at = c(1981, 1991, 2010, 2020),
            col = cmcoladj,
            zlab = '')
mtext(text = 'Year', side = 4, line = 9, cex = 4/5)
par(mfg = c(3,3)) # plot area 11
par(mar = palmar)
cmcol <- hcl.colors(5, palette = 'RdYlGn')[c(1, 2, 5)]
cmcoladj <- colorspace::lighten(cmcol, amount = 0.5)
drawPalette(breaks = c(1981, 1991, 2011, 2020),
            at = c(1981, 1991, 2010, 2020),
            col = cmcoladj,
            zlab = '')
mtext(text = 'Year', side = 4, line = 9, cex = 4/5)

dev.off()

save(nout, ratout, file = paste(destDirData, 'nout.rda', sep = '/'))
