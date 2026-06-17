rm(list=ls())
source('00_setupFile.R')
data("ctd")
ghostctd <- ctd
load(paste(destDirData, 'climateAndArchiveCTD.rda', sep = '/'))
# get information out of ctd data
startTime <- as.POSIXct(unlist(lapply(allctd, function(k) k[['startTime']])), origin = '1970-01-01', tz = 'UTC')
startMonths <- as.POSIXlt(startTime)$mon + 1
startYears <- as.POSIXlt(startTime)$year + 1900
# define output list
climatology <- vector(mode = 'list', length = length(climatologyYears))
# define variables
vars <- c('temperature', 'salinity', 'sigmaTheta')
# iterate through each climatology period, get ctd objects within
for(ic in 1:length(climatologyYears)){
  lookyears <- climatologyYears[[ic]]
  cat(paste("Getting data for climatology reference period", min(lookyears), 'to', max(lookyears)), sep = '\n')
  okctd <- startYears %in% lookyears
  climctd <- allctd[okctd]
  cat(paste("Found", length(climctd), 'profiles'), sep = '\n')
  # get all p, T, S, ST data
  time <- as.POSIXct(unlist(lapply(climctd, function(k) rep(k[['startTime']], length = length(k[['pressure']])))), origin = '1970-01-01', tz = 'UTC')
  p <- unlist(lapply(climctd, function(k) k[['pressure']]))
  T <- unlist(lapply(climctd, function(k) k[['temperature']]))
  S <- unlist(lapply(climctd, function(k) if('salinity' %in% names(k@data)) k[['salinity']] else rep(NA, length(k[['pressure']]))))
  ST <- unlist(lapply(climctd, function(k) if('salinity' %in% names(k@data)) k[['sigmaTheta']] else rep(NA, length(k[['pressure']]))))
  month <- as.POSIXlt(time)$mon + 1
  df <- data.frame(time = time,
                   year = as.POSIXlt(time)$year + 1900,
                   month = month,
                   pressure = p,
                   temperature = T,
                   salinity = S,
                   sigmaTheta = ST)
  # set deltaz and breaks for cutting
  deltaz <- 5
  breaks1 <- seq(2.5, 92.5, deltaz) # set start to 2.5 to give first point at 5dbar
  # split by month
  sdfm <- split(df, f = df[['month']])
  # cut by breaks
  sdfmb <- lapply(sdfm, function(k) split(k, cut(k[['pressure']], breaks1, dig.lab = max(nchar(breaks1) - 1))))
  # calculate mean and standard deviation at each month and bin
  ## doing this is a loop because the lapply life can get a bit tangly
  outmonth <- names(sdfmb)
  monthlyClim <- vector(mode = 'list', length = length(outmonth))
  monthlyClimCtd <- vector(mode = 'list', length = length(outmonth))
  for(im in 1:length(sdfmb)){
    m <- sdfmb[[im]]
    # get pressure value from breaks
    mbreaks <- names(m)
    breaksplit <- strsplit(mbreaks, split = ',')
    minbreakdepth <- as.numeric(unlist(lapply(breaksplit, function(k) gsub('\\((.*)', '\\1', k[1]))))
    maxbreakdepth <- as.numeric(unlist(lapply(breaksplit, function(k) gsub('(.*)\\]', '\\1', k[2]))))
    breakdepthsm <- matrix(data = c(minbreakdepth, maxbreakdepth), nrow = length(minbreakdepth), ncol = 2)
    outdepths <- apply(breakdepthsm, 1, mean)
    dout <- NULL
    for(id in 1:length(m)){
      d <- m[[id]]
      ok <- names(d) %in% vars
      dm <- apply(d[,ok], 2, mean, na.rm = TRUE)
      dsd <- apply(d[,ok], 2, sd, na.rm = TRUE)
      names(dsd) <- paste0(names(dsd), 'SD')
      doutadd <- data.frame(month = as.numeric(outmonth[im]),
                            pressure = outdepths[id],
                            t(dm),
                            t(dsd))
      if(is.null(dout)){
        dout <- doutadd
      } else {
        dout <- rbind(dout, doutadd)
      }
    } # closes id
    # create ctd object
    mctd <- as.ctd(salinity = dout[['salinity']],
                   temperature = dout[['temperature']],
                   pressure = dout[['pressure']],
                   startTime = as.POSIXct(paste(2020, dout[['month']][1], 15, sep = '-'), tz = 'UTC'))
    # add standard deviation
    for(iv in 1:length(vars)){
      lookvar <- paste0(vars[iv], 'SD')
      mctd <- oceSetData(object = mctd,
                         name = lookvar,
                         value = dout[[lookvar]])
    }
    monthlyClim[[im]] <- dout
    monthlyClimCtd[[im]] <- mctd
  } # closes im
  monthlyClimDf <- do.call('rbind', monthlyClim)
  climatology[[ic]][['climatologyYears']] <- climatologyYears[[ic]]
  climatology[[ic]][['allctd']] <- climctd
  climatology[[ic]][['monthlyData']] <- sdfmb
  climatology[[ic]][['monthlyClimatologyCtd']] <- monthlyClimCtd
  climatology[[ic]][['monthlyClimatologyDf']] <- monthlyClimDf
}
save(climatology, file = paste(destDirData, 'monthlyClimatology.rda', sep = '/'))
