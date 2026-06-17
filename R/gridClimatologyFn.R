# note, this isn't a formal function, written just to save space in scripts and to avoid drift
gridClimatology <- function(d, fakeYear = 2020){
  um <- unlist(lapply(d, function(k) k[['month']]))
  cnt <- 1
  mctd <- vector(mode = 'list')
  for(i in 1:length(um)){
    # fake a profile for first one, use decembers profile
      if(um[i] == 12){
        time <- as.POSIXct(paste(fakeYear - 1, '12', '15', sep = '-'), tz = 'UTC') # fake a time
        mmctd <- oceSetMetadata(d[[i]],
                                'startTime',
                                time)
        mctd[[cnt]] <- mmctd
        cnt <- cnt + 1
      }
    
      time <- as.POSIXct(paste(fakeYear, um[i], '15', sep = '-'), tz = 'UTC') # fake a time
      mmctd <- oceSetMetadata(d[[i]],
                              'startTime',
                              time)
      mctd[[cnt]] <- mmctd
      cnt <- cnt + 1
      # fake a profile for last, use january's profile
      if(um[i] == 1){
        time <- as.POSIXct(paste(fakeYear + 1, '01', '15', sep = '-'), tz = 'UTC') # fake a time
        mmctd <- oceSetMetadata(d[[i]],
                                'startTime',
                                time)
        mctd[[cnt]] <- mmctd
        cnt <- cnt + 1
      }
  }
  # going to spit out warnings for no longitude/latitude, but that's ok for this purpose
  start <- as.POSIXct(unlist(lapply(mctd, function(k) k[['startTime']])), origin = '1970-01-01', tz = 'UTC')
  o <- order(start)
  mctd <- mctd[o]
  s <- as.section(mctd)
  s@metadata$startTime <- as.POSIXct(unlist(lapply(mctd, function(k) k[['startTime']])), origin = '1970-01-01', tz = 'UTC')
  sg <- sectionGrid(s)
  sg
  }
