rm(list=ls())
source('00_setupFile.R')
# load data
load(paste(destDirData, 'dailyClimatology.rda', sep = '/'))
## get ctd data from 1981 to 2010 and 1991 to 2020 climatology
climatologyMinYear <- unlist(lapply(climatology, function(k) min(k[['climatologyYears']])))
sctd81 <- climatology[[which(climatologyMinYear == 1981)]][['dailyClimatologyCtd']]
sctd91 <- climatology[[which(climatologyMinYear == 1991)]][['dailyClimatologyCtd']]
# get startTime from sctd81 and sctd91
startTime81 <- as.POSIXct(unlist(lapply(sctd81, '[[', 'startTime')), tz = 'UTC')
startTime91 <- as.POSIXct(unlist(lapply(sctd91, '[[', 'startTime')), tz = 'UTC')
# iterate through each sctd91, match with sctd81, and calculate anomaly
## define variables
vars <- c('temperature', 'salinity', 'sigmaTheta')
## define output list
sctd91WAnomaly <- vector(mode = 'list', length(sctd91))
cnt <- 1
for(i in 1:length(sctd91)){
  nctd <- sctd91[[i]]
  ntime <- startTime91[i]
  cat(paste("Checking for time", ntime), sep = '\n')
  oko <- which(startTime81 == ntime)
  if(length(oko) == 0){
    cat(paste("No match found for time", ntime, "in sctd81. Skipping..."), sep = '\n')
  } else {
    octd <- sctd81[[oko]]
    # pressure values should be the same, but double check, use nctd as reference
    pressurelook <- nctd[['pressure']]
    # get pressure index that match between octd and nctd
    okop <- octd[['pressure']] %in% pressurelook
    oknp <- nctd[['pressure']] %in% pressurelook
    # calculate anomaly for variables and add to nctd
    for(var in vars){
        anomaly <- nctd[[var]][oknp] - octd[[var]][okop]
        nctd <- oceSetData(object = nctd,
                           name = paste0(var, 'Anomaly'),
                           value = anomaly)
    }
    # save nctd with anomalies
    sctd91WAnomaly[[cnt]] <- nctd
    cnt <- cnt + 1
  }
}

climatologyWAnomaly <- vector(mode = 'list', length = 1)
climatologyWAnomaly[[1]][['dailyClimatologyCtd']] <- sctd91WAnomaly
climatologyWAnomaly[[1]][['climatologyYears']] <- climatology[[which(climatologyMinYear == 1991)]][['climatologyYears']]

climatology <- c(climatology,
                 climatologyWAnomaly)
save(climatology, file = paste(destDirData, 'dailyClimatologyWAnomaly.rda', sep = '/'))
