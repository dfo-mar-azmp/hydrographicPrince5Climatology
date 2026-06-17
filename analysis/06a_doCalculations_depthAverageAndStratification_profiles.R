rm(list=ls())
library(csasAtlPhys)
library(pracma)
source('00_setupFile.R')
# load data
load(paste(destDirData, 'climateAndArchiveCTD.rda', sep = '/'))
# function for capitalizing text
capitalize <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
# order the profiles by time for simplicity (not really critical though)
startTime <- as.POSIXlt(unlist(lapply(allctd, function(k) k[['startTime']])), origin = '1970-01-01', tz = 'UTC')
o <- order(startTime)
allctd <- allctd[o]
startTime <- startTime[o]
# define bins to average the data
deltaz <- 5
breaks1 <- seq(0, 90, deltaz)
depthBins <- data.frame(bin = breaks1,
                        tolerance = deltaz)
# bin each profile
ctdavg <- lapply(allctd,
                 binMeanPressureCtd,
                 bin = depthBins$bin,
                 tolerance = depthBins$tolerance)
# define climatology years
climatologyYears <- list(1981:2010,
                         1991:2020)
# define variables to integrate the data
vars <- c('temperature',
          'salinity',
          'sigmaTheta')
# calculate depth averaged variables, stratification, and stratification index
## define integrate limits
### define upper and lower depth values for integration
### as well as the minimum number of points
### test at surface, earlier data only has 0m data, but near surface data not QC'd well, so have to use 5
integrationLimits <- list(list(upper = 5, lower = 50, minnumpoints = 2))
for(il in 1:length(integrationLimits)){
  upper <- integrationLimits[[il]][['upper']]
  lower <- integrationLimits[[il]][['lower']]
  minnumpoints <- integrationLimits[[il]][['minnumpoints']]
  vars <- c('temperature',
            'salinity',
            'sigmaTheta'
  )
  dfint <- NULL
  for(i in 1:length(ctdavg)){
    d <- ctdavg[[i]]
    startTime <- as.POSIXlt(d[['startTime']])
    year <- startTime$year + 1900
    month <- startTime$mon + 1
    yearDay <- startTime$yday
    # is there a value at the specified depth limit values
    # and at least 2 points
    varint <- varcomments <- vector(length = length(vars))
    for(iv in 1:length(vars)){
      comments <- NULL
      var <- vars[iv]
      if(!var %in% names(d@data)){
        varint[iv] <- NA
        comments <- c(comments, paste('No', var, 'data'))
      } else {
        dvar <- d[[var]]
        p <- d[['pressure']]
        check1 <- length(dvar[p == upper]) != 0 # there is a value at upper
        check2 <- !is.na(dvar[p == upper]) # the value at upper isn't NA
        if(length(check2) == 0){
          comments <- c(comments, paste('No data at', upper, 'm'))
        } else if(!check1 | !check2){
          comments <- c(comments, paste('No data at', upper, 'm'))
        }
        check3 <- length(dvar[p == lower]) != 0 # same as above but for lower
        check4 <- !is.na(dvar[p == lower]) # same as above but for lower
        if(length(check4) == 0){
          comments <- c(comments, paste('No data at', lower, 'm'))
        } else if(!check3 | !check4){
          comments <- c(comments, paste('No data at', lower, 'm'))
        }
        check5 <- length(is.na(dvar[p %in% upper:lower])) > minnumpoints # there are at least the min number of points between upper and lower
        if(!check5){
          comments <- c(comments, paste('Not enough data between', upper, 'and', lower, 'm'))
        }
        okvar <- all(c(check1, check2, check3, check4, check5))
        if(okvar){
          oklower <- p == lower
          okupper <- p == upper
          okp <- p %in% upper:lower
          pgood <- p[okp]
          dvargood <- dvar[okp]
          okvar <- !is.na(dvargood)
          pgood <- pgood[okvar]
          dvargood <- dvargood[okvar]

          varint[iv] <- trapz(pgood, dvargood) / (lower - upper)
          if(var == 'sigmaTheta') {
            stratification <- dvar[oklower] - dvar[okupper] # watch this for NAs
          }
        } else {
          varint[iv] <- NA
          stratification <- NA
        }
      }
      varcomments[iv] <- paste(comments, collapse = ',')
    }
    stratIdx <- calculateStratificationIndex(d, depth1 = upper, depth2 = lower)
    stratificationIndex <- stratIdx[['stratificationIndex']]
    names(varint) <- paste0('integrated', unname(sapply(vars, capitalize)))
    names(varcomments) <- paste0(vars, 'Comments')
    if(is.null(dfint)){
      dfint <- data.frame(year = year, month = month, yearDay = yearDay, t(varint), t(varcomments), stratification = stratification, stratificationIndex = stratificationIndex)
    } else {
      dfadd <- data.frame(year = year, month = month, yearDay = yearDay, t(varint), t(varcomments), stratification = stratification, stratificationIndex = stratificationIndex)
      dfint <- rbind(dfint, dfadd)
    }
  }
  depthAverageData <- dfint
  # save
  save(depthAverageData, upper, lower, minnumpoints, file = paste(destDirData, paste0('depthAverageData_', upper, 'to', lower, '.rda'), sep = '/'))
}