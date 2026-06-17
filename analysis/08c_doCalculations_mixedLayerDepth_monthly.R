rm(list=ls())
library(csasAtlPhys)
library(pracma)
source('00_setupFile.R')
# load data file
load(paste(destDirData, 'mixedLayerDepthData.rda', sep = '/'))
# define climatology years
climatologyYears <- list(1981:2010,
                         1991:2020)
# calculate climatology
# define months
months <- 1:12
# define variables to create a climatology
climvars <- c('mixedLayerDepthDefault')
# define output
mixedLayerDepthClimatology <- vector(mode = 'list', length = length(climatologyYears))
for(ic in 1:length(climatologyYears)){
  climYears <- climatologyYears[[ic]]
  okclim <- mixedLayerDepthData$year %in% climYears
  subdf <- mixedLayerDepthData[okclim, ]
  # do the analysis
  monthlyoutput <- lapply(c(climvars, paste0(climvars, 'SD')), function(k) matrix(data = NA, nrow = 1, ncol = length(months), byrow = FALSE, dimnames = list(k, months)))
  names(monthlyoutput) <- c(climvars, paste0(climvars, 'SD'))
  for(icv in 1:length(climvars)){
    cvar <- climvars[icv]
    # iterate through each month
    for(im in 1:length(months)){
      lookmonth <- months[im]
      # subset data to lookmonth
      okmonth <- subdf[['month']] == lookmonth
      subdfmonth <- subdf[okmonth, ]
      # calculate mean and sd
      varmean <- mean(subdfmonth[[cvar]], na.rm = TRUE)
      varsd <- sd(subdfmonth[[cvar]], na.rm = TRUE)
      # save results
      monthlyoutput[[cvar]][1, im] <- varmean
      monthlyoutput[[paste0(cvar, 'SD')]][1, im] <- varsd
    }
  }
  # make a data frame of entire climatology
  monthlyClimatologyDf <- NULL
  for(iv in 1:length(monthlyoutput)){
    vo <- monthlyoutput[[iv]]
    vodf <- as.data.frame(as.table(vo))
    colnames(vodf) <- c('varname', 'month', names(monthlyoutput)[iv])
    vodf <- vodf[, !names(vodf) %in% 'varname'] # omit the variable name
    if(is.null(monthlyClimatologyDf)){
      monthlyClimatologyDf <- vodf
    } else {
      monthlyClimatologyDf <- merge(monthlyClimatologyDf, vodf, by = 'month', all = TRUE)
    }
  }
  # order by month
  o <- order(monthlyClimatologyDf[['month']])
  monthlyClimatologyDf <- monthlyClimatologyDf[o, ]
  # save to output
  mixedLayerDepthClimatology[[ic]][['dailyData']] <- subdf
  mixedLayerDepthClimatology[[ic]][['monthlyClimatologyDf']] <- monthlyClimatologyDf
  mixedLayerDepthClimatology[[ic]][['climatologyYears']] <- climYears
}
# save
save(mixedLayerDepthClimatology, file = paste(destDirData, 'mixedLayerDepthClimatologyMonthly.rda', sep = '/'))