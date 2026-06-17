rm(list=ls())
library(csasAtlPhys)
library(pracma)
source('00_setupFile.R')
# get files
depthAvgFiles <- list.files(path = destDirData,
                            pattern = 'depthAverageData.*',
                            full.names = TRUE)
# function for capitalizing text
capitalize <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
# define climatology years
climatologyYears <- list(1981:2010,
                         1991:2020)
for(fi in 1:length(depthAvgFiles)){
  # load data
  load(depthAvgFiles[fi])
  # calculate climatology
  # define months
  months <- 1:12
  # define variables to create a climatology
  climvars <- c('integratedTemperature',
                'integratedSalinity',
                'integratedSigmaTheta',
                'stratification',
                'stratificationIndex')
  # define output
  depthAverageClimatology <- vector(mode = 'list', length = length(climatologyYears))
  for(ic in 1:length(climatologyYears)){
    climYears <- climatologyYears[[ic]]
    okclim <- depthAverageData$year %in% climYears
    subdf <- depthAverageData[okclim, ]
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
    depthAverageClimatology[[ic]][['dailyData']] <- subdf
    depthAverageClimatology[[ic]][['monthlyClimatologyDf']] <- monthlyClimatologyDf
    depthAverageClimatology[[ic]][['climatologyYears']] <- climYears
  }
  # save
  save(depthAverageClimatology, file = paste(destDirData, paste0('depthAverageClimatologyMonthly_', upper, 'to', lower, '.rda'), sep = '/'))
}