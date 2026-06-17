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
# define days to predict
ydaypredict <- 1:365
# define variables to create a climatology
climvars <- c('mixedLayerDepthDefault')
# define output
mixedLayerDepthClimatology <- vector(mode = 'list', length = length(climatologyYears))
for(ic in 1:length(climatologyYears)){
  climYears <- climatologyYears[[ic]]
  okclim <- mixedLayerDepthData$year %in% climYears
  subdf <- mixedLayerDepthData[okclim, ]
  # do the analysis
  # pad the timeseries for a smooth dec 31 to jan 01 transition
  #   use 28 days
  subdays <- 28
  startadd <- subdf[subdf[['yearDay']] >= (365 - subdays), ]
  startadd[['yearDay']] <- startadd[['yearDay']] - 365
  endadd <- subdf[subdf[['yearDay']] <= subdays, ]
  endadd[['yearDay']] <- endadd[['yearDay']] + 365
  subdffull <- rbind(subdf,
                     startadd,
                     endadd)
  fitoutput <- lapply(c(climvars, paste0(climvars, 'SD')), function(k) matrix(data = NA, nrow = 1, ncol = length(ydaypredict), byrow = FALSE, dimnames = list(k, ydaypredict)))
  names(fitoutput) <- c(climvars, paste0(climvars, 'SD'))
  for(icv in 1:length(climvars)){
    cvar <- climvars[icv]
    equation <- paste(cvar, '~ yearDay')
    loe <- loess(formula = equation,
                 data = subdffull,
                 span = 0.5)
    ploe <- predict(loe, newdata = data.frame(yearDay = ydaypredict), se = TRUE)
    # variance predicted function modified from cran msir function loess.sd
    r <- residuals(loe)
    yearDayResid <- subdffull[['yearDay']][!is.na(subdffull[[cvar]])]
    modr <- loess(I(r^2) ~ yearDayResid, span = 0.5)
    sd <- sqrt(pmax(0, predict(modr, data.frame(yearDayResid = ydaypredict))))
    # save results
    fitoutput[[cvar]][1, ] <- ploe$fit
    fitoutput[[paste0(cvar, 'SD')]][1, ] <- sd
  }
  # make a data frame of entire climatology
  dailyClimatologyDf <- NULL
  for(iv in 1:length(fitoutput)){
    vo <- fitoutput[[iv]]
    vodf <- as.data.frame(as.table(vo))
    colnames(vodf) <- c('varname', 'yearDay', names(fitoutput)[iv])
    vodf <- vodf[, !names(vodf) %in% 'varname'] # omit the variable name
    if(is.null(dailyClimatologyDf)){
      dailyClimatologyDf <- vodf
    } else {
      dailyClimatologyDf <- merge(dailyClimatologyDf, vodf, by = 'yearDay', all = TRUE)
    }
  }
  # order by yearDay
  o <- order(dailyClimatologyDf[['yearDay']])
  dailyClimatologyDf <- dailyClimatologyDf[o, ]
  # save to output
  mixedLayerDepthClimatology[[ic]][['dailyData']] <- subdffull
  mixedLayerDepthClimatology[[ic]][['dailyClimatologyDf']] <- dailyClimatologyDf
  mixedLayerDepthClimatology[[ic]][['climatologyYears']] <- climYears
}
# save
save(mixedLayerDepthClimatology, file = paste(destDirData, 'mixedLayerDepthClimatologyDailyScatter.rda', sep = '/'))