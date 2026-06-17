rm(list=ls())
method <- 'density'
referenceThreshold <- 0.03
referenceDepth <- 5
library(oce)
library(csasAtlPhys)
source('00_setupFile.R')
# load data
load(paste(destDirData, 'climateAndArchiveCTD.rda', sep = '/'))
# order the profiles by time for simplicity (not really critical though)
startTime <- as.POSIXlt(unlist(lapply(allctd, function(k) k[['startTime']])), origin = '1970-01-01', tz = 'UTC')
o <- order(startTime)
allctd <- allctd[o]
startTime <- startTime[o]
# calculate MLD for each profile
## define output
mlddf <- NULL
for(i in 1:length(allctd)){
  d <- allctd[[i]]
  mlddfadd <- calculateMixedLayerDepth(d,
                                       method = method,
                                       densityThreshold = referenceThreshold,
                                       densityReferenceDepth = referenceDepth,
                                       debug = FALSE)
  if(is.null(mlddf)){
    mlddf <- mlddfadd
  } else {
    mlddf <- rbind(mlddf, mlddfadd)
  }
}
mixedLayerDepthData <- mlddf
save(mixedLayerDepthData, file = paste(destDirData, 'mixedLayerDepthData.rda', sep = '/'))