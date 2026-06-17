rm(list=ls())
source('00_setupFile.R')
topoFile <- download.topo(west = -75, east = -50,
                          south = 38, north = 50,
                          resolution = 1)
ocetopo <- read.topo(topoFile)
# load archiveFileList
load(paste(destDirData, 'archiveFileList.rda', sep = '/'))
# read in files
d <- lapply(archiveFiles, read.ctd.odf)
# trim ctd's in the event that there is a file with repeated values [this is the case for one file]
d <- lapply(d, ctdTrim)
# handle any flags that are in the file before checking for questionable values
d <- lapply(d, function(k) if(length(k[['flags']]) != 0){handleFlags(k, flags = 2:4)} else {k})
# add data type when comparing it to climate data, use 'CD' for abbreviation for 'CTD'
d <- lapply(d, function(k) {ctd <- oceSetMetadata(k, 'dataType', 'CD');
                            ctd})
allctd <- d

# do some basic checks on the data
## check the bottom depth.
lon <- unlist(lapply(allctd, function(k) k[['longitude']][1]))
lat <- unlist(lapply(allctd, function(k) k[['latitude']][1]))
zs <- abs(interp.surface(obj = list(x = ocetopo[['longitude']],
                                    y = ocetopo[['latitude']],
                                    z = ocetopo[['z']]),
                         loc = cbind(lon,
                                     lat)))
maxDepth <- unlist(lapply(allctd, function(k) max(k[['pressure']])))
# negative = bathymetry deeper than profile, these are OK to retain.
# positive = profile deeper than bathymetry, have to decide on a threshold value
# doing a histogram reveals that most of the profile are within 40 m, so we'll use
#   that as a threshold both in the positive direction.
dz <- maxDepth - zs
okdepth <- dz < 40

# check salinity,
## bad salinity, a value anywhere in profile where it is less than 25
## no salinity represented by 'NA'
badSalinity <- unlist(lapply(allctd, function(k) if(any(names(k@data) == 'salinity')) {any(k[['salinity']][!is.na(k[['salinity']])] < 25) | all(is.na(k[['salinity']]))} else {NA}))
noSalinity <- is.na(badSalinity)
badSalinity[noSalinity] <- FALSE # to avoid weirdness

# make a dataframe of the three issues, those being
# 1. bottom depth is greater than 50m of expected depth
# 2. bad salinity, profile has a value of 25 anywhere in profile
# 3. no salinity
issuesDf <- data.frame(badMaxDepth = !okdepth,
                       badSalinity = badSalinity,
                       noSalinity = noSalinity)
hasIssue <- apply(issuesDf, 1, any)
badFileIdx <- which(hasIssue)
badctd <- allctd[badFileIdx]
badyear <- as.POSIXlt(unlist(lapply(badctd, function(k) k[['startTime']])),
                      origin = '1970-01-01', tz = 'UTC')$year + 1900
badfilename <- unlist(lapply(badctd, function(k) tail(strsplit(k[['filename']], split = '\\\\')[[1]],1)))
badProblem <- apply(issuesDf[hasIssue, ], 1, function(k) names(issuesDf)[k])
dfout <- data.frame(year = badyear,
                    filename = badfilename,
                    problem = badProblem,
                    depth = maxDepth[badFileIdx],
                    topoDepth = zs[badFileIdx]
                    )
write.table(dfout, file = paste(destDirData, 'listOfArchiveFilesWithIssuesAfterApplyingChecks.txt', sep = '/'), row.names = FALSE)

allctd <- allctd[-badFileIdx]
save(allctd, file = paste(destDirData, 'archiveCTD.rda', sep = '/'))

omittedDataDf <- dfout
save(omittedDataDf, file = paste(destDirData, 'omittedArchiveCTD.rda', sep = '/'))
