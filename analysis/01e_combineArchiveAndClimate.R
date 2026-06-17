rm(list=ls())
library(oce)
source('00_setupFile.R')
load(paste(destDirData, 'climateCTD.rda', sep = '/')) # ctd
load(paste(destDirData, 'archiveCTD.rda', sep = '/')) # allctd
# number of climate ctd before omitting
nClimateCtdStart <- length(ctd)
# 1. find duplicates in climate
cliStart <- as.POSIXct(unlist(lapply(ctd, function(k) k[['startTime']])), origin = '1970-01-01', tz = 'UTC')
duplicates <- lapply(cliStart, function(k) {dt <- difftime(k, cliStart, units = 'hours');
                                            which(abs(dt) < 3)})

# have to remove the index from each list
for(i in 1:length(duplicates)){
  duplicates[[i]] <- duplicates[[i]][duplicates[[i]] != i]
}
duplicatesInClimate <- unique(unlist(duplicates))
nduplicatesInClimate <- length(duplicatesInClimate)
climatekeep <- ctd[-duplicatesInClimate]

# 1. find duplicates between climate and archive by the startTime, know that the location is within
#    some defined box, retain archive profiles
cliStart <- as.POSIXct(unlist(lapply(climatekeep, function(k) k[['startTime']])), origin = '1970-01-01', tz = 'UTC')
arcStart <- as.POSIXct(unlist(lapply(allctd, function(k) k[['startTime']])), origin = '1970-01-01', tz = 'UTC')
# duplicates defined as any that were within 6 hours of eachother
duplicates <- lapply(arcStart, function(k) {dt <- difftime(k, cliStart, units = 'hours');
which(abs(dt) < 3)})
climateDuplicates <- unique(unlist(duplicates))
nduplicatesInClimateAndArchive <- length(climateDuplicates)
clictd <- climatekeep[-climateDuplicates]

# 2. check the data type. unique data types include BO, MB, XB, CD, BF, TE
#    from some examination of other data, we will not use
#       MB
#       TE
clitype <- unlist(lapply(clictd, function(k) k[['dataType']]))
ok <- !clitype %in% c('MB', 'TE')
#ok <- !clitype %in% 'TE'
nNotIncludedType <- length(which(!ok))
clictd <- clictd[ok]
nClimateCtdEnd <- length(clictd)
nArchiveCtd <- length(allctd)

# 3. join the climate and archive data together
allctd <- c(clictd, allctd)
## for each profile, check that 'pressure' is a variable
##   if not, add to data
for(id in 1:length(allctd)){
  if(!'pressure' %in% names(allctd[[id]]@data)){
    cat(paste('Adding pressure to ctd object index', id), sep = '\n')
    allctd[[id]] <- oceSetData(object = allctd[[id]],
                               name = 'pressure',
                               value = swPressure(depth = allctd[[id]][['depth']],
                                                  latitude = allctd[[id]][['latitude']][1]))
  }
}
# save
save(allctd, file = paste(destDirData, 'climateAndArchiveCTD.rda', sep = '/'))
# output some things for report
save(nduplicatesInClimate,
     nduplicatesInClimateAndArchive,
     nNotIncludedType,
     nClimateCtdStart,
     nClimateCtdEnd,
     nArchiveCtd,
     file = paste(destDirData, 'duplicateNumbers.rda', sep = '/'))