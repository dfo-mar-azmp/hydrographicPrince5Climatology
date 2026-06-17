rm(list=ls())
source('00_setupFile.R')
topoFile <- download.topo(west = -75, east = -50,
                          south = 38, north = 50,
                          resolution = 1)
ocetopo <- read.topo(topoFile)
load(paste(destDirData, 'climateData.rda', sep = '/'))

# add posixct time to df
timeadj <- ifelse(nchar(df$cruise_time) == 1, paste('000', df$cruise_time),
                  ifelse(nchar(df$cruise_time) == 2, paste0('00', df$cruise_time),
                         ifelse(nchar(df$cruise_time) == 3, paste0('0', df$cruise_time), df$cruise_time)))
time <- as.POSIXct(paste(paste(df$year, df$month, df$day, sep = '-'), timeadj),
                   format = '%Y-%m-%d %H%M', tz = 'UTC')
df <- data.frame(df,
                 time = time)
# make ctd objects from each station id
stationIds <- unique(df$stn_id)
allctd <- lapply(stationIds, function(k) {ok <- df$stn_id %in% k;
                                       d <- df[ok,];
                                       d <- d[with(d, order(pressure)),]
                                       ctd <- as.ctd(salinity = d$salinity,
                                                     temperature = d$temperature,
                                                     pressure = d$pressure,
                                                     time = d$time,
                                                     startTime = d$time[1],
                                                     longitude = d$longitude[1],
                                                     latitude = d$latitude[1]);
                                       ctd <- oceSetMetadata(ctd, 'depthMax', max(d$maximum_depth));
                                       ctd <- oceSetMetadata(ctd, 'cruiseNumber', d$cruise_id[1]);
                                       ctd <- oceSetMetadata(ctd, 'event', k);
                                       ctd <- oceSetMetadata(ctd, 'dataType', d$datatype[1]);
                                       ctd})
cat(paste('From climate database, ', length(allctd), 'CTD profiles constructed.'), sep = '\n')
# check profiles that are in the polygon
cat("Checking which profiles are in polygon", sep = '\n')
lon <- unlist(lapply(allctd, function(k) k[['longitude']][1]))
lat <- unlist(lapply(allctd, function(k) k[['latitude']][1]))
pip <- point.in.polygon(point.x = lon,
                        point.y = lat,
                        pol.x = prince5Polygon[['longitude']],
                        pol.y = prince5Polygon[['latitude']])
ok <- pip > 0
cat(paste('      Excluding ', length(which(!ok)), 'CTD profiles.'), sep = '\n')
allctd <- allctd[ok]
# check profiles are in climatology years
cat("Checking which profiles are in climatology years.", sep = '\n')
allClimatologyYears <- seq(from = min(unlist(climatologyYears)),
             to = max(unlist(climatologyYears)),
             by = 1)
year <- as.POSIXlt(unlist(lapply(allctd, '[[', 'startTime')), orign = '1970-01-01')$year + 1900
okclimyear <- year %in% allClimatologyYears
cat(paste('      Excluding ', length(which(!okclimyear)), 'CTD profiles.'), sep = '\n')
allctd <- allctd[okclimyear]

# to remove any profiles where the bottom value is repeated for extended period of time
allctd <- lapply(allctd, ctdTrim)

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
# negative = bathymetry deeper than profile
# positive = profile deeper than bathymetry
#   use the same threhold value as archive CTD data, 40 in the positive direction
dz <- maxDepth - zs
okdepth <- dz < 40

# check salinity,
## bad salinity, a value anywhere in profile where it is less than 25
## no salinity represented by 'NA'
badSalinity <- unlist(lapply(allctd, function(k) if(any(names(k@data) == 'salinity')) {any(k[['salinity']][!is.na(k[['salinity']])] < 25)} else {NA}))
noSalinity <- is.na(badSalinity)
badSalinity[noSalinity] <- FALSE

# make a dataframe of the three issues, those being
# 1. bottom depth is greater than 50m of expected depth
# 2. bad salinity, profile has a value of 25 anywhere in profile
# 3. no salinity - omit this for this data set, it is OK if there is missing salinity

issuesDf <- data.frame(badMaxDepth = !okdepth,
                       badSalinity = badSalinity)
hasIssue <- apply(issuesDf, 1, any)
badFileIdx <- which(hasIssue)
badctd <- allctd[badFileIdx]
badyear <- as.POSIXlt(unlist(lapply(badctd, function(k) k[['startTime']])),
                      origin = '1970-01-01', tz = 'UTC')$year + 1900
badcruisenumber <- unlist(lapply(badctd, '[[', 'cruiseNumber'))
badeventnumber <- unlist(lapply(badctd, '[[', 'event'))
badProblem <- unlist(apply(issuesDf[hasIssue, ], 1, function(k) paste(names(issuesDf)[k], collapse = ', ')))
dfout <- data.frame(year = badyear,
                    cruiseNumber = badcruisenumber,
                    event = badeventnumber,
                    problem = badProblem)
write.table(dfout, file = paste(destDirData, 'listOfClimateProfilessWithIssuesAfterApplyingChecks.txt', sep = '/'), row.names = FALSE)
# remove problem profiles
ctd <- allctd[!hasIssue]
save(ctd, file = paste(destDirData, 'climateCTD.rda', sep = '/'))

omittedClimateDataDf <- dfout
save(omittedClimateDataDf, file = paste(destDirData, 'omittedClimateCTD.rda', sep = '/'))
