rm(list=ls())
library(ncdf4)
library(oce)
source('00_setupFile.R')
# load daily hydrographic climatology ----
load(paste(destDirData, 'dailyClimatology.rda', sep = '/'))
# iterate through each climatology ----
for(ic in 1:length(climatology)){
  df <- climatology[[ic]][['dailyClimatologyDf']]
  # convert some rows from factor to numeric values ----
  df[['yearDay']] <- as.numeric(as.character(df[['yearDay']]))
  df[['depth']] <- as.numeric(as.character(df[['depth']]))
  # re-name columns to be the same as the netCDF ----
  names(df)[names(df) == 'depth'] <- 'pressure'
  names(df)[names(df) == 'temperature'] <- 'sea_water_temperature'
  names(df)[names(df) == 'temperatureSD'] <- 'sea_water_temperature_standard_deviation'
  names(df)[names(df) == 'salinity'] <- 'sea_water_practical_salinity'
  names(df)[names(df) == 'salinitySD'] <- 'sea_water_practical_salinity_standard_deviation'
  names(df)[names(df) == 'sigmaTheta'] <- 'sea_water_sigma_theta'
  names(df)[names(df) == 'sigmaThetaSD'] <- 'sea_water_sigma_theta_standard_deviation'
  # order by depth and yearDay
  o <- with(df, order(pressure, yearDay))
  df <- df[o, ]
  # define file name ----
  filename <- paste0(paste('Maritimes',
                           'AZMP',
                           'fixedStation',
                           'prince5',
                           'hydrographic',
                           'daily',
                           paste0(min(climatology[[ic]][['climatologyYears']]),
                                  'to',
                                  max(climatology[[ic]][['climatologyYears']]),
                                  'climatology'),
                           sep = '_'),
                     '.csv')
  # save data ----
  write.csv(x = df,
            file = paste(destDirCsv, filename, sep = '/'),
            row.names = FALSE)
}
# load monthly hydrographic climatology ----
load(paste(destDirData, 'monthlyClimatology.rda', sep = '/'))
# iterate through each climatology ----
for(ic in 1:length(climatology)){
  df <- climatology[[ic]][['monthlyClimatologyDf']]
  # re-name columns to be the same as the netCDF ----
  names(df)[names(df) == 'temperature'] <- 'sea_water_temperature'
  names(df)[names(df) == 'temperatureSD'] <- 'sea_water_temperature_standard_deviation'
  names(df)[names(df) == 'salinity'] <- 'sea_water_practical_salinity'
  names(df)[names(df) == 'salinitySD'] <- 'sea_water_practical_salinity_standard_deviation'
  names(df)[names(df) == 'sigmaTheta'] <- 'sea_water_sigma_theta'
  names(df)[names(df) == 'sigmaThetaSD'] <- 'sea_water_sigma_theta_standard_deviation'
  # define file name ----
  filename <- paste0(paste('Maritimes',
                           'AZMP',
                           'fixedStation',
                           'prince5',
                           'hydrographic',
                           'monthly',
                           paste0(min(climatology[[ic]][['climatologyYears']]),
                                  'to',
                                  max(climatology[[ic]][['climatologyYears']]),
                                  'climatology'),
                           sep = '_'),
                     '.csv')
  # save data ----
  write.csv(x = df,
            file = paste(destDirCsv, filename, sep = '/'),
            row.names = FALSE)
}
# daily indicators climatology ----
# get files ----
climFiles <- list.files(path = destDirData,
                        pattern = 'depthAverageClimatologyDailyScatter.*',
                        full.names = TRUE)
intLimits <- unlist(lapply(climFiles, function(k) gsub("depthAverageClimatologyDailyScatter_(.*)\\.rda", '\\1', basename(k))))
intLimits <- gsub('to', '_', intLimits)
# load the daily mixed layer depth (MLD) ----
load(paste(destDirData, 'mixedLayerDepthClimatologyDailyScatter.rda', sep = '/'))
for(icf in 1:length(climFiles)){
  # load file ----
  cat(paste("loading", climFiles[icf]), sep = '\n')
  load(climFiles[icf])
  # get min climatology year for depth average and MLD ----
  minClimYearDa <- unlist(lapply(depthAverageClimatology, function(k) min(k[['climatologyYears']])))
  minClimYearMld <- unlist(lapply(mixedLayerDepthClimatology, function(k) min(k[['climatologyYears']])))
  # use the depth average as the 'primary' iterator ----
  for(ic in 1:length(depthAverageClimatology)){
    # get data ----
    da <- depthAverageClimatology[[ic]]
    okmld <- which(minClimYearMld == minClimYearDa[ic])
    mld <- mixedLayerDepthClimatology[[okmld]]
    ## get the daily climatology data frame from both ----
    dadf <- da[['dailyClimatologyDf']]
    mlddf <- mld[['dailyClimatologyDf']]
    ## merge depth average and mld together by 'yearDay' ----
    df <- merge(dadf, mlddf, by = 'yearDay', all = TRUE)
    ## order by yearDay ----
    o <- order(df[['yearDay']])
    df <- df[o, ]
    # convert some rows from factor to numeric values ----
    df[['yearDay']] <- as.numeric(as.character(df[['yearDay']]))
    # re-name columns to be the same as the netCDF ----
    names(df)[names(df) == 'integratedTemperature'] <- paste('average_temperature', intLimits[icf], sep = '_')
    names(df)[names(df) == 'integratedTemperatureSD'] <- paste('average_temperature', intLimits[icf], 'standard_deviation', sep = '_')
    names(df)[names(df) == 'integratedSalinity'] <- paste('average_salinity', intLimits[icf], sep = '_')
    names(df)[names(df) == 'integratedSalinitySD'] <- paste('average_salinity', intLimits[icf], 'standard_deviation', sep = '_')
    names(df)[names(df) == 'integratedSigmaTheta'] <- paste('average_sigma_theta', intLimits[icf], sep = '_')
    names(df)[names(df) == 'integratedSigmaThetaSD'] <- paste('average_sigma_theta', intLimits[icf], 'standard_deviation', sep = '_')
    names(df)[names(df) == 'stratification'] <- paste('stratification', intLimits[icf], sep = '_')
    names(df)[names(df) == 'stratificationSD'] <- paste('stratification', intLimits[icf], 'standard_deviation', sep = '_')
    names(df)[names(df) == 'stratificationIndex'] <- 'stratification_index'
    names(df)[names(df) == 'stratificationIndexSD'] <- 'stratification_index_standard_deviation'
    names(df)[names(df) == 'mixedLayerDepthDefault'] <- 'mixed_layer_depth'
    names(df)[names(df) == 'mixedLayerDepthDefaultSD'] <- 'mixed_layer_depth_standard_deviation'
    # define file name ----
    filename <- paste0(paste('Maritimes',
                             'AZMP',
                             'fixedStation',
                             'prince5',
                             'indicators',
                             'daily',
                             paste0(min(da[['climatologyYears']]),
                                    'to',
                                    max(da[['climatologyYears']]),
                                    'climatology'),
                             intLimits[icf],
                             sep = '_'),
                       '.csv')
    # save data ----
    write.csv(x = df,
              file = paste(destDirCsv, filename, sep = '/'),
              row.names = FALSE)
  }
}
# monthly indicators climatology ----
# get files ----
climFiles <- list.files(path = destDirData,
                        pattern = 'depthAverageClimatologyMonthly.*',
                        full.names = TRUE)
intLimits <- unlist(lapply(climFiles, function(k) gsub("depthAverageClimatologyMonthly_(.*)\\.rda", '\\1', basename(k))))
intLimits <- gsub('to', '_', intLimits)
# load the daily mixed layer depth (MLD) ----
load(paste(destDirData, 'mixedLayerDepthClimatologyMonthly.rda', sep = '/'))
for(icf in 1:length(climFiles)){
  # load file ----
  cat(paste("loading", climFiles[icf]), sep = '\n')
  load(climFiles[icf])
  # get min climatology year for depth average and MLD ----
  minClimYearDa <- unlist(lapply(depthAverageClimatology, function(k) min(k[['climatologyYears']])))
  minClimYearMld <- unlist(lapply(mixedLayerDepthClimatology, function(k) min(k[['climatologyYears']])))
  # use the depth average as the 'primary' iterator ----
  for(ic in 1:length(depthAverageClimatology)){
    # get data ----
    da <- depthAverageClimatology[[ic]]
    okmld <- which(minClimYearMld == minClimYearDa[ic])
    mld <- mixedLayerDepthClimatology[[okmld]]
    ## get the daily climatology data frame from both ----
    dadf <- da[['monthlyClimatologyDf']]
    mlddf <- mld[['monthlyClimatologyDf']]
    ## merge depth average and mld together by 'month' ----
    df <- merge(dadf, mlddf, by = 'month', all = TRUE)
    # convert some rows from factor to numeric values ----
    df[['month']] <- as.numeric(as.character(df[['month']]))
    # re-name columns to be the same as the netCDF ----
    names(df)[names(df) == 'integratedTemperature'] <- paste('average_temperature', intLimits[icf], sep = '_')
    names(df)[names(df) == 'integratedTemperatureSD'] <- paste('average_temperature', intLimits[icf], 'standard_deviation', sep = '_')
    names(df)[names(df) == 'integratedSalinity'] <- paste('average_salinity', intLimits[icf], sep = '_')
    names(df)[names(df) == 'integratedSalinitySD'] <- paste('average_salinity', intLimits[icf], 'standard_deviation', sep = '_')
    names(df)[names(df) == 'integratedSigmaTheta'] <- paste('average_sigma_theta', intLimits[icf], sep = '_')
    names(df)[names(df) == 'integratedSigmaThetaSD'] <- paste('average_sigma_theta', intLimits[icf], 'standard_deviation', sep = '_')
    names(df)[names(df) == 'stratification'] <- paste('stratification', intLimits[icf], sep = '_')
    names(df)[names(df) == 'stratificationSD'] <- paste('stratification', intLimits[icf], 'standard_deviation', sep = '_')
    names(df)[names(df) == 'stratificationIndex'] <- 'stratification_index'
    names(df)[names(df) == 'stratificationIndexSD'] <- 'stratification_index_standard_deviation'
    names(df)[names(df) == 'mixedLayerDepthDefault'] <- 'mixed_layer_depth'
    names(df)[names(df) == 'mixedLayerDepthDefaultSD'] <- 'mixed_layer_depth_standard_deviation'
    ## order by month ----
    o <- order(df[['month']])
    df <- df[o, ]
    # define file name ----
    filename <- paste0(paste('Maritimes',
                             'AZMP',
                             'fixedStation',
                             'prince5',
                             'indicators',
                             'monthly',
                             paste0(min(da[['climatologyYears']]),
                                    'to',
                                    max(da[['climatologyYears']]),
                                    'climatology'),
                             intLimits[icf],
                             sep = '_'),
                       '.csv')
    # save data ----
    write.csv(x = df,
              file = paste(destDirCsv, filename, sep = '/'),
              row.names = FALSE)
  }
}