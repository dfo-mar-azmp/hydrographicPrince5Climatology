rm(list=ls())
source('00_setupFile.R')
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
    ## order by month ----
    o <- order(df[['month']])
    df <- df[o, ]
    # set up netCDF output ----
    ## define dimensions ----
    ### time ----
    climYears <- da[['climatologyYears']]
    umon <- unique(df[['month']])
    fakeYear <- min(climYears) - 1 + ((diff(range(climYears)) + 1)/2)
    fakeDay <- 15
    fakeTime <- as.POSIXct(paste(fakeYear, umon, fakeDay, sep = '-'), origin = '1970-01-01', tz = 'UTC')
    dimtime <- ncdim_def(name = 'time',
                         units = "seconds since 1970-01-01 00:00:00",
                         vals = as.numeric(fakeTime),
                         unlim = TRUE, # think this is standard practice for the time variable
                         calendar = 'standard')
    ### nv for depth bounds
    dimnv <- ncdim_def(name = 'nv',
                       units = "",
                       vals = 1:2,
                       create_dimvar = FALSE)
    ## define variables ----
    varT <- ncvar_def(name = paste('average_temperature', intLimits[icf], sep = '_'), # CF
                      #name = 'TEMPP901', # BODC
                      units = 'degree_C',
                      longname = "Temperature (ITS-90) of the water body",
                      missval = NA,
                      prec = 'double',
                      dim = list(dimtime))
    varTSD <- ncvar_def(name = paste('average_temperature', intLimits[icf], 'standard_deviation', sep = '_'),
                        #name = "TEMPSD01",
                        units = 'degree_C',
                        longname = "Temperature (ITS-90) standard deviation of the water body",
                        missval = NA,
                        prec = 'double',
                        dim = list(dimtime))
    varS <- ncvar_def(name = paste('average_salinity', intLimits[icf], sep = '_'), # CF
                      #name = 'PSLTZZ01', # BODC
                      units = 'none',
                      longname = "Practical salinity of the water body",
                      missval = NA,
                      prec = 'double',
                      dim = list(dimtime))
    varSSD <- ncvar_def(name = paste('average_salinity', intLimits[icf], 'standard_deviation', sep = '_'),
                        #name = "SDALPR01",
                        units = 'none',
                        longname = 'Practical salinity standard deviation of the water body by conductivity cell and computation using UNESCO 1983 algorithm',
                        missval = NA,
                        prec = 'double',
                        dim = list(dimtime))
    varST <- ncvar_def(name = paste('average_sigma_theta', intLimits[icf], sep = '_'),
                       #name = 'SIGTPR01',
                       units = 'kg m-3',
                       longname = "Sigma-theta of the water body by CTD and computation from salinity and potential temperature using UNESCO algorithm",
                       missval = NA,
                       prec = 'double',
                       dim = list(dimtime))
    varSTSD <- ncvar_def(name = paste('average_sigma_theta', intLimits[icf], 'standard_deviation', sep = '_'),
                         units = 'kg m-3',
                         longname = "Sigma-theta standard deviation of the water body by CTD and computation from salinity and potential temperature using UNESCO algorithm",
                         missval = NA,
                         prec = 'double',
                         dim = list(dimtime))
    varStrat <- ncvar_def(name = paste('stratification', intLimits[icf], sep = '_'),
                          units = 'kg m-4',
                          longname = "Stratification",
                          missval = NA,
                          prec = 'double',
                          dim = list(dimtime))
    varStratSD <- ncvar_def(name = paste('stratification', intLimits[icf], 'standard_deviation', sep = '_'),
                            units = 'kg m-4',
                            longname = "Stratification standard deviation",
                            missval = NA,
                            prec = 'double',
                            dim = list(dimtime))
    varStratIdx <- ncvar_def(name = 'stratification_index',
                             units = 'kg m-4',
                             longname = "Stratification index",
                             missval = NA,
                             prec = 'double',
                             dim = list(dimtime))
    varStratIdxSD <- ncvar_def(name = 'stratification_index_standard_deviation',
                               units = 'kg m-4',
                               longname = "Stratification index standard deviation",
                               missval = NA,
                               prec = 'double',
                               dim = list(dimtime))
    varMLD <- ncvar_def(name = 'mixed_layer_depth',
                        units = 'm',
                        longname = "Mixed layer depth",
                        missval = NA,
                        prec = 'double',
                        dim = list(dimtime))
    varMLDSD <- ncvar_def(name = 'mixed_layer_depth_standard_deviation',
                          units = 'm',
                          longname = "Mixed layer depth standard deviation",
                          missval = NA,
                          prec = 'double',
                          dim = list(dimtime))
    climatologyBounds <- ncvar_def(name = 'climatology_bounds',
                                   units = 'seconds since 1970-01-01',
                                   dim = list(dimnv, dimtime),
                                   missval = NA,
                                   prec = 'double')
    depthBounds <- ncvar_def(name = 'depth_bounds',
                             units = 'm',
                             dim = list(dimnv),
                             missval = NA,
                             prec = 'double')
    ## combine all ncvar defined variables ----
    varlist <- list(varT, varTSD,
                    varS, varSSD,
                    varST, varSTSD,
                    varStrat, varStratSD,
                    varStratIdx, varStratIdxSD,
                    varMLD, varMLDSD,
                    depthBounds, climatologyBounds)
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
                       '.nc')
    # initialize file ----
    con <- nc_create(filename = paste(destDirNetCDF, filename, sep = '/'),
                     vars = varlist)
    # add additional attributes to defined variables  ----
    ## temperature ----
    ncatt_put(con, varT, 'standard_name', 'sea_water_temperature')
    ncatt_put(con, varT, 'sdn_parameter_name', 'Temperature of the water body')
    ncatt_put(con, varT, 'sdn_parameter_urn', 'TEMPPR01')
    ncatt_put(con, varT, 'sdn_uom_name', 'degrees Celcius')
    ncatt_put(con, varT, 'sdn_uom_urn', 'UPAA')
    ncatt_put(con, varT, 'cell_method', 'time: mean over years depth: mean') # for climatology calculation (see note below 'set up netCDF output' code section header)
    ncatt_put(con, varT, 'coordinates', 'depth_bounds')
    ## temperature standard deviation ----
    ncatt_put(con, varTSD, 'standard_name', 'sea_water_temperature_standard_deviation')
    ncatt_put(con, varTSD, 'sdn_parameter_name', 'Temperature (ITS-90) standard deviation of the water body')
    ncatt_put(con, varTSD, 'sdn_parameter_urn', 'TEMPSD01')
    ncatt_put(con, varTSD, 'sdn_uom_name', 'degrees Celcius')
    ncatt_put(con, varTSD, 'sdn_uom_urn', 'UPAA')
    ncatt_put(con, varTSD, 'cell_method', 'time: standard_deviation over years depth: mean')
    ncatt_put(con, varTSD, 'coordinates', 'depth_bounds')
    ## salinity ----
    ncatt_put(con, varS, 'standard_name', 'sea_water_practical_salinity')
    ncatt_put(con, varS, 'sdn_parameter_name', 'Practical salinity of the water body')
    ncatt_put(con, varS, 'sdn_parameter_urn', 'PSLTZZ01')
    ncatt_put(con, varS, 'sdn_uom_name', '')
    ncatt_put(con, varS, 'sdn_uom_urn', '')
    ncatt_put(con, varS, 'cell_method', 'time: mean over years depth: mean')
    ncatt_put(con, varS, 'coordinates', 'depth_bounds')
    ## salinity standard deviation ----
    ncatt_put(con, varSSD, 'standard_name', 'sea_water_practical_salinity_standard_deviation')
    ncatt_put(con, varSSD, 'sdn_parameter_name', 'Practical salinity standard deviation of the water body by conductivity cell and computation using UNESCO 1983 algorithm')
    ncatt_put(con, varSSD, 'sdn_parameter_urn', 'SDALPR01')
    ncatt_put(con, varSSD, 'sdn_uom_name', '')
    ncatt_put(con, varSSD, 'sdn_uom_urn', '')
    ncatt_put(con, varSSD, 'cell_method', 'time: standard_deviation over years depth: mean')
    ncatt_put(con, varSSD, 'coordinates', 'depth_bounds')
    ## sigmaTheta ----
    ncatt_put(con, varST, 'standard_name', 'sea_water_sigma_theta')
    ncatt_put(con, varST, 'sdn_parameter_name', "Sigma-theta of the water body by CTD and computation from salinity and potential temperature using UNESCO algorithm")
    ncatt_put(con, varST, 'sdn_parameter_urn', 'SIGTPR01')
    ncatt_put(con, varST, 'sdn_uom_name', 'kilograms per cubic meter')
    ncatt_put(con, varST, 'sdn_uom_urn', 'UKMC')
    ncatt_put(con, varST, 'cell_method', 'time: mean over years')
    ncatt_put(con, varST, 'coordinates', 'depth_bounds')
    ## sigmaTheta standard deviation ----
    ncatt_put(con, varSTSD, 'cell_method', 'time: standard_deviation over years depth: mean')
    ncatt_put(con, varSTSD, 'coordinates', 'depth_bounds')
    ## time
    ncatt_put(con, "time", 'climatology', 'climatology_bounds')
    # store global attributes (indicated by varid = 0 in ncatt_put) ----
    ## define some information ----
    title <- paste("The Maritimes region Atlantic Zone Monitoring Program",
                   'Prince 5',
                   'monthly indicators',
                   'climatology for the',
                   paste(min(da[['climatologyYears']]), max(da[['climatologyYears']]), sep = ' to '),
                   'reference period.')
    institute <- 'Department of Fisheries and Oceans Canada, Bedford Institute of Oceanography'
    author <- 'Chantelle Layton'
    source <- ''
    history <- ''
    references <- ''
    comment <- ''
    ## add ----
    ncatt_put(con, varid = 0, attname = 'title', attval = title)
    ncatt_put(con, varid = 0, attname = 'source', attval = source)
    ncatt_put(con, varid = 0, attname = 'history', attval = history)
    ncatt_put(con, varid = 0, attname = 'comment', attval = comment)
    ncatt_put(con, varid = 0, attname = 'Conventions', attval = 'CF-1.8')
    ncatt_put(con, varid = 0, attname = "country_code", attval = 1810)
    ncatt_put(con, varid = 0, attname = "sdn_country_id", attval = "SDN:C18::18")
    ncatt_put(con, varid = 0, attname = "sdn_country_vocabulary", attval = "http://vocab.nerc.ac.uk/collection/C18/current/")
    ncatt_put(con, varid = 0, attname = "institution", attval= "DFO BIO")
    ncatt_put(con, varid = 0, attname = "sdn_institution_id", attval= "SDN:EDMO::1811")
    ncatt_put(con, varid = 0, attname = "sdn_institution_vocabulary", attval = "https://edmo.seadatanet.org, EUROPEAN DIRECTORY OF MARINE ORGANISATIONS (EDMO)")
    ncatt_put(con, varid = 0, attname = "creator_type", attval = "person")
    ncatt_put(con, varid = 0, attname = "creator_name", attval = "Chantelle Layton")
    ncatt_put(con, varid = 0, attname = "creator_country", attval = "Canada")
    ncatt_put(con, varid = 0, attname = "creator_email", attval = "BIO.Datashop@dfo-mpo.gc.ca")
    ncatt_put(con, varid = 0, attname = "creator_institution", attval = "Bedford Institute of Oceanography")
    ncatt_put(con, varid = 0, attname = "creator_address", attval= "P.O. Box 1006, 1 Challenger Dr.")
    ncatt_put(con, varid = 0, attname = "creator_city", attval = "Dartmouth")
    ncatt_put(con, varid = 0, attname = "creator_sector", attval = "gov_federal")
    ncatt_put(con, varid = 0, attname = "creator_url", attval = "https://www.bio.gc.ca/index-en.php")
    ncatt_put(con, varid = 0, attname = "sdn_creator_id", attval = "SDN:EDMO::1811")
    ncatt_put(con, varid = 0, attname = "license", attval = "Open Government Licence – Canada,  https://open.canada.ca/en/open-government-licence-canada")
    ## station coordinates ----
    ncatt_put(con, varid = 0, attname = 'longitude', attval = p5lon)
    ncatt_put(con, varid = 0, attname = 'latitude', attval = p5lat)
    # store data ----
    ncvar_put(nc = con, varid = varT, vals = df[['integratedTemperature']])
    ncvar_put(nc = con, varid = varTSD, vals = df[['integratedTemperatureSD']])
    ncvar_put(nc = con, varid = varS, vals = df[['integratedSalinity']])
    ncvar_put(nc = con, varid = varSSD, vals = df[['integratedSalinitySD']])
    ncvar_put(nc = con, varid = varST, vals = df[['integratedSigmaTheta']])
    ncvar_put(nc = con, varid = varSTSD, vals = df[['integratedSigmaThetaSD']])
    ncvar_put(nc = con, varid = varStrat, vals = df[['stratification']])
    ncvar_put(nc = con, varid = varStratSD, vals = df[['stratificationSD']])
    ncvar_put(nc = con, varid = varStratIdx, vals = df[['stratificationIndex']])
    ncvar_put(nc = con, varid = varTSD, vals = df[['stratificationIndexSD']])
    ncvar_put(nc = con, varid = varMLD, vals = df[['mixedLayerDepthDefault']])
    ncvar_put(nc = con, varid = varMLDSD, vals = df[['mixedLayerDepthDefaultSD']])
    ## depth bounds
    ncvar_put(nc = con, varid = depthBounds, vals = as.numeric(strsplit(x = intLimits[icf], split = '_')[[1]]))
    ## construct climatologyBounds data
    climBoundsStart <- as.POSIXct(paste(min(climYears), umon, '01', sep = '-'), origin = '1970-01-01', tz = 'UTC')
    spd <- 60 * 60 * 24
    ### last year of reference period, end day is last day of the month, so contruct first of every month then subtract a day
    climBoundsEnd <- seq.POSIXt(from = as.POSIXct(paste(max(climYears), min(umon) + 1, '01', sep = '-'), tz = 'UTC'),
                                to = as.POSIXct(paste(max(climYears) + 1, min(umon), '01', sep = '-'), tz = 'UTC'),
                                by = 'month') - spd
    climBounds <- matrix(data = c(climBoundsStart,
                                  climBoundsEnd),
                         nrow = 2,
                         ncol = length(umon),
                         byrow = TRUE)
    ncvar_put(nc = con, varid = climatologyBounds, vals = climBounds)
    # close file connection ----
    (con)
    nc_close(con)
  }
}