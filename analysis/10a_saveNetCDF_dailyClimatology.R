rm(list=ls())
library(ncdf4)
library(oce)
source('00_setupFile.R')
# load daily climatology ----
load(paste(destDirData, 'dailyClimatology.rda', sep = '/'))
# iterate through each climatology ----
for(ic in 1:length(climatology)){
  df <- climatology[[ic]][['dailyClimatologyDf']]
  # convert some rows from factor to numeric values ----
  df[['yearDay']] <- as.numeric(as.character(df[['yearDay']]))
  df[['depth']] <- as.numeric(as.character(df[['depth']]))
  # make a matrix of each variable by depth (rows) and yearDay (columns) ----
  ## get unique days for columns ----
  uday <- sort(unique(df[['yearDay']]))
  ## get unique depths for rows ----
  udepth <- sort(unique(df[['depth']]))
  ## get list of variables ----
  vars <- names(df)[!names(df) %in% c('yearDay', 'depth')]
  ## define output ----
  mvars <- vector(mode = 'list', length = length(vars))
  ## put data into matrix ----
  for(iv in 1:length(vars)){
    # get data ----
    var <- vars[iv]
    data <- df[[var]]
    # define matrix ----
    mvars[[iv]] <- matrix(data = NA, nrow = length(udepth), ncol = length(uday))
    for(iday in 1:length(uday)){ # rows
      lookday <- uday[iday]
      okrow <- df[['yearDay']] == lookday
      for(idep in 1:length(udepth)){ # columns
        lookdepth <- udepth[idep]
        okcol <- df[['depth']] == lookdepth
        okdata <- which(okrow & okcol)
        mvars[[iv]][idep, iday] <- data[okdata]
      } # closes idep
    } # closes iday
  } # closes vars
  ## name list with variables ----
  names(mvars) <- vars
  # set up netCDF output ----
  ## define dimensions ----
  ### pressure (depth) ----
  dimpress <- ncdim_def(#name = 'PRESPR01', # BODC
    name = 'sea_water_pressure', # CF
    units = 'dbar',
    vals = udepth,
    longname = 'Pressure (spatial coordinate) exerted by the water body by profiling pressure sensor and correction to read zero at sea level')
  ### time ----
  dimtime <- ncdim_def(name = 'time',
                       units = "days since 0001-01-01 00:00:00",
                       vals = uday,
                       unlim = TRUE, # think this is standard practice for the time variable
                       calendar = 'noleap')
  ## define variables ----
  varT <- ncvar_def(name = 'sea_water_temperature', # CF
                    #name = 'TEMPP901', # BODC
                    units = 'degree_C',
                    longname = "Temperature (ITS-90) of the water body",
                    missval = NA,
                    prec = 'double',
                    dim = list(dimpress, dimtime))
  varTSD <- ncvar_def(name = 'sea_water_temperature_standard_deviation',
                      #name = "TEMPSD01",
                      units = 'degree_C',
                      longname = "Temperature (ITS-90) standard deviation of the water body",
                      missval = NA,
                      prec = 'double',
                      dim = list(dimpress, dimtime))
  varS <- ncvar_def(name = 'sea_water_practical_salinity', # CF
                    #name = 'PSLTZZ01', # BODC
                    units = 'none',
                    longname = "Practical salinity of the water body",
                    missval = NA,
                    prec = 'double',
                    dim = list(dimpress, dimtime))
  varSSD <- ncvar_def(name = 'sea_water_practical_salinity_standard_deviation',
                      #name = "SDALPR01",
                      units = 'none',
                      longname = 'Practical salinity standard deviation of the water body by conductivity cell and computation using UNESCO 1983 algorithm',
                      missval = NA,
                      prec = 'double',
                      dim = list(dimpress, dimtime))
  varST <- ncvar_def(name = 'sea_water_sigma_theta',
                     #name = 'SIGTPR01',
                     units = 'kg m-3',
                     longname = "Sigma-theta of the water body by CTD and computation from salinity and potential temperature using UNESCO algorithm",
                     missval = NA,
                     prec = 'double',
                     dim = list(dimpress, dimtime))
  varSTSD <- ncvar_def(name = 'sea_water_sigma_theta_standard_deviation',
                       units = 'kg m-3',
                       longname = "Sigma-theta standard deviation of the water body by CTD and computation from salinity and potential temperature using UNESCO algorithm",
                       missval = NA,
                       prec = 'double',
                       dim = list(dimpress, dimtime))
  ## combine all ncvar defined variables ----
  varlist <- list(varT, varTSD,
                  varS, varSSD,
                  varST, varSTSD)
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
  ## temperature standard deviation ----
  ncatt_put(con, varTSD, 'standard_name', 'sea_water_temperature_standard_deviation')
  ncatt_put(con, varTSD, 'sdn_parameter_name', 'Temperature (ITS-90) standard deviation of the water body')
  ncatt_put(con, varTSD, 'sdn_parameter_urn', 'TEMPSD01')
  ncatt_put(con, varTSD, 'sdn_uom_name', 'degrees Celcius')
  ncatt_put(con, varTSD, 'sdn_uom_urn', 'UPAA')
  ## salinity ----
  ncatt_put(con, varS, 'standard_name', 'sea_water_practical_salinity')
  ncatt_put(con, varS, 'sdn_parameter_name', 'Practical salinity of the water body')
  ncatt_put(con, varS, 'sdn_parameter_urn', 'PSLTZZ01')
  ncatt_put(con, varS, 'sdn_uom_name', '')
  ncatt_put(con, varS, 'sdn_uom_urn', '')
  ## salinity standard deviation ----
  ncatt_put(con, varSSD, 'standard_name', 'sea_water_practical_salinity_standard_deviation')
  ncatt_put(con, varSSD, 'sdn_parameter_name', 'Practical salinity standard deviation of the water body by conductivity cell and computation using UNESCO 1983 algorithm')
  ncatt_put(con, varSSD, 'sdn_parameter_urn', 'SDALPR01')
  ncatt_put(con, varSSD, 'sdn_uom_name', '')
  ncatt_put(con, varSSD, 'sdn_uom_urn', '')
  ## sigmaTheta ----
  ncatt_put(con, varST, 'standard_name', 'sea_water_sigma_theta')
  ncatt_put(con, varST, 'sdn_parameter_name', "Sigma-theta of the water body by CTD and computation from salinity and potential temperature using UNESCO algorithm")
  ncatt_put(con, varST, 'sdn_parameter_urn', 'SIGTPR01')
  ncatt_put(con, varST, 'sdn_uom_name', 'kilograms per cubic meter')
  ncatt_put(con, varST, 'sdn_uom_urn', 'UKMC')
  # store global attributes (indicated by varid = 0 in ncatt_put) ----
  ## define some information ----
  title <- paste("The Maritimes region Atlantic Zone Monitoring Program",
                 'Prince 5',
                 'daily hydrographic',
                 'climatology for the',
                 paste(min(climatology[[ic]][['climatologyYears']]), max(climatology[[ic]][['climatologyYears']]), sep = ' to '),
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
  ncvar_put(nc = con, varid = varT, vals = mvars[['temperature']])
  ncvar_put(nc = con, varid = varTSD, vals = mvars[['temperatureSD']])
  ncvar_put(nc = con, varid = varS, vals = mvars[['salinity']])
  ncvar_put(nc = con, varid = varSSD, vals = mvars[['salinitySD']])
  ncvar_put(nc = con, varid = varST, vals = mvars[['sigmaTheta']])
  ncvar_put(nc = con, varid = varSTSD, vals = mvars[['sigmaThetaSD']])
  # close file connection ----
  (con)
  nc_close(con)
} # closes climatology