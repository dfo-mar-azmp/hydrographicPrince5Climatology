rm(list=ls())
source('00_setupFile.R')
# define years
## no unique cruisename in filename
years1 <- 1981:1999
## naming convention with BCDyyy669 in filename
years2 <- 2000:2020
## combine together
years <- c(years1, years2)

# iterate through each year
## years1
stnfiles <- NULL
for(y in years){
  cat(paste('Reading in year :', y), sep = '\n')
  path <- paste(arcPath, y, sep = '/')
  if(y %in% years1){ # not known cruisname
    # list all files
    files <- list.files(path, pattern = '^CTD_\\w+_\\w+_\\w+_DN\\.ODF$', full.names = TRUE)
    files <- files[!basename(files) %in% badfiles]
    cat(paste('     Found', length(files), 'files for year', y,'.'), sep = '\n')
    # read in all files
    d <- lapply(files, read.ctd.odf)
    # get lon/lat out of each file
    lon <- unlist(lapply(d, function(k) k[['longitude']][1]))
    lat <- unlist(lapply(d, function(k) k[['latitude']][1]))
    # check if it's in the station2Polygon
    pip <- point.in.polygon(point.x = lon,
                            point.y = lat,
                            pol.x = prince5Polygon[['longitude']],
                            pol.y = prince5Polygon[['latitude']])
    ok <- pip > 0
    if(any(ok == TRUE)){
      cat(paste('       Found ', length(files[ok]), 'Prince 5 files.'), sep = '\n')
      stnfiles <- c(stnfiles,
                    files[ok])
    }
  }
  if(y %in% years2){ # known cruisename
    path <- paste(arcPath, y, sep = '/')
    files <- list.files(path, pattern = paste0('^CTD_BCD',y,'669_\\w+_\\w+_DN\\.ODF$'), full.names = TRUE)
    cat(paste('     Found', length(files), 'Prince 5 files.'), sep = '\n')
    if(length(files) != 0){
      stnfiles <- c(stnfiles, files)
    } else {
      cat(paste('No files found for year ', y), sep = '\n')
    }
  }
}

archiveFiles <- stnfiles
save(archiveFiles, file = paste(destDirData, 'archiveFileList.rda', sep = '/'))