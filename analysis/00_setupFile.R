library(csasAtlPhys)
library(oce)
library(sp)
library(fields)
# check if makeDirs exists (might be defined in other scripts that are sourcing it, e.g. in the report)
if(!exists('makeDirs')){
  makeDirs <- TRUE
}
# define bounds
prince5Polygon <- data.frame(longitude = c(-66.9, -66.9, -66.6, -66.6),
                             latitude = c(44.8, 45, 45, 44.8))
p5lon <- -66.850
p5lat <- 44.930
# define climatologyYears
climatologyYears <- list(1981:2010,
                         1991:2020)
# load file that defines arcPath
## define file
pathToArcFile <- ifelse(basename(getwd()) == 'analysis', './', '../analysis') # to avoid errors when building report
arcPathFile <- paste(pathToArcFile, '00_arcPath.R', sep = '/')
## function to check
checkArcPath <- function(file){
  if(file.exists(file)){
    source(file)
    if(!exists('arcPath')){
      stop(paste("Please define 'arcPath' in", file))
    }
  } else {
    stop(paste(file, 'does not exist.',
               "Please create the file and define 'arcPath'.",
               "See README.md for details."))
  }
}
## check
checkArcPath(file = arcPathFile)

# these files will cause issues due to their long header or formatting issues
badfiles <- c('CTD_TEL2006615_092_288086_DN.ODF',
              'CTD_NED2003003_000_258853_DN.ODF',
              'CTD_TEM2004004_088_263653_DN.ODF',
              'CTD_HUD2013037_042_1_DN.ODF',
              "CTD_HUD2015030_127_1_DN.ODF",
              "CTD_96999_003_001_DN.ODF", # variable names bad
              "CTD_96999_003_002_DN.ODF" # variable names bad
)

# check if directory for saving various data files has been created
destDirData <- './data'
destDirFigures <- './figures'
destDirSuppFigures <- './supplementaryFigures'
destDirNetCDF <- './netCDFclimatology'
destDirCsv <- './csvClimatology'

dirsToMake <- c(destDirData,
                destDirFigures,
                destDirSuppFigures,
                destDirNetCDF,
                destDirCsv)
if(makeDirs){
  for(i in 1:length(dirsToMake)){
    if(!dir.exists(dirsToMake[i])) dir.create(dirsToMake[i], recursive = TRUE)
  }
}