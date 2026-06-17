rm(list = ls())
source('00_setupFile.R')
# source functions to extract from database
source("../R/.Rprofile")
source("../R/Run_Database_Query.R")
source("../R/Write_Database_Table.R")
source("../R/Remove_Database_Table.R")
source("../R/Read_SQL.R")

sql_file <- 'extractP5.sql'
# get all the data
data <- Run_Database_Query(sql_file = sql_file,
                           sql_file_lines = NULL,
                           my.env$host,
                           my.env$port,
                           my.env$sid,
                           my.env$username,
                           my.env$password)
# remove temporary table
Remove_Database_Table("TMP", my.env$host, my.env$port, my.env$sid, my.env$username, my.env$password)

sql <- data[[1]]
df <- data[[2]]
names(df) <- tolower(names(df)) # for my sanity
save(df, file = paste(destDirData, 'climateData.rda', sep = '/'))
