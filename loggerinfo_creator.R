#################################################

# COMBINE METADATA FOR ALL SITES IN LOGGERINFO CSV

#################################################
# Attach packages
library(plyr)
library(readr)
library(here)
library(lubridate)
library(tidyverse)
library(purrr)
library(Rfast)
library(tidyr)
library(splitstackshape)
library(stringr)

#################################################

# Access file_name_csv, loggerID, site, and zone from csv files

#################################################

# Import all csv files 
csv_files <- list.files(
  path = "~/Documents/Research/Intertidal-Temp-Shiny-App/robo_updated/logger_data_csv/"
)

#################################################

# Initialize file_name_csv, loggerID, and site; then fill
file_name_csv <- rep(NA, length.out = length(csv_files))
loggerID <- rep(NA, length.out = length(csv_files))
site <- rep(NA, length.out = length(csv_files))
zone <- rep(NA, length.out = length(csv_files))

for (i in 1:length(csv_files)){
  #if (paste(basename(csv_files[i])) == "BMRMUSCAAG30_2015NOV.csv"){
  #  loggerID[i] <- str_sub(file_name_csv[i], end = 12)
  #}
  #else {
  file_name_csv[i] <- basename(csv_files[i])
  loggerID[i] <- str_sub(file_name_csv[i], end = -11)
  site[i] <- substr(file_name_csv[i], 9, 10)
  #}
}
# PROBLEM AREAS: [16, 43] BUT NA ZONES SO EXCLUDE

# Initialize zone
zone <- rep(NA, length.out = length(csv_files))

# Fill zone
zone <- paste(
  str_sub(
    info_microsite_ca$zone[match(
      loggerID, info_microsite_ca$microsite_id, nomatch = NA_character_,
      incomparables = TRUE)],
    start = 3))

# Combine all into df
logger_info <- data.frame(file_name_csv, loggerID, site, zone)


# EXCLUDE UPPER-MID and LOWER-MID
logger_info_2 <- subset(
  logger_info,
  logger_info$zone == "Low" | logger_info$zone == "Mid" | logger_info$zone == "High")


#################################################

# Write to a csv
write.csv(
  logger_info_2,
  "~/Documents/Research/Intertidal-Temp-Shiny-App/logger_data/logger_info4.csv",
  row.names = FALSE)

#################################################

