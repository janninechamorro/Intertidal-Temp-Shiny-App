############################################################

# PROGRAM TO SAVE ALL SITE METADATA IN LOGGERINFO2.csv

############################################################
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

#################################################

# Import all temp data

CP_files <- list.files(
  path = "~/Documents/Research/Intertidal-Temp-Shiny-App/robo_updated/CP_CoalOilPoint/",
  pattern = "*.txt", full.names = TRUE)

AG_files <- list.files(
  path = "~/Documents/Research/Intertidal-Temp-Shiny-App/robo_updated/AG_Alegria/",
  pattern = "*.txt", full.names = TRUE)

LL_files <- list.files(
  path = "~/Documents/Research/Intertidal-Temp-Shiny-App/robo_updated/LL_LompocLanding/",
  pattern = "*.txt", full.names = TRUE)

#################################################

# fileCombiner definition

fileCombiner <- function(filesList){
  
  # Initialize file_paths vector for storing loggernames
  filepaths <- rep(NA, length.out = length(filesList))
  
  # Store filepaths in vector, filepaths
  for (i in 1:length(filesList)) { #loop through 
    if (paste(substr(basename(filesList[i]), 12, 12)) == "_"){
      filepaths[i] <- paste(substr(basename(filesList[i]), 1, 11))
    }
    if (paste(substr(basename(filesList[i]), 13, 13)) == "_"){
      filepaths[i] <- paste(substr(basename(filesList[i]), 1, 12))
    }
    else {
      filepaths[i] <- paste(substr(basename(filesList[i]), 1, 13))
    }
  }
  
  # Read all .txt files; combine as df
  files_df <- ldply(filesList, read.table, sep = "\t", fill = TRUE,
                    header = TRUE, .id = NA)
  
  # Initialize loggernames
  loggernames <- rep(NA,length.out = nrow(files_df))
  
  # Initialize nrow_per_file, then fill
  nrow_per_file <- rep(NA, length.out = length(filesList))
  
  for (i in 1:length(filesList)){
    nrow_per_file[i] <- nrow(read.table(filesList[i], sep = "\t", 
                                        fill = TRUE, header = T))
  }
  
  # Replicate (file path of each file) x (nrow for that file)
  # to fill loggernames for all datapoints for all files
  file_info <- data.frame(filepaths, nrow_per_file)
  
  loggernames <- expandRows(file_info, count = nrow_per_file,
                            count.is.col = F, drop = T) 
  
  # Isolate loggernames vector as loggerID
  loggerID <- loggernames$filepaths
  
  # Use loggerIDs to fill sitenames
  for (i in 1:length(loggernames)){
    site[i] <- substr(loggerID[i], start = 5, stop = 10)
  }
  
  data <- data.frame(files_df, loggerID, site)
  
  return(data)
}


################################################

# Combine all .txt files

CP_data <- fileCombiner(CP_files)
AG_data <- fileCombiner(AG_files)
LL_data <- fileCombiner(LL_files)
View(CP_data); View(AG_data); View(LL_data)

# Combine all data into one df
all_data <- rbind(AG_data, CP_data, LL_data)
View(all_data)

################################################

# Add zone & subzone data

info_microsite <- read.csv("~/Documents/Research/Intertidal-Temp-Shiny-App/InfoMicrosite_2016_mussel.csv")

info_microsite_ca <- filter(info_microsite, info_microsite$site %in% all_data$site)

View(info_microsite_ca)

# Initialize vectors to store zones & subzones
all_data$zone <- rep(NA, length.out = nrow(all_data))
all_data$subzone <- rep(NA, length.out = nrow(all_data))

# Fill zone and subzone vectors in all_data
all_data$zone <- paste(info_microsite_ca$zone[match(
  all_data$loggerID, info_microsite_ca$microsite_id, nomatch = NA_character_,
  incomparables = TRUE)])

all_data$subzone <- paste(info_microsite_ca$sub_zone[match(
  all_data$loggerID, info_microsite_ca$microsite_id, nomatch = NA_character_,
  incomparables = TRUE)])

################################################

# Write all_data to .csv

write.csv(
  all_data,
  "~/Documents/Research/Intertidal-Temp-Shiny-App/logger_data/logger_info3.csv")

################################################









