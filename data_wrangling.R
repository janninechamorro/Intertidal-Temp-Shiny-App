
#Trying to use Sam Csik's code write something similar for robomussels data, as of 8/25/20, I havent gotten it to work yet. 
#What it should do is create a function to upload the files all together.

library(tidyverse)
library(here)
library(chron)
library(lubridate)
library(naniar)

metadata<-read_csv(here::here("logger_data","logger_info2.csv"))

import_temp_files<-function(file_name_csv, site, zone, loggerID){
  read_csv(here::here("logger_data","robomussel_data", file_name_csv), col_names=TRUE, col_types=cols(.default=col_character())) %>% 
    mutate(Time_GMT=mdy_hm(Time_GMT), local_datetime=with_tz(Time_GMT, tz="America/Los_Angeles"),
           year=substr(local_datetime, 1,4), month=substr(local_datetime,6,7), day=substr(local_datetime,9,10)) %>% 
    mutate(Time_GMT=as.POSIXct(Time_GMT, "%m/%d/%Y %H:%M:%S", tz="GMT"), 
           local_datetime=with_tz(Time_GMT, tz="America/Los_Angeles"),
           year=substr(local_datetime, 1,4), 
           month=substr(local_datetime,6,7), 
           day=substr(local_datetime,9,10),
           year=as.factor(year),
           month=as.factor(month),
           day=as.numeric(day),
           Temp_C=as.numeric(Temp_C),
           site=rep(site),
           zone=rep(zone),
           loggerID=rep(loggerID))}

#initialize empty df to fill with cleaned data

temp_data<-data.frame(local_datetime=as.Date(character()),
                      year=factor(),
                      month=factor(),
                      day=numeric(),
                      site=as.character(),
                      zone=as.character(),
                      loggerID=as.character(),
                      Temp_C=numeric())

#for loop reads in/wrangles all files using the import_temp_files() function above

for(row in 1:nrow(metadata)){
  data_row<-metadata[row,]
  file_name_csv<-as.character(data_row[,1])
  print(file_name_csv)
  site<-as.character(data_row[,3])
  print(site)
  zone<-as.character(data_row[,4])
  print(zone)
  loggerID<-as.character(data_row[,2])
  print(loggerID)
  table<-import_temp_files(file_name_csv, site, zone, loggerID)
   temp_data<-rbind(temp_data, table)
   }

