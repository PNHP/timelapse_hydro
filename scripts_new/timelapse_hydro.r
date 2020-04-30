library(tidyverse)
library(here)
library(ggplot2)
library(lubridate)




# get local sunrise/sunset time

#######################################################################################################################
# beginnings of a function to read exif information from the camera
library(exifr)
files <- list.files(here::here("data_new","sites","testsite1"), recursive=TRUE, pattern="JPG", full.names=TRUE)
exifinfo <- read_exif(files)

# find the photos taken at 15minute intervals
phototimes <- exifinfo$FileModifyDate
phototimes
datetime <- ymd_hms(phototimes) # convert to date format
datetime <- with_tz(datetime, "America/New_York") # convert to eastern time


datetime1 <- datetime[which(minute(datetime) %in% c(0,15,30,45))]



######################################################################################################################
# function to get gage data
library(dataRetrieval)
siteNumber <- "03081500"
#parameterCd <- "00060"  # Discharge
parameterCd <- "00065" # gage height
startDate <- "2020-04-20" 
endDate <- "2020-04-29" 
dischargeUnit <- readNWISuv(siteNumber, parameterCd, startDate, endDate)
dischargeUnit <- renameNWISColumns(dischargeUnit)

plot(dischargeUnit$dateTime,dischargeUnit$GH_Inst)



