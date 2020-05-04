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

imageWidth <- unique(exifinfo$ImageWidth)
imageHeight <- unique(exifinfo$ImageHeight)

exifinfo <- exifinfo[c("FileName","Directory","FileSize","FileModifyDate","GPSLatitude","GPSLongitude")] # drop the unneeded fields

# find the photos taken at 15minute intervals
exifinfo$datetime <- ymd_hms(exifinfo$FileModifyDate) # convert to date format
exifinfo$datetime <- with_tz(exifinfo$datetime, "America/New_York") # convert to eastern time

exifinfo$intervalshot <- NA
exifinfo$intervalshot <- ifelse( minute(exifinfo$datetime) %in% c(0,15,30,45)   , "yes", "no")

suitablePhotos <- exifinfo[which(exifinfo$intervalshot=="yes"),]

# get the earliest dates

datetime_start <- min(suitablePhotos$datetime)
datetime_end <- max(suitablePhotos$datetime)
date_start <- date(datetime_start)            
date_end <- date(datetime_end)            


######################################################################################################################
# function to get gage data
library(dataRetrieval)
siteNumber <- "03081500"
#parameterCd <- "00060"  # Discharge
parameterCd <- "00065" # gage height
startDate <- date_start 
endDate <- date_end 
dischargeUnit <- readNWISuv(siteNumber, parameterCd, startDate, endDate)
dischargeUnit <- renameNWISColumns(dischargeUnit)

#subset discharge units by photo time period
dischargeUnit <- dischargeUnit[which(dischargeUnit$dateTime>=datetime_start&dischargeUnit$dateTime<=datetime_end),]


##########
# make graphs

df <- merge(dischargeUnit, suitablePhotos, by.x="dateTime", by.y="datetime")

df_gagemin <- min(floor(df$GH_Inst))
df_gagemax <- max(ceiling(df$GH_Inst))
df_datemin <- min(df$dateTime)
df_datemax <- max(df$dateTime)

library(jpeg)
library(grid)

for(i in 1:nrow(df)){
  df1 <- df[1:i,]
  img <- readJPEG(paste(df$Directory[i],df$FileName[i], sep="/")) # PNG(system.file("img", "Rlogo.png", package="png"), TRUE)
  gpp <- rasterGrob(img, interpolate=TRUE)
  gpp$width <- unit(1, "npc") 
  gpp$height <- unit(1, "npc")
  a <- ggplot(df1,aes(x=dateTime,y=GH_Inst)) + 
    annotation_custom(gpp) +
    geom_line(color='steelblue', size=3) + 
    expand_limits(x=c(df_datemin,df_datemax),y=c(df_gagemin,df_gagemax)) +
    theme(panel.ontop=TRUE, panel.background=element_rect(colour="black",fill="transparent"))
  ggsave(filename = paste(df1$Directory[i],"output",paste0("photo",i,".jpg"),sep = "/"))
  print("photo saved")
}


# make a movie
library(av)

outputfiles <- list.files(here::here("data_new","sites","testsite1","output"), recursive=TRUE, pattern="jpg", full.names=TRUE)

av_encode_video(outputfiles, output=here::here("data_new","sites","testsite1","output","output.gif"), framerate=12)


# img <- readJPEG(paste(df$Directory[i],df$FileName[i], sep="/")) # PNG(system.file("img", "Rlogo.png", package="png"), TRUE)
# gpp <- rasterGrob(img, interpolate=TRUE)
# gpp$width <- unit(1, "npc") 
# gpp$height <- unit(1, "npc")
# #df <- data.frame(x=seq(1,2,0.01),y=seq(1,2,0.01))
# ggplot(df,aes(x=dateTime,y=GH_Inst)) + 
#   annotation_custom(gpp) +
#   geom_line() + 
#   theme(panel.ontop=TRUE, panel.background=element_rect(colour="black",fill="transparent"))
# 
# 
# 
# 
# 
# 
# plot(dischargeUnit$dateTime,dischargeUnit$GH_Inst)
# 
# ggplot(dischargeUnit)
# 
# p <- ggplot(dischargeUnit, aes(x=dateTime, y=GH_Inst)) +
#   geom_line() +
#   theme_minimal()
# 
# library(gganimate)
# anim <- p + 
#   transition_reveal(dateTime)
# anim
# 
