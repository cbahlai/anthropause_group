library(reshape2)
library(plyr)
library(ggplot2)
library(gridExtra)
library(lubridate)

#create comparative fisheries plot

#will use mcr, ntl and sbc fish data

#going to brute force it for first approximation

sbc<-read.csv(file="https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-sbc.50.7&entityid=24d18d9ebe4f6e8b94e222840096963c", 
              header=T, na.strings=c("",".","NA", -99999,-99999.00))
summary(sbc)

#only want the fish

sbc.1<-sbc[which(sbc$COARSE_GROUPING=="FISH"),]

#get a list of common names so we can pull those out later

unique(sbc.1$COMMON_NAME)

#ok, let's just turn this into raw abundance per sampling per year

sbc$isodate<-ymd(sbc$DATE)



