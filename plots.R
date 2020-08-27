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

#ok, let's just turn this into raw abundance per sampling year- turns out multiple dates are
#just the same sampling event

sbc$isodate<-ymd(sbc$DATE)

summary.sbc<-ddply(sbc.1, c("YEAR", "SITE", "TRANSECT"), summarise,
                         tot.biomass=sum(DRY_GM2, na.rm=T))

summary.sbc.bysite<-ddply(summary.sbc, "YEAR", summarise,
                          mean.mass=mean(tot.biomass), 
                          se.mass=(sd(tot.biomass)/sqrt(length(tot.biomass))))


plot(summary.sbc.bysite$YEAR, summary.sbc.bysite$mean.mass)


#ok, work-with-able time series there


#let's get mcr now

mcr<-read.csv(file="MCR_LTER_Annual_Fish_Survey_20190519.csv", 
              header=T, na.strings=c("",".","NA", -99999,-99999.00))
summary(mcr)

#only want the fish

#this one IS just fish

#get a list of species names so we can pull those out later

unique(mcr$Taxonomy)

#ok, let's just turn this into raw biomass per sampling year- turns out multiple dates are
#just the same sampling event


summary.mcr<-ddply(mcr, c("Year", "Site", "Transect", "Swath"), summarise,
                   tot.biomass=(sum(Biomass, na.rm=T))/1000)

summary.mcr.bysite<-ddply(summary.mcr, "Year", summarise,
                          mean.mass=mean(tot.biomass), 
                          se.mass=(sd(tot.biomass)/sqrt(length(tot.biomass))))


plot(summary.mcr.bysite$Year, summary.mcr.bysite$mean.mass)


#another work-with-able time series 

#let's get ntl now

ntl<-read.csv(file="https://lter.limnology.wisc.edu/sites/default/files/data/ntl7_v8.csv", 
                   header=T, na.strings=c("",".","NA"))

summary(ntl)

# it's a bit inelegant and reductive, but let's just use biomass as our response variables- totals per day per lake


#looks like largemouth bass should be the species of focus because they're most common

#ntl.fish2<-ntl.fish1[which(ntl.fish1$spname=="LARGEMOUTHBASS"),]

#then we want to get rid of the day column, because we're just treating


#get a list of species names so we can pull those out later

unique(ntl$spname)

#ok, let's just turn this into raw biomass per sampling year- turns out multiple dates are
#just the same sampling event


summary.ntl<-ddply(ntl, c("year4", "lakeid", "effort"), summarise,
                   tot.caught=(sum(total_caught, na.rm=T)))

summary.ntl.bysite<-ddply(summary.ntl, "year4", summarise,
                          mean.mass=mean(tot.biomass), 
                          se.mass=(sd(tot.biomass)/sqrt(length(tot.biomass))))


plot(summary.ntl.bysite$year4, summary.ntl.bysite$mean.mass)


#another work-with-able time series 




