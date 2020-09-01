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

#need to standardize sampling effort, first let's just use BSEINE becasue it's been used consistently
#and lake ME because it's the urban one so prbably is most affected by local games fishers

ntl.2<-ntl[which(ntl$lakeid=="ME" & ntl$gearid=="BSEINE"),]


summary.ntl<-ddply(ntl.2, c("year4", "effort"), summarise,
                   tot.caught=(sum(total_caught, na.rm=T)))

#compute fish per unit effort
summary.ntl$fish.effort<-summary.ntl$tot.caught/summary.ntl$effort



plot(summary.ntl$year4, summary.ntl$fish.effort)


#another work-with-able time series 


#ok! let's create the individual figures



#start with NTL because it's the longest

ntl.timeseries<-ggplot(summary.ntl, aes(year4, fish.effort))+
  geom_smooth(method="loess", se=T, color="midnightblue", fill="azure3")+
  geom_line(color="cadetblue", size=1)+
  geom_point(pch=21, color="black", fill="cadetblue", size=2)+
  theme_classic(base_size = 12)+
  xlab("Year")+
  ylab("Fish per sample")+
  xlim(1980,2022)+
  annotate("text", x=1981, y=425, label="NTL", size=8)+
  geom_segment(aes(x=2019, xend=2022, ), colour="blue", linetype="longdash")+ 

ntl.timeseries


#now sbc

sbc.timeseries<-ggplot(summary.sbc.bysite, aes(YEAR, mean.mass))+
  geom_smooth(method="loess", se=T, color="darkblue", fill="azure3")+
  geom_line(color="darkcyan", size=1)+
  #geom_errorbar(aes(ymin=(mean.mass-se.mass), ymax=(mean.mass+se.mass)), width=0.2)+
  geom_point(pch=21, color="black", fill="darkcyan", size=2)+
  theme_classic(base_size = 12)+
  xlab(NULL)+
  ylab(expression(paste("Mean biomass, g/m"^"2")))+
  xlim(1980,2022)+
  theme(axis.line.x = element_blank(), axis.ticks.x=element_blank(),
        axis.text.x=element_blank())+
  annotate("text", x=1981, y=32, label="SBC", size=8)

sbc.timeseries

#now mcr

mcr.timeseries<-ggplot(summary.mcr.bysite, aes(Year, mean.mass))+
    geom_smooth(method="loess", se=T, color="navy", fill="azure3")+
  geom_line(color="deepskyblue4", size=1)+
  geom_point(pch=21, color="black", fill="deepskyblue", size=2)+
  theme_classic(base_size = 12)+
  xlab(NULL)+
  ylab(expression(paste("Mean biomass per sampling swath, g")))+
  xlim(1980,2022)+
  theme(axis.line.x = element_blank(), axis.ticks.x=element_blank(),
        axis.text.x=element_blank())+
  annotate("text", x=1981, y=32, label="MCR", size=8)

mcr.timeseries


