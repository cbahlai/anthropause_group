library(reshape2)
library(plyr)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(stringr)
library(grid)

#create comparative fisheries plot

#will use mcr, ntl and sbc fish data

#for sbc, lobster fisheries are most important monitored taxon- see this citation:
#Zellmer, A. J., Claisse, J. T., Williams, C. M., and Pondella, D. J. (2018). Long-term, spatial marine harvest intensity as an indicator of human impact on shallow rocky reef ecosystems. Mar. Ecol. 39, 1–9. doi:10.1111/maec.12463.
#Red sea urchin is the biggest fishery but not really targeted along the coast where we have LTER sites.  After that is rock crab (which we don't sample) and spiny lobster (which we do).  Thus, I think spiny lobster would be our best bet.



sbc<-read.csv(file="https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-sbc.77.4&entityid=f32823fba432f58f66c06b589b7efac6", 
              header=T, na.strings=c("",".","NA", -99999,-99999.00))
summary(sbc)

#sites where fishing is legal
fished.sites<-c("MOHK", "AQUE", "CARP")


#there! ok, let's pull out only the fished, er, fishes
#we also want to cut off fishes less than 15cm length, which are not targetted by fishers

sbc.fished<-sbc[which(sbc$SITE%in%fished.sites),]




summary.sbc<-ddply(sbc.fished, c("YEAR", "SITE", "TRANSECT"), summarise,
                         count=sum(COUNT, na.rm=T), traps=length(COUNT))

summary.sbc$pertrap<-summary.sbc$count/summary.sbc$traps

summary.sbc.bysite<-ddply(summary.sbc, "YEAR", summarise,
                          mean.count=mean(pertrap), 
                          se.count=(sd(pertrap)/sqrt(length(pertrap))))


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

#pull out genera
mcr$genus<-word(mcr$Taxonomy, 1)

#read in list of fished genera 
fished<-read.csv(file="MCR_fished_genera.csv", 
              header=T, stringsAsFactors = F)

unique(mcr$genus)

mcr$fished<-mcr$genus%in%fished$genus

#there! ok, let's pull out only the fished, er, fishes
#we also want to cut off fishes less than 15cm length, which are not targetted by fishers

mcr.fished<-mcr[which(mcr$fished==TRUE&mcr$Total_Length>15),]


#ok, let's just turn this into raw biomass per sampling year- turns out multiple dates are
#just the same sampling event



summary.mcr<-ddply(mcr.fished, c("Year", "Site", "Transect", "Swath"), summarise,
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
  geom_smooth(method="loess", se=T, color="midnightblue", fill="azure3", linetype="dotted")+
  geom_line(color="cadetblue", size=1)+
  geom_point(pch=21, color="black", fill="cadetblue", size=2)+
  theme_classic(base_size = 12)+
  xlab("Year")+
  ylab("Fish per sample\n")+
  xlim(1980,2022)+
  annotate("text", x=1981, y=425, label="NTL", size=6)+
  geom_segment(aes(x=2019, xend=2022, y=120, yend=120), 
               colour="grey51", linetype="longdash", size=1)+ 
  geom_segment(aes(x=2019, xend=2022, y=0, yend=0), 
               colour="grey51", linetype="longdash", size=1)

ntl.timeseries


#now sbc

sbc.timeseries<-ggplot(summary.sbc.bysite, aes(YEAR, mean.count))+
  geom_smooth(method="loess", se=T, color="darkblue", fill="azure3", linetype="dotted")+
  geom_line(color="darkcyan", size=1)+
  #geom_errorbar(aes(ymin=(mean.mass-se.mass), ymax=(mean.mass+se.mass)), width=0.2)+
  geom_point(pch=21, color="black", fill="darkcyan", size=2)+
  theme_classic(base_size = 12)+
  xlab(NULL)+
  ylab(expression(paste("Lobsters per trap")))+
  xlim(1980,2022)+
  theme(axis.line.x = element_blank(), axis.ticks.x=element_blank(),
        axis.text.x=element_blank())+
  annotate("text", x=1981, y=1.45, label="SBC", size=6)+
  geom_segment(aes(x=2019, xend=2022, y=1.3, yend=1.6), 
               colour="grey51", linetype="longdash", size=1)+ 
  geom_segment(aes(x=2019, xend=2022, y=0.9, yend=1.2), 
               colour="grey51", linetype="longdash", size=1)


sbc.timeseries

#now mcr

mcr.timeseries<-ggplot(summary.mcr.bysite, aes(Year, mean.mass))+
    geom_smooth(method="loess", se=T, color="navy", fill="azure3", linetype="dotted")+
  geom_line(color="deepskyblue4", size=1)+
  geom_point(pch=21, color="black", fill="deepskyblue4", size=2)+
  theme_classic(base_size = 12)+
  xlab(NULL)+
  ylab(expression(paste("Fishable biomass (g)")))+
  xlim(1980,2022)+
  theme(axis.line.x = element_blank(), axis.ticks.x=element_blank(),
        axis.text.x=element_blank())+
  annotate("text", x=1981, y=18, label="MCR", size=6)+
  geom_segment(aes(x=2018, xend=2022, y=13, yend=8), 
               colour="grey51", linetype="longdash", size=1)+ 
  geom_segment(aes(x=2018, xend=2022, y=8, yend=3), 
               colour="grey51", linetype="longdash", size=1)

mcr.timeseries

grid.draw(rbind(ggplotGrob(sbc.timeseries), ggplotGrob(mcr.timeseries),
                ggplotGrob(ntl.timeseries),
                size="first"))


pdf("fish_timeseries.pdf", height=5, width=8)
grid.draw(rbind(ggplotGrob(sbc.timeseries), ggplotGrob(mcr.timeseries),
                ggplotGrob(ntl.timeseries),
                size="first"))
dev.off()