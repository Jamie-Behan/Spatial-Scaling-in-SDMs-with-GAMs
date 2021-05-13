library(magrittr)
library(dplyr)
library(mgcv) # Run gam function to build general additive model (GAM)
library(RColorBrewer) # Run brewer.pal function to plot simulated abundance
library(classInt) # Run classIntervals function to plot simulated abundance
library(fields) # Run image.plot function to plot omega matrix
library(readxl)
library(spdep) # MoranI 
library(grid)
library(mapdata) #for mapping
library(maps) #for mapping
setwd("D:/MENHtrawl/Cir data")

GAM_data<- read_excel("DATAREDO2.xlsx") # Read Maine-New Hampshier Bottom Trawl Survey data. 
EastData<- read_excel("D:/MENHtrawl/Cir data/EastData2.xlsx") #trawl survey data I subdivided, which are located in eastern GOM
WestData<- read_excel("D:/MENHtrawl/Cir data/WestData2.xlsx") #trawl survey data I subdivided, which are located in western GOM
mainecoast= readOGR("D:/MENHTrawl/data/gis/ne_10m_coastline/ne_10m_coastline.shp") #shapefile for plotting maps with islands in Penobscot Bay
GAM_data<- na.omit(GAM_data)#Remove NAs
EastData<- na.omit(EastData)#Remove NAs
WestData<- na.omit(WestData)#Remove NAs
####add year column####
GAM_data$Start_Date = as.Date(GAM_data$Start_Date, "%m/%d/%Y") #didn't end up using year as a variable
GAM_data$Year = as.numeric(format(GAM_data$Start_Date, "%Y"))

EastData$Start_Date = as.Date(EastData$Start_Date, "%m/%d/%Y")
EastData$Year = as.numeric(format(EastData$Start_Date, "%Y"))

WestData$Start_Date = as.Date(WestData$Start_Date, "%m/%d/%Y")
WestData$Year = as.numeric(format(WestData$Start_Date, "%Y"))
##### GAM FOR EAST ######

FalldataE<-EastData[!grepl("SP", EastData$Survey),]
FLFJE<-FalldataE[-c(1:2,4:13)]
FLMJE<-FalldataE[-c(1:3,5:13)]
FLFAE<-FalldataE[-c(1:5,7:13)]
FLMAE<-FalldataE[-c(1:4,6:13)]

SpringdataE<-EastData[!grepl("FL",EastData$Survey),]
SPFJE<-SpringdataE[-c(1:2,4:13)]
SPMJE<-SpringdataE[-c(1:3,5:13)]
SPFAE<-SpringdataE[-c(1:5,7:13)]
SPMAE<-SpringdataE[-c(1:4,6:13)]

FLFJE<-FLFJE%>% rename(catch=c(1))
FLMJE<-FLMJE%>% rename(catch=c(1))
FLFAE<-FLFAE%>% rename(catch=c(1))
FLMAE<-FLMAE%>% rename(catch=c(1))
SPFJE<-SPFJE%>% rename(catch=c(1))
SPMJE<-SPMJE%>% rename(catch=c(1))
SPFAE<-SPFAE%>% rename(catch=c(1))
SPMAE<-SPMAE%>% rename(catch=c(1))

FLFJE<-FLFJE[!is.na(FLFJE$Bottom_WaterTempDegC) & !is.na(FLFJE$Bottom_Salinity_psu),]#Remove NAs
FLMJE<-FLMJE[!is.na(FLMJE$Bottom_WaterTempDegC) & !is.na(FLMJE$Bottom_Salinity_psu),]
FLFAE<-FLFAE[!is.na(FLFAE$Bottom_WaterTempDegC) & !is.na(FLFAE$Bottom_Salinity_psu),]
FLMAE<-FLMAE[!is.na(FLMAE$Bottom_WaterTempDegC) & !is.na(FLMAE$Bottom_Salinity_psu),]
SPFJE<-SPFJE[!is.na(SPFJE$Bottom_WaterTempDegC) & !is.na(SPFJE$Bottom_Salinity_psu),]
SPMJE<-SPMJE[!is.na(SPMJE$Bottom_WaterTempDegC) & !is.na(SPMJE$Bottom_Salinity_psu),]
SPFAE<-SPFAE[!is.na(SPFAE$Bottom_WaterTempDegC) & !is.na(SPFAE$Bottom_Salinity_psu),]
SPMAE<-SPMAE[!is.na(SPMAE$Bottom_WaterTempDegC) & !is.na(SPMAE$Bottom_Salinity_psu),]

###Drop outliars#### 
#not dropping any because dropping made model predictions worse
#### 1. Simulate lobster abundance by using GAM: FOR EASTERN DATA ####
####FLFJE##########
hist(FLFJE$catch,breaks=30)

# Abundance GAM#
FLFJEabundance.full<-gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(),data=FLFJE) # Build second stage GAM with all possible habitat variables
summary(FLFJEabundance.full) # Find significant variables based on p-value
FLFJEabundance.full$aic
FLFJEabundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(), data=FLFJE) # Build first stage GAM with significant habitat variables, keeping salinity because including it has lower aic (Zurr et al.)
summary(FLFJEabundance.sig)
FLFJEabundance.sig$aic
# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/NS1 Fall/curveplots01.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(FLFJEabundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=1.8,cex.axis=1.5)
plot(FLFJEabundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Temperature)),cex.lab=1.8,cex.axis=1.5)
plot.new()
plot(FLFJEabundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Distance~Offshore)),cex.lab=1.8,cex.axis=1.5)
plot(FLFJEabundance.sig, select =4, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Sediment)),cex.lab=1.8,cex.axis=1.5)
dev.off()
## 1.2 Fit GAM #####
FLFJEp.abundance=as.matrix(FLFJEabundance.sig$fitted.values) # Abundance values of lobsters at current trawl survey stations

####FLMJE##########
hist(FLMJE$catch,breaks=30)
# Abundance GAM#
FLMJEabundance.full<-gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(),data=FLMJE) # Build second stage GAM with all possible habitat variables
summary(FLMJEabundance.full) # Find significant variables based on p-value
FLMJEabundance.full$aic
FLMJEabundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(), data=FLMJE) # Build first stage GAM with significant habitat variables
summary(FLMJEabundance.sig)
FLMJEabundance.sig$aic
# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/NS1 Fall/curveplots02.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(FLMJEabundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=1.8,cex.axis=1.5)
plot(FLMJEabundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Temperature)),cex.lab=1.8,cex.axis=1.5)
plot.new()
plot(FLMJEabundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Distance~Offshore)),cex.lab=1.8,cex.axis=1.5)
plot(FLMJEabundance.sig, select =4, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Sediment)),cex.lab=1.8,cex.axis=1.5)
dev.off()
## 1.2 Fit GAM #####
FLMJEp.abundance=as.matrix(FLMJEabundance.sig$fitted.values) # Abundance values of lobsters at current trawl survey stations

#########

####FLFAE##########
hist(FLFAE$catch,breaks=30)
# Abundance GAM#
FLFAEabundance.full<-gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(),data=(FLFAE)) # Build second stage GAM with all possible habitat variables
summary(FLFAEabundance.full) # Find significant variables based on p-value
FLFAEabundance.full$aic
FLFAEabundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(), data=FLFAE) # Build first stage GAM with significant habitat variables
summary(FLFAEabundance.sig)
FLFAEabundance.sig$aic
# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/NS1 Fall/curveplots03.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(FLFAEabundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=1.8,cex.axis=1.5)
plot(FLFAEabundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Temperature)),cex.lab=1.8,cex.axis=1.5)
plot.new()
plot(FLFAEabundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Distance~Offshore)),cex.lab=1.8,cex.axis=1.5)
plot(FLFAEabundance.sig, select =4, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Sediment)),cex.lab=1.8,cex.axis=1.5)
dev.off()
## 1.2 Fit GAM #####
FLFAEp.abundance=as.matrix(FLFAEabundance.sig$fitted.values) # Abundance values of lobsters at current trawl survey stations

#####FLMAE#########
hist(FLMAE$catch,breaks=30)
# Abundance GAM#
FLMAEabundance.full<-gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(),data=FLMAE) # Build second stage GAM with all possible habitat variables
summary(FLMAEabundance.full) # Find significant variables based on p-value
FLMAEabundance.full$aic
FLMAEabundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(), data=FLMAE) # Build first stage GAM with significant habitat variables
summary(FLMAEabundance.sig)
FLMAEabundance.sig$aic
# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/NS1 Fall/curveplots04.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(FLMAEabundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=1.8,cex.axis=1.5)
plot(FLMAEabundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Temperature)),cex.lab=1.8,cex.axis=1.5)
plot(FLMAEabundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Bottom~Salinity)),cex.lab=1.8,cex.axis=1.5)
plot(FLMAEabundance.sig, select =4, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Distance~Offshore)),cex.lab=1.8,cex.axis=1.5)
plot(FLMAEabundance.sig, select =5, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Sediment)),cex.lab=1.8,cex.axis=1.5)
dev.off()
## 1.2 Fit GAM #####
FLMAEp.abundance=as.matrix(FLMAEabundance.sig$fitted.values) # Abundance values of lobsters at current trawl survey stations

############

#####SPFJE########
hist(SPFJE$catch,breaks=30)

# Abundance GAM#
SPFJEabundance.full<-gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(),data=SPFJE) # Build second stage GAM with all possible habitat variables
summary(SPFJEabundance.full) # Find significant variables based on p-value

SPFJEabundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(), data=SPFJE) # Build first stage GAM with significant habitat variables
summary(SPFJEabundance.sig)

# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/NS1 Spring/curveplots05.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(SPFJEabundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=1.8,cex.axis=1.5)
plot(SPFJEabundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Temperature)),cex.lab=1.8,cex.axis=1.5)
plot(SPFJEabundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Bottom~Salinity)),cex.lab=1.8,cex.axis=1.5)
plot(SPFJEabundance.sig, select =4, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Distance~Offshore)),cex.lab=1.8,cex.axis=1.5)
plot(SPFJEabundance.sig, select =5, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Sediment)),cex.lab=1.8,cex.axis=1.5)
dev.off()
## 1.2 Fit GAM #####
SPFJEp.abundance=as.matrix(SPFJEabundance.sig$fitted.values) # Abundance values of lobsters at current trawl survey stations

#####SPMJE######
hist(SPMJE$catch,breaks=30)
# Abundance GAM#
SPMJEabundance.full<-gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(),data=SPMJE) # Build second stage GAM with all possible habitat variables
summary(SPMJEabundance.full) # Find significant variables based on p-value

SPMJEabundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(), data=SPMJE) # Build first stage GAM with significant habitat variables
summary(SPMJEabundance.sig)

# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/NS1 Spring/curveplots06.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(SPMJEabundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=1.8,cex.axis=1.5)
plot(SPMJEabundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Temperature)),cex.lab=1.8,cex.axis=1.5)
plot(SPMJEabundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Bottom~Salinity)),cex.lab=1.8,cex.axis=1.5)
plot(SPMJEabundance.sig, select =4, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Distance~Offshore)),cex.lab=1.8,cex.axis=1.5)
plot(SPMJEabundance.sig, select =5, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Sediment)),cex.lab=1.8,cex.axis=1.5)
dev.off()
## 1.2 Fit GAM #####
SPMJEp.abundance=as.matrix(SPMJEabundance.sig$fitted.values) # Abundance values of lobsters at current trawl survey stations

########

#####SPFAE######
hist(SPFAE$catch,breaks=30)

# Abundance GAM#
SPFAEabundance.full<-gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(),data=SPFAE) # Build second stage GAM with all possible habitat variables
summary(SPFAEabundance.full) # Find significant variables based on p-value

SPFAEabundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(), data=SPFAE) # Build first stage GAM with significant habitat variables
summary(SPFAEabundance.sig)

# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/NS1 Spring/curveplots07.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(SPFAEabundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=1.8,cex.axis=1.5)
plot(SPFAEabundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Temperature)),cex.lab=1.8,cex.axis=1.5)
plot(SPFAEabundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Bottom~Salinity)),cex.lab=1.8,cex.axis=1.5)
plot(SPFAEabundance.sig, select =4, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Distance~Offshore)),cex.lab=1.8,cex.axis=1.5)
plot(SPFAEabundance.sig, select =5, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Sediment)),cex.lab=1.8,cex.axis=1.5)
dev.off()
## 1.2 Fit GAM #####
SPFAEp.abundance=as.matrix(SPFAEabundance.sig$fitted.values) # Abundance values of lobsters at current trawl survey stations

########

#####SPMAE######
hist(SPMAE$catch,breaks=30)
# Abundance GAM#
SPMAEabundance.full<-gam((catch) ~ s(Latitude, k=5)+s(Longitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(),data=SPMAE) # Build second stage GAM with all possible habitat variables
summary(SPMAEabundance.full) # Find significant variables based on p-value

SPMAEabundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(), data=SPMAE) # Build first stage GAM with significant habitat variables
summary(SPMAEabundance.sig)

# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/NS1 Spring/curveplots08.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(SPMAEabundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=1.8,cex.axis=1.5)
plot(SPMAEabundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Temperature)),cex.lab=1.8,cex.axis=1.5)
plot(SPMAEabundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Bottom~Salinity)),cex.lab=1.8,cex.axis=1.5)
plot(SPMAEabundance.sig, select =4, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Distance~Offshore)),cex.lab=1.8,cex.axis=1.5)
plot(SPMAEabundance.sig, select =5, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Sediment)),cex.lab=1.8,cex.axis=1.5)
dev.off()
## 1.2 Fit GAM #####
SPMAEp.abundance=as.matrix(SPMAEabundance.sig$fitted.values) # Abundance values of lobsters at current trawl survey stations

########

######################################

#### GAM FOR WEST ########
FalldataW<-WestData[!grepl("SP", WestData$Survey),]
FLFJW<-FalldataW[-c(1:2,4:13)]
FLMJW<-FalldataW[-c(1:3,5:13)]
FLFAW<-FalldataW[-c(1:5,7:13)]
FLMAW<-FalldataW[-c(1:4,6:13)]

SpringdataW<-WestData[!grepl("FL",WestData$Survey),]
SPFJW<-SpringdataW[-c(1:2,4:13)]
SPMJW<-SpringdataW[-c(1:3,5:13)]
SPFAW<-SpringdataW[-c(1:5,7:13)]
SPMAW<-SpringdataW[-c(1:4,6:13)]

FLFJW<-FLFJW%>% rename(catch=c(1))
FLMJW<-FLMJW%>% rename(catch=c(1))
FLFAW<-FLFAW%>% rename(catch=c(1))
FLMAW<-FLMAW%>% rename(catch=c(1))
SPFJW<-SPFJW%>% rename(catch=c(1))
SPMJW<-SPMJW%>% rename(catch=c(1))
SPFAW<-SPFAW%>% rename(catch=c(1))
SPMAW<-SPMAW%>% rename(catch=c(1))

FLFJW<-FLFJW[!is.na(FLFJW$Bottom_WaterTempDegC) & !is.na(FLFJW$Bottom_Salinity_psu),]#Remove NAs
FLMJW<-FLMJW[!is.na(FLMJW$Bottom_WaterTempDegC) & !is.na(FLMJW$Bottom_Salinity_psu),]
FLFAW<-FLFAW[!is.na(FLFAW$Bottom_WaterTempDegC) & !is.na(FLFAW$Bottom_Salinity_psu),]
FLMAW<-FLMAW[!is.na(FLMAW$Bottom_WaterTempDegC) & !is.na(FLMAW$Bottom_Salinity_psu),]
SPFJW<-SPFJW[!is.na(SPFJW$Bottom_WaterTempDegC) & !is.na(SPFJW$Bottom_Salinity_psu),]
SPMJW<-SPMJW[!is.na(SPMJW$Bottom_WaterTempDegC) & !is.na(SPMJW$Bottom_Salinity_psu),]
SPFAW<-SPFAW[!is.na(SPFAW$Bottom_WaterTempDegC) & !is.na(SPFAW$Bottom_Salinity_psu),]
SPMAW<-SPMAW[!is.na(SPMAW$Bottom_WaterTempDegC) & !is.na(SPMAW$Bottom_Salinity_psu),]

###Drop outliars#### 
#only dropping from SPFJW because it was the only model to improve abundance predictions after dropping
SPFJW <- SPFJW[!(SPFJW$catch==181), ]
SPFJW <- SPFJW[!(SPFJW$catch==159), ]
#### 1. Simulate lobster abundance by using GAM ####
 
##########
####FLFJW##########
hist(FLFJW$catch,breaks=30)
# Abundance GAM#
FLFJWabundance.full<-gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(),data=FLFJW) # Build second stage GAM with all possible habitat variables
summary(FLFJWabundance.full) # Find significant variables based on p-value
FLFJWabundance.full$aic
FLFJWabundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5), family=tw(), data=FLFJW) # Build first stage GAM with significant habitat variables. Including sediment and not salinity resulted in lowest aic value
summary(FLFJWabundance.sig)
FLFJWabundance.sig$aic
# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/NS1 Fall/curveplots09.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(FLFJWabundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=1.8,cex.axis=1.5)
plot(FLFJWabundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Temperature)),cex.lab=1.8,cex.axis=1.5)
plot(FLFJWabundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Bottom~Salinity)),cex.lab=1.8,cex.axis=1.5)
plot(FLFJWabundance.sig, select =4, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Distance~Offshore)),cex.lab=1.8,cex.axis=1.5)
plot.new()
dev.off()
## 1.2 Fit GAM #####
FLFJWp.abundance=as.matrix(FLFJWabundance.sig$fitted.values) # Abundance values of lobsters at current trawl survey stations

########
####FLMJW##########
hist(FLMJW$catch,breaks=30)

# Abundance GAM#
FLMJWabundance.full<-gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(),data=FLMJW) # Build second stage GAM with all possible habitat variables
summary(FLMJWabundance.full) # Find significant variables based on p-value
FLMJWabundance.full$aic
FLMJWabundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5) , family=tw(), data=FLMJW) # Build first stage GAM with significant habitat variables
summary(FLMJWabundance.sig)
FLMJWabundance.sig$aic
# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/NS1 Fall/curveplots10.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(FLMJWabundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=1.8,cex.axis=1.5)
plot(FLMJWabundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Temperature)),cex.lab=1.8,cex.axis=1.5)
plot(FLMJWabundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Bottom~Salinity)),cex.lab=1.8,cex.axis=1.5)
plot(FLMJWabundance.sig, select =4, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Distance~Offshore)),cex.lab=1.8,cex.axis=1.5)
plot.new()
dev.off()
## 1.2 Fit GAM #####
FLMJWp.abundance=as.matrix(FLMJWabundance.sig$fitted.values) # Abundance values of lobsters at current trawl survey stations


############
####FLFAW##########
hist(FLFAW$catch,breaks=30)
# Abundance GAM#
FLFAWabundance.full<-gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(),data=FLFAW) # Build second stage GAM with all possible habitat variables
summary(FLFAWabundance.full) # Find significant variables based on p-value
FLFAWabundance.full$aic
FLFAWabundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5), family=tw(), data=FLFAW) # Build first stage GAM with significant habitat variables
summary(FLFAWabundance.sig)
FLFAWabundance.sig$aic
# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/NS1 Fall/curveplots11.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(FLFAWabundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=1.8,cex.axis=1.5)
plot(FLFAWabundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Temperature)),cex.lab=1.8,cex.axis=1.5)
plot(FLFAWabundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Bottom~Salinity)),cex.lab=1.8,cex.axis=1.5)
plot(FLFAWabundance.sig, select =4, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Distance~Offshore)),cex.lab=1.8,cex.axis=1.5)
plot.new()
dev.off()
## 1.2 Fit GAM #####
FLFAWp.abundance=as.matrix(FLFAWabundance.sig$fitted.values) # Abundance values of lobsters at current trawl survey stations

#####FLMAW#########
hist(FLMAW$catch,breaks=30)
# Abundance GAM#
FLMAWabundance.full<-gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(),data=FLMAW) # Build second stage GAM with all possible habitat variables
summary(FLMAWabundance.full) # Find significant variables based on p-value
FLMAWabundance.full$aic
FLMAWabundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5), family=tw(), data=FLMAW) # Build first stage GAM with significant habitat variables
summary(FLMAWabundance.sig)
FLMAWabundance.sig$aic
# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/NS1 Fall/curveplots12.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(FLMAWabundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=1.8,cex.axis=1.5)
plot(FLMAWabundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Distance~Offshore)),cex.lab=1.8,cex.axis=1.5)
plot(FLMAWabundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Distance~Offshore)),cex.lab=1.8,cex.axis=1.5)
plot(FLMAWabundance.sig, select =4, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Distance~Offshore)),cex.lab=1.8,cex.axis=1.5)
plot.new()
dev.off()
## 1.2 Fit GAM #####
FLMAWp.abundance=as.matrix(FLMAWabundance.sig$fitted.values) # Abundance values of lobsters at current trawl survey stations

############

#####SPFJW########
hist(SPFJW$catch,breaks=30)
# Abundance GAM#
SPFJWabundance.full<-gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(),data=SPFJW) # Build second stage GAM with all possible habitat variables
summary(SPFJWabundance.full) # Find significant variables based on p-value
SPFJWabundance.full$aic
SPFJWabundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(), data=SPFJW) # Build first stage GAM with significant habitat variables
summary(SPFJWabundance.sig)
SPFJWabundance.sig$aic
# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/NS1 Spring/curveplots13.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(SPFJWabundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=1.8,cex.axis=1.5)
plot(SPFJWabundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Temperature)),cex.lab=1.8,cex.axis=1.5)
plot(SPFJWabundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Bottom~Salinity)),cex.lab=1.8,cex.axis=1.5)
plot(SPFJWabundance.sig, select =4, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Distance~Offshore)),cex.lab=1.8,cex.axis=1.5)
plot(SPFJWabundance.sig, select =5, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Sediment)),cex.lab=1.8,cex.axis=1.5)
dev.off()
## 1.2 Fit GAM #####
SPFJWp.abundance=as.matrix(SPFJWabundance.sig$fitted.values) # Abundance values of lobsters at current trawl survey stations

#####SPMJW######
hist(SPMJW$catch,breaks=30)
# Abundance GAM#
SPMJWabundance.full<-gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(),data=SPMJW) # Build second stage GAM with all possible habitat variables
summary(SPMJWabundance.full) # Find significant variables based on p-value
SPMJWabundance.full$aic
SPMJWabundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5), family=tw(), data=SPMJW) # Build first stage GAM with significant habitat variables
summary(SPMJWabundance.sig)
SPMJWabundance.sig$aic
# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/NS1 Spring/curveplots14.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(SPMJWabundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=1.8,cex.axis=1.5)
plot(SPMJWabundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Temperature)),cex.lab=1.8,cex.axis=1.5)
plot(SPMJWabundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Bottom~Salinity)),cex.lab=1.8,cex.axis=1.5)
plot(SPMJWabundance.sig, select =4, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Distance~Offshore)),cex.lab=1.8,cex.axis=1.5)
plot.new()
dev.off()
## 1.2 Fit GAM #####
SPMJWp.abundance=as.matrix(SPMJWabundance.sig$fitted.values) # Abundance values of lobsters at current trawl survey stations

########

#####SPFAW######
hist(SPFAW$catch,breaks=30)

# Abundance GAM#
SPFAWabundance.full<-gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(),data=SPFAW) # Build second stage GAM with all possible habitat variables
summary(SPFAWabundance.full) # Find significant variables based on p-value
SPFAWabundance.full$aic
SPFAWabundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(), data=SPFAW) # Build first stage GAM with significant habitat variables
summary(SPFAWabundance.sig)
SPFAWabundance.sig$aic
# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/NS1 Spring/curveplots15.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(SPFAWabundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=1.8,cex.axis=1.5)
plot(SPFAWabundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Temperature)),cex.lab=1.8,cex.axis=1.5)
plot(SPFAWabundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Bottom~Salinity)),cex.lab=1.8,cex.axis=1.5)
plot(SPFAWabundance.sig, select =4, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Distance~Offshore)),cex.lab=1.8,cex.axis=1.5)
plot(SPFAWabundance.sig, select =5, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Sediment)),cex.lab=1.8,cex.axis=1.5)
dev.off()
## 1.2 Fit GAM #####
SPFAWp.abundance=as.matrix(SPFAWabundance.sig$fitted.values) # Abundance values of lobsters at current trawl survey stations

########

#####SPMAW######
hist(SPMAW$catch,breaks=30)

# Abundance GAM#
SPMAWabundance.full<-gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(),data=SPMAW) # Build second stage GAM with all possible habitat variables
summary(SPMAWabundance.full) # Find significant variables based on p-value
SPMAWabundance.full$aic
SPMAWabundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(), data=SPMAW) # Build first stage GAM with significant habitat variables
summary(SPMAWabundance.sig)
SPMAWabundance.sig$aic
# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/NS1 Spring/curveplots16.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(SPMAWabundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=1.8,cex.axis=1.5)
plot(SPMAWabundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Temperature)),cex.lab=1.8,cex.axis=1.5)
plot(SPMAWabundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Bottom~Salinity)),cex.lab=1.8,cex.axis=1.5)
plot(SPMAWabundance.sig, select =4, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Distance~Offshore)),cex.lab=1.8,cex.axis=1.5)
plot(SPMAWabundance.sig, select =5, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Sediment)),cex.lab=1.8,cex.axis=1.5)
dev.off()
## 1.2 Fit GAM #####
SPMAWp.abundance=as.matrix(SPMAWabundance.sig$fitted.values) # Abundance values of lobsters at current trawl survey stations

#################
###Check Residuals####
checkresid<-function(groupabundance.sig,group){
  plot(groupabundance.sig$fitted.values, groupabundance.sig$residuals, type = "p")
  plot(group$Latitude, groupabundance.sig$residuals, type = "p")
  plot(group$Longitude, groupabundance.sig$residuals, type = "p")
  plot(group$Bottom_WaterTempDegC, groupabundance.sig$residuals, type = "p")
  plot(group$Bottom_Salinity_psu, groupabundance.sig$residuals, type = "p")
  plot(group$AvgDepth, groupabundance.sig$residuals, type = "p")
  plot(group$sediment, groupabundance.sig$residuals, type = "p")
  gam.check(groupabundance.sig,type="response")}

checkresid(FLFJEabundance.sig,FLFJE)
checkresid(FLMJEabundance.sig,FLMJE)
checkresid(FLFAEabundance.sig,FLFAE)
checkresid(FLMAEabundance.sig,FLMAE)
checkresid(SPFJEabundance.sig,SPFJE)
checkresid(SPMJEabundance.sig,SPMJE)
checkresid(SPFAEabundance.sig,SPFAE)
checkresid(SPMAEabundance.sig,SPMAE)

checkresid(FLFJWabundance.sig,FLFJW)
checkresid(FLMJWabundance.sig,FLMJW)
checkresid(FLFAWabundance.sig,FLFAW)
checkresid(FLMAWabundance.sig,FLMAW)
checkresid(SPFJWabundance.sig,SPFJW)
checkresid(SPMJWabundance.sig,SPMJW)
checkresid(SPFAWabundance.sig,SPFAW)
checkresid(SPMAWabundance.sig,SPMAW)

lmtest::bptest(FLFJEabundance.sig)
lmtest::bptest(FLMJEabundance.sig)
lmtest::bptest(FLFAEabundance.sig)
lmtest::bptest(FLMAEabundance.sig)
lmtest::bptest(SPFJEabundance.sig)
lmtest::bptest(SPMJEabundance.sig)
lmtest::bptest(SPFAEabundance.sig)
lmtest::bptest(SPMAEabundance.sig)

lmtest::bptest(FLFJWabundance.sig)
lmtest::bptest(FLMJWabundance.sig)
lmtest::bptest(FLFAWabundance.sig)
lmtest::bptest(FLMAWabundance.sig)
lmtest::bptest(SPFJWabundance.sig)
lmtest::bptest(SPMJWabundance.sig)
lmtest::bptest(SPFAWabundance.sig)
lmtest::bptest(SPMAWabundance.sig)



dotchart(SPFA$catch)
#####MAPPING NONSTATIONARY########
FLFJE.d<-as.data.frame(cbind(FLFJE,FLFJEp.abundance)) #adding abundance estimates to corresponding data frame
names(FLFJE.d)[10]<- "p.abundance"
FLMJE.d<-as.data.frame(cbind(FLMJE,FLMJEp.abundance))
names(FLMJE.d)[10]<- "p.abundance"
FLFAE.d<-as.data.frame(cbind(FLFAE,FLFAEp.abundance))
names(FLFAE.d)[10]<- "p.abundance"
FLMAE.d<-as.data.frame(cbind(FLMAE,FLMAEp.abundance))
names(FLMAE.d)[10]<- "p.abundance"
SPFJE.d<-as.data.frame(cbind(SPFJE,SPFJEp.abundance))
names(SPFJE.d)[10]<- "p.abundance"
SPMJE.d<-as.data.frame(cbind(SPMJE,SPMJEp.abundance))
names(SPMJE.d)[10]<- "p.abundance"
SPFAE.d<-as.data.frame(cbind(SPFAE,SPFAEp.abundance))
names(SPFAE.d)[10]<- "p.abundance"
SPMAE.d<-as.data.frame(cbind(SPMAE,SPMAEp.abundance))
names(SPMAE.d)[10]<- "p.abundance"

FLFJW.d<-as.data.frame(cbind(FLFJW,FLFJWp.abundance))
names(FLFJW.d)[10]<- "p.abundance"
FLMJW.d<-as.data.frame(cbind(FLMJW,FLMJWp.abundance))
names(FLMJW.d)[10]<- "p.abundance"
FLFAW.d<-as.data.frame(cbind(FLFAW,FLFAWp.abundance))
names(FLFAW.d)[10]<- "p.abundance"
FLMAW.d<-as.data.frame(cbind(FLMAW,FLMAWp.abundance))
names(FLMAW.d)[10]<- "p.abundance"
SPFJW.d<-as.data.frame(cbind(SPFJW,SPFJWp.abundance))
names(SPFJW.d)[10]<- "p.abundance"
SPMJW.d<-as.data.frame(cbind(SPMJW,SPMJWp.abundance))
names(SPMJW.d)[10]<- "p.abundance"
SPFAW.d<-as.data.frame(cbind(SPFAW,SPFAWp.abundance))
names(SPFAW.d)[10]<- "p.abundance"
SPMAW.d<-as.data.frame(cbind(SPMAW,SPMAWp.abundance))
names(SPMAW.d)[10]<- "p.abundance"

FLFJew=rbind(FLFJE.d,FLFJW.d) #row binding east and west data frames into one dataframe
FLMJew=rbind(FLMJE.d,FLMJW.d)
FLFAew=rbind(FLFAE.d,FLFAW.d)
FLMAew=rbind(FLMAE.d,FLMAW.d)
SPFJew=rbind(SPFJE.d,SPFJW.d)
SPMJew=rbind(SPMJE.d,SPMJW.d)
SPFAew=rbind(SPFAE.d,SPFAW.d)
SPMAew=rbind(SPMAE.d,SPMAW.d)


##### 1.4 Plot simulated abundance at trawl locations#########
nonstationarysim<- function(simabundance,size){
        start_x <- range(simabundance$Longitude)[1]
        end_x <- range(simabundance$Longitude)[2]
        start_y <- range(simabundance$Latitude)[1]
        end_y <- range(simabundance$Latitude)[2]
        colors <- brewer.pal(9, "YlOrRd")
colbrks<-classIntervals(simabundance$p.abundance, n=8, style="fixed", 
                        fixedBreaks=
                                if(size=="J")c(0,1,3,7,15,28,43,200,425) #breaks calculated by using average "quantile" breaks
                                else c(0,13,26,42,65,93,118,155,502)
                        )
brks<- colbrks$brks
par(mfrow=c(1,1)); par(mar=c(4,4,1,1))
plot(simabundance$Longitude, simabundance$Latitude, col=colors[findInterval(simabundance$p.abundance, brks ,all.inside=TRUE)], cex=1.5,pch = 20, xlab="Longitude", ylab="Latitude")
map(database = mainecoast, ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "lightgreen", fill = TRUE, add = TRUE)
legend("topleft", "Non-Stationary GOM Abundance", bty="n")


colcode <- findColours(colbrks,colors)

legend("bottomright", # position
       legend = names(attr(colcode, "table")), 
       title = "Abundance",
       fill = attr(colcode, "palette"),
       cex = 0.9,
       bty = "n") # border
}

nonstationarysim(FLFJew,"J")
nonstationarysim(FLMJew,"J")
nonstationarysim(FLFAew,"A")
nonstationarysim(FLMAew,"A")
nonstationarysim(SPFJew,"J")
nonstationarysim(SPMJew,"J")
nonstationarysim(SPFAew,"A")
nonstationarysim(SPMAew,"A")

######################
##### 1.5 Plot abundance residuals at trawl locations#########
nonstationaryresids<- function(simabundance, size){
        start_x <- range(simabundance$Longitude)[1]
        end_x <- range(simabundance$Longitude)[2]
        start_y <- range(simabundance$Latitude)[1]
        end_y <- range(simabundance$Latitude)[2]
        colors <- rev(brewer.pal(8,"RdBu"))
        colbrks<-classIntervals((simabundance$p.abundance-simabundance$catch), n=8, style="fixed", 
                                fixedBreaks=
                                        if(size=="J")c(-420,-16,-1,0,1,3,8,19,296) #breaks calculated by using average "qauntile" breaks for each juv and adult
                                        else c(-685,-60,-20,0,6,16,29,56,435)
                                )
        brks<- colbrks$brks
        par(mfrow=c(1,1)); par(mar=c(4,4,1,1))
        plot(simabundance$Longitude, simabundance$Latitude, col=colors[findInterval(simabundance$p.abundance-simabundance$catch, brks ,all.inside=TRUE)],cex=1.5, pch = 20, xlab="Longitude", ylab="Latitude")
        map(database = mainecoast, ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "lightgreen", fill = TRUE, add = TRUE)
        legend("topleft", "Non-Stationary Abundance Residuals", bty="n")
        
        
        colcode <- findColours(colbrks,colors)
        
        legend("bottomright", # position
               legend = names(attr(colcode, "table")), 
               title = "Abundance Residuals",
               fill = attr(colcode, "palette"),
               cex = 0.9,
               bty = "n") # border
}

nonstationaryresids(FLFJew,"J")
nonstationaryresids(FLMJew,"J")
nonstationaryresids(FLFAew,"A")
nonstationaryresids(FLMAew,"A")
nonstationaryresids(SPFJew,"J")
nonstationaryresids(SPMJew,"J")
nonstationaryresids(SPFAew,"A")
nonstationaryresids(SPMAew,"A")



######Calculate RMSE values####
FLFJresid=append(FLFJEabundance.sig$residuals,FLFJWabundance.sig$residuals) #appending residuals from east and west estimates into one
FLMJresid=append(FLMJEabundance.sig$residuals,FLMJWabundance.sig$residuals)
FLFAresid=append(FLFAEabundance.sig$residuals,FLFAWabundance.sig$residuals)
FLMAresid=append(FLMAEabundance.sig$residuals,FLMAWabundance.sig$residuals)
SPFJresid=append(SPFJEabundance.sig$residuals,SPFJWabundance.sig$residuals)
SPMJresid=append(SPMJEabundance.sig$residuals,SPMJWabundance.sig$residuals)
SPFAresid=append(SPFAEabundance.sig$residuals,SPFAWabundance.sig$residuals)
SPMAresid=append(SPMAEabundance.sig$residuals,SPMAWabundance.sig$residuals)

RMSEfunction<- function(data){
        sqrt(mean(data^2))
}

RMSEfunction(FLFJresid) #calculating RMSE for combined east/west
RMSEfunction(FLMJresid)
RMSEfunction(FLFAresid)
RMSEfunction(FLMAresid)
RMSEfunction(SPFJresid)
RMSEfunction(SPMJresid)
RMSEfunction(SPFAresid)
RMSEfunction(SPMAresid)
################################################

#### 1.6 Cross Validation ####

NSCrossvalidation<- function(groupE,groupW, gam_model_optionE,gam_model_optionW, PE,PW,Title){
        
        groupE = groupE [!is.na(groupE$Bottom_WaterTempDegC) & !is.na(groupE$Bottom_Salinity_psu),]#Remove NAs
        groupW = groupW [!is.na(groupW$Bottom_WaterTempDegC) & !is.na(groupW$Bottom_Salinity_psu),]#Remove NAs
        
        NE = nrow(groupE)
        PE= (1/(1+sqrt(PE-1)))
        resE = matrix(0, ncol = 3, nrow = 100)
        colnames(resE) = c("Intercept", "Slope", "R.Squared")
        for (i in 1:100){          
                print(i)
                sub = sample(1:NE, size = (NE*PE))
                train = groupE[-sub,]
                test = groupE[sub,]
                
                if(gam_model_optionE==1) abundance_cv<- gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)
                                                           +s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(), data=subset(train), select=TRUE)
                if(gam_model_optionE==2) abundance_cv<- gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)
                                                           +s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(), data=subset(train), select=TRUE)
                
                predensity=predict.gam(abundance_cv, test, se.fit=TRUE, type = "response")
                result = cbind(test$catch, predensity$fit) 
                final = result[result[,1]>0 & result[,2]>0,]
                q = lm(final[,1] ~ final[,2], data = as.data.frame(final))
                resE[i,] = c(summary(q)$coefficient[,1], summary(q)$r.squared)
        }
        
        
        NW = nrow(groupW)
        PW= (1/(1+sqrt(PW-1)))
        resW = matrix(0, ncol = 3, nrow = 100)
        colnames(resW) = c("Intercept", "Slope", "R.Squared")
        for (i in 1:100){          
                print(i)
                sub = sample(1:NW, size = (NW*PW))
                train = groupW[-sub,]
                test = groupW[sub,]
                
                if(gam_model_optionW==1) abundance_cv<- gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)
                                                           +s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(), data=subset(train), select=TRUE) #bottom salinity not significant
                if(gam_model_optionW==2) abundance_cv<- gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)
                                                           +s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(), data=subset(train), select=TRUE) #all significant
                
                if(gam_model_optionW==3) abundance_cv<- gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)
                                                            +s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5) , family=tw(), data=subset(train), select=TRUE)#no sediment
                if(gam_model_optionW==4) abundance_cv<- gam((catch) ~ s(Longitude,Latitude, k=5)+
                                                            +s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(), data=subset(train), select=TRUE)#no salinity/temp
                if(gam_model_optionW==5) abundance_cv<- gam((catch) ~ s(Longitude,Latitude, k=5)
                                                            +s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(), data=subset(train), select=TRUE)#no temp
                
                
                
                
                predensity=predict.gam(abundance_cv, test, se.fit=TRUE, type = "response")
                result = cbind(test$catch, predensity$fit) 
                final = result[result[,1]>0 & result[,2]>0,]
                q = lm(final[,1] ~ final[,2], data = as.data.frame(final))
                resW[i,] = c(summary(q)$coefficient[,1], summary(q)$r.squared)
        }
        
        
        res=((resE+resW)/2)
        summary(res)
        plot(NULL,xlim=c(0,max(groupE$catch)),ylim=c(0,max(groupE$catch)), xlab = expression(bold(" Number of Predicted Lobsters")), 
             ylab = expression(bold("Observed Lobsters")), xaxt="n", yaxt="n")
        axis(side=1, font=2)
        axis(side=2, font=2)
        addlines <- function(x) abline(x, col="gray70")
        apply(res[,-3],1,addlines)
        
        abline(a = mean(res[,1]), b = mean(res[,2]), col="black", lwd=3)
        abline(0,1,lty =2, lwd=3)
        legend("topleft", Title,bty="n")
}
par(mar=c(2,2,0,0), mfrow=c(2,4))
NSCrossvalidation(FLFJE,FLFJW,1,3,5,5,Title="FLFJ, RMSE=1.53")
NSCrossvalidation(FLMJE,FLMJW,2,3,6,5,Title="FLMJ, RMSE=1.54")
NSCrossvalidation(FLFAE,FLFAW,1,1,5,5,Title="FLFA, RMSE=1.17")
NSCrossvalidation(FLMAE,FLMAW,1,5,5,5,Title="FLMA, RMSE=1.12")
NSCrossvalidation(SPFJE,SPFJW,2,2,6,6,Title="SPFJ, RMSE=1.57")
NSCrossvalidation(SPMJE,SPMJW,2,5,6,5,Title="SPMJ, RMSE=1.58")
NSCrossvalidation(SPFAE,SPFAW,2,2,6,6,Title="SPFA, RMSE=1.37")
NSCrossvalidation(SPMAE,SPMAW,2,2,6,6,Title="SPMA, RMSE=1.32")


###AIC Values#####
(FLFAEabundance.sig$aic+FLFAWabundance.sig$aic)/2
(FLFJEabundance.sig$aic+FLFJWabundance.sig$aic)/2
(FLMAEabundance.sig$aic+FLMAWabundance.sig$aic)/2
(FLMJEabundance.sig$aic+FLMJWabundance.sig$aic)/2

(SPFAEabundance.sig$aic+SPFAWabundance.sig$aic)/2
(SPFJEabundance.sig$aic+SPFJWabundance.sig$aic)/2
(SPMAEabundance.sig$aic+SPMAWabundance.sig$aic)/2
(SPMJEabundance.sig$aic+SPMJWabundance.sig$aic)/2

FLFAEabundance.sig$aic
FLFAWabundance.sig$aic
FLFJEabundance.sig$aic
FLFJWabundance.sig$aic
FLMAEabundance.sig$aic
FLMAWabundance.sig$aic
FLMJEabundance.sig$aic
FLMJWabundance.sig$aic

SPFAEabundance.sig$aic
SPFAWabundance.sig$aic
SPFJEabundance.sig$aic
SPFJWabundance.sig$aic
SPMAEabundance.sig$aic
SPMAWabundance.sig$aic
SPMJEabundance.sig$aic
SPMJWabundance.sig$aic

###Moran's I####

SPFAew$Latitude=ifelse(duplicated(SPFAew$Latitude), SPFAew$Latitude+0.00000001,SPFAew$Latitude) #changing duplicate lat/long values tiny bit so Moran's I function can run
SPFAew$Longitude=ifelse(duplicated(SPFAew$Longitude), SPFAew$Longitude+0.00000001,SPFAew$Longitude)

SPFJew$Latitude=ifelse(duplicated(SPFJew$Latitude), SPFJew$Latitude+0.00000001,SPFJew$Latitude)
SPFJew$Longitude=ifelse(duplicated(SPFJew$Longitude), SPFJew$Longitude+0.00000001,SPFJew$Longitude)

SPMAew$Latitude=ifelse(duplicated(SPMAew$Latitude), SPMAew$Latitude+0.00000001,SPMAew$Latitude)
SPMAew$Longitude=ifelse(duplicated(SPMAew$Longitude), SPMAew$Longitude+0.00000001,SPMAew$Longitude)

SPMJew$Latitude=ifelse(duplicated(SPMJew$Latitude), SPMJew$Latitude+0.00000001,SPMJew$Latitude)
SPMJew$Longitude=ifelse(duplicated(SPMJew$Longitude), SPMJew$Longitude+0.00000001,SPMJew$Longitude)

FLFAew$Latitude=ifelse(duplicated(FLFAew$Latitude), FLFAew$Latitude+0.00000001,FLFAew$Latitude)
FLFAew$Longitude=ifelse(duplicated(FLFAew$Longitude), FLFAew$Longitude+0.00000001,FLFAew$Longitude)

FLMAew$Latitude=ifelse(duplicated(FLMAew$Latitude), FLMAew$Latitude+0.00000001,FLMAew$Latitude)
FLMAew$Longitude=ifelse(duplicated(FLMAew$Longitude), FLMAew$Longitude+0.00000001,FLMAew$Longitude)

FLFJew$Latitude=ifelse(duplicated(FLFJew$Latitude), FLFJew$Latitude+0.00000001,FLFJew$Latitude)
FLFJew$Longitude=ifelse(duplicated(FLFJew$Longitude), FLFJew$Longitude+0.00000001,FLFJew$Longitude)

FLMJew$Latitude=ifelse(duplicated(FLMJew$Latitude), FLMJew$Latitude+0.00000001,FLMJew$Latitude)
FLMJew$Longitude=ifelse(duplicated(FLMJew$Longitude), FLMJew$Longitude+0.00000001,FLMJew$Longitude)


moranI <- function(residuals, group){
  
  IDs<-row.names(as(group, "data.frame"))
  coords<- coordinates(select(group,Latitude,Longitude))
  temp_kn1 <- knn2nb(knearneigh(coords, k=8), row.names=IDs)
  temp_kn2_w <- nb2listw(temp_kn1)
  moran.test(group$catch, listw=temp_kn2_w)
  moran.plot(group$catch, temp_kn2_w)
  
  I <- moran.test(residuals, listw=temp_kn2_w)
  moran.plot(residuals, temp_kn2_w)
  return(list("Moran I estimate"=I$estimate[1], "Moran I pvalue"=I$p.value))
}
moranI(FLFJresid,FLFJew)
moranI(FLMJresid,FLMJew)
moranI(FLFAresid,FLFAew)
moranI(FLMAresid,FLMAew)
moranI(SPFJresid,SPFJew)
moranI(SPMJresid,SPMJew)
moranI(SPFAresid,SPFAew)
moranI(SPMAresid,SPMAew)








