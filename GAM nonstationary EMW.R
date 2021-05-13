library(magrittr)
library(dplyr)
library(mgcv) # Run gam function to build general additive model (GAM)
library(RColorBrewer) # Run brewer.pal function to plot simulated abundance
library(classInt) # Run classIntervals function to plot simulated abundance
library(fields) # Run image.plot function to plot omega matrix
library(readxl)
library(spdep) # MoranI
library(rgdal)
setwd("D:/MENHtrawl/Cir data")

###Seaparting East, Middle, West Data####

DATAREDO2<-read_excel("D:/MENHTrawl/Cir data/DATAREDO2.xlsx")

WData = subset(DATAREDO2, Region <= 2,
                   select=c(Survey:dist_frm_shore))
MData = subset(DATAREDO2, Region == 3,
                   select=c(Survey:dist_frm_shore))

EData = subset(DATAREDO2, Region >= 4,
                   select=c(Survey:dist_frm_shore))

GAM_data<- read_excel("DATAREDO2.xlsx") # Read Maine-New Hampshier Bottom TrAW2l Survey data. 

mainecoast= readOGR("D:/MENHTrawl/data/gis/ne_10m_coastline/ne_10m_coastline.shp") #shapefile for plotting maps with islands in Penobscot Bay
GAM_data<- na.omit(GAM_data)#Remove NAs
EData<- na.omit(EData)#Remove NAs
MData<- na.omit(MData)#Remove NAs
WData<- na.omit(WData)#Remove NAs
####add year column####
GAM_data$Start_Date = as.Date(GAM_data$Start_Date, "%m/%d/%Y")
GAM_data$Year = as.numeric(format(GAM_data$Start_Date, "%Y"))

EData$Start_Date = as.Date(EData$Start_Date, "%m/%d/%Y")
EData$Year = as.numeric(format(EData$Start_Date, "%Y"))

MData$Start_Date = as.Date(MData$Start_Date, "%m/%d/%Y")
MData$Year = as.numeric(format(MData$Start_Date, "%Y"))
o
WData$Start_Date = as.Date(WData$Start_Date, "%m/%d/%Y")
WData$Year = as.numeric(format(WData$Start_Date, "%Y"))
##### GAM FOR EAST ######

FalldataE2<-EData[!grepl("SP", EData$Survey),]
FLFJE2<-FalldataE2[-c(1:2,4:13)]
FLMJE2<-FalldataE2[-c(1:3,5:13)]
FLFAE2<-FalldataE2[-c(1:5,7:13)]
FLMAE2<-FalldataE2[-c(1:4,6:13)]

SpringdataE2<-EData[!grepl("FL",EData$Survey),]
SPFJE2<-SpringdataE2[-c(1:2,4:13)]
SPMJE2<-SpringdataE2[-c(1:3,5:13)]
SPFAE2<-SpringdataE2[-c(1:5,7:13)]
SPMAE2<-SpringdataE2[-c(1:4,6:13)]

FLFJE2<-FLFJE2%>% rename(catch=c(1))
FLMJE2<-FLMJE2%>% rename(catch=c(1))
FLFAE2<-FLFAE2%>% rename(catch=c(1))
FLMAE2<-FLMAE2%>% rename(catch=c(1))
SPFJE2<-SPFJE2%>% rename(catch=c(1))
SPMJE2<-SPMJE2%>% rename(catch=c(1))
SPFAE2<-SPFAE2%>% rename(catch=c(1))
SPMAE2<-SPMAE2%>% rename(catch=c(1))

FLFJE2<-FLFJE2[!is.na(FLFJE2$Bottom_WaterTempDegC) & !is.na(FLFJE2$Bottom_Salinity_psu),]#Remove NAs
FLMJE2<-FLMJE2[!is.na(FLMJE2$Bottom_WaterTempDegC) & !is.na(FLMJE2$Bottom_Salinity_psu),]
FLFAE2<-FLFAE2[!is.na(FLFAE2$Bottom_WaterTempDegC) & !is.na(FLFAE2$Bottom_Salinity_psu),]
FLMAE2<-FLMAE2[!is.na(FLMAE2$Bottom_WaterTempDegC) & !is.na(FLMAE2$Bottom_Salinity_psu),]
SPFJE2<-SPFJE2[!is.na(SPFJE2$Bottom_WaterTempDegC) & !is.na(SPFJE2$Bottom_Salinity_psu),]
SPMJE2<-SPMJE2[!is.na(SPMJE2$Bottom_WaterTempDegC) & !is.na(SPMJE2$Bottom_Salinity_psu),]
SPFAE2<-SPFAE2[!is.na(SPFAE2$Bottom_WaterTempDegC) & !is.na(SPFAE2$Bottom_Salinity_psu),]
SPMAE2<-SPMAE2[!is.na(SPMAE2$Bottom_WaterTempDegC) & !is.na(SPMAE2$Bottom_Salinity_psu),]

###Drop outliars#### 
#not dropping any because dropping made model predictions worse

#FLFAE2 <- FLFAE2[!(FLFAE2$catch==535), ]
#FLFAE2 <- FLFAE2[!(FLFAE2$catch==504), ]
#FLMAE2 <- FLMAE2[!(FLMAE2$catch==742), ]
#FLMAE2 <- FLMAE2[!(FLMAE2$catch==538), ]

#SPFAE2 <- SPFAE2[!(SPFAE2$catch==928), ]

#SPMAE2 <- SPMAE2[!(SPMAE2$catch==650), ]
#SPMAE2 <- SPMAE2[!(SPMAE2$catch==682), ]
#SPMAE2 <- SPMAE2[!(SPMAE2$catch==704), ]

#### 1. Simulate lobster abundance by using GAM ####
####FLFJE2##########
hist(FLFJE2$catch,breaks=30)

# Abundance GAM#
FLFJE2abundance.full<-gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(),data=FLFJE2) # Build second stage GAM with all possible habitat variables
summary(FLFJE2abundance.full) # Find significant variables based on p-value
FLFJE2abundance.full$aic
FLFJE2abundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(), data=FLFJE2) # Build first stage GAM with significant habitat variables, keeping salinity because including it has lower aic (Zurr et al.)
summary(FLFJE2abundance.sig)
FLFJE2abundance.sig$aic
# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/NS2 Fall/curveplots01.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(FLFJE2abundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=2,cex.axis=1.6)
plot(FLFJE2abundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Bottom~Temperature)),cex.lab=2,cex.axis=1.6)
plot.new()
plot(FLFJE2abundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Distance~Offshore)),cex.lab=2,cex.axis=1.6)
plot(FLFJE2abundance.sig, select =4, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Sediment)),cex.lab=2,cex.axis=1.6)
dev.off()

## 1.2 Fit GAM #####
FLFJE2p.abundance=as.matrix(FLFJE2abundance.sig$fitted.values) # Abundance values of lobsters at current trAW2l survey stations

####FLMJE2##########
hist(FLMJE2$catch,breaks=30)
# Abundance GAM#
FLMJE2abundance.full<-gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(),data=FLMJE2) # Build second stage GAM with all possible habitat variables
summary(FLMJE2abundance.full) # Find significant variables based on p-value
FLMJE2abundance.full$aic
FLMJE2abundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(), data=FLMJE2) # Build first stage GAM with significant habitat variables
summary(FLMJE2abundance.sig)
FLMJE2abundance.sig$aic
# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/NS2 Fall/curveplots02.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(FLMJE2abundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=2,cex.axis=1.6)
plot(FLMJE2abundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Bottom~Temperature)),cex.lab=2,cex.axis=1.6)
plot.new()
plot(FLMJE2abundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Distance~Offshore)),cex.lab=2,cex.axis=1.6)
plot(FLMJE2abundance.sig, select =4, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Sediment)),cex.lab=2,cex.axis=1.6)
dev.off()
## 1.2 Fit GAM #####
FLMJE2p.abundance=as.matrix(FLMJE2abundance.sig$fitted.values) # Abundance values of lobsters at current trAW2l survey stations

#########

####FLFAE2##########
hist(FLFAE2$catch,breaks=30)
# Abundance GAM#
FLFAE2abundance.full<-gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(),data=(FLFAE2)) # Build second stage GAM with all possible habitat variables
summary(FLFAE2abundance.full) # Find significant variables based on p-value
FLFAE2abundance.full$aic
FLFAE2abundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(), data=FLFAE2) # Build first stage GAM with significant habitat variables
summary(FLFAE2abundance.sig)
FLFAE2abundance.sig$aic
# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/NS2 Fall/curveplots03.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(FLFAE2abundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=2,cex.axis=1.6)
plot(FLFAE2abundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Temperature)),cex.lab=2,cex.axis=1.6)
plot(FLFAE2abundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Bottom~Salinity)),cex.lab=2,cex.axis=1.6)
plot(FLFAE2abundance.sig, select =4, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Distance~Offshore)),cex.lab=2,cex.axis=1.6)
plot(FLFAE2abundance.sig, select =5, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Sediment)),cex.lab=2,cex.axis=1.6)
dev.off()
## 1.2 Fit GAM #####
FLFAE2p.abundance=as.matrix(FLFAE2abundance.sig$fitted.values) # Abundance values of lobsters at current trAW2l survey stations

#####FLMAE2#########
hist(FLMAE2$catch,breaks=30)
# Abundance GAM#
FLMAE2abundance.full<-gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(),data=FLMAE2) # Build second stage GAM with all possible habitat variables
summary(FLMAE2abundance.full) # Find significant variables based on p-value
FLMAE2abundance.full$aic
FLMAE2abundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(), data=FLMAE2) # Build first stage GAM with significant habitat variables
summary(FLMAE2abundance.sig)
FLMAE2abundance.sig$aic
# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/NS2 Fall/curveplots04.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(FLMAE2abundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=2,cex.axis=1.6)
plot(FLMAE2abundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Temperature)),cex.lab=2,cex.axis=1.6)
plot(FLMAE2abundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Bottom~Salinity)),cex.lab=2,cex.axis=1.6)
plot(FLMAE2abundance.sig, select =4, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Distance~Offshore)),cex.lab=2,cex.axis=1.6)
plot(FLMAE2abundance.sig, select =5, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Sediment)),cex.lab=2,cex.axis=1.6)
dev.off()
## 1.2 Fit GAM #####
FLMAE2p.abundance=as.matrix(FLMAE2abundance.sig$fitted.values) # Abundance values of lobsters at current trAW2l survey stations

############

#####SPFJE2########
hist(SPFJE2$catch,breaks=30)

# Abundance GAM#
SPFJE2abundance.full<-gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(),data=SPFJE2) # Build second stage GAM with all possible habitat variables
summary(SPFJE2abundance.full) # Find significant variables based on p-value
SPFJE2abundance.full$aic
SPFJE2abundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(), data=SPFJE2) # Build first stage GAM with significant habitat variables
summary(SPFJE2abundance.sig)
SPFJE2abundance.sig$aic
# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/NS2 Spring/curveplots05.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(SPFJE2abundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=2,cex.axis=1.6)
plot(SPFJE2abundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Temperature)),cex.lab=2,cex.axis=1.6)
plot(SPFJE2abundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Bottom~Salinity)),cex.lab=2,cex.axis=1.6)
plot(SPFJE2abundance.sig, select =4, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Distance~Offshore)),cex.lab=2,cex.axis=1.6)
plot(SPFJE2abundance.sig, select =5, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Sediment)),cex.lab=2,cex.axis=1.6)
dev.off()
## 1.2 Fit GAM #####
SPFJE2p.abundance=as.matrix(SPFJE2abundance.sig$fitted.values) # Abundance values of lobsters at current trAW2l survey stations

#####SPMJE2######
hist(SPMJE2$catch,breaks=30)
# Abundance GAM#
SPMJE2abundance.full<-gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(),data=SPMJE2) # Build second stage GAM with all possible habitat variables
summary(SPMJE2abundance.full) # Find significant variables based on p-value
SPMJE2abundance.full$aic
SPMJE2abundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(), data=SPMJE2) # Build first stage GAM with significant habitat variables
summary(SPMJE2abundance.sig)
SPMJE2abundance.sig$aic
# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/NS2 Spring/curveplots06.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(SPMJE2abundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=2,cex.axis=1.6)
plot(SPMJE2abundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Temperature)),cex.lab=2,cex.axis=1.6)
plot(SPMJE2abundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Bottom~Salinity)),cex.lab=2,cex.axis=1.6)
plot(SPMJE2abundance.sig, select =4, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Distance~Offshore)),cex.lab=2,cex.axis=1.6)
plot(SPMJE2abundance.sig, select =5, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Sediment)),cex.lab=2,cex.axis=1.6)
dev.off()
## 1.2 Fit GAM #####
SPMJE2p.abundance=as.matrix(SPMJE2abundance.sig$fitted.values) # Abundance values of lobsters at current trAW2l survey stations

########

#####SPFAE2######
hist(SPFAE2$catch,breaks=30)

# Abundance GAM#
SPFAE2abundance.full<-gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(),data=SPFAE2) # Build second stage GAM with all possible habitat variables
summary(SPFAE2abundance.full) # Find significant variables based on p-value
SPFAE2abundance.full$aic
SPFAE2abundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(), data=SPFAE2) # Build first stage GAM with significant habitat variables
summary(SPFAE2abundance.sig)
SPFAE2abundance.sig$aic
# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/NS2 Spring/curveplots07.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(SPFAE2abundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=2,cex.axis=1.6)
plot(SPFAE2abundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Temperature)),cex.lab=2,cex.axis=1.6)
plot(SPFAE2abundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Bottom~Salinity)),cex.lab=2,cex.axis=1.6)
plot(SPFAE2abundance.sig, select =4, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Distance~Offshore)),cex.lab=2,cex.axis=1.6)
plot(SPFAE2abundance.sig, select =5, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Sediment)),cex.lab=2,cex.axis=1.6)
dev.off()
## 1.2 Fit GAM #####
SPFAE2p.abundance=as.matrix(SPFAE2abundance.sig$fitted.values) # Abundance values of lobsters at current trAW2l survey stations

########

#####SPMAE2######
hist(SPMAE2$catch,breaks=30)
# Abundance GAM#
SPMAE2abundance.full<-gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(),data=SPMAE2) # Build second stage GAM with all possible habitat variables
summary(SPMAE2abundance.full) # Find significant variables based on p-value
SPMAE2abundance.full$aic
SPMAE2abundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(), data=SPMAE2) # Build first stage GAM with significant habitat variables
summary(SPMAE2abundance.sig)
SPMAE2abundance.sig$aic
# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/NS2 Spring/curveplots08.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(SPMAE2abundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=2,cex.axis=1.6)
plot(SPMAE2abundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Temperature)),cex.lab=2,cex.axis=1.6)
plot(SPMAE2abundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Bottom~Salinity)),cex.lab=2,cex.axis=1.6)
plot(SPMAE2abundance.sig, select =4, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Distance~Offshore)),cex.lab=2,cex.axis=1.6)
plot(SPMAE2abundance.sig, select =5, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Sediment)),cex.lab=2,cex.axis=1.6)
dev.off()
## 1.2 Fit GAM #####
SPMAE2p.abundance=as.matrix(SPMAE2abundance.sig$fitted.values) # Abundance values of lobsters at current trAW2l survey stations

########


##### GAM FOR MIDDLE ######

FalldataM2<-MData[!grepl("SP", MData$Survey),]
FLFJM2<-FalldataM2[-c(1:2,4:13)]
FLMJM2<-FalldataM2[-c(1:3,5:13)]
FLFAM2<-FalldataM2[-c(1:5,7:13)]
FLMAM2<-FalldataM2[-c(1:4,6:13)]

SpringdataM2<-MData[!grepl("FL",MData$Survey),]
SPFJM2<-SpringdataM2[-c(1:2,4:13)]
SPMJM2<-SpringdataM2[-c(1:3,5:13)]
SPFAM2<-SpringdataM2[-c(1:5,7:13)]
SPMAM2<-SpringdataM2[-c(1:4,6:13)]

FLFJM2<-FLFJM2%>% rename(catch=c(1))
FLMJM2<-FLMJM2%>% rename(catch=c(1))
FLFAM2<-FLFAM2%>% rename(catch=c(1))
FLMAM2<-FLMAM2%>% rename(catch=c(1))
SPFJM2<-SPFJM2%>% rename(catch=c(1))
SPMJM2<-SPMJM2%>% rename(catch=c(1))
SPFAM2<-SPFAM2%>% rename(catch=c(1))
SPMAM2<-SPMAM2%>% rename(catch=c(1))

FLFJM2<-FLFJM2[!is.na(FLFJM2$Bottom_WaterTempDegC) & !is.na(FLFJM2$Bottom_Salinity_psu),]#Remove NAs
FLMJM2<-FLMJM2[!is.na(FLMJM2$Bottom_WaterTempDegC) & !is.na(FLMJM2$Bottom_Salinity_psu),]
FLFAM2<-FLFAM2[!is.na(FLFAM2$Bottom_WaterTempDegC) & !is.na(FLFAM2$Bottom_Salinity_psu),]
FLMAM2<-FLMAM2[!is.na(FLMAM2$Bottom_WaterTempDegC) & !is.na(FLMAM2$Bottom_Salinity_psu),]
SPFJM2<-SPFJM2[!is.na(SPFJM2$Bottom_WaterTempDegC) & !is.na(SPFJM2$Bottom_Salinity_psu),]
SPMJM2<-SPMJM2[!is.na(SPMJM2$Bottom_WaterTempDegC) & !is.na(SPMJM2$Bottom_Salinity_psu),]
SPFAM2<-SPFAM2[!is.na(SPFAM2$Bottom_WaterTempDegC) & !is.na(SPFAM2$Bottom_Salinity_psu),]
SPMAM2<-SPMAM2[!is.na(SPMAM2$Bottom_WaterTempDegC) & !is.na(SPMAM2$Bottom_Salinity_psu),]

###Drop outliars#### 
#not dropping any because dropping made model predictions worse

#FLFAM2 <- FLFAM2[!(FLFAM2$catch==535), ]
#FLFAM2 <- FLFAM2[!(FLFAM2$catch==504), ]
#FLMAM2 <- FLMAM2[!(FLMAM2$catch==742), ]
#FLMAM2 <- FLMAM2[!(FLMAM2$catch==538), ]

#SPFAM2 <- SPFAM2[!(SPFAM2$catch==928), ]

#SPMAM2 <- SPMAM2[!(SPMAM2$catch==650), ]
#SPMAM2 <- SPMAM2[!(SPMAM2$catch==682), ]
#SPMAM2 <- SPMAM2[!(SPMAM2$catch==704), ]

#### 1. Simulate lobster abundance by using GAM ####
####FLFJM2##########
hist(FLFJM2$catch,breaks=30)

# Abundance GAM#
FLFJM2abundance.full<-gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(),data=FLFJM2) # Build second stage GAM with all possible habitat variables
summary(FLFJM2abundance.full) # Find significant variables based on p-value
FLFJM2abundance.full$aic
FLFJM2abundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(sediment, k=5) , family=tw(), data=FLFJM2) # Build first stage GAM with significant habitat variables, keeping salinity because including it has lower aic (Zurr et al.)
summary(FLFJM2abundance.sig)
FLFJM2abundance.sig$aic
# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/NS2 Fall/curveplots09.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(FLFJM2abundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=2,cex.axis=1.6)
plot(FLFJM2abundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Temperature)),cex.lab=2,cex.axis=1.6)
plot.new()
plot.new()
plot(FLFJM2abundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Sediment)),cex.lab=2,cex.axis=1.6)
dev.off()
## 1.2 Fit GAM #####
FLFJM2p.abundance=as.matrix(FLFJM2abundance.sig$fitted.values) # Abundance values of lobsters at current trAW2l survey stations

####FLMJM2##########
hist(FLMJM2$catch,breaks=30)
# Abundance GAM#
FLMJM2abundance.full<-gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(),data=FLMJM2) # Build second stage GAM with all possible habitat variables
summary(FLMJM2abundance.full) # Find significant variables based on p-value
FLMJM2abundance.full$aic
FLMJM2abundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(), data=FLMJM2) # Build first stage GAM with significant habitat variables
summary(FLMJM2abundance.sig)
FLMJM2abundance.sig$aic
# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/NS2 Fall/curveplots10.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(FLMJM2abundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=2,cex.axis=1.6)
plot(FLMJM2abundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Temperature)),cex.lab=2,cex.axis=1.6)
plot.new()
plot(FLMJM2abundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Distance~Offshore)),cex.lab=2,cex.axis=1.6)
plot(FLMJM2abundance.sig, select =4, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Sediment)),cex.lab=2,cex.axis=1.6)
dev.off()
## 1.2 Fit GAM #####
FLMJM2p.abundance=as.matrix(FLMJM2abundance.sig$fitted.values) # Abundance values of lobsters at current trAW2l survey stations

#########

####FLFAM2##########
hist(FLFAM2$catch,breaks=30)
# Abundance GAM#
FLFAM2abundance.full<-gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(),data=(FLFAM2)) # Build second stage GAM with all possible habitat variables
summary(FLFAM2abundance.full) # Find significant variables based on p-value
FLFAM2abundance.full$aic
FLFAM2abundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5), family=tw(), data=FLFAM2) # Build first stage GAM with significant habitat variables
summary(FLFAM2abundance.sig)
FLFAM2abundance.sig$aic
# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/NS2 Fall/curveplots11.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(FLFAM2abundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=2,cex.axis=1.6)
plot(FLFAM2abundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Temperature)),cex.lab=2,cex.axis=1.6)
plot.new()
plot.new()
plot.new()
dev.off()
## 1.2 Fit GAM #####
FLFAM2p.abundance=as.matrix(FLFAM2abundance.sig$fitted.values) # Abundance values of lobsters at current trAW2l survey stations

#####FLMAM2#########
hist(FLMAM2$catch,breaks=30)
# Abundance GAM#
FLMAM2abundance.full<-gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(),data=FLMAM2) # Build second stage GAM with all possible habitat variables
summary(FLMAM2abundance.full) # Find significant variables based on p-value
FLMAM2abundance.full$aic
FLMAM2abundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(sediment, k=5) , family=tw(), data=FLMAM2) # Build first stage GAM with significant habitat variables
summary(FLMAM2abundance.sig)
FLMAM2abundance.sig$aic
# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/NS2 Fall/curveplots12.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(FLMAM2abundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=2,cex.axis=1.6)
plot(FLMAM2abundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Temperature)),cex.lab=2,cex.axis=1.6)
plot.new()
plot.new()
plot(FLMAM2abundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Sediment)),cex.lab=2,cex.axis=1.6)
dev.off()
## 1.2 Fit GAM #####
FLMAM2p.abundance=as.matrix(FLMAM2abundance.sig$fitted.values) # Abundance values of lobsters at current trAW2l survey stations

############

#####SPFJM2########
hist(SPFJM2$catch,breaks=30)

# Abundance GAM#
SPFJM2abundance.full<-gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(),data=SPFJM2) # Build second stage GAM with all possible habitat variables
summary(SPFJM2abundance.full) # Find significant variables based on p-value
SPFJM2abundance.full$aic
SPFJM2abundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(dist_frm_shore, k=5), family=tw(), data=SPFJM2) # Build first stage GAM with significant habitat variables
summary(SPFJM2abundance.sig)
SPFJM2abundance.sig$aic
# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/NS2 Spring/curveplots13.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(SPFJM2abundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=2,cex.axis=1.6)
plot(SPFJM2abundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Temperature)),cex.lab=2,cex.axis=1.6)
plot.new()
plot(SPFJM2abundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Distance~Offshore)),cex.lab=2,cex.axis=1.6)
plot(SPFJM2abundance.sig, select =4, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Sediment)),cex.lab=2,cex.axis=1.6)
dev.off()
## 1.2 Fit GAM #####
SPFJM2p.abundance=as.matrix(SPFJM2abundance.sig$fitted.values) # Abundance values of lobsters at current trAW2l survey stations

#####SPMJM2######
hist(SPMJM2$catch,breaks=30)
# Abundance GAM#
SPMJM2abundance.full<-gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(),data=SPMJM2) # Build second stage GAM with all possible habitat variables
summary(SPMJM2abundance.full) # Find significant variables based on p-value
SPMJM2abundance.full$aic
SPMJM2abundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(dist_frm_shore, k=5), family=tw(), data=SPMJM2) # Build first stage GAM with significant habitat variables
summary(SPMJM2abundance.sig)
SPMJM2abundance.sig$aic
# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/NS2 Spring/curveplots14.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(SPMJM2abundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=2,cex.axis=1.6)
plot(SPMJM2abundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Temperature)),cex.lab=2,cex.axis=1.6)
plot.new()
plot(SPMJM2abundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Distance~Offshore)),cex.lab=2,cex.axis=1.6)
plot.new()
dev.off()
## 1.2 Fit GAM #####
SPMJM2p.abundance=as.matrix(SPMJM2abundance.sig$fitted.values) # Abundance values of lobsters at current trAW2l survey stations

########

#####SPFAM2######
hist(SPFAM2$catch,breaks=30)

# Abundance GAM#
SPFAM2abundance.full<-gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(),data=SPFAM2) # Build second stage GAM with all possible habitat variables
summary(SPFAM2abundance.full) # Find significant variables based on p-value
SPFAM2abundance.full$aic
SPFAM2abundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(dist_frm_shore, k=5), family=tw(), data=SPFAM2) # Build first stage GAM with significant habitat variables
summary(SPFAM2abundance.sig)
SPFAM2abundance.sig$aic
# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/NS2 Spring/curveplots15.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(SPFAM2abundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=2,cex.axis=1.6)
plot(SPFAM2abundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Temperature)),cex.lab=2,cex.axis=1.6)
plot.new()
plot(SPFAM2abundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Distance~Offshore)),cex.lab=2,cex.axis=1.6)
plot.new()
dev.off()
## 1.2 Fit GAM #####
SPFAM2p.abundance=as.matrix(SPFAM2abundance.sig$fitted.values) # Abundance values of lobsters at current trAW2l survey stations

########

#####SPMAM2######
hist(SPMAM2$catch,breaks=30)
# Abundance GAM#
SPMAM2abundance.full<-gam((catch) ~ s(Latitude, k=5)+s(Longitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(),data=SPMAM2) # Build second stage GAM with all possible habitat variables
summary(SPMAM2abundance.full) # Find significant variables based on p-value
SPMAM2abundance.full$aic
SPMAM2abundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(sediment, k=5) , family=tw(), data=SPMAM2) # Build first stage GAM with significant habitat variables
summary(SPMAM2abundance.sig)
SPMAM2abundance.sig$aic
# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/NS2 Spring/curveplots16.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(SPMAM2abundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=2,cex.axis=1.6)
plot(SPMAM2abundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Temperature)),cex.lab=2,cex.axis=1.6)
plot.new()
plot.new()
dev.off()
plot(SPMAM2abundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Sediment)),cex.lab=2,cex.axis=1.6)
## 1.2 Fit GAM #####
SPMAM2p.abundance=as.matrix(SPMAM2abundance.sig$fitted.values) # Abundance values of lobsters at current trAW2l survey stations

########






######################################

#### GAM FOR WEST ########
FalldataW2<-WData[!grepl("SP", WData$Survey),]
FLFJW2<-FalldataW2[-c(1:2,4:13)]
FLMJW2<-FalldataW2[-c(1:3,5:13)]
FLFAW2<-FalldataW2[-c(1:5,7:13)]
FLMAW2<-FalldataW2[-c(1:4,6:13)]

SpringdataW2<-WData[!grepl("FL",WData$Survey),]
SPFJW2<-SpringdataW2[-c(1:2,4:13)]
SPMJW2<-SpringdataW2[-c(1:3,5:13)]
SPFAW2<-SpringdataW2[-c(1:5,7:13)]
SPMAW2<-SpringdataW2[-c(1:4,6:13)]

FLFJW2<-FLFJW2%>% rename(catch=c(1))
FLMJW2<-FLMJW2%>% rename(catch=c(1))
FLFAW2<-FLFAW2%>% rename(catch=c(1))
FLMAW2<-FLMAW2%>% rename(catch=c(1))
SPFJW2<-SPFJW2%>% rename(catch=c(1))
SPMJW2<-SPMJW2%>% rename(catch=c(1))
SPFAW2<-SPFAW2%>% rename(catch=c(1))
SPMAW2<-SPMAW2%>% rename(catch=c(1))

FLFJW2<-FLFJW2[!is.na(FLFJW2$Bottom_WaterTempDegC) & !is.na(FLFJW2$Bottom_Salinity_psu),]#Remove NAs
FLMJW2<-FLMJW2[!is.na(FLMJW2$Bottom_WaterTempDegC) & !is.na(FLMJW2$Bottom_Salinity_psu),]
FLFAW2<-FLFAW2[!is.na(FLFAW2$Bottom_WaterTempDegC) & !is.na(FLFAW2$Bottom_Salinity_psu),]
FLMAW2<-FLMAW2[!is.na(FLMAW2$Bottom_WaterTempDegC) & !is.na(FLMAW2$Bottom_Salinity_psu),]
SPFJW2<-SPFJW2[!is.na(SPFJW2$Bottom_WaterTempDegC) & !is.na(SPFJW2$Bottom_Salinity_psu),]
SPMJW2<-SPMJW2[!is.na(SPMJW2$Bottom_WaterTempDegC) & !is.na(SPMJW2$Bottom_Salinity_psu),]
SPFAW2<-SPFAW2[!is.na(SPFAW2$Bottom_WaterTempDegC) & !is.na(SPFAW2$Bottom_Salinity_psu),]
SPMAW2<-SPMAW2[!is.na(SPMAW2$Bottom_WaterTempDegC) & !is.na(SPMAW2$Bottom_Salinity_psu),]

###Drop outliars#### 
#only dropping one from SPFJW2 because it was the only model to improve abundance predictions after dropping

#FLFJW2 <- FLFJW2[!(FLFJW2$catch==332), ]
#FLMJW2 <- FLMJW2[!(FLMJW2$catch==306), ]
SPFJW2 <- SPFJW2[!(SPFJW2$catch==181), ]
SPFJW2 <- SPFJW2[!(SPFJW2$catch==159), ]
#SPMJW2 <- SPMJW2[!(SPMJW2$catch==164), ]
#SPFAW2 <- SPFAW2[!(SPFAW2$catch==379), ]
#SPFAW2 <- SPFAW2[!(SPFAW2$catch==323), ]
#SPMAW2 <- SPMAW2[!(SPMAW2$catch==476), ]
#SPMAW2 <- SPMAW2[!(SPMAW2$catch==370), ]
#### 1. Simulate lobster abundance by using GAM ####

##########
####FLFJW2##########
hist(FLFJW2$catch,breaks=30)
# Abundance GAM#
FLFJW2abundance.full<-gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(),data=FLFJW2) # Build second stage GAM with all possible habitat variables
summary(FLFJW2abundance.full) # Find significant variables based on p-value
FLFJW2abundance.full$aic
FLFJW2abundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5) , family=tw(), data=FLFJW2) # Build first stage GAM with significant habitat variables. Including sediment and not salinity resulted in lowest aic value
summary(FLFJW2abundance.sig)
FLFJW2abundance.sig$aic
# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/NS2 Fall/curveplots17.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(FLFJW2abundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=2,cex.axis=1.6)
plot(FLFJW2abundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Temperature)),cex.lab=2,cex.axis=1.6)
plot(FLFJW2abundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Bottom~Salinity)),cex.lab=2,cex.axis=1.6)
plot(FLFJW2abundance.sig, select =4, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Distance~Offshore)),cex.lab=2,cex.axis=1.6)
plot.new()
dev.off()
## 1.2 Fit GAM #####
FLFJW2p.abundance=as.matrix(FLFJW2abundance.sig$fitted.values) # Abundance values of lobsters at current trAW2l survey stations

########
####FLMJW2##########
hist(FLMJW2$catch,breaks=30)

# Abundance GAM#
FLMJW2abundance.full<-gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(),data=FLMJW2) # Build second stage GAM with all possible habitat variables
summary(FLMJW2abundance.full) # Find significant variables based on p-value
FLMJW2abundance.full$aic
FLMJW2abundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5) , family=tw(), data=FLMJW2) # Build first stage GAM with significant habitat variables
summary(FLMJW2abundance.sig)
FLMJW2abundance.sig$aic
# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/NS2 Fall/curveplots18.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(FLMJW2abundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=2,cex.axis=1.6)
plot(FLMJW2abundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Temperature)),cex.lab=2,cex.axis=1.6)
plot(FLMJW2abundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Bottom~Salinity)),cex.lab=2,cex.axis=1.6)
plot(FLMJW2abundance.sig, select =4, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Distance~Offshore)),cex.lab=2,cex.axis=1.6)
plot.new()
dev.off()
## 1.2 Fit GAM #####
FLMJW2p.abundance=as.matrix(FLMJW2abundance.sig$fitted.values) # Abundance values of lobsters at current trAW2l survey stations


############
####FLFAW2##########
hist(FLFAW2$catch,breaks=30)
# Abundance GAM#
FLFAW2abundance.full<-gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(),data=FLFAW2) # Build second stage GAM with all possible habitat variables
summary(FLFAW2abundance.full) # Find significant variables based on p-value
FLFAW2abundance.full$aic
FLFAW2abundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5), family=tw(), data=FLFAW2) # Build first stage GAM with significant habitat variables
summary(FLFAW2abundance.sig)
FLFAW2abundance.sig$aic
# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/NS2 Fall/curveplots19.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(FLFAW2abundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=2,cex.axis=1.6)
plot(FLFAW2abundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Temperature)),cex.lab=2,cex.axis=1.6)
plot(FLFAW2abundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Salinity)),cex.lab=2,cex.axis=1.6)
plot(FLFAW2abundance.sig, select =4, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Distance~Offshore)),cex.lab=2,cex.axis=1.6)
plot.new()
dev.off()
## 1.2 Fit GAM #####
FLFAW2p.abundance=as.matrix(FLFAW2abundance.sig$fitted.values) # Abundance values of lobsters at current trAW2l survey stations

#####FLMAW2#########
hist(FLMAW2$catch,breaks=30)
# Abundance GAM#
FLMAW2abundance.full<-gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(),data=FLMAW2) # Build second stage GAM with all possible habitat variables
summary(FLMAW2abundance.full) # Find significant variables based on p-value
FLMAW2abundance.full$aic
FLMAW2abundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5), family=tw(), data=FLMAW2) # Build first stage GAM with significant habitat variables
summary(FLMAW2abundance.sig)
FLMAW2abundance.sig$aic
# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/NS2 Fall/curveplots20.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(FLMAW2abundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=2,cex.axis=1.6)
plot(FLMAW2abundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Temperatue)),cex.lab=2,cex.axis=1.6)
plot(FLMAW2abundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Bottom~Salinity)),cex.lab=2,cex.axis=1.6)
plot(FLMAW2abundance.sig, select =4, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Distance~Offshore)),cex.lab=2,cex.axis=1.6)
plot.new()
dev.off()
## 1.2 Fit GAM #####
FLMAW2p.abundance=as.matrix(FLMAW2abundance.sig$fitted.values) # Abundance values of lobsters at current trAW2l survey stations

############

#####SPFJW2########
hist(SPFJW2$catch,breaks=30)
# Abundance GAM#
SPFJW2abundance.full<-gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(),data=SPFJW2) # Build second stage GAM with all possible habitat variables
summary(SPFJW2abundance.full) # Find significant variables based on p-value
SPFJW2abundance.full$aic
SPFJW2abundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(), data=SPFJW2) # Build first stage GAM with significant habitat variables
summary(SPFJW2abundance.sig)
SPFJW2abundance.sig$aic
# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/NS2 Spring/curveplots21.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(SPFJW2abundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=2,cex.axis=1.6)
plot(SPFJW2abundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Temperature)),cex.lab=2,cex.axis=1.6)
plot(SPFJW2abundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Bottom~Salinity)),cex.lab=2,cex.axis=1.6)
plot(SPFJW2abundance.sig, select =4, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Distance~Offshore)),cex.lab=2,cex.axis=1.6)
plot(SPFJW2abundance.sig, select =5, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Sediment)),cex.lab=2,cex.axis=1.6)
dev.off()
## 1.2 Fit GAM #####
SPFJW2p.abundance=as.matrix(SPFJW2abundance.sig$fitted.values) # Abundance values of lobsters at current trAW2l survey stations

#####SPMJW2######
hist(SPMJW2$catch,breaks=30)
# Abundance GAM#
SPMJW2abundance.full<-gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(),data=SPMJW2) # Build second stage GAM with all possible habitat variables
summary(SPMJW2abundance.full) # Find significant variables based on p-value
SPMJW2abundance.full$aic
SPMJW2abundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5), family=tw(), data=SPMJW2) # Build first stage GAM with significant habitat variables
summary(SPMJW2abundance.sig)
SPMJW2abundance.sig$aic
# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/NS2 Spring/curveplots22.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(SPMJW2abundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=2,cex.axis=1.6)
plot(SPMJW2abundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Temperature)),cex.lab=2,cex.axis=1.6)
plot(SPMJW2abundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Bottom~Salinity)),cex.lab=2,cex.axis=1.6)
plot(SPMJW2abundance.sig, select =4, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Distance~Offshore)),cex.lab=2,cex.axis=1.6)
plot.new()
dev.off()
## 1.2 Fit GAM #####
SPMJW2p.abundance=as.matrix(SPMJW2abundance.sig$fitted.values) # Abundance values of lobsters at current trAW2l survey stations

########

#####SPFAW2######
hist(SPFAW2$catch,breaks=30)

# Abundance GAM#
SPFAW2abundance.full<-gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(),data=SPFAW2) # Build second stage GAM with all possible habitat variables
summary(SPFAW2abundance.full) # Find significant variables based on p-value
SPFAW2abundance.full$aic
SPFAW2abundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(), data=SPFAW2) # Build first stage GAM with significant habitat variables
summary(SPFAW2abundance.sig)
SPFAW2abundance.sig$aic
# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/NS2 Spring/curveplots23.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(SPFAW2abundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=2,cex.axis=1.6)
plot(SPFAW2abundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Temperature)),cex.lab=2,cex.axis=1.6)
plot(SPFAW2abundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Bottom~Salintiy)),cex.lab=2,cex.axis=1.6)
plot(SPFAW2abundance.sig, select =4, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Distance~Offshore)),cex.lab=2,cex.axis=1.6)
plot(SPFAW2abundance.sig, select =5, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Sediment)),cex.lab=2,cex.axis=1.6)
dev.off()
## 1.2 Fit GAM #####
SPFAW2p.abundance=as.matrix(SPFAW2abundance.sig$fitted.values) # Abundance values of lobsters at current trAW2l survey stations

########

#####SPMAW2######
hist(SPMAW2$catch,breaks=30)

# Abundance GAM#
SPMAW2abundance.full<-gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(),data=SPMAW2) # Build second stage GAM with all possible habitat variables
summary(SPMAW2abundance.full) # Find significant variables based on p-value
SPMAW2abundance.full$aic
SPMAW2abundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(), data=SPMAW2) # Build first stage GAM with significant habitat variables
summary(SPMAW2abundance.sig)
SPMAW2abundance.sig$aic
# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/NS2 Spring/curveplots24.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(SPMAW2abundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=2,cex.axis=1.6)
plot(SPMAW2abundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Temperature)),cex.lab=1.8,cex.axis=1.5)
plot(SPMAW2abundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Bottom~Salinity)),cex.lab=2,cex.axis=1.6)
plot(SPMAW2abundance.sig, select =4, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Distance~Offshore)),cex.lab=2,cex.axis=1.6)
plot(SPMAW2abundance.sig, select =5, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Sediment)),cex.lab=2,cex.axis=1.6)
dev.off()
## 1.2 Fit GAM #####
SPMAW2p.abundance=as.matrix(SPMAW2abundance.sig$fitted.values) # Abundance values of lobsters at current trAW2l survey stations

##################

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

checkresid(FLFJE2abundance.sig,FLFJE2)
checkresid(FLMJE2abundance.sig,FLMJE2)
checkresid(FLFAE2abundance.sig,FLFAE2)
checkresid(FLMAE2abundance.sig,FLMAE2)
checkresid(SPFJE2abundance.sig,SPFJE2)
checkresid(SPMJE2abundance.sig,SPMJE2)
checkresid(SPFAE2abundance.sig,SPFAE2)
checkresid(SPMAE2abundance.sig,SPMAE2)

checkresid(FLFJW2abundance.sig,FLFJW2)
checkresid(FLMJW2abundance.sig,FLMJW2)
checkresid(FLFAW2abundance.sig,FLFAW2)
checkresid(FLMAW2abundance.sig,FLMAW2)
checkresid(SPFJW2abundance.sig,SPFJW2)
checkresid(SPMJW2abundance.sig,SPMJW2)
checkresid(SPFAW2abundance.sig,SPFAW2)
checkresid(SPMAW2abundance.sig,SPMAW2)

lmtest::bptest(FLFJE2abundance.sig)
lmtest::bptest(FLMJE2abundance.sig)
lmtest::bptest(FLFAE2abundance.sig)
lmtest::bptest(FLMAE2abundance.sig)
lmtest::bptest(SPFJE2abundance.sig)
lmtest::bptest(SPMJE2abundance.sig)
lmtest::bptest(SPFAE2abundance.sig)
lmtest::bptest(SPMAE2abundance.sig)

lmtest::bptest(FLFJW2abundance.sig)
lmtest::bptest(FLMJW2abundance.sig)
lmtest::bptest(FLFAW2abundance.sig)
lmtest::bptest(FLMAW2abundance.sig)
lmtest::bptest(SPFJW2abundance.sig)
lmtest::bptest(SPMJW2abundance.sig)
lmtest::bptest(SPFAW2abundance.sig)
lmtest::bptest(SPMAW2abundance.sig)



dotchart(SPFA$catch)
#####MAPPING NONSTATIONARY########

library(mapdata)
library(maps)

FLFJE2.d<-as.data.frame(cbind(FLFJE2,FLFJE2p.abundance))
names(FLFJE2.d)[10]<- "p.abundance"
FLMJE2.d<-as.data.frame(cbind(FLMJE2,FLMJE2p.abundance))
names(FLMJE2.d)[10]<- "p.abundance"
FLFAE2.d<-as.data.frame(cbind(FLFAE2,FLFAE2p.abundance))
names(FLFAE2.d)[10]<- "p.abundance"
FLMAE2.d<-as.data.frame(cbind(FLMAE2,FLMAE2p.abundance))
names(FLMAE2.d)[10]<- "p.abundance"
SPFJE2.d<-as.data.frame(cbind(SPFJE2,SPFJE2p.abundance))
names(SPFJE2.d)[10]<- "p.abundance"
SPMJE2.d<-as.data.frame(cbind(SPMJE2,SPMJE2p.abundance))
names(SPMJE2.d)[10]<- "p.abundance"
SPFAE2.d<-as.data.frame(cbind(SPFAE2,SPFAE2p.abundance))
names(SPFAE2.d)[10]<- "p.abundance"
SPMAE2.d<-as.data.frame(cbind(SPMAE2,SPMAE2p.abundance))
names(SPMAE2.d)[10]<- "p.abundance"

FLFJW2.d<-as.data.frame(cbind(FLFJW2,FLFJW2p.abundance))
names(FLFJW2.d)[10]<- "p.abundance"
FLMJW2.d<-as.data.frame(cbind(FLMJW2,FLMJW2p.abundance))
names(FLMJW2.d)[10]<- "p.abundance"
FLFAW2.d<-as.data.frame(cbind(FLFAW2,FLFAW2p.abundance))
names(FLFAW2.d)[10]<- "p.abundance"
FLMAW2.d<-as.data.frame(cbind(FLMAW2,FLMAW2p.abundance))
names(FLMAW2.d)[10]<- "p.abundance"
SPFJW2.d<-as.data.frame(cbind(SPFJW2,SPFJW2p.abundance))
names(SPFJW2.d)[10]<- "p.abundance"
SPMJW2.d<-as.data.frame(cbind(SPMJW2,SPMJW2p.abundance))
names(SPMJW2.d)[10]<- "p.abundance"
SPFAW2.d<-as.data.frame(cbind(SPFAW2,SPFAW2p.abundance))
names(SPFAW2.d)[10]<- "p.abundance"
SPMAW2.d<-as.data.frame(cbind(SPMAW2,SPMAW2p.abundance))
names(SPMAW2.d)[10]<- "p.abundance"

FLFJM2.d<-as.data.frame(cbind(FLFJM2,FLFJM2p.abundance))
names(FLFJM2.d)[10]<- "p.abundance"
FLMJM2.d<-as.data.frame(cbind(FLMJM2,FLMJM2p.abundance))
names(FLMJM2.d)[10]<- "p.abundance"
FLFAM2.d<-as.data.frame(cbind(FLFAM2,FLFAM2p.abundance))
names(FLFAM2.d)[10]<- "p.abundance"
FLMAM2.d<-as.data.frame(cbind(FLMAM2,FLMAM2p.abundance))
names(FLMAM2.d)[10]<- "p.abundance"
SPFJM2.d<-as.data.frame(cbind(SPFJM2,SPFJM2p.abundance))
names(SPFJM2.d)[10]<- "p.abundance"
SPMJM2.d<-as.data.frame(cbind(SPMJM2,SPMJM2p.abundance))
names(SPMJM2.d)[10]<- "p.abundance"
SPFAM2.d<-as.data.frame(cbind(SPFAM2,SPFAM2p.abundance))
names(SPFAM2.d)[10]<- "p.abundance"
SPMAM2.d<-as.data.frame(cbind(SPMAM2,SPMAM2p.abundance))
names(SPMAM2.d)[10]<- "p.abundance"

FLFJemw=rbind(FLFJE2.d,FLFJM2.d,FLFJW2.d)
FLMJemw=rbind(FLMJE2.d,FLMJM2.d,FLMJW2.d)
FLFAemw=rbind(FLFAE2.d,FLFAM2.d,FLFAW2.d)
FLMAemw=rbind(FLMAE2.d,FLMAM2.d,FLMAW2.d)
SPFJemw=rbind(SPFJE2.d,SPFJM2.d,SPFJW2.d)
SPMJemw=rbind(SPMJE2.d,SPMJM2.d,SPMJW2.d)
SPFAemw=rbind(SPFAE2.d,SPFAM2.d,SPFAW2.d)
SPMAemw=rbind(SPMAE2.d,SPMAM2.d,SPMAW2.d)


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

nonstationarysim(FLFJemw,"J")
nonstationarysim(FLMJemw,"J")
nonstationarysim(FLFAemw,"A")
nonstationarysim(FLMAemw,"A")
nonstationarysim(SPFJemw,"J")
nonstationarysim(SPMJemw,"J")
nonstationarysim(SPFAemw,"A")
nonstationarysim(SPMAemw,"A")

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

nonstationaryresids(FLFJemw,"J")
nonstationaryresids(FLMJemw,"J")
nonstationaryresids(FLFAemw,"A")
nonstationaryresids(FLMAemw,"A")
nonstationaryresids(SPFJemw,"J")
nonstationaryresids(SPMJemw,"J")
nonstationaryresids(SPFAemw,"A")
nonstationaryresids(SPMAemw,"A")



######Calculate RMSE values####
FLFJresid=append(FLFJE2abundance.sig$residuals,FLFJW2abundance.sig$residuals)
FLMJresid=append(FLMJE2abundance.sig$residuals,FLMJW2abundance.sig$residuals)
FLFAresid=append(FLFAE2abundance.sig$residuals,FLFAW2abundance.sig$residuals)
FLMAresid=append(FLMAE2abundance.sig$residuals,FLMAW2abundance.sig$residuals)
SPFJresid=append(SPFJE2abundance.sig$residuals,SPFJW2abundance.sig$residuals)
SPMJresid=append(SPMJE2abundance.sig$residuals,SPMJW2abundance.sig$residuals)
SPFAresid=append(SPFAE2abundance.sig$residuals,SPFAW2abundance.sig$residuals)
SPMAresid=append(SPMAE2abundance.sig$residuals,SPMAW2abundance.sig$residuals)

FLFJresid=append(FLFJresid,FLFJM2abundance.sig$residuals)
FLMJresid=append(FLMJresid,FLMJM2abundance.sig$residuals)
FLFAresid=append(FLFAresid,FLFAM2abundance.sig$residuals)
FLMAresid=append(FLMAresid,FLMAM2abundance.sig$residuals)
SPFJresid=append(SPFJresid,SPFJM2abundance.sig$residuals)
SPMJresid=append(SPMJresid,SPMJM2abundance.sig$residuals)
SPFAresid=append(SPFAresid,SPFAM2abundance.sig$residuals)
SPMAresid=append(SPMAresid,SPMAM2abundance.sig$residuals)

RMSEfunction<- function(data){
  sqrt(mean(data^2))
}

RMSEfunction(FLFJresid)
RMSEfunction(FLMJresid)
RMSEfunction(FLFAresid)
RMSEfunction(FLMAresid)
RMSEfunction(SPFJresid)
RMSEfunction(SPMJresid)
RMSEfunction(SPFAresid)
RMSEfunction(SPMAresid)
################################################

#### 1.6 Cross Validation ####

NSCrossvalidation2<- function(groupE,groupM,groupW, gam_model_optionE,gam_model_optionM,gam_model_optionW, PE,PM,PW,Title){
  
  groupE = groupE [!is.na(groupE$Bottom_WaterTempDegC) & !is.na(groupE$Bottom_Salinity_psu),]#Remove NAs
  groupM = groupM [!is.na(groupM$Bottom_WaterTempDegC) & !is.na(groupM$Bottom_Salinity_psu),]#Remove NAs
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
                                                +s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(), data=subset(train), select=TRUE)#all sig
    if(gam_model_optionE==3) abundance_cv<- gam((catch) ~ s(Longitude,Latitude, k=5)
                                                +s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(), data=subset(train), select=TRUE)#no temp/sal
    
    predensity=predict.gam(abundance_cv, test, se.fit=TRUE, type = "response")
    result = cbind(test$catch, predensity$fit) 
    final = result[result[,1]>0 & result[,2]>0,]
    q = lm(final[,1] ~ final[,2], data = as.data.frame(final))
    resE[i,] = c(summary(q)$coefficient[,1], summary(q)$r.squared)
  }
  NM = nrow(groupM)
  PM= (1/(1+sqrt(PM-1)))
  resM = matrix(0, ncol = 3, nrow = 100)
  colnames(resM) = c("Intercept", "Slope", "R.Squared")
  for (i in 1:100){          
    print(i)
    sub = sample(1:NM, size = (NM*PM))
    train = groupM[-sub,]
    test = groupM[sub,]
    
    if(gam_model_optionM==1) abundance_cv<- gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)
                                                +s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(), data=subset(train), select=TRUE)
    if(gam_model_optionM==2) abundance_cv<- gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)
                                                +s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(), data=subset(train), select=TRUE)
    if(gam_model_optionM==3) abundance_cv<- gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)
                                                +s(sediment, k=5) , family=tw(), data=subset(train), select=TRUE) #no sal/dfs
    if(gam_model_optionM==4) abundance_cv<- gam((catch) ~ s(Longitude,Latitude, k=5)
                                                +s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(), data=subset(train), select=TRUE)#no sal/temp
    if(gam_model_optionM==5) abundance_cv<- gam((catch) ~ s(Longitude,Latitude, k=5)
                                                +s(sediment, k=5) , family=tw(), data=subset(train), select=TRUE)#no sal/temp/dfs
    if(gam_model_optionM==6) abundance_cv<- gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)
                                                +s(Bottom_Salinity_psu, k=5)+s(sediment, k=5) , family=tw(), data=subset(train), select=TRUE)#no dfs
    predensity=predict.gam(abundance_cv, test, se.fit=TRUE, type = "response")
    result = cbind(test$catch, predensity$fit) 
    final = result[result[,1]>0 & result[,2]>0,]
    q = lm(final[,1] ~ final[,2], data = as.data.frame(final))
    resM[i,] = c(summary(q)$coefficient[,1], summary(q)$r.squared)
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
    if(gam_model_optionW==4) abundance_cv<- gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)
                                                +s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(), data=subset(train), select=TRUE) #not temp/sal 
    if(gam_model_optionW==7) abundance_cv<- gam((catch) ~ s(Longitude,Latitude, k=5)+
                                                +s(dist_frm_shore, k=5) , family=tw(), data=subset(train), select=TRUE) #not salinity/sediment
    if(gam_model_optionW==8) abundance_cv<- gam((catch) ~ s(Longitude,Latitude, k=5)
                                                +s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(), data=subset(train), select=TRUE) #nottemp
    if(gam_model_optionW==9) abundance_cv<- gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)
                                                +s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5) , family=tw(), data=subset(train), select=TRUE) #not sediment
    predensity=predict.gam(abundance_cv, test, se.fit=TRUE, type = "response")
    result = cbind(test$catch, predensity$fit) 
    final = result[result[,1]>0 & result[,2]>0,]
    q = lm(final[,1] ~ final[,2], data = as.data.frame(final))
    resW[i,] = c(summary(q)$coefficient[,1], summary(q)$r.squared)
  }
  
  
  res=((resE+resW+resM)/3)
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
NSCrossvalidation2(FLFJE2,FLFJM2,FLFJW2,3,3,7,4,4,4,Title="FLFJ, RMSE=1.44")
NSCrossvalidation2(FLMJE2,FLMJM2,FLMJW2,2,1,9,6,5,5,Title="FLMJ, RMSE=1.43")
NSCrossvalidation2(FLFAE2,FLFAM2,FLFAW2,2,5,1,6,3,5,Title="FLFA, RMSE=1.09")
NSCrossvalidation2(FLMAE2,FLMAM2,FLMAW2,2,3,4,6,4,4,Title="FLMA, RMSE=1.06")
NSCrossvalidation2(SPFJE2,SPFJM2,SPFJW2,2,4,2,6,4,6,Title="SPFJ, RMSE=1.51")
NSCrossvalidation2(SPMJE2,SPMJM2,SPMJW2,2,1,8,6,5,5,Title="SPMJ, RMSE=1.52")
NSCrossvalidation2(SPFAE2,SPFAM2,SPFAW2,2,1,2,6,5,6,Title="SPFA, RMSE=1.32")
NSCrossvalidation2(SPMAE2,SPMAM2,SPMAW2,2,6,2,6,5,6,Title="SPMA, RMSE=1.28")


###AIC Values#####
(FLFAE2abundance.sig$aic+FLFAM2abundance.sig$aic+FLFAW2abundance.sig$aic)/3
(FLFJE2abundance.sig$aic+FLFJM2abundance.sig$aic+FLFJW2abundance.sig$aic)/3
(FLMAE2abundance.sig$aic+FLMAM2abundance.sig$aic+FLMAW2abundance.sig$aic)/3
(FLMJE2abundance.sig$aic+FLMJM2abundance.sig$aic+FLMJW2abundance.sig$aic)/3

(SPFAE2abundance.sig$aic+SPFAM2abundance.sig$aic+SPFAW2abundance.sig$aic)/3
(SPFJE2abundance.sig$aic+SPFJM2abundance.sig$aic+SPFJW2abundance.sig$aic)/3
(SPMAE2abundance.sig$aic+SPMAM2abundance.sig$aic+SPMAW2abundance.sig$aic)/3
(SPMJE2abundance.sig$aic+SPMJM2abundance.sig$aic+SPMJW2abundance.sig$aic)/3

FLFAE2abundance.sig$aic
FLFAW2abundance.sig$aic
FLFJE2abundance.sig$aic
FLFJW2abundance.sig$aic
FLMAE2abundance.sig$aic
FLMAW2abundance.sig$aic
FLMJE2abundance.sig$aic
FLMJW2abundance.sig$aic

SPFAE2abundance.sig$aic
SPFAW2abundance.sig$aic
SPFJE2abundance.sig$aic
SPFJW2abundance.sig$aic
SPMAE2abundance.sig$aic
SPMAW2abundance.sig$aic
SPMJE2abundance.sig$aic
SPMJW2abundance.sig$aic

FLFAM2abundance.sig$aic
FLFJM2abundance.sig$aic
FLMAM2abundance.sig$aic
FLMJM2abundance.sig$aic

SPFAM2abundance.sig$aic
SPFJM2abundance.sig$aic
SPMAM2abundance.sig$aic
SPMJM2abundance.sig$aic

###Moran's I####

SPFAemw$Latitude=ifelse(duplicated(SPFAemw$Latitude), SPFAemw$Latitude+0.00000001,SPFAemw$Latitude) #changing duplicate lat/long values tiny bit so Moran's I function can run
SPFAemw$Longitude=ifelse(duplicated(SPFAemw$Longitude), SPFAemw$Longitude+0.00000001,SPFAemw$Longitude)

SPFJemw$Latitude=ifelse(duplicated(SPFJemw$Latitude), SPFJemw$Latitude+0.00000001,SPFJemw$Latitude)
SPFJemw$Longitude=ifelse(duplicated(SPFJemw$Longitude), SPFJemw$Longitude+0.00000001,SPFJemw$Longitude)

SPMAemw$Latitude=ifelse(duplicated(SPMAemw$Latitude), SPMAemw$Latitude+0.00000001,SPMAemw$Latitude)
SPMAemw$Longitude=ifelse(duplicated(SPMAemw$Longitude), SPMAemw$Longitude+0.00000001,SPMAemw$Longitude)

SPMJemw$Latitude=ifelse(duplicated(SPMJemw$Latitude), SPMJemw$Latitude+0.00000001,SPMJemw$Latitude)
SPMJemw$Longitude=ifelse(duplicated(SPMJemw$Longitude), SPMJemw$Longitude+0.00000001,SPMJemw$Longitude)

FLFAemw$Latitude=ifelse(duplicated(FLFAemw$Latitude), FLFAemw$Latitude+0.00000001,FLFAemw$Latitude)
FLFAemw$Longitude=ifelse(duplicated(FLFAemw$Longitude), FLFAemw$Longitude+0.00000001,FLFAemw$Longitude)

FLMAemw$Latitude=ifelse(duplicated(FLMAemw$Latitude), FLMAemw$Latitude+0.00000001,FLMAemw$Latitude)
FLMAemw$Longitude=ifelse(duplicated(FLMAemw$Longitude), FLMAemw$Longitude+0.00000001,FLMAemw$Longitude)

FLFJemw$Latitude=ifelse(duplicated(FLFJemw$Latitude), FLFJemw$Latitude+0.00000001,FLFJemw$Latitude)
FLFJemw$Longitude=ifelse(duplicated(FLFJemw$Longitude), FLFJemw$Longitude+0.00000001,FLFJemw$Longitude)

FLMJemw$Latitude=ifelse(duplicated(FLMJemw$Latitude), FLMJemw$Latitude+0.00000001,FLMJemw$Latitude)
FLMJemw$Longitude=ifelse(duplicated(FLMJemw$Longitude), FLMJemw$Longitude+0.00000001,FLMJemw$Longitude)


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
moranI(FLFJresid,FLFJemw)
moranI(FLMJresid,FLMJemw)
moranI(FLFAresid,FLFAemw)
moranI(FLMAresid,FLMAemw)
moranI(SPFJresid,SPFJemw)
moranI(SPMJresid,SPMJemw)
moranI(SPFAresid,SPFAemw)
moranI(SPMAresid,SPMAemw)


####Making multiple curves on one plot for paper####
t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  
  ## Save the color
  invisible(t.col)
}
t_col("#FFADAD",50,"plot_greent")

########## all plots#####
par("mar"=c(4, 5, 1, 1))
plot.gam(SPMAabundance.sig, select =2, scale =0,ylab = expression(bold(SPMA~~Abundance)), xlab = expression(bold(Temperature)),
         cex.lab=1.8,cex.axis=1.5,col = "#FF0000",shade = TRUE,shade.col=t_col("#FFADAD",50,"plot_rdt"),lwd = 4, lty=2,rug=FALSE)
rug(SPMA$Bottom_WaterTempDegC, ticksize = 0.1, side = 1, lwd = 3, col = "#FF0000")
# second plot  EDIT: needs to have same ylim
par(new = TRUE)
plot(SPMAEabundance.sig, select =2, scale =0,ylab = "", xlab = "",col="#FFFF00",axes = FALSE,shade = TRUE,
     shade.col=t_col("#FFFFE0",60,"plot_ylwt"),lwd = 4,lty=1,ticksize=0.06)
rug(SPMAE$Bottom_WaterTempDegC, ticksize = 0.085, side = 1, lwd = 4, col = "#FFFF00")
par(new = TRUE)
plot(SPMAE2abundance.sig, select =2, scale =0,ylab = "", xlab = "",col="#01FF00",axes = FALSE,shade = TRUE,
     shade.col=t_col("#DBFFDB",60,"plot_greent"),lwd = 4,lty=3,rug=FALSE)
rug(SPMAE$Bottom_WaterTempDegC, ticksize = 0.07, side = 1, lwd = 3, col = "#01FF00")
par(new = TRUE)
plot(SPMAM2abundance.sig, select =2, scale =0,ylab = "", xlab = "",col="cyan",axes = FALSE,shade = TRUE,
     shade.col=t_col("cyan",70,"plot_ylwt"),lwd = 4,lty=1,ticksize=0.04)
par(new = TRUE)
plot(SPMAW2abundance.sig, select =2, scale =0,ylab = "", xlab = "",col="#0000FF",axes = FALSE,shade = TRUE,
     shade.col=t_col("#8080FF",80,"plot_bluet"),lwd = 4,lty=1,ticksize=0.02) #colored as blue bc NSV1 and NSV2 are the same for west



legend("bottomleft", inset=0.2, # position
       legend = c("GOM-GAM","East1-GAM","East2-GAM","Central-GAM","West-GAM"), 
       col = c("#FF0000","#FFFF00","#01FF00","cyan","#0000FF"),
       title = "Legend",
       cex = 1.2,
       lwd = c(4,4,4),
       lty = c(2,1,3),
       text.col = "black",
       box.col = "black",
       box.lty=1, 
       box.lwd=1,
       bty = "o",
       bg="gray95") # border

########## FLFJ all plots#####
par("mar"=c(4, 5, 1, 1))
plot.gam(FLFJabundance.sig, select =2, scale =0,ylab = expression(bold(FLFJ~~Abundance)), xlab = expression(bold(Temperature)),
         cex.lab=1.8,cex.axis=1.5,col = "#FF0000",shade = TRUE,shade.col=t_col("#FFADAD",50,"plot_rdt"),lwd = 4, lty=2,xlim = c(6,15),ylim = c(-5,1),rug=FALSE)
rug(FLFJ$Bottom_WaterTempDegC, ticksize = 0.1, side = 1, lwd = 3, col = "#FF0000")
# second plot  EDIT: needs to have same ylim
par(new = TRUE)
plot(FLFJEabundance.sig, select =2, scale =0,ylab = "", xlab = "",col="#FFFF00",axes = FALSE,shade = TRUE,
     shade.col=t_col("#FFFFE0",60,"plot_ylwt"),lwd = 4,lty=1,ticksize=0.06,xlim = c(6,15),ylim = c(-5,1))
rug(FLFJE$Bottom_WaterTempDegC, ticksize = 0.085, side = 1, lwd = 4, col = "#FFFF00")
par(new = TRUE)
plot(FLFJE2abundance.sig, select =2, scale =0,ylab = "", xlab = "",col="#01FF00",axes = FALSE,shade = TRUE,
     shade.col=t_col("#DBFFDB",60,"plot_greent"),lwd = 4,lty=3,rug=FALSE,xlim = c(6,15),ylim = c(-5,1))
rug(FLFJE$Bottom_WaterTempDegC, ticksize = 0.07, side = 1, lwd = 3, col = "#01FF00")
par(new = TRUE)
plot(FLFJM2abundance.sig, select =2, scale =0,ylab = "", xlab = "",col="cyan",axes = FALSE,shade = TRUE,
     shade.col=t_col("cyan",70,"plot_ylwt"),lwd = 4,lty=1,ticksize=0.04,xlim = c(6,15),ylim = c(-5,1))
par(new = TRUE)
plot(FLFJW2abundance.sig, select =2, scale =0,ylab = "", xlab = "",col="#0000FF",axes = FALSE,shade = TRUE,
     shade.col=t_col("#8080FF",80,"plot_bluet"),lwd = 4,lty=1,ticksize=0.02,xlim = c(6,15),ylim = c(-5,1)) #colored as blue bc NSV1 and NSV2 are the same for west



legend("bottomright", inset=0.2, # position
       legend = c("GOM-GAM","East1-GAM","East2-GAM","Central-GAM","West-GAM"), 
       col = c("#FF0000","#FFFF00","#01FF00","cyan","#0000FF"),
       title = "Legend",
       cex = 1.2,
       lwd = c(4,4,4),
       lty = c(2,1,3),
       text.col = "black",
       box.col = "black",
       box.lty=1, 
       box.lwd=1,
       bty = "o",
       bg="gray95") # border

#######FLFJ_paper_curve_plots#####
################# FLFJ comparing stationary (GOM-GAM), East-GAM, and West-GAMs#####
par("mar"=c(4, 5, 1, 1))
plot.gam(FLFJabundance.sig, select =2, scale =0,ylab = expression(bold(FLFJ~~Abundance)), xlab = expression(bold(Temperature)),
         cex.lab=1.8,cex.axis=1.5,col = "#FF0000",shade = TRUE,shade.col=t_col("#FFADAD",10,"plot_rdt"),lwd = 4, lty=2,xlim = c(6,15),ylim = c(-5,1),rug=FALSE)
rug(FLFJ$Bottom_WaterTempDegC, ticksize = 0.065, side = 1, lwd = 3.5, col = "#FF0000")
# second plot  EDIT: needs to have same ylim
par(new = TRUE)
plot(FLFJWabundance.sig, select =2, scale =0,ylab = "", xlab = "",col="#0000FF",axes = FALSE,shade = TRUE,
     shade.col=t_col("#8080FF",50,"plot_bluet"),lwd = 4,lty=1,prob=TRUE,xlim = c(6,15),ylim = c(-5,1),rug=FALSE) 
rug(FLFJW$Bottom_WaterTempDegC, ticksize = 0.045, side = 1, lwd = 3.5, col = "#0000FF")
par(new = TRUE)
plot(FLFJEabundance.sig, select =2, scale =0,ylab = "", xlab = "",col="orange",axes = FALSE,shade = TRUE,
     shade.col=t_col("orange",85,"plot_ylwt"),lwd = 4,lty=3,prob=TRUE,xlim = c(6,15),ylim = c(-5,1),rug=FALSE)
rug(FLFJE$Bottom_WaterTempDegC, ticksize = 0.03, side = 1, lwd = 3.5, col = "orange")
legend("bottomright", inset=0.2, # position
       legend = c("GOM-GAM","West-GAM", "East1-GAM"), 
       col = c("#FF0000","#0000FF","orange"),
       title = "Legend",
       cex = 1.2,
       lwd = c(4,4,4),
       lty = c(2,1,3),
       text.col = "black",
       box.col = "black",
       box.lty=1, 
       box.lwd=1,
       bty = "o",
       bg="gray95") # border

####NSV2_EMW_#####
par("mar"=c(4, 5, 1, 1))
plot(SPMAM2abundance.sig, select =2, scale =0,ylab = "", xlab = "",col="cyan",axes = FALSE,shade = TRUE,
     shade.col=t_col("cyan",60,"plot_ylwt"),lwd = 4,lty=1,xlim = c(2.5,11),ylim = c(-5,1),rug=FALSE)
rug(SPMAM2$Bottom_WaterTempDegC, ticksize = 0.075, side = 1, lwd = 3.5, col = "cyan")
# second plot  EDIT: needs to have same ylim
par(new = TRUE)
plot.gam(SPMAabundance.sig, select =2, scale =0,ylab = expression(bold(SPMA~~Abundance)), xlab = expression(bold(Temperature)),
         cex.lab=1.8,cex.axis=1.5,col = "#FF0000",shade = TRUE,prob=TRUE,shade.col=t_col("#FFADAD",10,"plot_rdt"),lwd = 4, lty=2,xlim = c(2.5,11),ylim = c(-5,1),rug=FALSE)
rug(SPMA$Bottom_WaterTempDegC, ticksize = 0.055, side = 1, lwd = 3.5, col = "#FF0000")
par(new = TRUE)
plot(SPMAW2abundance.sig, select =2, scale =0,ylab = "", xlab = "",col="#0000FF",axes = FALSE,shade = TRUE,
     shade.col=t_col("#8080FF",50,"plot_bluet"),lwd = 4,lty=1,prob=TRUE,xlim = c(2.5,11),ylim = c(-5,1),rug=FALSE) 
rug(SPMAW2$Bottom_WaterTempDegC, ticksize = 0.040, side = 1, lwd = 3.0, col="#0000FF")
par(new = TRUE)
plot(SPMAE2abundance.sig, select =2, scale =0,ylab = "", xlab = "",col="orange",axes = FALSE,shade = TRUE,
     shade.col=t_col("orange",70,"plot_ylwt"),lwd = 4,lty=3,prob=TRUE,xlim = c(2.5,11),ylim = c(-5,1),rug=FALSE)
rug(SPMAW2$Bottom_WaterTempDegC, ticksize = 0.025, side = 1, lwd = 3.0, col = "orange")

legend("bottomleft", inset=0.25, # position
       legend = c("GOM-GAM","West-GAM","East2-GAM","Central-GAM"), 
       col = c("#FF0000","#0000FF","orange","cyan"),
       title = "Legend",
       cex = 1.2,
       lwd = c(4,4,4,4),
       lty = c(2,1,3,1),
       text.col = "black",
       box.col = "black",
       box.lty=1, 
       box.lwd=1,
       bty = "o",
       bg="gray95") # border



par("mar"=c(4, 5, 1, 1))

# second plot  EDIT: needs to have same ylim

plot(FLFJM2abundance.sig, select =2, scale =0,ylab = "", xlab = "",col="cyan",axes = FALSE,shade = TRUE,
     shade.col=t_col("cyan",60,"plot_ylwt"),lwd = 4,lty=1,prob=TRUE,xlim = c(6,15),ylim = c(-5,1),rug=FALSE)
rug(FLFJM2$Bottom_WaterTempDegC, ticksize = 0.075, side = 1, lwd = 3.5, col = "cyan")
par(new = TRUE)
plot.gam(FLFJabundance.sig, select =2, scale =0,ylab = expression(bold(FLFJ~~Abundance)), xlab = expression(bold(Temperature)),
         cex.lab=1.8,cex.axis=1.5,col = "#FF0000",shade = TRUE,shade.col=t_col("#FFADAD",10,"plot_rdt"),lwd = 4, lty=2,xlim = c(6,15),ylim = c(-5,1),rug=FALSE)
rug(FLFJ$Bottom_WaterTempDegC, ticksize = 0.055, side = 1, lwd = 3.5, col = "#FF0000")
par(new = TRUE)
plot(FLFJW2abundance.sig, select =2, scale =0,ylab = "", xlab = "",col="#0000FF",axes = FALSE,shade = TRUE,
     shade.col=t_col("#8080FF",50,"plot_bluet"),lwd = 4,lty=1,prob=TRUE,xlim = c(6,15),ylim = c(-5,1),rug=FALSE) 
rug(FLFJW2$Bottom_WaterTempDegC, ticksize = 0.04, side = 1, lwd = 3.5, col = "#0000FF")
par(new = TRUE)
plot(FLFJE2abundance.sig, select =2, scale =0,ylab = "", xlab = "",col="orange",axes = FALSE,shade = TRUE,
     shade.col=t_col("orange",70,"plot_ylwt"),lwd = 4,lty=3,prob=TRUE,xlim = c(6,15),ylim = c(-5,1),rug=FALSE)
rug(FLFJE2$Bottom_WaterTempDegC, ticksize = 0.025, side = 1, lwd = 3.5, col = "orange")
legend("bottomright", inset=0.20, # position
       legend = c("Stationary","NSV2 West","NSV2 East","NSV2 Central"), 
       col = c("#FF0000","#0000FF","orange","cyan"),
       title = "Legend",
       cex = 1.2,
       lwd = c(4,4,4,4),
       lty = c(2,1,3,1),
       text.col = "black",
       box.col = "black",
       box.lty=1, 
       box.lwd=1,
       bty = "o",
       bg="gray95") # border


nonstationarysim<- function(simabundance,size){
  start_x <- range(simabundance$Longitude)[1]
  end_x <- range(simabundance$Longitude)[2]
  start_y <- range(simabundance$Latitude)[1]
  end_y <- range(simabundance$Latitude)[2]
  colors <- brewer.pal(9, "YlOrRd")
  colbrks<-classIntervals(simabundance$Bottom_WaterTempDegC, n=8, style="fixed", 
                          fixedBreaks=
                            if(size=="J")c(0,1,3,7,15,28,43,200,425) #breaks calculated by using average "quantile" breaks
                          else c(0,13,26,42,65,93,118,155,502)
  )
  brks<- colbrks$brks
  par(mfrow=c(1,1)); par(mar=c(4,4,1,1))
  plot(simabundance$Bottom_WaterTempDegC,simabundance$catch, col="gray", cex=1.5,pch = 20, xlab="temp", ylab="catch")
  legend("topleft", "Catch v. Temp - SPMA", bty="n")}
  


nonstationarysim(SPMA,"A")



#####function for all season x sex x size curve plots#####
t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  
  ## Save the color
  invisible(t.col)
}
t_col("#FFADAD",50,"plot_greent")

allgroupcurves<- function(group, group.sig, group.sigE, group.sigE2,group.sigM2,group.sigW2){
par("mar"=c(4, 5, 1, 1))
plot.gam(group.sig, select =2, scale =0,ylab = expression (bold(Abundance)), xlab = expression(bold(Temperature)),
         cex.lab=1.8,cex.axis=1.5,col = "#FF0000",shade = TRUE,shade.col=t_col("#FFADAD",50,"plot_rdt"),
         lwd = 4, lty=2,xlim = c(6,14),ylim=c(-5,1))


# second plot  EDIT: needs to have same ylim
par(new = TRUE)
plot(group.sigE, select =2, scale =0,ylab = "", xlab = "",col="#FFFF00",shade = TRUE,
     shade.col=t_col("#FFFFE0",60,"plot_ylwt"),lwd = 4,lty=1,cex.lab=1.8,cex.axis=1.5, xlim = c(6,14),ylim=c(-5,1))
par(new = TRUE)
plot(group.sigE2, select =2, scale =0,ylab = "", xlab = "",col="#01FF00",shade = TRUE,
     shade.col=t_col("#DBFFDB",60,"plot_greent"),lwd = 4,lty=3,cex.lab=1.8,cex.axis=1.5,xlim = c(6,14),ylim=c(-5,1))
par(new = TRUE)
plot(group.sigM2, select =2, scale =0,ylab = "", xlab = "",col="cyan",shade = TRUE,
     shade.col=t_col("cyan",70,"plot_ylwt"),lwd = 4,lty=1,cex.lab=1.8,cex.axis=1.5,xlim = c(6,14),ylim=c(-5,1))
par(new = TRUE)
plot(group.sigW2, select =2, scale =0,ylab = "", xlab = "",col="#0000FF",shade = TRUE,
     shade.col=t_col("#8080FF",80,"plot_bluet"),lwd = 4,lty=1,cex.lab=1.8,cex.axis=1.5, xlim = c(6,14),ylim=c(-5,1)) #colored as blue bc NSV1 and NSV2 are the same for west


legend("topleft", paste(group, "-", "Abundance", sep=""), bty="n", cex=1.5)
legend("bottomright", inset=0.2, # position
       legend = c("GOM-GAM","East1-GAM","East2-GAM","Central-GAM","West-GAM"), 
       col = c("#FF0000","#FFFF00","#01FF00","cyan","#0000FF"),
       title = "Legend",
       cex = 1.2,
       lwd = c(4,4,4),
       lty = c(2,1,3),
       text.col = "black",
       box.col = "black",
       box.lty=1, 
       box.lwd=1,
       bty = "o",
       bg="gray95") # border
}

allgroupcurves(FLFJ, FLFJabundance.sig, FLFJEabundance.sig, FLFJE2abundance.sig,FLFJM2abundance.sig,FLFJW2abundance.sig)
allgroupcurves(FLMJ, FLMJabundance.sig, FLMJEabundance.sig, FLMJE2abundance.sig,FLMJM2abundance.sig,FLMJW2abundance.sig)
allgroupcurves(FLFA, FLFAabundance.sig, FLFAEabundance.sig, FLFAE2abundance.sig,FLFAM2abundance.sig,FLFAW2abundance.sig)
allgroupcurves(FLMA, FLMAabundance.sig, FLMAEabundance.sig, FLMAE2abundance.sig,FLMAM2abundance.sig,FLMAW2abundance.sig)

SPallgroupcurves<- function(group, group.sig, group.sigE, group.sigE2,group.sigM2,group.sigW2){
  par("mar"=c(4, 5, 1, 1))
  plot.gam(group.sig, select =2, scale =0,ylab = expression (bold(Abundance)), xlab = expression(bold(Temperature)),
           cex.lab=1.8,cex.axis=1.5,col = "#FF0000",shade = TRUE,shade.col=t_col("#FFADAD",50,"plot_rdt"),
           lwd = 4, lty=2,xlim = c(3,11),ylim=c(-5,1))
  
  
  # second plot  EDIT: needs to have same ylim
  par(new = TRUE)
  plot(group.sigE, select =2, scale =0,ylab = "", xlab = "",col="#FFFF00",shade = TRUE,
       shade.col=t_col("#FFFFE0",60,"plot_ylwt"),lwd = 4,lty=1,cex.lab=1.8,cex.axis=1.5, xlim = c(3,11),ylim=c(-5,1))
  par(new = TRUE)
  plot(group.sigE2, select =2, scale =0,ylab = "", xlab = "",col="#01FF00",shade = TRUE,
       shade.col=t_col("#DBFFDB",60,"plot_greent"),lwd = 4,lty=3,cex.lab=1.8,cex.axis=1.5,xlim = c(3,11),ylim=c(-5,1))
  par(new = TRUE)
  plot(group.sigM2, select =2, scale =0,ylab = "", xlab = "",col="cyan",shade = TRUE,
       shade.col=t_col("cyan",70,"plot_ylwt"),lwd = 4,lty=1,cex.lab=1.8,cex.axis=1.5,xlim = c(3,11),ylim=c(-5,1))
  par(new = TRUE)
  plot(group.sigW2, select =2, scale =0,ylab = "", xlab = "",col="#0000FF",shade = TRUE,
       shade.col=t_col("#8080FF",80,"plot_bluet"),lwd = 4,lty=1,cex.lab=1.8,cex.axis=1.5, xlim = c(3,11),ylim=c(-5,1)) #colored as blue bc NSV1 and NSV2 are the same for west
  
  
  legend("topleft", paste(group, "-", "Abundance", sep=""), bty="n", cex=1.5)
  #legend("bottomleft", inset=0.2, # position
  #legend = c("Stationary","NSV1 East","NSV2 East","NSV2 Central","NSV1/NSV2 West"), 
  #col = c("#FF0000","#FFFF00","#01FF00","cyan","#0000FF"),
  #title = "Legend",
  #cex = 1.2,
  #lwd = c(4,4,4),
  #lty = c(2,1,3),
  #text.col = "black",
  #box.col = "black",
  #box.lty=1, 
  #box.lwd=1,
  #bty = "o",
  #bg="gray95") # border
}

SPallgroupcurves(SPFJ, SPFJabundance.sig, SPFJEabundance.sig, SPFJE2abundance.sig,SPFJM2abundance.sig,SPFJW2abundance.sig)
SPallgroupcurves(SPMJ, SPMJabundance.sig, SPMJEabundance.sig, SPMJE2abundance.sig,SPMJM2abundance.sig,SPMJW2abundance.sig)
SPallgroupcurves(SPFA, SPFAabundance.sig, SPFAEabundance.sig, SPFAE2abundance.sig,SPFAM2abundance.sig,SPFAW2abundance.sig)
SPallgroupcurves(SPMA, SPMAabundance.sig, SPMAEabundance.sig, SPMAE2abundance.sig,SPMAM2abundance.sig,SPMAW2abundance.sig)

salSPallgroupcurves<- function(group, group.sig, group.sigE, group.sigE2,group.sigM2,group.sigW2){
  par("mar"=c(4, 5, 1, 1))
  plot.gam(group.sig, select =3, scale =0,ylab = expression (bold(Abundance)), xlab = expression(bold(salinity)),
           cex.lab=1.8,cex.axis=1.5,col = "#FF0000",shade = TRUE,shade.col=t_col("#FFADAD",50,"plot_rdt"),
           lwd = 4, lty=2,xlim = c(25,35),ylim=c(-5,1))
  
  
  # second plot  EDIT: needs to have same ylim
  par(new = TRUE)
  plot(group.sigE, select =3, scale =0,ylab = "", xlab = "",col="#FFFF00",shade = TRUE,
       shade.col=t_col("#FFFFE0",60,"plot_ylwt"),lwd = 4,lty=1,cex.lab=1.8,cex.axis=1.5, xlim = c(25,35),ylim=c(-5,1))
  par(new = TRUE)
  plot(group.sigE2, select =3, scale =0,ylab = "", xlab = "",col="#01FF00",shade = TRUE,
       shade.col=t_col("#DBFFDB",60,"plot_greent"),lwd = 4,lty=3,cex.lab=1.8,cex.axis=1.5,xlim = c(25,35),ylim=c(-5,1))
  par(new = TRUE)
  plot(group.sigM2, select =3, scale =0,ylab = "", xlab = "",col="cyan",shade = TRUE,
       shade.col=t_col("cyan",70,"plot_ylwt"),lwd = 4,lty=1,cex.lab=1.8,cex.axis=1.5,xlim = c(25,35),ylim=c(-5,1))
  par(new = TRUE)
  plot(group.sigW2, select =3, scale =0,ylab = "", xlab = "",col="#0000FF",shade = TRUE,
       shade.col=t_col("#8080FF",80,"plot_bluet"),lwd = 4,lty=1,cex.lab=1.8,cex.axis=1.5, xlim = c(25,35),ylim=c(-5,1)) #colored as blue bc NSV1 and NSV2 are the same for west
  
  
  legend("topleft", paste(group, "-", "Abundance", sep=""), bty="n", cex=1.5)
  #legend("bottomleft", inset=0.2, # position
  #legend = c("Stationary","NSV1 East","NSV2 East","NSV2 Central","NSV1/NSV2 West"), 
  #col = c("#FF0000","#FFFF00","#01FF00","cyan","#0000FF"),
  #title = "Legend",
  #cex = 1.2,
  #lwd = c(4,4,4),
  #lty = c(2,1,3),
  #text.col = "black",
  #box.col = "black",
  #box.lty=1, 
  #box.lwd=1,
  #bty = "o",
  #bg="gray95") # border
}

salSPallgroupcurves(SPFJ, SPFJabundance.sig, SPFJEabundance.sig, SPFJE2abundance.sig,SPFJM2abundance.sig,SPFJW2abundance.sig)
salSPallgroupcurves(SPMJ, SPMJabundance.sig, SPMJEabundance.sig, SPMJE2abundance.sig,SPMJM2abundance.sig,SPMJW2abundance.sig)
salSPallgroupcurves(SPFA, SPFAabundance.sig, SPFAEabundance.sig, SPFAE2abundance.sig,SPFAM2abundance.sig,SPFAW2abundance.sig)
salSPallgroupcurves(SPMA, SPMAabundance.sig, SPMAEabundance.sig, SPMAE2abundance.sig,SPMAM2abundance.sig,SPMAW2abundance.sig)

salallgroupcurves<- function(group, group.sig, group.sigE, group.sigE2,group.sigM2,group.sigW2){
  par("mar"=c(4, 5, 1, 1))
  plot.gam(group.sig, select =3, scale =0,ylab = expression (bold(Abundance)), xlab = expression(bold(Temperature)),
           cex.lab=1.8,cex.axis=1.5,col = "#FF0000",shade = TRUE,shade.col=t_col("#FFADAD",50,"plot_rdt"),
           lwd = 4, lty=2,xlim = c(25,35),ylim=c(-6,2))
  
  
  # second plot  EDIT: needs to have same ylim
  par(new = TRUE)
  plot(group.sigE, select =3, scale =0,ylab = "", xlab = "",col="#FFFF00",shade = TRUE,
       shade.col=t_col("#FFFFE0",60,"plot_ylwt"),lwd = 4,lty=1,cex.lab=1.8,cex.axis=1.5, xlim = c(25,35),ylim=c(-6,2))
  par(new = TRUE)
  plot(group.sigE2, select =3, scale =0,ylab = "", xlab = "",col="#01FF00",shade = TRUE,
       shade.col=t_col("#DBFFDB",60,"plot_greent"),lwd = 4,lty=3,cex.lab=1.8,cex.axis=1.5,xlim = c(25,35),ylim=c(-6,2))
  par(new = TRUE)
  plot(group.sigM2, select =3, scale =0,ylab = "", xlab = "",col="cyan",shade = TRUE,
       shade.col=t_col("cyan",70,"plot_ylwt"),lwd = 4,lty=1,cex.lab=1.8,cex.axis=1.5,xlim = c(25,35),ylim=c(-6,2))
  par(new = TRUE)
  plot(group.sigW2, select =3, scale =0,ylab = "", xlab = "",col="#0000FF",shade = TRUE,
       shade.col=t_col("#8080FF",80,"plot_bluet"),lwd = 4,lty=1,cex.lab=1.8,cex.axis=1.5, xlim = c(25,35),ylim=c(-6,2)) #colored as blue bc NSV1 and NSV2 are the same for west
  
  
  legend("topleft", paste(group, "-", "Abundance", sep=""), bty="n", cex=1.5)
  #legend("bottomleft", inset=0.2, # position
  #legend = c("Stationary","NSV1 East","NSV2 East","NSV2 Central","NSV1/NSV2 West"), 
  #col = c("#FF0000","#FFFF00","#01FF00","cyan","#0000FF"),
  #title = "Legend",
  #cex = 1.2,
  #lwd = c(4,4,4),
  #lty = c(2,1,3),
  #text.col = "black",
  #box.col = "black",
  #box.lty=1, 
  #box.lwd=1,
  #bty = "o",
  #bg="gray95") # border
}

salallgroupcurves(FLFJ, FLFJabundance.sig, FLFJEabundance.sig, FLFJE2abundance.sig,FLFJM2abundance.sig,FLFJW2abundance.sig)
salallgroupcurves(FLMJ, FLMJabundance.sig, FLMJEabundance.sig, FLMJE2abundance.sig,FLMJM2abundance.sig,FLMJW2abundance.sig)
salallgroupcurves(FLFA, FLFAabundance.sig, FLFAEabundance.sig, FLFAE2abundance.sig,FLFAM2abundance.sig,FLFAW2abundance.sig)
salallgroupcurves(FLMA, FLMAabundance.sig, FLMAEabundance.sig, FLMAE2abundance.sig,FLMAM2abundance.sig,FLMAW2abundance.sig)




dfsSPallgroupcurves<- function(group, group.sig, group.sigE, group.sigE2,group.sigM2,group.sigW2){
  par("mar"=c(4, 5, 1, 1))
  plot.gam(group.sig, select =4, scale =0,ylab = expression (bold(Abundance)), xlab = expression(bold(salinity)),
           cex.lab=1.8,cex.axis=1.5,col = "#FF0000",shade = TRUE,shade.col=t_col("#FFADAD",50,"plot_rdt"),
           lwd = 4, lty=2,xlim = c(0,0.5),ylim=c(-5,1))
  
  
  # second plot  EDIT: needs to have same ylim
  par(new = TRUE)
  plot(group.sigE, select =4, scale =0,ylab = "", xlab = "",col="#FFFF00",shade = TRUE,
       shade.col=t_col("#FFFFE0",60,"plot_ylwt"),lwd = 4,lty=1,cex.lab=1.8,cex.axis=1.5, xlim = c(0,0.5),ylim=c(-5,1))
  par(new = TRUE)
  plot(group.sigE2, select =4, scale =0,ylab = "", xlab = "",col="#01FF00",shade = TRUE,
       shade.col=t_col("#DBFFDB",60,"plot_greent"),lwd = 4,lty=3,cex.lab=1.8,cex.axis=1.5,xlim = c(0,0.5),ylim=c(-5,1))
  par(new = TRUE)
  plot(group.sigM2, select =4, scale =0,ylab = "", xlab = "",col="cyan",shade = TRUE,
       shade.col=t_col("cyan",70,"plot_ylwt"),lwd = 4,lty=1,cex.lab=1.8,cex.axis=1.5,xlim = c(0,0.5),ylim=c(-5,1))
  par(new = TRUE)
  plot(group.sigW2, select =4, scale =0,ylab = "", xlab = "",col="#0000FF",shade = TRUE,
       shade.col=t_col("#8080FF",80,"plot_bluet"),lwd = 4,lty=1,cex.lab=1.8,cex.axis=1.5, xlim = c(0,0.5),ylim=c(-5,1)) #colored as blue bc NSV1 and NSV2 are the same for west
  
  
  legend("topleft", paste(group, "-", "Abundance", sep=""), bty="n", cex=1.5)
  #legend("bottomleft", inset=0.2, # position
  #legend = c("Stationary","NSV1 East","NSV2 East","NSV2 Central","NSV1/NSV2 West"), 
  #col = c("#FF0000","#FFFF00","#01FF00","cyan","#0000FF"),
  #title = "Legend",
  #cex = 1.2,
  #lwd = c(4,4,4),
  #lty = c(2,1,3),
  #text.col = "black",
  #box.col = "black",
  #box.lty=1, 
  #box.lwd=1,
  #bty = "o",
  #bg="gray95") # border
}

dfsSPallgroupcurves(SPFJ, SPFJabundance.sig, SPFJEabundance.sig, SPFJE2abundance.sig,SPFJM2abundance.sig,SPFJW2abundance.sig)
dfsSPallgroupcurves(SPMJ, SPMJabundance.sig, SPMJEabundance.sig, SPMJE2abundance.sig,SPMJM2abundance.sig,SPMJW2abundance.sig)
dfsSPallgroupcurves(SPFA, SPFAabundance.sig, SPFAEabundance.sig, SPFAE2abundance.sig,SPFAM2abundance.sig,SPFAW2abundance.sig)
dfsSPallgroupcurves(SPMA, SPMAabundance.sig, SPMAEabundance.sig, SPMAE2abundance.sig,SPMAM2abundance.sig,SPMAW2abundance.sig)


sedSPallgroupcurves<- function(group, group.sig, group.sigE, group.sigE2,group.sigM2,group.sigW2){
  par("mar"=c(4, 5, 1, 1))
  plot.gam(group.sig, select =5, scale =0,ylab = expression (bold(Abundance)), xlab = expression(bold(salinity)),
           cex.lab=1.8,cex.axis=1.5,col = "#FF0000",shade = TRUE,shade.col=t_col("#FFADAD",50,"plot_rdt"),
           lwd = 4, lty=2,xlim = c(-5,10),ylim=c(-5,1))
  
  
  # second plot  EDIT: needs to have same ylim
  par(new = TRUE)
  plot(group.sigE, select =5, scale =0,ylab = "", xlab = "",col="#FFFF00",shade = TRUE,
       shade.col=t_col("#FFFFE0",60,"plot_ylwt"),lwd = 4,lty=1,cex.lab=1.8,cex.axis=1.5, xlim = c(-5,10),ylim=c(-5,1))
  par(new = TRUE)
  plot(group.sigE2, select =5, scale =0,ylab = "", xlab = "",col="#01FF00",shade = TRUE,
       shade.col=t_col("#DBFFDB",60,"plot_greent"),lwd = 4,lty=3,cex.lab=1.8,cex.axis=1.5,xlim = c(-5,10),ylim=c(-5,1))
  par(new = TRUE)
  plot(group.sigM2, select =5, scale =0,ylab = "", xlab = "",col="cyan",shade = TRUE,
       shade.col=t_col("cyan",70,"plot_ylwt"),lwd = 4,lty=1,cex.lab=1.8,cex.axis=1.5,xlim = c(-5,10),ylim=c(-5,1))
  par(new = TRUE)
  plot(group.sigW2, select =5, scale =0,ylab = "", xlab = "",col="#0000FF",shade = TRUE,
       shade.col=t_col("#8080FF",80,"plot_bluet"),lwd = 4,lty=1,cex.lab=1.8,cex.axis=1.5, xlim = c(-5,10),ylim=c(-5,1)) #colored as blue bc NSV1 and NSV2 are the same for west
  
  
  legend("topleft", paste(group, "-", "Abundance", sep=""), bty="n", cex=1.5)
  #legend("bottomleft", inset=0.2, # position
  #legend = c("Stationary","NSV1 East","NSV2 East","NSV2 Central","NSV1/NSV2 West"), 
  #col = c("#FF0000","#FFFF00","#01FF00","cyan","#0000FF"),
  #title = "Legend",
  #cex = 1.2,
  #lwd = c(4,4,4),
  #lty = c(2,1,3),
  #text.col = "black",
  #box.col = "black",
  #box.lty=1, 
  #box.lwd=1,
  #bty = "o",
  #bg="gray95") # border
}

sedSPallgroupcurves(SPFJ, SPFJabundance.sig, SPFJEabundance.sig, SPFJE2abundance.sig,SPFJM2abundance.sig,SPFJW2abundance.sig)
sedSPallgroupcurves(SPMJ, SPMJabundance.sig, SPMJEabundance.sig, SPMJE2abundance.sig,SPMJM2abundance.sig,SPMJW2abundance.sig)
sedSPallgroupcurves(SPFA, SPFAabundance.sig, SPFAEabundance.sig, SPFAE2abundance.sig,SPFAM2abundance.sig,SPFAW2abundance.sig)
sedSPallgroupcurves(SPMA, SPMAabundance.sig, SPMAEabundance.sig, SPMAE2abundance.sig,SPMAM2abundance.sig,SPMAW2abundance.sig)
