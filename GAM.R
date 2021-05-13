####BUILDING GAM#### Set working directory and library required packages ####
setwd("D:/MENHtrawl")

library(mgcv) # Run gam function to build general additive model (GAM)
library(RColorBrewer) # Run brewer.pal function to plot simulated abundance
library(classInt) # Run classIntervals function to plot simulated abundance
library(fields) # Run image.plot function to plot omega matrix
library(readxl)
library(rgdal)#reads shapefile
library(magrittr)
library(dplyr)
library(jpeg) #used to save jpeg files onto computer
library(mapdata) #used to plot maps
library(maps) #used to plot maps
library(spdep) #used to run Moran's I function
#### 1. Simulate lobster abundance by using GAM ####

GAM_data<- read_excel("D:/MENHtrawl/Cir data/DATAREDO2.xlsx") # Read Maine-New Hampshier Bottom Trawl Survey data. 
GAM_data<- na.omit(GAM_data)#Remove NAs
mainecoast= readOGR("D:/MENHTrawl/data/gis/ne_10m_coastline/ne_10m_coastline.shp") #coast shapefile I sourced onlince, but you could used any coastline shape file you want
####add year column####
GAM_data$Start_Date = as.Date(GAM_data$Start_Date, "%m/%d/%Y") #didn't end up using year as a variable

GAM_data$Year = as.numeric(format(GAM_data$Start_Date, "%Y"))
###########################GAM Season sex size###################
Falldata<-GAM_data[!grepl("SP", GAM_data$Survey),] #creating seperate data frames for each season, sex, and size lobster group
FLFJ<-Falldata[-c(1:2,4:13)]
FLMJ<-Falldata[-c(1:3,5:13)]
FLFA<-Falldata[-c(1:5,7:13)]
FLMA<-Falldata[-c(1:4,6:13)]
#######
#######
Springdata<-GAM_data[!grepl("FL",GAM_data$Survey),]
SPFJ<-Springdata[-c(1:2,4:13)]
SPMJ<-Springdata[-c(1:3,5:13)]
SPFA<-Springdata[-c(1:5,7:13)]
SPMA<-Springdata[-c(1:4,6:13)]
#######
FLFJ$catch<-FLFJ$Female_juvs
FLMJ$catch<-FLMJ$Male_juvs
FLFA$catch<-FLFA$Female_adults
FLMA$catch<-FLMA$Male_adults
SPFJ$catch<-SPFJ$Female_juvs
SPMJ$catch<-SPMJ$Male_juvs
SPFA$catch<-SPFA$Female_adults
SPMA$catch<-SPMA$Male_adults
#######

FLFJ<-FLFJ[!is.na(FLFJ$Bottom_WaterTempDegC) & !is.na(FLFJ$Bottom_Salinity_psu),]#Remove NAs
FLMJ<-FLMJ[!is.na(FLMJ$Bottom_WaterTempDegC) & !is.na(FLMJ$Bottom_Salinity_psu),]
FLFA<-FLFA[!is.na(FLFA$Bottom_WaterTempDegC) & !is.na(FLFA$Bottom_Salinity_psu),]
FLMA<-FLMA[!is.na(FLMA$Bottom_WaterTempDegC) & !is.na(FLMA$Bottom_Salinity_psu),]
SPFJ<-SPFJ[!is.na(SPFJ$Bottom_WaterTempDegC) & !is.na(SPFJ$Bottom_Salinity_psu),]
SPMJ<-SPMJ[!is.na(SPMJ$Bottom_WaterTempDegC) & !is.na(SPMJ$Bottom_Salinity_psu),]
SPFA<-SPFA[!is.na(SPFA$Bottom_WaterTempDegC) & !is.na(SPFA$Bottom_Salinity_psu),]
SPMA<-SPMA[!is.na(SPMA$Bottom_WaterTempDegC) & !is.na(SPMA$Bottom_Salinity_psu),]

###Drop outliars#### 
#only dropping from SPMJ because it was the only model to improve abundance predictions after dropping
SPFJ <- SPFJ[!(SPFJ$catch==274), ]
SPFJ <- SPFJ[!(SPFJ$catch==426), ]
## 1.1 Build GAM ######

####FLFJ##########
hist(FLFJ$catch,breaks=30)

# Abundance GAM#
FLFJabundance.full<-gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s
        (dist_frm_shore, k=5)+s(sediment, k=5), family=tw(),data=FLFJ) # Build second stage GAM with all possible habitat variables
summary(FLFJabundance.full) # Find significant variables based on p-value
FLFJabundance.full$aic
FLFJabundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)
        +s(dist_frm_shore, k=5)+s(sediment, k=5), family=tw(), data=FLFJ) # Build first stage GAM with significant habitat variables
summary(FLFJabundance.sig)
FLFJabundance.sig$aic
# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/Stationary 1-8/curveplots01.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(FLFJabundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=1.8,cex.axis=1.5)
plot(FLFJabundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Temperature)),cex.lab=1.8,cex.axis=1.5)
plot.new()
plot(FLFJabundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Distance~Offshore)),cex.lab=1.8,cex.axis=1.5)
plot(FLFJabundance.sig, select =4, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Sediment)),cex.lab=1.8,cex.axis=1.5)
dev.off()
## 1.2 Fit GAM #####
FLFJp.abundance=as.matrix(FLFJabundance.sig$fitted.values) # Abundance values of lobsters at current trawl survey stations
####FLMJ##########
hist(FLMJ$catch,breaks=30)
# Abundance GAM#
FLMJabundance.full<-gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(),data=FLMJ) # Build second stage GAM with all possible habitat variables
summary(FLMJabundance.full) # Find significant variables based on p-value
FLMJabundance.full$aic
FLMJabundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(), data=FLMJ) # Build first stage GAM with significant habitat variables
summary(FLMJabundance.sig)
FLMJabundance.sig$aic
# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/Stationary 1-8/curveplots02.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(FLMJabundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=1.8,cex.axis=1.5)
plot(FLMJabundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Temperature)),cex.lab=1.8,cex.axis=1.5)
plot(FLMJabundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Bottom~Salinity)),cex.lab=1.8,cex.axis=1.5)
plot(FLMJabundance.sig, select =4, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Distance~Offshore)),cex.lab=1.8,cex.axis=1.5)
plot(FLMJabundance.sig, select =5, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Sediment)),cex.lab=1.8,cex.axis=1.5)
dev.off()
## 1.2 Fit GAM #####
FLMJp.abundance=as.matrix(FLMJabundance.sig$fitted.values) # Abundance values of lobsters at current trawl survey stations
####FLFA##########
hist(FLFA$catch,breaks=30)
# Abundance GAM#
FLFAabundance.full<-gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(),data=FLFA) # Build second stage GAM with all possible habitat variables
summary(FLFAabundance.full) # Find significant variables based on p-value
FLFAabundance.full$aic
FLFAabundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(), data=FLFA) # Build first stage GAM with significant habitat variables
summary(FLFAabundance.sig)
FLFAabundance.sig$aic
# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/Stationary 1-8/curveplots03.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(FLFAabundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=1.8,cex.axis=1.5)
plot(FLFAabundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Temperature)),cex.lab=1.8,cex.axis=1.5)
plot.new()
plot(FLFAabundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Distance~Offshore)),cex.lab=1.8,cex.axis=1.5)
plot(FLFAabundance.sig, select =4, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Sediment)),cex.lab=1.8,cex.axis=1.5)
dev.off()
## 1.2 Fit GAM #####
FLFAp.abundance=as.matrix(FLFAabundance.sig$fitted.values) # Abundance values of lobsters at current trawl survey stations
####FLMA##########
hist(FLMA$catch,breaks=30)

# Abundance GAM#
FLMAabundance.full<-gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(),data=FLMA) # Build second stage GAM with all possible habitat variables
summary(FLMAabundance.full) # Find significant variables based on p-value
FLMAabundance.full$aic
FLMAabundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(), data=FLMA) # Build first stage GAM with significant habitat variables
summary(FLMAabundance.sig)
FLMAabundance.sig$aic
# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/Stationary 1-8/curveplots04.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(FLMAabundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=1.8,cex.axis=1.5)
plot(FLMAabundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Temperature)),cex.lab=1.8,cex.axis=1.5)
plot.new()
plot(FLMAabundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Distance~Offshore)),cex.lab=1.8,cex.axis=1.5)
plot(FLMAabundance.sig, select =4, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Sediment)),cex.lab=1.8,cex.axis=1.5)
dev.off()
## 1.2 Fit GAM #####
FLMAp.abundance=as.matrix(FLMAabundance.sig$fitted.values) # Abundance values of lobsters at current trawl survey stations

#####SPFJ########
hist(SPFJ$catch,breaks=30)

# Abundance GAM#
SPFJabundance.full<-gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(),data=SPFJ) # Build second stage GAM with all possible habitat variables
summary(SPFJabundance.full) # Find significant variables based on p-value

SPFJabundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(), data=SPFJ) # Build first stage GAM with significant habitat variables
summary(SPFJabundance.sig)


# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/Stationary 1-8/curveplots05.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(SPFJabundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=1.8,cex.axis=1.5)
plot(SPFJabundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Temperature)),cex.lab=1.8,cex.axis=1.5)
plot(SPFJabundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Bottom~Salinity)),cex.lab=1.8,cex.axis=1.5)
plot(SPFJabundance.sig, select =4, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Distance~Offshore)),cex.lab=1.8,cex.axis=1.5)
plot(SPFJabundance.sig, select =5, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Sediment)),cex.lab=1.8,cex.axis=1.5)
dev.off()
## 1.2 Fit GAM #####
SPFJp.abundance=as.matrix(SPFJabundance.sig$fitted.values) # Abundance values of lobsters at current trawl survey stations


#####SPMJ######
hist(SPMJ$catch,breaks=30)

# Abundance GAM#
SPMJabundance.full<-gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(),data=SPMJ) # Build second stage GAM with all possible habitat variables
summary(SPMJabundance.full) # Find significant variables based on p-value

SPMJabundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(), data=SPMJ) # Build first stage GAM with significant habitat variables
summary(SPMJabundance.sig)

# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/Stationary 1-8/curveplots06.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(SPMJabundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=1.8,cex.axis=1.5)
plot(SPMJabundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Temperature)),cex.lab=1.8,cex.axis=1.5)
plot(SPMJabundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Bottom~Salinity)),cex.lab=1.8,cex.axis=1.5)
plot(SPMJabundance.sig, select =4, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Distance~Offshore)),cex.lab=1.8,cex.axis=1.5)
plot(SPMJabundance.sig, select =5, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Sediment)),cex.lab=1.8,cex.axis=1.5)
dev.off()
## 1.2 Fit GAM #####
SPMJp.abundance=as.matrix(SPMJabundance.sig$fitted.values) # Abundance values of lobsters at current trawl survey stations

#####SPFA######
hist(SPFA$catch,breaks=30)
# Abundance GAM#
SPFAabundance.full<-gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(),data=SPFA) # Build second stage GAM with all possible habitat variables
summary(SPFAabundance.full) # Find significant variables based on p-value
SPFAabundance.full$aic
SPFAabundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(), data=SPFA) # Build first stage GAM with significant habitat variables
summary(SPFAabundance.sig)
SPFAabundance.sig$aic
# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/Stationary 1-8/curveplots07.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(SPFAabundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=1.8,cex.axis=1.5)
plot(SPFAabundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Temperature)),cex.lab=1.8,cex.axis=1.5)
plot(SPFAabundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Bottom~Salinity)),cex.lab=1.8,cex.axis=1.5)
plot(SPFAabundance.sig, select =4, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Distance~Offshore)),cex.lab=1.8,cex.axis=1.5)
plot(SPFAabundance.sig, select =5, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Sediment)),cex.lab=1.8,cex.axis=1.5)
dev.off()
## 1.2 Fit GAM #####
SPFAp.abundance=as.matrix(SPFAabundance.sig$fitted.values) # Abundance values of lobsters at current trawl survey stations

#####SPMA######
hist(SPMA$catch,breaks=30)

# Abundance GAM#
SPMAabundance.full<-gam(((catch)) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(),data=SPMA) # Build second stage GAM with all possible habitat variables
summary(SPMAabundance.full) # Find significant variables based on p-value

SPMAabundance.sig<-gam((catch) ~s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(), data=SPMA) # Build first stage GAM with significant habitat variables
summary(SPMAabundance.sig)

# Plot GAM response curves#####
jpeg("D:/MENHtrawl/Plots/curves/Stationary 1-8/curveplots08.jpeg",width = 350, height = 900)
par(mar=c(4,4,1,1))
layout(matrix(1:5, ncol=1, byrow=FALSE))
plot(SPMAabundance.sig, select =1, scale =0,ylab = expression(bold(Latitude)), xlab = expression(bold(Longitude)),scheme = 2,cex.lab=1.8,cex.axis=1.5)
plot(SPMAabundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Temperature)),cex.lab=1.8,cex.axis=1.5)
plot(SPMAabundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Bottom~Salinity)),cex.lab=1.8,cex.axis=1.5)
plot(SPMAabundance.sig, select =4, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Distance~Offshore)),cex.lab=1.8,cex.axis=1.5)
plot(SPMAabundance.sig, select =5, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Sediment)),cex.lab=1.8,cex.axis=1.5)
dev.off()
## 1.2 Fit GAM #####
SPMAp.abundance=as.matrix(SPMAabundance.sig$fitted.values) # Abundance values of lobsters at current trawl survey stations

######################################################
###Check Residuals####
checkresid<-function(groupabundance.sig,group){
        plot(groupabundance.sig$fitted.values, groupabundance.sig$residuals, type = "p")
        plot(group$Latitude, groupabundance.sig$residuals, type = "p")
        plot(group$Longitude, groupabundance.sig$residuals, type = "p")
        plot(group$Bottom_WaterTempDegC, groupabundance.sig$residuals, type = "p")
        plot(group$Bottom_Salinity_psu, groupabundance.sig$residuals, type = "p")
        plot(group$AvgDepth, groupabundance.sig$residuals, type = "p")
        plot(group$sediment, groupabundance.sig$residuals, type = "p")
        plot(group$Year, groupabundance.sig$residuals, type = "p")
        gam.check(groupabundance.sig,type="response")
        }

checkresid(FLFJabundance.sig,FLFJ)
checkresid(FLMJabundance.sig,FLMJ)
checkresid(FLFAabundance.sig,FLFA)
checkresid(FLMAabundance.sig,FLMA)
checkresid(SPFJabundance.sig,SPFJ)
checkresid(SPMJabundance.sig,SPMJ)
checkresid(SPFAabundance.sig,SPFA)
checkresid(SPMAabundance.sig,SPMA)

lmtest::bptest(FLFJabundance.sig)
lmtest::bptest(FLMJabundance.sig)
lmtest::bptest(FLFAabundance.sig)
lmtest::bptest(FLMAabundance.sig)
lmtest::bptest(SPFJabundance.sig)
lmtest::bptest(SPMJabundance.sig)
lmtest::bptest(SPFAabundance.sig)
lmtest::bptest(SPMAabundance.sig)
#####MAPPING STATIONARY########

FLFJ.d<-as.data.frame(cbind(FLFJ,FLFJp.abundance)) #adding abundance estimates to corresponding data frames
names(FLFJ.d)[11]<- "p.abundance"
FLMJ.d<-as.data.frame(cbind(FLMJ,FLMJp.abundance))
names(FLMJ.d)[11]<- "p.abundance"
FLFA.d<-as.data.frame(cbind(FLFA,FLFAp.abundance))
names(FLFA.d)[11]<- "p.abundance"
FLMA.d<-as.data.frame(cbind(FLMA,FLMAp.abundance))
names(FLMA.d)[11]<- "p.abundance"
SPFJ.d<-as.data.frame(cbind(SPFJ,SPFJp.abundance))
names(SPFJ.d)[11]<- "p.abundance"
SPMJ.d<-as.data.frame(cbind(SPMJ,SPMJp.abundance))
names(SPMJ.d)[11]<- "p.abundance"
SPFA.d<-as.data.frame(cbind(SPFA,SPFAp.abundance))
names(SPFA.d)[11]<- "p.abundance"
SPMA.d<-as.data.frame(cbind(SPMA,SPMAp.abundance))
names(SPMA.d)[11]<- "p.abundance"



##### 1.4 Plot simulated abundance at trawl locations#########
stationarysim<- function(group.d, size){
        start_x <- range(group.d$Longitude)[1]
        end_x <- range(group.d$Longitude)[2]
        start_y <- range(group.d$Latitude)[1]
        end_y <- range(group.d$Latitude)[2]
        colors <- brewer.pal(8, "YlOrRd")
        colbrks<-classIntervals(group.d$p.abundance, n=8, style="quantile")
        #,
                                #fixedBreaks=
                                        #if(size=="J")c(0,1,3,7,15,28,43,200,425) #breaks calculated by using average nonstationary "quantile" breaks
                                        #else c(0,13,26,42,65,93,118,155,502)
        #)
        brks<- colbrks$brks
        par(mfrow=c(1,1)); par(mar=c(4,4,1,1))
        plot(group.d$Longitude, group.d$Latitude, col=colors[findInterval(group.d$p.abundance, brks ,all.inside=TRUE)], cex=1.5,pch = 20, xlab="Longitude", ylab="Latitude")
        map(database = mainecoast, ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "lightgreen", fill = TRUE, add = TRUE)
        legend("topleft", "Stationary GOM Abundance", bty="n")
        
        
        colcode <- findColours(colbrks,colors)
        
        legend("bottomright", # position
               legend = names(attr(colcode, "table")), 
               title = "Abundance",
               fill = attr(colcode, "palette"),
               cex = 0.9,
               bty = "n") # border
}

stationarysim(FLFJ.d,"J")
stationarysim(FLMJ.d,"J")
stationarysim(FLFA.d,"A")
stationarysim(FLMA.d,"A")
stationarysim(SPFJ.d,"J")
stationarysim(SPMJ.d,"J")
stationarysim(SPFA.d,"A")
stationarysim(SPMA.d,"A")



##### 1.5 Plot abundance residuals at trawl locations#########
stationaryresids<- function(group.d, size){
        start_x <- range(group.d$Longitude)[1]
        end_x <- range(group.d$Longitude)[2]
        start_y <- range(group.d$Latitude)[1]
        end_y <- range(group.d$Latitude)[2]
        colors <- rev(brewer.pal(8,"RdBu"))
        colbrks<-classIntervals((group.d$p.abundance-group.d$catch), n=8,style="fixed", 
                                fixedBreaks=
                                        if(size=="J")c(-420,-16,-1,0,1,3,8,19,296) #breaks calculated by using average nonstationary "qauntile" breaks for each juv and adult
                                        else c(-685,-60,-20,0,6,16,29,56,435)
        )
        brks<- colbrks$brks
        par(mfrow=c(1,1)); par(mar=c(4,4,1,1))
        plot(group.d$Longitude, group.d$Latitude, col=colors[findInterval(group.d$p.abundance-group.d$catch, brks ,all.inside=TRUE)], pch = 20, xlab="Longitude", ylab="Latitude")
        map(database = mainecoast, ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "lightgreen", fill = TRUE, add = TRUE)
        legend("topleft", "Stationary Abundance Residuals", bty="n")
        
        
        colcode <- findColours(colbrks,colors)
        
        legend("bottomright", # position
               legend = names(attr(colcode, "table")), 
               title = "Abundance Residuals",
               fill = attr(colcode, "palette"),
               cex = 0.9,
               bty = "n") # border
}

stationaryresids(FLFJ.d, "J")
stationaryresids(FLMJ.d, "J")
stationaryresids(FLFA.d, "A")
stationaryresids(FLMA.d, "A")
stationaryresids(SPFJ.d, "J")
stationaryresids(SPMJ.d, "J")
stationaryresids(SPFA.d, "A")
stationaryresids(SPMA.d, "A")

######Calculate RMSE values####

RMSEfunction<- function(data){
        sqrt(mean(data^2))
}

RMSEfunction(FLFJabundance.sig$residuals)
RMSEfunction(FLMJabundance.sig$residuals)
RMSEfunction(FLFAabundance.sig$residuals)
RMSEfunction(FLMAabundance.sig$residuals)
RMSEfunction(SPFJabundance.sig$residuals)
RMSEfunction(SPMJabundance.sig$residuals)
RMSEfunction(SPFAabundance.sig$residuals)
RMSEfunction(SPMAabundance.sig$residuals)
###AIC Values#####
FLFAabundance.sig$aic
FLFJabundance.sig$aic
FLMAabundance.sig$aic
FLMJabundance.sig$aic
SPFAabundance.sig$aic
SPFJabundance.sig$aic
SPMAabundance.sig$aic
SPMJabundance.sig$aic

#### 1.6 Cross Validation: making the visual plots of 100 iterations ####
Crossvalidation<- function(group, gam_model_option, P, Title){
        
        group = group [!is.na(group$Bottom_WaterTempDegC) & !is.na(group$Bottom_Salinity_psu),]#Remove NAs
        
        N = nrow(group)
        P1= (1/(1+sqrt(P-1)))
        res = matrix(0, ncol = 3, nrow = 100)
        colnames(res) = c("Intercept", "Slope", "R.Squared")
        for (i in 1:100){          
                print(i)
                sub = sample(1:N, size = (N*P1))
                train = group[-sub,]
                test = group[sub,]
                
                if(gam_model_option==1) abundance_cv<- gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(), data=subset(train), select=TRUE)
                if(gam_model_option==2) abundance_cv<- gam((catch) ~ s(Longitude,Latitude, k=5)+s(Bottom_WaterTempDegC, k=5)+s(Bottom_Salinity_psu, k=5)+s(dist_frm_shore, k=5)+s(sediment, k=5) , family=tw(), data=subset(train), select=TRUE)
                
                predensity=predict.gam(abundance_cv, test, se.fit=TRUE, type = "response")
                result = cbind(test$catch, predensity$fit) 
                final = result[result[,1]>0 & result[,2]>0,]
                q = lm(final[,1] ~ final[,2], data = as.data.frame(final))
                res[i,] = c(summary(q)$coefficient[,1], summary(q)$r.squared)
        }
        summary(res)
        plot(NULL,xlim=c(0,max(group$catch)),ylim=c(0,max(group$catch)), xlab = expression(bold(" Number of Predicted Lobsters")), 
             ylab = expression(bold("Observed Lobsters")), xaxt="n", yaxt="n")
        axis(side=1, font=2)
        axis(side=2, font=2)
        addlines <- function(x) abline(x, col="gray70")
        apply(res[,-3],1,addlines)
        
        abline(a = mean(res[,1]), b = mean(res[,2]), col="black", lwd=3)
        abline(0,1,lty =2, lwd=3)
        #text(min(group$catch)+10, max(group$catch)-25,expesssion(bold(group,~"RSME ="~RMSEvalue)))
        legend("topleft", Title,bty="n")
}
par(mar=c(2,2,0,0), mfrow=c(2,4))
Crossvalidation(FLFJ, gam_model_option = 2, P=6,Title="FLFJ, RMSE=1.67")
Crossvalidation(FLMJ, gam_model_option = 2, P=6,Title="FLMJ, RMSE=1.67")
Crossvalidation(FLFA, gam_model_option = 2, P=6,Title="FLFA, RMSE=1.29")
Crossvalidation(FLMA, gam_model_option = 2, P=6,Title="FLMA, RMSE=1.24")
Crossvalidation(SPFA, gam_model_option = 2, P=6,Title="SPFJ, RMSE=1.68")
Crossvalidation(SPFJ, gam_model_option = 2, P=6,Title="SPMJ, RMSE=1.67")
Crossvalidation(SPMA, gam_model_option = 2, P=6,Title="SPFA, RMSE=1.49")
Crossvalidation(SPMJ, gam_model_option = 2, P=6,Title="SPMA, RMSE=1.41") 

###Moran's I prep####

SPFA$Latitude=ifelse(duplicated(SPFA$Latitude), SPFA$Latitude+0.00000001,SPFA$Latitude) #changing duplicate lat/long values tiny bit so Moran's I function can run
SPFA$Longitude=ifelse(duplicated(SPFA$Longitude), SPFA$Longitude+0.00000001,SPFA$Longitude)

SPFJ$Latitude=ifelse(duplicated(SPFJ$Latitude), SPFJ$Latitude+0.00000001,SPFJ$Latitude)
SPFJ$Longitude=ifelse(duplicated(SPFJ$Longitude), SPFJ$Longitude+0.00000001,SPFJ$Longitude)

SPMA$Latitude=ifelse(duplicated(SPMA$Latitude), SPMA$Latitude+0.00000001,SPMA$Latitude)
SPMA$Longitude=ifelse(duplicated(SPMA$Longitude), SPMA$Longitude+0.00000001,SPMA$Longitude)

SPMJ$Latitude=ifelse(duplicated(SPMJ$Latitude), SPMJ$Latitude+0.00000001,SPMJ$Latitude)
SPMJ$Longitude=ifelse(duplicated(SPMJ$Longitude), SPMJ$Longitude+0.00000001,SPMJ$Longitude)

FLFA$Latitude=ifelse(duplicated(FLFA$Latitude), FLFA$Latitude+0.00000001,FLFA$Latitude)
FLFA$Longitude=ifelse(duplicated(FLFA$Longitude), FLFA$Longitude+0.00000001,FLFA$Longitude)

FLMA$Latitude=ifelse(duplicated(FLMA$Latitude), FLMA$Latitude+0.00000001,FLMA$Latitude)
FLMA$Longitude=ifelse(duplicated(FLMA$Longitude), FLMA$Longitude+0.00000001,FLMA$Longitude)

FLFJ$Latitude=ifelse(duplicated(FLFJ$Latitude), FLFJ$Latitude+0.00000001,FLFJ$Latitude)
FLFJ$Longitude=ifelse(duplicated(FLFJ$Longitude), FLFJ$Longitude+0.00000001,FLFJ$Longitude)

FLMJ$Latitude=ifelse(duplicated(FLMJ$Latitude), FLMJ$Latitude+0.00000001,FLMJ$Latitude)
FLMJ$Longitude=ifelse(duplicated(FLMJ$Longitude), FLMJ$Longitude+0.00000001,FLMJ$Longitude)

### Moran's I fuction ####
moranI <- function(residuals, group){
        
        IDs<-row.names(as(group, "data.frame"))
        coords<- coordinates(select(group,Latitude,Longitude))
        temp_kn1 <- knn2nb(knearneigh(coords, k=5), row.names=IDs)
        temp_kn2_w <- nb2listw(temp_kn1)
        moran.test(group$catch, listw=temp_kn2_w)
        moran.plot(group$catch, temp_kn2_w)
        
        I <- moran.test(residuals, listw=temp_kn2_w)
        moran.plot(residuals, temp_kn2_w)
        return(list("Moran I estimate"=I$estimate[1], "Moran I pvalue"=I$p.value))
}
####IF you get an error while running this code below, make sure the package raster is unloaded
moranI(FLFJabundance.sig$residuals,FLFJ)
moranI(FLMJabundance.sig$residuals,FLMJ)
moranI(FLFAabundance.sig$residuals,FLFA)
moranI(FLMAabundance.sig$residuals,FLMA)
moranI(SPFJabundance.sig$residuals,SPFJ)###0 pvalue? failure to reject null hypothesis that data are randomly dispersed
moranI(SPMJabundance.sig$residuals,SPMJ)###0 pvalue?
moranI(SPFAabundance.sig$residuals,SPFA)
moranI(SPMAabundance.sig$residuals,SPMA)














