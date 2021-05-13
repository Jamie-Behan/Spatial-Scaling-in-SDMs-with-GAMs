#### Set working directory and library required packages ####
setwd("D:/MENHtrawl")

library(mgcv) # Run gam function to build general additive model (GAM)
library(RColorBrewer) # Run brewer.pal function to plot simulated abundance
library(classInt) # Run classIntervals function to plot simulated abundance
library(fields) # Run image.plot function to plot omega matrix
library(readxl)
library(magrittr)
library(dplyr)
library(maps) #for creating maps
library(maptools)#for creating maps
library(mapdata)#for creating maps
library(RColorBrewer)#for creating maps
library(classInt)#for creating maps
library(akima) #for creating raster data for interpolating
library(raster) #for creating raster data for interpolating

GAM_data<- read_excel("D:/MENHtrawl/Cir data/DATAREDO2.xlsx") # Read Maine-New Hampshier Bottom Trawl Survey data.
GAM_data<- na.omit(GAM_data)#Remove NAs
######
load("D://MENHtrawl/FVCOM_csv/TRD_SP00.RData")##load in temperature and salinity raster data
load("D://MENHtrawl/FVCOM_csv/TRD_FL00.RData")
load("D://MENHtrawl/FVCOM_csv/TRD_SP06.RData")
load("D://MENHtrawl/FVCOM_csv/TRD_FL06.RData")
load("D://MENHtrawl/FVCOM_csv/TRD_SP12.RData")
load("D://MENHtrawl/FVCOM_csv/TRD_FL12.RData")
load("D://MENHtrawl/FVCOM_csv/TRD_SP17.RData")
load("D://MENHtrawl/FVCOM_csv/TRD_FL17.RData")

load("D://MENHtrawl/FVCOM_csv/SRD_SP00.RData")
load("D://MENHtrawl/FVCOM_csv/SRD_FL00.RData")
load("D://MENHtrawl/FVCOM_csv/SRD_SP06.RData")
load("D://MENHtrawl/FVCOM_csv/SRD_FL06.RData")
load("D://MENHtrawl/FVCOM_csv/SRD_SP12.RData")
load("D://MENHtrawl/FVCOM_csv/SRD_FL12.RData")
load("D://MENHtrawl/FVCOM_csv/SRD_SP17.RData")
load("D://MENHtrawl/FVCOM_csv/SRD_FL17.RData")


library(popbio)

meantemprastersp00<- as.data.frame(list(Reduce(`+`, TRD_SP00) / length(TRD_SP00))) ###taking temperature averages of each day during the season at each location
meantemprasterfl00<- as.data.frame(list(Reduce(`+`, TRDFL00) / length(TRDFL00)))
meantemprastersp06<- as.data.frame(list(Reduce(`+`, TRDSP06) / length(TRDSP06)))
meantemprasterfl06<- as.data.frame(list(Reduce(`+`, TRDFL06) / length(TRDFL06)))
meantemprastersp12<- as.data.frame(list(Reduce(`+`, TRDSP12) / length(TRDSP12)))
meantemprasterfl12<- as.data.frame(list(Reduce(`+`, TRDFL12) / length(TRDFL12)))
meantemprastersp17<- as.data.frame(list(Reduce(`+`, TRD_SP17) / length(TRD_SP17)))
meantemprasterfl17<- as.data.frame(list(Reduce(`+`, TRD_FL17) / length(TRD_FL17)))

meansalrastersp00<- as.data.frame(list(Reduce(`+`, SRD_SP00) / length(SRD_SP00))) ###taking salinity averages of each day during the season at each location
meansalrasterfl00<- as.data.frame(list(Reduce(`+`, SRDFL00) / length(SRDFL00)))
meansalrastersp06<- as.data.frame(list(Reduce(`+`, SRDSP06) / length(SRDSP06)))
meansalrasterfl06<- as.data.frame(list(Reduce(`+`, SRDFL06) / length(SRDFL06)))
meansalrastersp12<- as.data.frame(list(Reduce(`+`, SRDSP12) / length(SRDSP12)))
meansalrasterfl12<- as.data.frame(list(Reduce(`+`, SRDFL12) / length(SRDFL12)))
meansalrastersp17<- as.data.frame(list(Reduce(`+`, SRD_SP17) / length(SRD_SP17)))
meansalrasterfl17<- as.data.frame(list(Reduce(`+`, SRD_FL17) / length(SRD_FL17)))

#### Load depth data and distance from shore data ####
depth_grid_plot000612 <- read.csv("D://MENHtrawl/grid_depth_data000612.csv") #Depth data for years 2000,2006, and 2012
depth_grid_plot2017 <- read.csv("D://MENHtrawl/grid_depth_data2017.csv")#depth data for 2017
load("D://MENHtrawl/hindcast_variables/DFS000612.RData")
load("D://MENHtrawl/hindcast_variables/DFS17.RData")

#####Combine temperature, salinity, latitude, longitude, distance from shore####
SP00<-cbind(depth_grid_plot000612,meantemprastersp00,meansalrastersp00,DFS000612)
SP00<-data.frame(SP00[c(2,3,6,7,8)])
names(SP00)[1]<-"Longitude"
names(SP00)[2]<-"Latitude"
names(SP00)[3]<-"Bottom_WaterTempDegC"
names(SP00)[4]<-"Bottom_Salinity_psu"
names(SP00)[5]<-"dist_frm_shore"
#save(SP00, file="D://MENHtrawl/hindcast_variables/SP00.RData")

FL00<-cbind(depth_grid_plot000612,meantemprasterfl00,meansalrasterfl00,DFS000612)
FL00<-data.frame(FL00[c(2,3,6,7,8)])
names(FL00)[1]<-"Longitude"
names(FL00)[2]<-"Latitude"
names(FL00)[3]<-"Bottom_WaterTempDegC"
names(FL00)[4]<-"Bottom_Salinity_psu"
names(FL00)[5]<-"dist_frm_shore"
#save(FL00, file="D://MENHtrawl/hindcast_variables/FL00.RData")

SP06<-cbind(depth_grid_plot000612,meantemprastersp06,meansalrastersp06,DFS000612)
SP06<-data.frame(SP06[c(2,3,6,7,8)])
names(SP06)[1]<-"Longitude"
names(SP06)[2]<-"Latitude"
names(SP06)[3]<-"Bottom_WaterTempDegC"
names(SP06)[4]<-"Bottom_Salinity_psu"
names(SP06)[5]<-"dist_frm_shore"
#save(SP06, file="D://MENHtrawl/hindcast_variables/SP06.RData")

FL06<-cbind(depth_grid_plot000612,meantemprasterfl06,meansalrasterfl06,DFS000612)
FL06<-data.frame(FL06[c(2,3,6,7,8)])
names(FL06)[1]<-"Longitude"
names(FL06)[2]<-"Latitude"
names(FL06)[3]<-"Bottom_WaterTempDegC"
names(FL06)[4]<-"Bottom_Salinity_psu"
names(FL06)[5]<-"dist_frm_shore"
#save(FL06, file="D://MENHtrawl/hindcast_variables/FL06.RData")

SP12<-cbind(depth_grid_plot000612,meantemprastersp12,meansalrastersp12,DFS000612)
SP12<-data.frame(SP12[c(2,3,6,7,8)])
names(SP12)[1]<-"Longitude"
names(SP12)[2]<-"Latitude"
names(SP12)[3]<-"Bottom_WaterTempDegC"
names(SP12)[4]<-"Bottom_Salinity_psu"
names(SP12)[5]<-"dist_frm_shore"
#save(SP12, file="D://MENHtrawl/hindcast_variables/SP12.RData")

FL12<-cbind(depth_grid_plot000612,meantemprasterfl12,meansalrasterfl12,DFS000612)
FL12<-data.frame(FL12[c(2,3,6,7,8)])
names(FL12)[1]<-"Longitude"
names(FL12)[2]<-"Latitude"
names(FL12)[3]<-"Bottom_WaterTempDegC"
names(FL12)[4]<-"Bottom_Salinity_psu"
names(FL12)[5]<-"dist_frm_shore"
#save(FL12, file="D://MENHtrawl/hindcast_variables/FL12.RData")

SP17<-cbind(depth_grid_plot2017,meantemprastersp17,meansalrastersp17,DFS17)
SP17<-data.frame(SP17[c(2,3,6,7,8)])
names(SP17)[1]<-"Longitude"
names(SP17)[2]<-"Latitude"
names(SP17)[3]<-"Bottom_WaterTempDegC"
names(SP17)[4]<-"Bottom_Salinity_psu"
names(SP17)[5]<-"dist_frm_shore"
#save(SP17, file="D://MENHtrawl/hindcast_variables/SP17.RData")

FL17<-cbind(depth_grid_plot2017,meantemprasterfl17,meansalrasterfl17,DFS17)
FL17<-data.frame(FL17[c(2,3,6,7,8)])
names(FL17)[1]<-"Longitude"
names(FL17)[2]<-"Latitude"
names(FL17)[3]<-"Bottom_WaterTempDegC"
names(FL17)[4]<-"Bottom_Salinity_psu"
names(FL17)[5]<-"dist_frm_shore"
#save(FL17, file="D://MENHtrawl/hindcast_variables/FL17.RData")


######get sediment data####
#sedpoints= read.csv("D:/MENHTrawl/SIE final project/sediment.csv")
#sedpointsdf=as.data.frame(sedpoints)

#sedimentRAST <- list()
#coordinates(sediment2017) <- ~Longitude + Latitude
#for(i in 1:1){
#  print(i)
#  temp_data <- sedpoints
#  rast_col <- ceiling((range(temp_data$LONGITUDE)[2]-range(temp_data$LONGITUDE)[1])/0.01)
#  rast_row <- ceiling((range(temp_data$LATITUDE)[2]-range(temp_data$LATITUDE)[1])/0.01)
#  akima.smooth <- with(temp_data, interp(LONGITUDE,LATITUDE, MEDIAN, nx=rast_col, ny=rast_row, duplicate="mean"))
#  rast <- raster(akima.smooth)
#  sedimentRAST[[i]] <- raster::extract(rast, sediment2017)
#}
#sediment2017<-as.data.frame(sedimentRAST[[1]])
#names(sediment2017)[1]<-"sediment"
#save(sediment2017, file="D://MENHtrawl/hindcast_variables/sediment2017.RData")
load("D://MENHtrawl/hindcast_variables/sediment0012.RData")
load("D://MENHtrawl/hindcast_variables/sediment2017.RData")

SP00<-cbind(SP00,sediment0012[1])
FL00<-cbind(FL00,sediment0012[1])
SP06<-cbind(SP06,sediment0012[1])
FL06<-cbind(FL06,sediment0012[1])
SP12<-cbind(SP12,sediment0012[1])
FL12<-cbind(FL12,sediment0012[1])
SP17<-cbind(SP17,sediment2017[1])
FL17<-cbind(FL17,sediment2017[1])

names(SP00)[6]<-"sediment"
names(FL00)[6]<-"sediment"
names(SP06)[6]<-"sediment"
names(FL06)[6]<-"sediment"
names(SP12)[6]<-"sediment"
names(FL12)[6]<-"sediment"
names(SP17)[6]<-"sediment"
names(FL17)[6]<-"sediment"

load("D://MENHtrawl/FVCOM_csv/FVdepth.RData")
colnames(FVdepth)<-c("Longitude","Latitude","AvgDepth")
colnames(FVdepth2017)<-c("Longitude","Latitude","AvgDepth")


SP00<-merge(SP00,FVdepth,by=c("Longitude","Latitude"))
FL00<-merge(FL00,FVdepth,by=c("Longitude","Latitude"))
SP06<-merge(SP06,FVdepth,by=c("Longitude","Latitude"))
FL06<-merge(FL06,FVdepth,by=c("Longitude","Latitude"))
SP12<-merge(SP12,FVdepth,by=c("Longitude","Latitude"))
FL12<-merge(FL12,FVdepth,by=c("Longitude","Latitude"))
SP17<-merge(SP17,FVdepth2017,by=c("Longitude","Latitude"))
FL17<-merge(FL17,FVdepth2017,by=c("Longitude","Latitude"))


#save(SP00, file="D://MENHtrawl/hindcast_variables/SP00.RData")
#save(FL00, file="D://MENHtrawl/hindcast_variables/FL00.RData")
#save(SP06, file="D://MENHtrawl/hindcast_variables/SP06.RData")
#save(FL06, file="D://MENHtrawl/hindcast_variables/FL06.RData")
#save(SP12, file="D://MENHtrawl/hindcast_variables/SP12.RData")
#save(FL12, file="D://MENHtrawl/hindcast_variables/FL12.RData")
#save(SP17, file="D://MENHtrawl/hindcast_variables/SP17.RData")
#save(FL17, file="D://MENHtrawl/hindcast_variables/FL17.RData")
#####
####once finished with top section and CSVs are saved, you can START HERE and just load them in for future run throughs of this code############################################################################################
#### Set working directory and library required packages ####
setwd("D:/MENHtrawl")

library(mgcv) # Run gam function to build general additive model (GAM)
library(RColorBrewer) # Run brewer.pal function to plot simulated abundance
library(classInt) # Run classIntervals function to plot simulated abundance
library(fields) # Run image.plot function to plot omega matrix
library(readxl)
library(magrittr)
library(dplyr)
library(maps) #for creating maps
library(maptools)#for creating maps
library(mapdata)#for creating maps
library(RColorBrewer)#for creating maps
library(classInt)#for creating maps
library(akima) #for creating raster data for interpolating
library(raster) #for creating raster data for interpolating


GAM_data<- read_excel("D:/MENHtrawl/Cir data/DATAREDO2.xlsx") # Read Maine-New Hampshier Bottom Trawl Survey data.
GAM_data<- na.omit(GAM_data)#Remove NAs
#####Load in needed data####
depth_grid_plot000612 <- read.csv("D://MENHtrawl/grid_depth_data000612.csv") #Depth data for years 2000,2006, and 2012
depth_grid_plot2017 <- read.csv("D://MENHtrawl/grid_depth_data2017.csv")#depth data for 2017

load("D://MENHtrawl/hindcast_variables/SP00.RData")
load("D://MENHtrawl/hindcast_variables/FL00.RData")
load("D://MENHtrawl/hindcast_variables/SP06.RData")
load("D://MENHtrawl/hindcast_variables/FL06.RData")
load("D://MENHtrawl/hindcast_variables/SP12.RData")
load("D://MENHtrawl/hindcast_variables/FL12.RData")
load("D://MENHtrawl/hindcast_variables/SP17.RData")
load("D://MENHtrawl/hindcast_variables/FL17.RData")


source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}

source_lines("D://MENHtrawl/GAM.R",1:245)
source_lines("D://MENHtrawl/GAM nonstationary.R", 1:481)
source_lines("D://MENHtrawl/GAM nonstationary EMW.R", 1:758)

library(Jmisc)
###Adding year column
FL00<-addCol(FL00, Year=2000)
SP00<-addCol(SP00, Year=2000)
FL06<-addCol(FL06, Year=2006)
SP06<-addCol(SP06, Year=2006)
FL12<-addCol(FL12, Year=2012)
SP12<-addCol(SP12, Year=2012)
FL17<-addCol(FL17, Year=2017)
SP17<-addCol(SP17, Year=2017)

FL00$Year<-as.numeric(as.character(FL00$Year))
SP00$Year<-as.numeric(as.character(SP00$Year))
FL06$Year<-as.numeric(as.character(FL06$Year))
SP06$Year<-as.numeric(as.character(SP06$Year))
FL12$Year<-as.numeric(as.character(FL12$Year))
SP12$Year<-as.numeric(as.character(SP12$Year))
FL17$Year<-as.numeric(as.character(FL17$Year))
SP17$Year<-as.numeric(as.character(SP17$Year))

cleanup<- function(groupfull){
  groupfull<-groupfull[!rowSums(groupfull[2] <42.8),]#cleaning up the data to only contain rows in the GOM
  groupfull<-groupfull[!rowSums(groupfull[2] >44.9),]
  groupfull<-groupfull[!rowSums(groupfull[1] <"-66.9"),]
  groupfull<-groupfull[!rowSums(groupfull[1] >"-70.8"),]
}

SP00<-cleanup(SP00)
FL00<-cleanup(FL00)
SP06<-cleanup(SP06)
FL06<-cleanup(FL06)
SP12<-cleanup(SP12)
FL12<-cleanup(FL12)
SP17<-cleanup(SP17)
FL17<-cleanup(FL17)

SP00<-SP00[complete.cases(SP00), ]
FL00<-FL00[complete.cases(FL00), ]
SP06<-SP06[complete.cases(SP06), ]
FL06<-FL06[complete.cases(FL06), ]
SP12<-SP12[complete.cases(SP12), ]
FL12<-FL12[complete.cases(FL12), ]
SP17<-SP17[complete.cases(SP17), ]
FL17<-FL17[complete.cases(FL17), ]


####12 Miles offhsore####

###assuming the earth is round,
#One radian is the angle for a line 3,958.761 miles long, so the angle for a line 12 miles long
#is 12/3,958.761 radians. 0.003031rad × 180/p = 0.1737°.

Miles12<- function(groupfull){
  groupfull<-groupfull[!rowSums(groupfull[5] >0.4776236),]#cleaning up the data to only contain rows up to 12miles offshore
}

SP00<-Miles12(SP00)
FL00<-Miles12(FL00)
SP06<-Miles12(SP06)
FL06<-Miles12(FL06)
SP12<-Miles12(SP12)
FL12<-Miles12(FL12)
SP17<-Miles12(SP17)
FL17<-Miles12(FL17)

####                                                  ####
########HINDCAST STATIONARY INTERPOLATED PLOTS########
#source("D:/MENHtrawl/GAM.R")

PredictGAM<- function(groupabundance.sig,group){
  
  predict.gam(groupabundance.sig, newdata=group, type="response")
}
FLFJ2000p.abundance<-PredictGAM(FLFJabundance.sig,FL00)
FLMJ2000p.abundance<-PredictGAM(FLMJabundance.sig,FL00)
FLFA2000p.abundance<-PredictGAM(FLFAabundance.sig,FL00)
FLMA2000p.abundance<-PredictGAM(FLMAabundance.sig,FL00)
SPFJ2000p.abundance<-PredictGAM(SPFJabundance.sig,SP00)
SPMJ2000p.abundance<-PredictGAM(SPMJabundance.sig,SP00)
SPFA2000p.abundance<-PredictGAM(SPFAabundance.sig,SP00)
SPMA2000p.abundance<-PredictGAM(SPMAabundance.sig,SP00)

FLFJ2006p.abundance<-PredictGAM(FLFJabundance.sig,FL06)
FLMJ2006p.abundance<-PredictGAM(FLMJabundance.sig,FL06)
FLFA2006p.abundance<-PredictGAM(FLFAabundance.sig,FL06)
FLMA2006p.abundance<-PredictGAM(FLMAabundance.sig,FL06)
SPFJ2006p.abundance<-PredictGAM(SPFJabundance.sig,SP06)
SPMJ2006p.abundance<-PredictGAM(SPMJabundance.sig,SP06)
SPFA2006p.abundance<-PredictGAM(SPFAabundance.sig,SP06)
SPMA2006p.abundance<-PredictGAM(SPMAabundance.sig,SP06)

FLFJ2012p.abundance<-PredictGAM(FLFJabundance.sig,FL12)
FLMJ2012p.abundance<-PredictGAM(FLMJabundance.sig,FL12)
FLFA2012p.abundance<-PredictGAM(FLFAabundance.sig,FL12)
FLMA2012p.abundance<-PredictGAM(FLMAabundance.sig,FL12)
SPFJ2012p.abundance<-PredictGAM(SPFJabundance.sig,SP12)
SPMJ2012p.abundance<-PredictGAM(SPMJabundance.sig,SP12)
SPFA2012p.abundance<-PredictGAM(SPFAabundance.sig,SP12)
SPMA2012p.abundance<-PredictGAM(SPMAabundance.sig,SP12)

namechange<- function(hindyear, groupp.abundance){
  
  hindyear1<-data.frame(hindyear,groupp.abundance)
  names(hindyear1)[8]<- "p.abundance"
  return(hindyear1)
}
FLFJ2000<-namechange(FL00,FLFJ2000p.abundance)
FLMJ2000<-namechange(FL00,FLMJ2000p.abundance)
FLFA2000<-namechange(FL00,FLFA2000p.abundance)
FLMA2000<-namechange(FL00,FLMA2000p.abundance)

SPFJ2000<-namechange(SP00,SPFJ2000p.abundance)
SPMJ2000<-namechange(SP00,SPMJ2000p.abundance)
SPFA2000<-namechange(SP00,SPFA2000p.abundance)
SPMA2000<-namechange(SP00,SPMA2000p.abundance)
###2006###
FLFJ2006<-namechange(FL06,FLFJ2006p.abundance)
FLMJ2006<-namechange(FL06,FLMJ2006p.abundance)
FLFA2006<-namechange(FL06,FLFA2006p.abundance)
FLMA2006<-namechange(FL06,FLMA2006p.abundance)

SPFJ2006<-namechange(SP06,SPFJ2006p.abundance)
SPMJ2006<-namechange(SP06,SPMJ2006p.abundance)
SPFA2006<-namechange(SP06,SPFA2006p.abundance)
SPMA2006<-namechange(SP06,SPMA2006p.abundance)
###2012###
FLFJ2012<-namechange(FL12,FLFJ2012p.abundance)
FLMJ2012<-namechange(FL12,FLMJ2012p.abundance)
FLFA2012<-namechange(FL12,FLFA2012p.abundance)
FLMA2012<-namechange(FL12,FLMA2012p.abundance)

SPFJ2012<-namechange(SP12,SPFJ2012p.abundance)
SPMJ2012<-namechange(SP12,SPMJ2012p.abundance)
SPFA2012<-namechange(SP12,SPFA2012p.abundance)
SPMA2012<-namechange(SP12,SPMA2012p.abundance)

###get raster data 2000-2012####

FLFJRAST00 <- list()
coordinates(depth_grid_plot000612) <- ~lon + lat
for(i in 1:1){
  print(i)
  temp_data <- FLFJ2000
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFJRAST00[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMJRAST00 <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMJ2000
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMJRAST00[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLFARAST00 <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFA2000
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFARAST00[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMARAST00 <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMA2000
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMARAST00[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFJRAST00 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFJ2000
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFJRAST00[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMJRAST00 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMJ2000
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMJRAST00[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFARAST00 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFA2000
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFARAST00[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMARAST00 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMA2000
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMARAST00[[i]] <- raster::extract(rast, depth_grid_plot000612)
}
###2006####
FLFJRAST06 <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFJ2006
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFJRAST06[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMJRAST06 <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMJ2006
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMJRAST06[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLFARAST06 <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFA2006
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFARAST06[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMARAST06 <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMA2006
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMARAST06[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFJRAST06 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFJ2006
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFJRAST06[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMJRAST06 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMJ2006
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMJRAST06[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFARAST06 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFA2006
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFARAST06[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMARAST06 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMA2006
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMARAST06[[i]] <- raster::extract(rast, depth_grid_plot000612)
}
###2012####
FLFJRAST12 <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFJ2012
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFJRAST12[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMJRAST12 <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMJ2012
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMJRAST12[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLFARAST12 <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFA2012
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFARAST12[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMARAST12 <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMA2012
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMARAST12[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFJRAST12 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFJ2012
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFJRAST12[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMJRAST12 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMJ2012
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMJRAST12[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFARAST12 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFA2012
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFARAST12[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMARAST12 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMA2012
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMARAST12[[i]] <- raster::extract(rast, depth_grid_plot000612)
}



#####
library(rgdal)
library(PBSmapping)
mainecoast= readOGR("D:/MENHTrawl/data/gis/ne_10m_coastline/ne_10m_coastline.shp")
plotS<-function(rasterdata, month,year,size){
  par(mar=c(2,2,0,0), mfrow=c(1,1))
  plotvar=unlist(rasterdata)
  nclr=9
  plotclr <- (brewer.pal(nclr,"YlOrRd"))
  class <- classIntervals(plotvar, nclr, style="quantile")
  fix_break<-round(class$brks, digits = 2)
  for(i in 1:length(rasterdata)){
    print(i)
    plotvar <- rasterdata[[i]]
    nclr=9
    plotclr <- (brewer.pal(nclr,"YlOrRd"))
    class <- classIntervals(plotvar, nclr, style="fixed", 
                            fixedBreaks=
                              if(size=="J")c(0,0.5,3,10,25,50,200,600) #breaks calculated by using average raw data "quantile" breaks
                            else c(0,5,15,40,65,85,115,160,250,900)) #breaks calculated by using average "qauntile" breaks
    
    colcode <- findColours(class, plotclr)
    
    start_x <- range(GAM_data$Longitude)[1]
    end_x <- range(GAM_data$Longitude)[2]
    start_y <- range(GAM_data$Latitude)[1]
    end_y <- range(GAM_data$Latitude)[2]
    plot(depth_grid_plot000612$lon, depth_grid_plot000612$lat, pch=16, col=colcode, cex=0.1, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, xlab="Longitude", ylab="Latitude")
    map(database = mainecoast, ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE, lwd=0.1)
    #plot(sa511_513, add=T)
    box()
    legend("topleft", paste(year, "-", month, sep=""), bty="n", cex=1.5)
    legend("bottomright", legend=c(names(attr(colcode, "table"))), fill=c(attr(colcode, "palette")), cex=0.9,bg="white",
           bty = "o", title="Stationary Abundance")
    axis(1, cex=0.5)
    axis(2, cex=0.5)
  }
}
#par(mar=c(2,2,0,0), mfrow=c(1,4))
plotS(FLFJRAST00,"FLFJ","2000","J")
plotS(FLMJRAST00,"FLMJ","2000","J")
plotS(FLFARAST00,"FLFA","2000","A")
plotS(FLMARAST00,"FLMA","2000","A")
plotS(SPFJRAST00,"SPFJ","2000","J")
plotS(SPMJRAST00,"SPMJ","2000","J")
plotS(SPFARAST00,"SPFA","2000","A")
plotS(SPMARAST00,"SPMA","2000","A")

plotS(FLFJRAST06,"FLFJ","2006","J")
plotS(FLMJRAST06,"FLMJ","2006","J")
plotS(FLFARAST06,"FLFA","2006","A")
plotS(FLMARAST06,"FLMA","2006","A")
plotS(SPFJRAST06,"SPFJ","2006","J")
plotS(SPMJRAST06,"SPMJ","2006","J")
plotS(SPFARAST06,"SPFA","2006","A")
plotS(SPMARAST06,"SPMA","2006","A")

plotS(FLFJRAST12,"FLFJ","2012","J")
plotS(FLMJRAST12,"FLMJ","2012","J")
plotS(FLFARAST12,"FLFA","2012","A")
plotS(FLMARAST12,"FLMA","2012","A")
plotS(SPFJRAST12,"SPFJ","2012","J")
plotS(SPMJRAST12,"SPMJ","2012","J")
plotS(SPFARAST12,"SPFA","2012","A")
plotS(SPMARAST12,"SPMA","2012","A")

### get predictd min/max values####
library(huxtable)
minmaxrast00 <- hux(
  Group = c('FLFJ', 'FLMJ', 'FLFA','FLMA','SPFJ', 'SPMJ', 'SPFA','SPMA'),
  Max = c(max(na.omit(as.data.frame(FLFJRAST00[[1]]))), max(na.omit(as.data.frame(FLMJRAST00[[1]]))), 
          max(na.omit(as.data.frame(FLFARAST00[[1]]))),
          max(na.omit(as.data.frame(FLMARAST00[[1]]))),max(na.omit(as.data.frame(SPFJRAST00[[1]]))), 
          max(na.omit(as.data.frame(SPMJRAST00[[1]]))), max(na.omit(as.data.frame(SPFARAST00[[1]]))),
          max(na.omit(as.data.frame(SPMARAST00[[1]])))),
  Min = c(min(na.omit(as.data.frame(FLFJRAST00[[1]]))), min(na.omit(as.data.frame(FLMJRAST00[[1]]))), 
          min(na.omit(as.data.frame(FLFARAST00[[1]]))),
          min(na.omit(as.data.frame(FLMARAST00[[1]]))),min(na.omit(as.data.frame(SPFJRAST00[[1]]))), 
          min(na.omit(as.data.frame(SPMJRAST00[[1]]))), min(na.omit(as.data.frame(SPFARAST00[[1]]))),
          min(na.omit(as.data.frame(SPMARAST00[[1]])))),add_colnames = TRUE)

bold(minmaxrast00)[1,]           <- TRUE
bottom_border(minmaxrast00)[1,]  <- 0.4
align(minmaxrast00)[,2]          <- 'right'
right_padding(minmaxrast00)      <- 10
left_padding(minmaxrast00)       <- 10
width(minmaxrast00)              <- 0.30
number_format(minmaxrast00)      <- 2

minmaxrast00
####                                                   ####
########HINDCAST NONSTATIONARY V1. INTERPOLATED PLOTS (East/west)############
PredictGAM<- function(groupabundance.sig,group){
  
  predict.gam(groupabundance.sig, newdata=group, type="response")
}
###EAST####
FLFJE2000p.abundance<-PredictGAM(FLFJEabundance.sig,FL00)
FLMJE2000p.abundance<-PredictGAM(FLMJEabundance.sig,FL00)
FLFAE2000p.abundance<-PredictGAM(FLFAEabundance.sig,FL00)
FLMAE2000p.abundance<-PredictGAM(FLMAEabundance.sig,FL00)
SPFJE2000p.abundance<-PredictGAM(SPFJEabundance.sig,SP00)
SPMJE2000p.abundance<-PredictGAM(SPMJEabundance.sig,SP00)
SPFAE2000p.abundance<-PredictGAM(SPFAEabundance.sig,SP00)
SPMAE2000p.abundance<-PredictGAM(SPMAEabundance.sig,SP00)

FLFJE2006p.abundance<-PredictGAM(FLFJEabundance.sig,FL06)
FLMJE2006p.abundance<-PredictGAM(FLMJEabundance.sig,FL06)
FLFAE2006p.abundance<-PredictGAM(FLFAEabundance.sig,FL06)
FLMAE2006p.abundance<-PredictGAM(FLMAEabundance.sig,FL06)
SPFJE2006p.abundance<-PredictGAM(SPFJEabundance.sig,SP06)
SPMJE2006p.abundance<-PredictGAM(SPMJEabundance.sig,SP06)
SPFAE2006p.abundance<-PredictGAM(SPFAEabundance.sig,SP06)
SPMAE2006p.abundance<-PredictGAM(SPMAEabundance.sig,SP06)

FLFJE2012p.abundance<-PredictGAM(FLFJEabundance.sig,FL12)
FLMJE2012p.abundance<-PredictGAM(FLMJEabundance.sig,FL12)
FLFAE2012p.abundance<-PredictGAM(FLFAEabundance.sig,FL12)
FLMAE2012p.abundance<-PredictGAM(FLMAEabundance.sig,FL12)
SPFJE2012p.abundance<-PredictGAM(SPFJEabundance.sig,SP12)
SPMJE2012p.abundance<-PredictGAM(SPMJEabundance.sig,SP12)
SPFAE2012p.abundance<-PredictGAM(SPFAEabundance.sig,SP12)
SPMAE2012p.abundance<-PredictGAM(SPMAEabundance.sig,SP12)

###WEST####
FLFJW2000p.abundance<-PredictGAM(FLFJWabundance.sig,FL00)
FLMJW2000p.abundance<-PredictGAM(FLMJWabundance.sig,FL00)
FLFAW2000p.abundance<-PredictGAM(FLFAWabundance.sig,FL00)
FLMAW2000p.abundance<-PredictGAM(FLMAWabundance.sig,FL00)
SPFJW2000p.abundance<-PredictGAM(SPFJWabundance.sig,SP00)
SPMJW2000p.abundance<-PredictGAM(SPMJWabundance.sig,SP00)
SPFAW2000p.abundance<-PredictGAM(SPFAWabundance.sig,SP00)
SPMAW2000p.abundance<-PredictGAM(SPMAWabundance.sig,SP00)

FLFJW2006p.abundance<-PredictGAM(FLFJWabundance.sig,FL06)
FLMJW2006p.abundance<-PredictGAM(FLMJWabundance.sig,FL06)
FLFAW2006p.abundance<-PredictGAM(FLFAWabundance.sig,FL06)
FLMAW2006p.abundance<-PredictGAM(FLMAWabundance.sig,FL06)
SPFJW2006p.abundance<-PredictGAM(SPFJWabundance.sig,SP06)
SPMJW2006p.abundance<-PredictGAM(SPMJWabundance.sig,SP06)
SPFAW2006p.abundance<-PredictGAM(SPFAWabundance.sig,SP06)
SPMAW2006p.abundance<-PredictGAM(SPMAWabundance.sig,SP06)

FLFJW2012p.abundance<-PredictGAM(FLFJWabundance.sig,FL12)
FLMJW2012p.abundance<-PredictGAM(FLMJWabundance.sig,FL12)
FLFAW2012p.abundance<-PredictGAM(FLFAWabundance.sig,FL12)
FLMAW2012p.abundance<-PredictGAM(FLMAWabundance.sig,FL12)
SPFJW2012p.abundance<-PredictGAM(SPFJWabundance.sig,SP12)
SPMJW2012p.abundance<-PredictGAM(SPMJWabundance.sig,SP12)
SPFAW2012p.abundance<-PredictGAM(SPFAWabundance.sig,SP12)
SPMAW2012p.abundance<-PredictGAM(SPMAWabundance.sig,SP12)
###Namechange####
namechange<- function(hindyear, groupp.abundance){
  
  hindyear<-data.frame(hindyear,groupp.abundance)
  names(hindyear)[8]<- "p.abundance"
  return(hindyear)
}
FLFJE2000<-namechange(FL00,FLFJE2000p.abundance)
FLMJE2000<-namechange(FL00,FLMJE2000p.abundance)
FLFAE2000<-namechange(FL00,FLFAE2000p.abundance)
FLMAE2000<-namechange(FL00,FLMAE2000p.abundance)

SPFJE2000<-namechange(SP00,SPFJE2000p.abundance)
SPMJE2000<-namechange(SP00,SPMJE2000p.abundance)
SPFAE2000<-namechange(SP00,SPFAE2000p.abundance)
SPMAE2000<-namechange(SP00,SPMAE2000p.abundance)
###2006###
FLFJE2006<-namechange(FL06,FLFJE2006p.abundance)
FLMJE2006<-namechange(FL06,FLMJE2006p.abundance)
FLFAE2006<-namechange(FL06,FLFAE2006p.abundance)
FLMAE2006<-namechange(FL06,FLMAE2006p.abundance)

SPFJE2006<-namechange(SP06,SPFJE2006p.abundance)
SPMJE2006<-namechange(SP06,SPMJE2006p.abundance)
SPFAE2006<-namechange(SP06,SPFAE2006p.abundance)
SPMAE2006<-namechange(SP06,SPMAE2006p.abundance)
###2012###
FLFJE2012<-namechange(FL12,FLFJE2012p.abundance)
FLMJE2012<-namechange(FL12,FLMJE2012p.abundance)
FLFAE2012<-namechange(FL12,FLFAE2012p.abundance)
FLMAE2012<-namechange(FL12,FLMAE2012p.abundance)

SPFJE2012<-namechange(SP12,SPFJE2012p.abundance)
SPMJE2012<-namechange(SP12,SPMJE2012p.abundance)
SPFAE2012<-namechange(SP12,SPFAE2012p.abundance)
SPMAE2012<-namechange(SP12,SPMAE2012p.abundance)

###2000###
FLFJW2000<-namechange(FL00,FLFJW2000p.abundance)
FLMJW2000<-namechange(FL00,FLMJW2000p.abundance)
FLFAW2000<-namechange(FL00,FLFAW2000p.abundance)
FLMAW2000<-namechange(FL00,FLMAW2000p.abundance)

SPFJW2000<-namechange(SP00,SPFJW2000p.abundance)
SPMJW2000<-namechange(SP00,SPMJW2000p.abundance)
SPFAW2000<-namechange(SP00,SPFAW2000p.abundance)
SPMAW2000<-namechange(SP00,SPMAW2000p.abundance)
###2006###
FLFJW2006<-namechange(FL06,FLFJW2006p.abundance)
FLMJW2006<-namechange(FL06,FLMJW2006p.abundance)
FLFAW2006<-namechange(FL06,FLFAW2006p.abundance)
FLMAW2006<-namechange(FL06,FLMAW2006p.abundance)

SPFJW2006<-namechange(SP06,SPFJW2006p.abundance)
SPMJW2006<-namechange(SP06,SPMJW2006p.abundance)
SPFAW2006<-namechange(SP06,SPFAW2006p.abundance)
SPMAW2006<-namechange(SP06,SPMAW2006p.abundance)
###2012###
FLFJW2012<-namechange(FL12,FLFJW2012p.abundance)
FLMJW2012<-namechange(FL12,FLMJW2012p.abundance)
FLFAW2012<-namechange(FL12,FLFAW2012p.abundance)
FLMAW2012<-namechange(FL12,FLMAW2012p.abundance)

SPFJW2012<-namechange(SP12,SPFJW2012p.abundance)
SPMJW2012<-namechange(SP12,SPMJW2012p.abundance)
SPFAW2012<-namechange(SP12,SPFAW2012p.abundance)
SPMAW2012<-namechange(SP12,SPMAW2012p.abundance)
###Cleanup####
cleanupE<- function(groupfull){
  groupfull<-groupfull[!rowSums(groupfull[2] <42.8),]#cleaning up the data to only contain rows in the GOM
  groupfull<-groupfull[!rowSums(groupfull[2] >44.9),]
  groupfull<-groupfull[!rowSums(groupfull[1] <"-66.9"),]
  groupfull<-groupfull[!rowSums(groupfull[1] >"-69.2612"),]
}

cleanupW<- function(groupfull){
  groupfull<-groupfull[!rowSums(groupfull[2] <42.8),]#cleaning up the data to only contain rows in the GOM
  groupfull<-groupfull[!rowSums(groupfull[2] >44.9),]
  groupfull<-groupfull[!rowSums(groupfull[1] <"-69.2612"),]
  groupfull<-groupfull[!rowSums(groupfull[1] >"-70.8"),]
}
###cleanup east####
FLFJE2000<-cleanupE(FLFJE2000)
FLMJE2000<-cleanupE(FLMJE2000)
FLFAE2000<-cleanupE(FLFAE2000)
FLMAE2000<-cleanupE(FLMAE2000)

SPFJE2000<-cleanupE(SPFJE2000)
SPMJE2000<-cleanupE(SPMJE2000)
SPFAE2000<-cleanupE(SPFAE2000)
SPMAE2000<-cleanupE(SPMAE2000)
###2006###
FLFJE2006<-cleanupE(FLFJE2006)
FLMJE2006<-cleanupE(FLMJE2006)
FLFAE2006<-cleanupE(FLFAE2006)
FLMAE2006<-cleanupE(FLMAE2006)

SPFJE2006<-cleanupE(SPFJE2006)
SPMJE2006<-cleanupE(SPMJE2006)
SPFAE2006<-cleanupE(SPFAE2006)
SPMAE2006<-cleanupE(SPMAE2006)
###2012###
FLFJE2012<-cleanupE(FLFJE2012)
FLMJE2012<-cleanupE(FLMJE2012)
FLFAE2012<-cleanupE(FLFAE2012)
FLMAE2012<-cleanupE(FLMAE2012)

SPFJE2012<-cleanupE(SPFJE2012)
SPMJE2012<-cleanupE(SPMJE2012)
SPFAE2012<-cleanupE(SPFAE2012)
SPMAE2012<-cleanupE(SPMAE2012)

###Cleanup WEST####
FLFJW2000<-cleanupW(FLFJW2000)
FLMJW2000<-cleanupW(FLMJW2000)
FLFAW2000<-cleanupW(FLFAW2000)
FLMAW2000<-cleanupW(FLMAW2000)

SPFJW2000<-cleanupW(SPFJW2000)
SPMJW2000<-cleanupW(SPMJW2000)
SPFAW2000<-cleanupW(SPFAW2000)
SPMAW2000<-cleanupW(SPMAW2000)
###2006###
FLFJW2006<-cleanupW(FLFJW2006)
FLMJW2006<-cleanupW(FLMJW2006)
FLFAW2006<-cleanupW(FLFAW2006)
FLMAW2006<-cleanupW(FLMAW2006)

SPFJW2006<-cleanupW(SPFJW2006)
SPMJW2006<-cleanupW(SPMJW2006)
SPFAW2006<-cleanupW(SPFAW2006)
SPMAW2006<-cleanupW(SPMAW2006)
###2012###
FLFJW2012<-cleanupW(FLFJW2012)
FLMJW2012<-cleanupW(FLMJW2012)
FLFAW2012<-cleanupW(FLFAW2012)
FLMAW2012<-cleanupW(FLMAW2012)

SPFJW2012<-cleanupW(SPFJW2012)
SPMJW2012<-cleanupW(SPMJW2012)
SPFAW2012<-cleanupW(SPFAW2012)
SPMAW2012<-cleanupW(SPMAW2012)
#####combine EW abundance predictions####

FLFJew2000=rbind(FLFJE2000,FLFJW2000)
FLMJew2000=rbind(FLMJE2000,FLMJW2000)
FLFAew2000=rbind(FLFAE2000,FLFAW2000)
FLMAew2000=rbind(FLMAE2000,FLMAW2000)
SPFJew2000=rbind(SPFJE2000,SPFJW2000)
SPMJew2000=rbind(SPMJE2000,SPMJW2000)
SPFAew2000=rbind(SPFAE2000,SPFAW2000)
SPMAew2000=rbind(SPMAE2000,SPMAW2000)

FLFJew2006=rbind(FLFJE2006,FLFJW2006)
FLMJew2006=rbind(FLMJE2006,FLMJW2006)
FLFAew2006=rbind(FLFAE2006,FLFAW2006)
FLMAew2006=rbind(FLMAE2006,FLMAW2006)
SPFJew2006=rbind(SPFJE2006,SPFJW2006)
SPMJew2006=rbind(SPMJE2006,SPMJW2006)
SPFAew2006=rbind(SPFAE2006,SPFAW2006)
SPMAew2006=rbind(SPMAE2006,SPMAW2006)

FLFJew2012=rbind(FLFJE2012,FLFJW2012)
FLMJew2012=rbind(FLMJE2012,FLMJW2012)
FLFAew2012=rbind(FLFAE2012,FLFAW2012)
FLMAew2012=rbind(FLMAE2012,FLMAW2012)
SPFJew2012=rbind(SPFJE2012,SPFJW2012)
SPMJew2012=rbind(SPMJE2012,SPMJW2012)
SPFAew2012=rbind(SPFAE2012,SPFAW2012)
SPMAew2012=rbind(SPMAE2012,SPMAW2012)

#####Creating smoothed interpolated Hindcast###

###Get raster data####
###2000####
FLFJewRAST00 <- list()
coordinates(depth_grid_plot000612) <- ~lon + lat
for(i in 1:1){
  print(i)
  temp_data <- FLFJew2000
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFJewRAST00[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMJewRAST00 <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMJew2000
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMJewRAST00[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLFAewRAST00 <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFAew2000
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFAewRAST00[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMAewRAST00 <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMAew2000
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMAewRAST00[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFJewRAST00 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFJew2000
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFJewRAST00[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMJewRAST00 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMJew2000
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMJewRAST00[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFAewRAST00 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFAew2000
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFAewRAST00[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMAewRAST00 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMAew2000
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMAewRAST00[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

###2006####
FLFJewRAST06 <- list()
coordinates(depth_grid_plot000612) <- ~lon + lat
for(i in 1:1){
  print(i)
  temp_data <- FLFJew2006
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFJewRAST06[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMJewRAST06 <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMJew2006
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMJewRAST06[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLFAewRAST06 <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFAew2006
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFAewRAST06[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMAewRAST06 <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMAew2006
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMAewRAST06[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFJewRAST06 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFJew2006
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row,duplicate = "mean"))
  rast <- raster(akima.smooth)
  SPFJewRAST06[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMJewRAST06 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMJew2006
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row,duplicate = "mean"))
  rast <- raster(akima.smooth)
  SPMJewRAST06[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFAewRAST06 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFAew2006
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row,duplicate = "mean"))
  rast <- raster(akima.smooth)
  SPFAewRAST06[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMAewRAST06 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMAew2006
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row,duplicate = "mean"))
  rast <- raster(akima.smooth)
  SPMAewRAST06[[i]] <- raster::extract(rast, depth_grid_plot000612)
}
###2012####
FLFJewRAST12 <- list()
coordinates(depth_grid_plot000612) <- ~lon + lat
for(i in 1:1){
  print(i)
  temp_data <- FLFJew2012
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFJewRAST12[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMJewRAST12 <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMJew2012
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMJewRAST12[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLFAewRAST12 <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFAew2012
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFAewRAST12[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMAewRAST12 <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMAew2012
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMAewRAST12[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFJewRAST12 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFJew2012
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFJewRAST12[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMJewRAST12 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMJew2012
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMJewRAST12[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFAewRAST12 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFAew2012
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFAewRAST12[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMAewRAST12 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMAew2012
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMAewRAST12[[i]] <- raster::extract(rast, depth_grid_plot000612)
}
####Plot raster data#####
library(rgdal)
mainecoast= readOGR("D:/MENHTrawl/data/gis/ne_10m_coastline/ne_10m_coastline.shp")
plottemp<-function(rasterdata, month,year,size){
  #jpeg(filename = paste("D://MENHtrawl/Plots/grid_NS_map.jpeg", sep=""), width=140, height=120, units = "mm", res = 600)
  par(mar=c(2,2,0,0), mfrow=c(1,1))
  plotvar=unlist(rasterdata)
  nclr=9
  plotclr <- (brewer.pal(nclr,"YlOrRd"))
  class <- classIntervals(plotvar, nclr, style="quantile")
  fix_break<-round(class$brks, digits = 2)
  for(i in 1:length(rasterdata)){
    print(i)
    plotvar <- rasterdata[[i]]
    nclr=9
    plotclr <- (brewer.pal(nclr,"YlOrRd"))
    class <- classIntervals(plotvar, nclr, style="fixed", 
                            fixedBreaks= 
                              if(size=="J")c(0,0.5,3,10,25,50,200,600) #breaks calculated by using average raw data "quantile" breaks
                            else c(0,5,15,40,65,85,115,160,250,900)) #breaks calculated by using average "qauntile" breaks

    colcode <- findColours(class, plotclr)
    
    start_x <- range(GAM_data$Longitude)[1]
    end_x <- range(GAM_data$Longitude)[2]
    start_y <- range(GAM_data$Latitude)[1]
    end_y <- range(GAM_data$Latitude)[2]
    plot(depth_grid_plot000612$lon, depth_grid_plot000612$lat, pch=16, col=colcode, cex=0.1, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, xlab="Longitude", ylab="Latitude")
    map(database = mainecoast, ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE, lwd=0.1)
    #plot(sa511_513, add=T)
    box()
    legend("topleft", paste(year, "-", month, sep=""), bty="n", cex=1.5)
    legend("bottomright", legend=c(names(attr(colcode, "table"))), fill=c(attr(colcode, "palette")), cex=0.9,bg="white",
           bty = "o", title="Nonstationary Abundance")
    axis(1, cex=0.5)
    axis(2, cex=0.5)
  }
  #dev.off()
}

plottemp(FLFJewRAST00,"FLFJ","2000","J")
plottemp(FLMJewRAST00,"FLMJ","2000","J")
plottemp(FLFAewRAST00,"FLFA","2000","A")
plottemp(FLMAewRAST00,"FLMA","2000","A")
plottemp(SPFJewRAST00,"SPFJ","2000","J")
plottemp(SPMJewRAST00,"SPMJ","2000","J")
plottemp(SPFAewRAST00,"SPFA","2000","A")
plottemp(SPMAewRAST00,"SPMA","2000","A")

plottemp(FLFJewRAST06,"FLFJ","2006","J")
plottemp(FLMJewRAST06,"FLMJ","2006","J")
plottemp(FLFAewRAST06,"FLFA","2006","A")
plottemp(FLMAewRAST06,"FLMA","2006","A")
plottemp(SPFJewRAST06,"SPFJ","2006","J")
plottemp(SPMJewRAST06,"SPMJ","2006","J")
plottemp(SPFAewRAST06,"SPFA","2006","A")
plottemp(SPMAewRAST06,"SPMA","2006","A")

plottemp(FLFJewRAST12,"FLFJ","2012","J")
plottemp(FLMJewRAST12,"FLMJ","2012","J")
plottemp(FLFAewRAST12,"FLFA","2012","A")
plottemp(FLMAewRAST12,"FLMA","2012","A")
plottemp(SPFJewRAST12,"SPFJ","2012","J")
plottemp(SPMJewRAST12,"SPMJ","2012","J")
plottemp(SPFAewRAST12,"SPFA","2012","A")
plottemp(SPMAewRAST12,"SPMA","2012","A")

####                                                   ####
########HINDCAST NONSTATIONARY V2. INTERPOLATED PLOTS (east, middle, west)############
PredictGAM<- function(groupabundance.sig,group){
  
  predict.gam(groupabundance.sig, newdata=group, type="response")
}
###EAST####
FLFJE22000p.abundance<-PredictGAM(FLFJE2abundance.sig,FL00)
FLMJE22000p.abundance<-PredictGAM(FLMJE2abundance.sig,FL00)
FLFAE22000p.abundance<-PredictGAM(FLFAE2abundance.sig,FL00)
FLMAE22000p.abundance<-PredictGAM(FLMAE2abundance.sig,FL00)
SPFJE22000p.abundance<-PredictGAM(SPFJE2abundance.sig,SP00)
SPMJE22000p.abundance<-PredictGAM(SPMJE2abundance.sig,SP00)
SPFAE22000p.abundance<-PredictGAM(SPFAE2abundance.sig,SP00)
SPMAE22000p.abundance<-PredictGAM(SPMAE2abundance.sig,SP00)

FLFJE22006p.abundance<-PredictGAM(FLFJE2abundance.sig,FL06)
FLMJE22006p.abundance<-PredictGAM(FLMJE2abundance.sig,FL06)
FLFAE22006p.abundance<-PredictGAM(FLFAE2abundance.sig,FL06)
FLMAE22006p.abundance<-PredictGAM(FLMAE2abundance.sig,FL06)
SPFJE22006p.abundance<-PredictGAM(SPFJE2abundance.sig,SP06)
SPMJE22006p.abundance<-PredictGAM(SPMJE2abundance.sig,SP06)
SPFAE22006p.abundance<-PredictGAM(SPFAE2abundance.sig,SP06)
SPMAE22006p.abundance<-PredictGAM(SPMAE2abundance.sig,SP06)

FLFJE22012p.abundance<-PredictGAM(FLFJE2abundance.sig,FL12)
FLMJE22012p.abundance<-PredictGAM(FLMJE2abundance.sig,FL12)
FLFAE22012p.abundance<-PredictGAM(FLFAE2abundance.sig,FL12)
FLMAE22012p.abundance<-PredictGAM(FLMAE2abundance.sig,FL12)
SPFJE22012p.abundance<-PredictGAM(SPFJE2abundance.sig,SP12)
SPMJE22012p.abundance<-PredictGAM(SPMJE2abundance.sig,SP12)
SPFAE22012p.abundance<-PredictGAM(SPFAE2abundance.sig,SP12)
SPMAE22012p.abundance<-PredictGAM(SPMAE2abundance.sig,SP12)

###MIDDLE####
FLFJM22000p.abundance<-PredictGAM(FLFJM2abundance.sig,FL00)
FLMJM22000p.abundance<-PredictGAM(FLMJM2abundance.sig,FL00)
FLFAM22000p.abundance<-PredictGAM(FLFAM2abundance.sig,FL00)
FLMAM22000p.abundance<-PredictGAM(FLMAM2abundance.sig,FL00)
SPFJM22000p.abundance<-PredictGAM(SPFJM2abundance.sig,SP00)
SPMJM22000p.abundance<-PredictGAM(SPMJM2abundance.sig,SP00)
SPFAM22000p.abundance<-PredictGAM(SPFAM2abundance.sig,SP00)
SPMAM22000p.abundance<-PredictGAM(SPMAM2abundance.sig,SP00)

FLFJM22006p.abundance<-PredictGAM(FLFJM2abundance.sig,FL06)
FLMJM22006p.abundance<-PredictGAM(FLMJM2abundance.sig,FL06)
FLFAM22006p.abundance<-PredictGAM(FLFAM2abundance.sig,FL06)
FLMAM22006p.abundance<-PredictGAM(FLMAM2abundance.sig,FL06)
SPFJM22006p.abundance<-PredictGAM(SPFJM2abundance.sig,SP06)
SPMJM22006p.abundance<-PredictGAM(SPMJM2abundance.sig,SP06)
SPFAM22006p.abundance<-PredictGAM(SPFAM2abundance.sig,SP06)
SPMAM22006p.abundance<-PredictGAM(SPMAM2abundance.sig,SP06)

FLFJM22012p.abundance<-PredictGAM(FLFJM2abundance.sig,FL12)
FLMJM22012p.abundance<-PredictGAM(FLMJM2abundance.sig,FL12)
FLFAM22012p.abundance<-PredictGAM(FLFAM2abundance.sig,FL12)
FLMAM22012p.abundance<-PredictGAM(FLMAM2abundance.sig,FL12)
SPFJM22012p.abundance<-PredictGAM(SPFJM2abundance.sig,SP12)
SPMJM22012p.abundance<-PredictGAM(SPMJM2abundance.sig,SP12)
SPFAM22012p.abundance<-PredictGAM(SPFAM2abundance.sig,SP12)
SPMAM22012p.abundance<-PredictGAM(SPMAM2abundance.sig,SP12)

###WEST####
FLFJW22000p.abundance<-PredictGAM(FLFJW2abundance.sig,FL00)
FLMJW22000p.abundance<-PredictGAM(FLMJW2abundance.sig,FL00)
FLFAW22000p.abundance<-PredictGAM(FLFAW2abundance.sig,FL00)
FLMAW22000p.abundance<-PredictGAM(FLMAW2abundance.sig,FL00)
SPFJW22000p.abundance<-PredictGAM(SPFJW2abundance.sig,SP00)
SPMJW22000p.abundance<-PredictGAM(SPMJW2abundance.sig,SP00)
SPFAW22000p.abundance<-PredictGAM(SPFAW2abundance.sig,SP00)
SPMAW22000p.abundance<-PredictGAM(SPMAW2abundance.sig,SP00)

FLFJW22006p.abundance<-PredictGAM(FLFJW2abundance.sig,FL06)
FLMJW22006p.abundance<-PredictGAM(FLMJW2abundance.sig,FL06)
FLFAW22006p.abundance<-PredictGAM(FLFAW2abundance.sig,FL06)
FLMAW22006p.abundance<-PredictGAM(FLMAW2abundance.sig,FL06)
SPFJW22006p.abundance<-PredictGAM(SPFJW2abundance.sig,SP06)
SPMJW22006p.abundance<-PredictGAM(SPMJW2abundance.sig,SP06)
SPFAW22006p.abundance<-PredictGAM(SPFAW2abundance.sig,SP06)
SPMAW22006p.abundance<-PredictGAM(SPMAW2abundance.sig,SP06)

FLFJW22012p.abundance<-PredictGAM(FLFJW2abundance.sig,FL12)
FLMJW22012p.abundance<-PredictGAM(FLMJW2abundance.sig,FL12)
FLFAW22012p.abundance<-PredictGAM(FLFAW2abundance.sig,FL12)
FLMAW22012p.abundance<-PredictGAM(FLMAW2abundance.sig,FL12)
SPFJW22012p.abundance<-PredictGAM(SPFJW2abundance.sig,SP12)
SPMJW22012p.abundance<-PredictGAM(SPMJW2abundance.sig,SP12)
SPFAW22012p.abundance<-PredictGAM(SPFAW2abundance.sig,SP12)
SPMAW22012p.abundance<-PredictGAM(SPMAW2abundance.sig,SP12)
###Namechange####
namechange<- function(hindyear, groupp.abundance){
  
  hindyear<-data.frame(hindyear,groupp.abundance)
  names(hindyear)[8]<- "p.abundance"
  return(hindyear)
}
FLFJE22000<-namechange(FL00,FLFJE22000p.abundance)
FLMJE22000<-namechange(FL00,FLMJE22000p.abundance)
FLFAE22000<-namechange(FL00,FLFAE22000p.abundance)
FLMAE22000<-namechange(FL00,FLMAE22000p.abundance)

SPFJE22000<-namechange(SP00,SPFJE22000p.abundance)
SPMJE22000<-namechange(SP00,SPMJE22000p.abundance)
SPFAE22000<-namechange(SP00,SPFAE22000p.abundance)
SPMAE22000<-namechange(SP00,SPMAE22000p.abundance)
###2006###
FLFJE22006<-namechange(FL06,FLFJE22006p.abundance)
FLMJE22006<-namechange(FL06,FLMJE22006p.abundance)
FLFAE22006<-namechange(FL06,FLFAE22006p.abundance)
FLMAE22006<-namechange(FL06,FLMAE22006p.abundance)

SPFJE22006<-namechange(SP06,SPFJE22006p.abundance)
SPMJE22006<-namechange(SP06,SPMJE22006p.abundance)
SPFAE22006<-namechange(SP06,SPFAE22006p.abundance)
SPMAE22006<-namechange(SP06,SPMAE22006p.abundance)
###2012###
FLFJE22012<-namechange(FL12,FLFJE22012p.abundance)
FLMJE22012<-namechange(FL12,FLMJE22012p.abundance)
FLFAE22012<-namechange(FL12,FLFAE22012p.abundance)
FLMAE22012<-namechange(FL12,FLMAE22012p.abundance)

SPFJE22012<-namechange(SP12,SPFJE22012p.abundance)
SPMJE22012<-namechange(SP12,SPMJE22012p.abundance)
SPFAE22012<-namechange(SP12,SPFAE22012p.abundance)
SPMAE22012<-namechange(SP12,SPMAE22012p.abundance)

###2000###
FLFJM22000<-namechange(FL00,FLFJM22000p.abundance)
FLMJM22000<-namechange(FL00,FLMJM22000p.abundance)
FLFAM22000<-namechange(FL00,FLFAM22000p.abundance)
FLMAM22000<-namechange(FL00,FLMAM22000p.abundance)

SPFJM22000<-namechange(SP00,SPFJM22000p.abundance)
SPMJM22000<-namechange(SP00,SPMJM22000p.abundance)
SPFAM22000<-namechange(SP00,SPFAM22000p.abundance)
SPMAM22000<-namechange(SP00,SPMAM22000p.abundance)
###2006###
FLFJM22006<-namechange(FL06,FLFJM22006p.abundance)
FLMJM22006<-namechange(FL06,FLMJM22006p.abundance)
FLFAM22006<-namechange(FL06,FLFAM22006p.abundance)
FLMAM22006<-namechange(FL06,FLMAM22006p.abundance)

SPFJM22006<-namechange(SP06,SPFJM22006p.abundance)
SPMJM22006<-namechange(SP06,SPMJM22006p.abundance)
SPFAM22006<-namechange(SP06,SPFAM22006p.abundance)
SPMAM22006<-namechange(SP06,SPMAM22006p.abundance)
###2012###
FLFJM22012<-namechange(FL12,FLFJM22012p.abundance)
FLMJM22012<-namechange(FL12,FLMJM22012p.abundance)
FLFAM22012<-namechange(FL12,FLFAM22012p.abundance)
FLMAM22012<-namechange(FL12,FLMAM22012p.abundance)

SPFJM22012<-namechange(SP12,SPFJM22012p.abundance)
SPMJM22012<-namechange(SP12,SPMJM22012p.abundance)
SPFAM22012<-namechange(SP12,SPFAM22012p.abundance)
SPMAM22012<-namechange(SP12,SPMAM22012p.abundance)
###2000###
FLFJW22000<-namechange(FL00,FLFJW22000p.abundance)
FLMJW22000<-namechange(FL00,FLMJW22000p.abundance)
FLFAW22000<-namechange(FL00,FLFAW22000p.abundance)
FLMAW22000<-namechange(FL00,FLMAW22000p.abundance)

SPFJW22000<-namechange(SP00,SPFJW22000p.abundance)
SPMJW22000<-namechange(SP00,SPMJW22000p.abundance)
SPFAW22000<-namechange(SP00,SPFAW22000p.abundance)
SPMAW22000<-namechange(SP00,SPMAW22000p.abundance)
###2006###
FLFJW22006<-namechange(FL06,FLFJW22006p.abundance)
FLMJW22006<-namechange(FL06,FLMJW22006p.abundance)
FLFAW22006<-namechange(FL06,FLFAW22006p.abundance)
FLMAW22006<-namechange(FL06,FLMAW22006p.abundance)

SPFJW22006<-namechange(SP06,SPFJW22006p.abundance)
SPMJW22006<-namechange(SP06,SPMJW22006p.abundance)
SPFAW22006<-namechange(SP06,SPFAW22006p.abundance)
SPMAW22006<-namechange(SP06,SPMAW22006p.abundance)
###2012###
FLFJW22012<-namechange(FL12,FLFJW22012p.abundance)
FLMJW22012<-namechange(FL12,FLMJW22012p.abundance)
FLFAW22012<-namechange(FL12,FLFAW22012p.abundance)
FLMAW22012<-namechange(FL12,FLMAW22012p.abundance)

SPFJW22012<-namechange(SP12,SPFJW22012p.abundance)
SPMJW22012<-namechange(SP12,SPMJW22012p.abundance)
SPFAW22012<-namechange(SP12,SPFAW22012p.abundance)
SPMAW22012<-namechange(SP12,SPMAW22012p.abundance)
###Cleanup####
cleanupE2<- function(groupfull){
  groupfull<-groupfull[!rowSums(groupfull[2] <42.8),]#cleaning up the data to only contain rows in the GOM
  groupfull<-groupfull[!rowSums(groupfull[2] >44.9),]
  groupfull<-groupfull[!rowSums(groupfull[1] <"-66.9"),]
  groupfull<-groupfull[!rowSums(groupfull[1] >"-68.55327"),]
}
cleanupM2<- function(groupfull){
  groupfull<-groupfull[!rowSums(groupfull[2] <42.8),]#cleaning up the data to only contain rows in the GOM
  groupfull<-groupfull[!rowSums(groupfull[2] >44.9),]
  groupfull<-groupfull[!rowSums(groupfull[1] <"-68.55327"),]
  groupfull<-groupfull[!rowSums(groupfull[1] >"-69.24226"),]
}
cleanupW2<- function(groupfull){
  groupfull<-groupfull[!rowSums(groupfull[2] <42.8),]#cleaning up the data to only contain rows in the GOM
  groupfull<-groupfull[!rowSums(groupfull[2] >44.9),]
  groupfull<-groupfull[!rowSums(groupfull[1] <"-69.24226"),]
  groupfull<-groupfull[!rowSums(groupfull[1] >"-70.8"),]
}
###cleanup east####
FLFJE22000<-cleanupE2(FLFJE22000)
FLMJE22000<-cleanupE2(FLMJE22000)
FLFAE22000<-cleanupE2(FLFAE22000)
FLMAE22000<-cleanupE2(FLMAE22000)

SPFJE22000<-cleanupE2(SPFJE22000)
SPMJE22000<-cleanupE2(SPMJE22000)
SPFAE22000<-cleanupE2(SPFAE22000)
SPMAE22000<-cleanupE2(SPMAE22000)
###2006###
FLFJE22006<-cleanupE2(FLFJE22006)
FLMJE22006<-cleanupE2(FLMJE22006)
FLFAE22006<-cleanupE2(FLFAE22006)
FLMAE22006<-cleanupE2(FLMAE22006)

SPFJE22006<-cleanupE2(SPFJE22006)
SPMJE22006<-cleanupE2(SPMJE22006)
SPFAE22006<-cleanupE2(SPFAE22006)
SPMAE22006<-cleanupE2(SPMAE22006)
###2012###
FLFJE22012<-cleanupE2(FLFJE22012)
FLMJE22012<-cleanupE2(FLMJE22012)
FLFAE22012<-cleanupE2(FLFAE22012)
FLMAE22012<-cleanupE2(FLMAE22012)

SPFJE22012<-cleanupE2(SPFJE22012)
SPMJE22012<-cleanupE2(SPMJE22012)
SPFAE22012<-cleanupE2(SPFAE22012)
SPMAE22012<-cleanupE2(SPMAE22012)

###cleanup middle####
FLFJM22000<-cleanupM2(FLFJM22000)
FLMJM22000<-cleanupM2(FLMJM22000)
FLFAM22000<-cleanupM2(FLFAM22000)
FLMAM22000<-cleanupM2(FLMAM22000)

SPFJM22000<-cleanupM2(SPFJM22000)
SPMJM22000<-cleanupM2(SPMJM22000)
SPFAM22000<-cleanupM2(SPFAM22000)
SPMAM22000<-cleanupM2(SPMAM22000)
###2006###
FLFJM22006<-cleanupM2(FLFJM22006)
FLMJM22006<-cleanupM2(FLMJM22006)
FLFAM22006<-cleanupM2(FLFAM22006)
FLMAM22006<-cleanupM2(FLMAM22006)

SPFJM22006<-cleanupM2(SPFJM22006)
SPMJM22006<-cleanupM2(SPMJM22006)
SPFAM22006<-cleanupM2(SPFAM22006)
SPMAM22006<-cleanupM2(SPMAM22006)
###2012###
FLFJM22012<-cleanupM2(FLFJM22012)
FLMJM22012<-cleanupM2(FLMJM22012)
FLFAM22012<-cleanupM2(FLFAM22012)
FLMAM22012<-cleanupM2(FLMAM22012)

SPFJM22012<-cleanupM2(SPFJM22012)
SPMJM22012<-cleanupM2(SPMJM22012)
SPFAM22012<-cleanupM2(SPFAM22012)
SPMAM22012<-cleanupM2(SPMAM22012)

###Cleanup WEST####
FLFJW22000<-cleanupW2(FLFJW22000)
FLMJW22000<-cleanupW2(FLMJW22000)
FLFAW22000<-cleanupW2(FLFAW22000)
FLMAW22000<-cleanupW2(FLMAW22000)

SPFJW22000<-cleanupW2(SPFJW22000)
SPMJW22000<-cleanupW2(SPMJW22000)
SPFAW22000<-cleanupW2(SPFAW22000)
SPMAW22000<-cleanupW2(SPMAW22000)
###2006###
FLFJW22006<-cleanupW2(FLFJW22006)
FLMJW22006<-cleanupW2(FLMJW22006)
FLFAW22006<-cleanupW2(FLFAW22006)
FLMAW22006<-cleanupW2(FLMAW22006)

SPFJW22006<-cleanupW2(SPFJW22006)
SPMJW22006<-cleanupW2(SPMJW22006)
SPFAW22006<-cleanupW2(SPFAW22006)
SPMAW22006<-cleanupW2(SPMAW22006)
###2012###
FLFJW22012<-cleanupW2(FLFJW22012)
FLMJW22012<-cleanupW2(FLMJW22012)
FLFAW22012<-cleanupW2(FLFAW22012)
FLMAW22012<-cleanupW2(FLMAW22012)

SPFJW22012<-cleanupW2(SPFJW22012)
SPMJW22012<-cleanupW2(SPMJW22012)
SPFAW22012<-cleanupW2(SPFAW22012)
SPMAW22012<-cleanupW2(SPMAW22012)
#####combine EMW abundance predictions####

FLFJew22000=rbind(FLFJE22000,FLFJM22000,FLFJW22000)
FLMJew22000=rbind(FLMJE22000,FLMJM22000,FLMJW22000)
FLFAew22000=rbind(FLFAE22000,FLFAM22000,FLFAW22000)
FLMAew22000=rbind(FLMAE22000,FLMAM22000,FLMAW22000)
SPFJew22000=rbind(SPFJE22000,SPFJM22000,SPFJW22000)
SPMJew22000=rbind(SPMJE22000,SPMJM22000,SPMJW22000)
SPFAew22000=rbind(SPFAE22000,SPFAM22000,SPFAW22000)
SPMAew22000=rbind(SPMAE22000,SPMAM22000,SPMAW22000)

FLFJew22006=rbind(FLFJE22006,FLFJM22006,FLFJW22006)
FLMJew22006=rbind(FLMJE22006,FLMJM22006,FLMJW22006)
FLFAew22006=rbind(FLFAE22006,FLFAM22006,FLFAW22006)
FLMAew22006=rbind(FLMAE22006,FLMAM22006,FLMAW22006)
SPFJew22006=rbind(SPFJE22006,SPFJM22006,SPFJW22006)
SPMJew22006=rbind(SPMJE22006,SPMJM22006,SPMJW22006)
SPFAew22006=rbind(SPFAE22006,SPFAM22006,SPFAW22006)
SPMAew22006=rbind(SPMAE22006,SPMAM22006,SPMAW22006)

FLFJew22012=rbind(FLFJE22012,FLFJM22012,FLFJW22012)
FLMJew22012=rbind(FLMJE22012,FLMJM22012,FLMJW22012)
FLFAew22012=rbind(FLFAE22012,FLFAM22012,FLFAW22012)
FLMAew22012=rbind(FLMAE22012,FLMAM22012,FLMAW22012)
SPFJew22012=rbind(SPFJE22012,SPFJM22012,SPFJW22012)
SPMJew22012=rbind(SPMJE22012,SPMJM22012,SPMJW22012)
SPFAew22012=rbind(SPFAE22012,SPFAM22012,SPFAW22012)
SPMAew22012=rbind(SPMAE22012,SPMAM22012,SPMAW22012)

#####Creating smoothed interpolated Hindcast###

###Get raster data####
###2000####
FLFJew2RAST00 <- list()
coordinates(depth_grid_plot000612) <- ~lon + lat
for(i in 1:1){
  print(i)
  temp_data <- FLFJew22000
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFJew2RAST00[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMJew2RAST00 <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMJew22000
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMJew2RAST00[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLFAew2RAST00 <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFAew22000
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFAew2RAST00[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMAew2RAST00 <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMAew22000
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMAew2RAST00[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFJew2RAST00 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFJew22000
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFJew2RAST00[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMJew2RAST00 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMJew22000
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMJew2RAST00[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFAew2RAST00 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFAew22000
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFAew2RAST00[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMAew2RAST00 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMAew22000
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMAew2RAST00[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

###2006####
FLFJew2RAST06 <- list()
coordinates(depth_grid_plot000612) <- ~lon + lat
for(i in 1:1){
  print(i)
  temp_data <- FLFJew22006
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFJew2RAST06[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMJew2RAST06 <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMJew22006
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMJew2RAST06[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLFAew2RAST06 <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFAew22006
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFAew2RAST06[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMAew2RAST06 <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMAew22006
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMAew2RAST06[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFJew2RAST06 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFJew22006
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row,duplicate = "mean"))
  rast <- raster(akima.smooth)
  SPFJew2RAST06[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMJew2RAST06 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMJew22006
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row,duplicate = "mean"))
  rast <- raster(akima.smooth)
  SPMJew2RAST06[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFAew2RAST06 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFAew22006
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row,duplicate = "mean"))
  rast <- raster(akima.smooth)
  SPFAew2RAST06[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMAew2RAST06 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMAew22006
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row,duplicate = "mean"))
  rast <- raster(akima.smooth)
  SPMAew2RAST06[[i]] <- raster::extract(rast, depth_grid_plot000612)
}
###2012####
FLFJew2RAST12 <- list()
coordinates(depth_grid_plot000612) <- ~lon + lat
for(i in 1:1){
  print(i)
  temp_data <- FLFJew22012
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFJew2RAST12[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMJew2RAST12 <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMJew22012
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMJew2RAST12[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLFAew2RAST12 <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFAew22012
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFAew2RAST12[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMAew2RAST12 <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMAew22012
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMAew2RAST12[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFJew2RAST12 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFJew22012
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFJew2RAST12[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMJew2RAST12 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMJew22012
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMJew2RAST12[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFAew2RAST12 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFAew22012
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFAew2RAST12[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMAew2RAST12 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMAew22012
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMAew2RAST12[[i]] <- raster::extract(rast, depth_grid_plot000612)
}
####Plot raster data#####
library(rgdal)
mainecoast= readOGR("D:/MENHTrawl/data/gis/ne_10m_coastline/ne_10m_coastline.shp")
plottemp<-function(rasterdata, month,year,size){
  #jpeg(filename = paste("D://MENHtrawl/Plots/grid_NS_map.jpeg", sep=""), width=140, height=120, units = "mm", res = 600)
  par(mar=c(2,2,0,0), mfrow=c(1,1))
  plotvar=unlist(rasterdata)
  nclr=9
  plotclr <- (brewer.pal(nclr,"YlOrRd"))
  class <- classIntervals(plotvar, nclr, style="quantile")
  fix_break<-round(class$brks, digits = 2)
  for(i in 1:length(rasterdata)){
    print(i)
    plotvar <- rasterdata[[i]]
    nclr=9
    plotclr <- (brewer.pal(nclr,"YlOrRd"))
    class <- classIntervals(plotvar, nclr, style="fixed", 
                            fixedBreaks= 
                              if(size=="J")c(0,0.5,3,10,25,50,200,600) #breaks calculated by using average raw data "quantile" breaks
                            else c(0,5,15,40,65,85,115,160,250,900)) #breaks calculated by using average "qauntile" breaks
    
    colcode <- findColours(class, plotclr)
    
    start_x <- range(GAM_data$Longitude)[1]
    end_x <- range(GAM_data$Longitude)[2]
    start_y <- range(GAM_data$Latitude)[1]
    end_y <- range(GAM_data$Latitude)[2]
    plot(depth_grid_plot000612$lon, depth_grid_plot000612$lat, pch=16, col=colcode, cex=0.1, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, xlab="Longitude", ylab="Latitude")
    map(database = mainecoast, ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE, lwd=0.1)
    #plot(sa511_513, add=T)
    box()
    legend("topleft", paste(year, "-", month, sep=""), bty="n", cex=1.5)
    legend("bottomright", legend=c(names(attr(colcode, "table"))), fill=c(attr(colcode, "palette")), cex=0.9,bg="white",
           bty = "o", title="Nonstationary Abundance")
    axis(1, cex=0.5)
    axis(2, cex=0.5)
  }
  #dev.off()
}

plottemp(FLFJew2RAST00,"FLFJ","2000","J")
plottemp(FLMJew2RAST00,"FLMJ","2000","J")
plottemp(FLFAew2RAST00,"FLFA","2000","A")
plottemp(FLMAew2RAST00,"FLMA","2000","A")
plottemp(SPFJew2RAST00,"SPFJ","2000","J")
plottemp(SPMJew2RAST00,"SPMJ","2000","J")
plottemp(SPFAew2RAST00,"SPFA","2000","A")
plottemp(SPMAew2RAST00,"SPMA","2000","A")

plottemp(FLFJew2RAST06,"FLFJ","2006","J")
plottemp(FLMJew2RAST06,"FLMJ","2006","J")
plottemp(FLFAew2RAST06,"FLFA","2006","A")
plottemp(FLMAew2RAST06,"FLMA","2006","A")
plottemp(SPFJew2RAST06,"SPFJ","2006","J")
plottemp(SPMJew2RAST06,"SPMJ","2006","J")
plottemp(SPFAew2RAST06,"SPFA","2006","A")
plottemp(SPMAew2RAST06,"SPMA","2006","A")

plottemp(FLFJew2RAST12,"FLFJ","2012","J")
plottemp(FLMJew2RAST12,"FLMJ","2012","J")
plottemp(FLFAew2RAST12,"FLFA","2012","A")
plottemp(FLMAew2RAST12,"FLMA","2012","A")
plottemp(SPFJew2RAST12,"SPFJ","2012","J")
plottemp(SPMJew2RAST12,"SPMJ","2012","J")
plottemp(SPFAew2RAST12,"SPFA","2012","A")
plottemp(SPMAew2RAST12,"SPMA","2012","A")

####                                                       ####                                                   ####
########HINDCAST NOT INTERPOLATED PLOTS (RAW TRAWL DATA BY YEAR)########
##### 1.4 Plot simulated abundance

trawlhind<- function(survey){
  
  trawlhindtemp = subset(GAM_data, Survey == survey)
  return(trawlhindtemp)
}
trawlhind(survey="FL00")
trawlhind(survey="FL06")
trawlhind(survey="FL12")

trawlhind(survey="SP01")
trawlhind(survey="SP06")
trawlhind(survey="SP12")





stationarysim<- function(group.d,group.dcolumn, size){
  start_x <- range(group.d$Longitude)[1]
  end_x <- range(group.d$Longitude)[2]
  start_y <- range(group.d$Latitude)[1]
  end_y <- range(group.d$Latitude)[2]
  colors <- brewer.pal(9, "YlOrRd")
  colbrks <- classIntervals(group.dcolumn, n=7, style="fixed", 
                          fixedBreaks= 
                            if(size=="J")c(0,0.5,3,10,25,50,200,600) #breaks calculated by using average raw data "quantile" breaks
                          else c(0,5,15,40,65,85,115,160,250,900)) #breaks calculated by using average "qauntile" breaks
  brks<- colbrks$brks
  par(mfrow=c(1,1)); par(mar=c(4,4,0,0))
  plot(group.d$Longitude, group.d$Latitude, col=colors[findInterval(group.dcolumn, brks ,all.inside=TRUE)],cex=3.1, pch = 20, xlab="Longitude", ylab="Latitude")
  map(database= mainecoast, ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "lightgreen", fill = TRUE, add = TRUE)
  legend("topleft", "Raw Trawl Survey Data", bty="n")
  
  
  colcode <- findColours(colbrks,colors)
  
  legend("bottomright", # position
         legend = names(attr(colcode, "table")), 
         title = "Abundance",
         fill = attr(colcode, "palette"),
         cex = 0.9,
         bty = "n") # border
}

stationarysim(trawlhind(survey="FL00"),trawlhind(survey="FL00")$Female_juvs,"J")
stationarysim(trawlhind(survey="FL00"),trawlhind(survey="FL00")$Male_juvs,"J")
stationarysim(trawlhind(survey="FL00"),trawlhind(survey="FL00")$Female_adults,"A")
stationarysim(trawlhind(survey="FL00"),trawlhind(survey="FL00")$Male_adults,"A")

stationarysim(trawlhind(survey="FL06"),trawlhind(survey="FL06")$Female_juvs,"J")
stationarysim(trawlhind(survey="FL06"),trawlhind(survey="FL06")$Male_juvs,"J")
stationarysim(trawlhind(survey="FL06"),trawlhind(survey="FL06")$Female_adults,"A")
stationarysim(trawlhind(survey="FL06"),trawlhind(survey="FL06")$Male_adults,"A")

stationarysim(trawlhind(survey="FL12"),trawlhind(survey="FL12")$Female_juvs,"J")
stationarysim(trawlhind(survey="FL12"),trawlhind(survey="FL12")$Male_juvs,"J")
stationarysim(trawlhind(survey="FL12"),trawlhind(survey="FL12")$Female_adults,"A")
stationarysim(trawlhind(survey="FL12"),trawlhind(survey="FL12")$Male_adults,"A")


stationarysim(trawlhind(survey="SP01"),trawlhind(survey="SP01")$Female_juvs,"J")
stationarysim(trawlhind(survey="SP01"),trawlhind(survey="SP01")$Male_juvs,"J")
stationarysim(trawlhind(survey="SP01"),trawlhind(survey="SP01")$Female_adults,"A")
stationarysim(trawlhind(survey="SP01"),trawlhind(survey="SP01")$Male_adults,"A")

stationarysim(trawlhind(survey="SP06"),trawlhind(survey="SP06")$Female_juvs,"J")
stationarysim(trawlhind(survey="SP06"),trawlhind(survey="SP06")$Male_juvs,"J")
stationarysim(trawlhind(survey="SP06"),trawlhind(survey="SP06")$Female_adults,"A")
stationarysim(trawlhind(survey="SP06"),trawlhind(survey="SP06")$Male_adults,"A")

stationarysim(trawlhind(survey="SP12"),trawlhind(survey="SP12")$Female_juvs,"J")
stationarysim(trawlhind(survey="SP12"),trawlhind(survey="SP12")$Male_juvs,"J")
stationarysim(trawlhind(survey="SP12"),trawlhind(survey="SP12")$Female_adults,"A")
stationarysim(trawlhind(survey="SP12"),trawlhind(survey="SP12")$Male_adults,"A")



###NOWCAST STATIONARY RAW TRAWL 2017####

#For worldhires to work, need to use library(maps) and unload library(purrr)#


  rawnowfall = subset(GAM_data, Survey == "FL17")
  rawnowspring = subset(GAM_data, Survey == "SP17")

stationarysim<- function(group.d,group.dcolumn, size){
  start_x <- range(group.d$Longitude)[1]
  end_x <- range(group.d$Longitude)[2]
  start_y <- range(group.d$Latitude)[1]
  end_y <- range(group.d$Latitude)[2]
  colors <- brewer.pal(9, "YlOrRd")
  colbrks<-classIntervals(group.dcolumn, n=9, style="fixed", 
                          fixedBreaks=
                            if(size=="J")c(0,0.5,3,10,25,50,200,600) #breaks calculated by using average raw data "quantile" breaks
                          else c(0,5,15,40,65,85,115,160,250,900)) #breaks calculated by using average "qauntile" breaks

  brks<- colbrks$brks
  par(mfrow=c(1,1)); par(mar=c(4,4,0,0))
  plot(group.d$Longitude, group.d$Latitude, col=colors[findInterval(group.dcolumn, brks ,all.inside=TRUE)],cex=2.5, pch = 20, xlab="Longitude", ylab="Latitude")
  map(database= mainecoast, ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "lightgreen", fill = TRUE, add = TRUE)
  legend("topleft", "Raw Trawl Survey Data", bty="n")
  
  
  colcode <- findColours(colbrks,colors)
  
  legend("bottomright", # position
         legend = names(attr(colcode, "table")), 
         title = "Abundance",
         fill = attr(colcode, "palette"),
         cex = 0.9,
         bty = "n") # border
}

stationarysim(rawnowfall,rawnowfall$Female_juvs,"J")
stationarysim(rawnowfall,rawnowfall$Male_juvs,"J")
stationarysim(rawnowfall,rawnowfall$Female_adults,"A")
stationarysim(rawnowfall,rawnowfall$Male_adults,"A")

stationarysim(rawnowspring,rawnowspring$Female_juvs,"J")
stationarysim(rawnowspring,rawnowspring$Male_juvs,"J")
stationarysim(rawnowspring,rawnowspring$Female_adults,"A")
stationarysim(rawnowspring,rawnowspring$Male_adults,"A")



###NOWCAST STATIONARY INTERPOLATED 2017####
PredictGAM<- function(groupabundance.sig,group){
  
  predict.gam(groupabundance.sig, newdata=group, type="response")
}
FLFJ2017p.abundance<-PredictGAM(FLFJabundance.sig,FL17)
FLMJ2017p.abundance<-PredictGAM(FLMJabundance.sig,FL17)
FLFA2017p.abundance<-PredictGAM(FLFAabundance.sig,FL17)
FLMA2017p.abundance<-PredictGAM(FLMAabundance.sig,FL17)
SPFJ2017p.abundance<-PredictGAM(SPFJabundance.sig,SP17)
SPMJ2017p.abundance<-PredictGAM(SPMJabundance.sig,SP17)
SPFA2017p.abundance<-PredictGAM(SPFAabundance.sig,SP17)
SPMA2017p.abundance<-PredictGAM(SPMAabundance.sig,SP17)



namechange<- function(hindyear, groupp.abundance){
  
  hindyear<-data.frame(hindyear,groupp.abundance)
  names(hindyear)[10]<- "p.abundance"
  return(hindyear)
}
FLFJ2017<-namechange(FL17,FLFJ2017p.abundance)
FLMJ2017<-namechange(FL17,FLMJ2017p.abundance)
FLFA2017<-namechange(FL17,FLFA2017p.abundance)
FLMA2017<-namechange(FL17,FLMA2017p.abundance)

SPFJ2017<-namechange(SP17,SPFJ2017p.abundance)
SPMJ2017<-namechange(SP17,SPMJ2017p.abundance)
SPFA2017<-namechange(SP17,SPFA2017p.abundance)
SPMA2017<-namechange(SP17,SPMA2017p.abundance)

###Get raster data####
FLFJRAST17 <- list()
coordinates(depth_grid_plot2017) <- ~lon + lat
for(i in 1:1){
  print(i)
  temp_data <- FLFJ2017
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFJRAST17[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

FLMJRAST17 <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMJ2017
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMJRAST17[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

FLFARAST17 <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFA2017
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFARAST17[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

FLMARAST17 <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMA2017
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMARAST17[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

SPFJRAST17 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFJ2017
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFJRAST17[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

SPMJRAST17 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMJ2017
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMJRAST17[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

SPFARAST17 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFA2017
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFARAST17[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

SPMARAST17 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMA2017
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMARAST17[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

plottemp<-function(rasterdata, month,year,size){
  #jpeg(filename = paste("D://MENHtrawl/Plots/grid_NS_map.jpeg", sep=""), width=140, height=120, units = "mm", res = 600)
  par(mar=c(2,2,0,0), mfrow=c(1,1))
  plotvar=unlist(rasterdata)
  nclr=5
  plotclr <- (brewer.pal(nclr,"YlOrRd"))
  class <- classIntervals(plotvar, nclr, style="quantile")
  fix_break<-round(class$brks, digits = 2)
  for(i in 1:length(rasterdata)){
    print(i)
    plotvar <- rasterdata[[i]]
    nclr=5
    plotclr <- (brewer.pal(nclr,"YlOrRd"))
    class <- classIntervals(plotvar, nclr, style="fixed", 
                            fixedBreaks= 
                              if(size=="J")c(0,0.5,3,10,25,50,200,600) #breaks calculated by using average raw data "quantile" breaks
                            else c(0,5,15,40,65,85,115,160,250,900)) #breaks calculated by using average "qauntile" breaks
    
    colcode <- findColours(class, plotclr)
    
    start_x <- range(GAM_data$Longitude)[1]
    end_x <- range(GAM_data$Longitude)[2]
    start_y <- range(GAM_data$Latitude)[1]
    end_y <- range(GAM_data$Latitude)[2]
    plot(depth_grid_plot2017$lon, depth_grid_plot2017$lat, pch=16, col=colcode, cex=0.1, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, xlab="Longitude", ylab="Latitude")
    map(database= mainecoast, ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE, lwd=0.1)
    #plot(sa511_513, add=T)
    box()
    legend("topleft", paste(year, "-", month, sep=""), bty="n", cex=1.5)
    legend("bottomright", legend=c(names(attr(colcode, "table"))), fill=c(attr(colcode, "palette")), cex=0.9,bg="white",
           bty = "o", title="Stationary Abundance")
    axis(1, cex=0.5)
    axis(2, cex=0.5)
  }
  #dev.off()
}

plottemp(FLFJRAST17,"FLFJ","2017","J")
plottemp(FLMJRAST17,"FLMJ","2017","J")
plottemp(FLFARAST17,"FLFA","2017","A")
plottemp(FLMARAST17,"FLMA","2017","A")
plottemp(SPFJRAST17,"SPFJ","2017","J")
plottemp(SPMJRAST17,"SPMJ","2017","J")
plottemp(SPFARAST17,"SPFA","2017","A")
plottemp(SPMARAST17,"SPMA","2017","A")



####                                                    ####
###NOWCAST NON_STATIONARY INTERPOLATED V1. 2017####
PredictGAM<- function(groupabundance.sig,group){
  
  predict.gam(groupabundance.sig, newdata=group, type="response")
}
###EAST####
FLFJE2017p.abundance<-PredictGAM(FLFJEabundance.sig,FL17)
FLMJE2017p.abundance<-PredictGAM(FLMJEabundance.sig,FL17)
FLFAE2017p.abundance<-PredictGAM(FLFAEabundance.sig,FL17)
FLMAE2017p.abundance<-PredictGAM(FLMAEabundance.sig,FL17)
SPFJE2017p.abundance<-PredictGAM(SPFJEabundance.sig,SP17)
SPMJE2017p.abundance<-PredictGAM(SPMJEabundance.sig,SP17)
SPFAE2017p.abundance<-PredictGAM(SPFAEabundance.sig,SP17)
SPMAE2017p.abundance<-PredictGAM(SPMAEabundance.sig,SP17)
###WEST####
FLFJW2017p.abundance<-PredictGAM(FLFJWabundance.sig,FL17)
FLMJW2017p.abundance<-PredictGAM(FLMJWabundance.sig,FL17)
FLFAW2017p.abundance<-PredictGAM(FLFAWabundance.sig,FL17)
FLMAW2017p.abundance<-PredictGAM(FLMAWabundance.sig,FL17)
SPFJW2017p.abundance<-PredictGAM(SPFJWabundance.sig,SP17)
SPMJW2017p.abundance<-PredictGAM(SPMJWabundance.sig,SP17)
SPFAW2017p.abundance<-PredictGAM(SPFAWabundance.sig,SP17)
SPMAW2017p.abundance<-PredictGAM(SPMAWabundance.sig,SP17)
###Namechange####
namechange<- function(hindyear, groupp.abundance){
  
  hindyear<-data.frame(hindyear,groupp.abundance)
  names(hindyear)[10]<- "p.abundance"
  return(hindyear)
}
FLFJE2017<-namechange(FL17,FLFJE2017p.abundance)
FLMJE2017<-namechange(FL17,FLMJE2017p.abundance)
FLFAE2017<-namechange(FL17,FLFAE2017p.abundance)
FLMAE2017<-namechange(FL17,FLMAE2017p.abundance)

SPFJE2017<-namechange(SP17,SPFJE2017p.abundance)
SPMJE2017<-namechange(SP17,SPMJE2017p.abundance)
SPFAE2017<-namechange(SP17,SPFAE2017p.abundance)
SPMAE2017<-namechange(SP17,SPMAE2017p.abundance)

###West2017###
FLFJW2017<-namechange(FL17,FLFJW2017p.abundance)
FLMJW2017<-namechange(FL17,FLMJW2017p.abundance)
FLFAW2017<-namechange(FL17,FLFAW2017p.abundance)
FLMAW2017<-namechange(FL17,FLMAW2017p.abundance)

SPFJW2017<-namechange(SP17,SPFJW2017p.abundance)
SPMJW2017<-namechange(SP17,SPMJW2017p.abundance)
SPFAW2017<-namechange(SP17,SPFAW2017p.abundance)
SPMAW2017<-namechange(SP17,SPMAW2017p.abundance)

###Cleanup####
cleanupE<- function(groupfull){
  groupfull<-groupfull[!rowSums(groupfull[2] <42.8),]#cleaning up the data to only contain rows in the GOM
  groupfull<-groupfull[!rowSums(groupfull[2] >44.9),]
  groupfull<-groupfull[!rowSums(groupfull[1] <"-66.9"),]
  groupfull<-groupfull[!rowSums(groupfull[1] >"-69.2612"),]
}

cleanupW<- function(groupfull){
  groupfull<-groupfull[!rowSums(groupfull[2] <42.8),]#cleaning up the data to only contain rows in the GOM
  groupfull<-groupfull[!rowSums(groupfull[2] >44.9),]
  groupfull<-groupfull[!rowSums(groupfull[1] <"-69.2612"),]
  groupfull<-groupfull[!rowSums(groupfull[1] >"-70.8"),]
}
###cleanup east####
FLFJE2017<-cleanupE(FLFJE2017)
FLMJE2017<-cleanupE(FLMJE2017)
FLFAE2017<-cleanupE(FLFAE2017)
FLMAE2017<-cleanupE(FLMAE2017)

SPFJE2017<-cleanupE(SPFJE2017)
SPMJE2017<-cleanupE(SPMJE2017)
SPFAE2017<-cleanupE(SPFAE2017)
SPMAE2017<-cleanupE(SPMAE2017)

###Cleanup WEST####
FLFJW2017<-cleanupW(FLFJW2017)
FLMJW2017<-cleanupW(FLMJW2017)
FLFAW2017<-cleanupW(FLFAW2017)
FLMAW2017<-cleanupW(FLMAW2017)

SPFJW2017<-cleanupW(SPFJW2017)
SPMJW2017<-cleanupW(SPMJW2017)
SPFAW2017<-cleanupW(SPFAW2017)
SPMAW2017<-cleanupW(SPMAW2017)

#####combine EW abundance predictions and plot interpolated ^  ####

FLFJew2017=rbind(FLFJE2017,FLFJW2017)
FLMJew2017=rbind(FLMJE2017,FLMJW2017)
FLFAew2017=rbind(FLFAE2017,FLFAW2017)
FLMAew2017=rbind(FLMAE2017,FLMAW2017)
SPFJew2017=rbind(SPFJE2017,SPFJW2017)
SPMJew2017=rbind(SPMJE2017,SPMJW2017)
SPFAew2017=rbind(SPFAE2017,SPFAW2017)
SPMAew2017=rbind(SPMAE2017,SPMAW2017)
#####Creating smoothed interpolated Hindcast###

###Get raster data####
###2017####
FLFJewRAST17 <- list()
coordinates(depth_grid_plot2017) <- ~lon + lat
for(i in 1:1){
  print(i)
  temp_data <- FLFJew2017
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFJewRAST17[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

FLMJewRAST17 <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMJew2017
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMJewRAST17[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

FLFAewRAST17 <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFAew2017
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFAewRAST17[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

FLMAewRAST17 <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMAew2017
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMAewRAST17[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

SPFJewRAST17 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFJew2017
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFJewRAST17[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

SPMJewRAST17 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMJew2017
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMJewRAST17[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

SPFAewRAST17 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFAew2017
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFAewRAST17[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

SPMAewRAST17 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMAew2017
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMAewRAST17[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

####Plot raster data#####
plottemp<-function(rasterdata, month,year,size){
  #jpeg(filename = paste("D://MENHtrawl/Plots/grid_NS_map.jpeg", sep=""), width=140, height=120, units = "mm", res = 600)
  par(mar=c(2,2,0,0), mfrow=c(1,1))
  plotvar=unlist(rasterdata)
  nclr=5
  plotclr <- (brewer.pal(nclr,"YlOrRd"))
  class <- classIntervals(plotvar, nclr, style="quantile")
  fix_break<-round(class$brks, digits = 2)
  for(i in 1:length(rasterdata)){
    print(i)
    plotvar <- rasterdata[[i]]
    nclr=5
    plotclr <- (brewer.pal(nclr,"YlOrRd"))
    class <- classIntervals(plotvar, nclr, style="fixed", 
                            fixedBreaks= 
                              if(size=="J")c(0,0.5,3,10,25,50,200,600) #breaks calculated by using average raw data "quantile" breaks
                            else c(0,5,15,40,65,85,115,160,250,900)) #breaks calculated by using average "qauntile" breaks
    
    colcode <- findColours(class, plotclr)
    
    start_x <- range(GAM_data$Longitude)[1]
    end_x <- range(GAM_data$Longitude)[2]
    start_y <- range(GAM_data$Latitude)[1]
    end_y <- range(GAM_data$Latitude)[2]
    plot(depth_grid_plot2017$lon, depth_grid_plot2017$lat, pch=16, col=colcode, cex=0.1, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, xlab="Longitude", ylab="Latitude")
    map(database= mainecoast, ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE, lwd=0.1)
    #plot(sa511_513, add=T)
    box()
    legend("topleft", paste(year, "-", month, sep=""), bty="n", cex=1.5)
    legend("bottomright", legend=c(names(attr(colcode, "table"))), fill=c(attr(colcode, "palette")), cex=0.9,bg="white",
           bty = "o", title="Nonstationary Abundance")
    axis(1, cex=0.5)
    axis(2, cex=0.5)
  }
  #dev.off()
}

plottemp(FLFJewRAST17,"FLFJ","2017","J")
plottemp(FLMJewRAST17,"FLMJ","2017","J")
plottemp(FLFAewRAST17,"FLFA","2017","A")
plottemp(FLMAewRAST17,"FLMA","2017","A")
plottemp(SPFJewRAST17,"SPFJ","2017","J")
plottemp(SPMJewRAST17,"SPMJ","2017","J")
plottemp(SPFAewRAST17,"SPFA","2017","A")
plottemp(SPMAewRAST17,"SPMA","2017","A")


####                                                      ####
###NOWCAST NON_STATIONARY INTERPOLATED V2. 2017####
PredictGAM<- function(groupabundance.sig,group){
  
  predict.gam(groupabundance.sig, newdata=group, type="response")
}
###EAST####
FLFJE22017p.abundance<-PredictGAM(FLFJE2abundance.sig,FL17)
FLMJE22017p.abundance<-PredictGAM(FLMJE2abundance.sig,FL17)
FLFAE22017p.abundance<-PredictGAM(FLFAE2abundance.sig,FL17)
FLMAE22017p.abundance<-PredictGAM(FLMAE2abundance.sig,FL17)
SPFJE22017p.abundance<-PredictGAM(SPFJE2abundance.sig,SP17)
SPMJE22017p.abundance<-PredictGAM(SPMJE2abundance.sig,SP17)
SPFAE22017p.abundance<-PredictGAM(SPFAE2abundance.sig,SP17)
SPMAE22017p.abundance<-PredictGAM(SPMAE2abundance.sig,SP17)
###MIDDLE####
FLFJM22017p.abundance<-PredictGAM(FLFJM2abundance.sig,FL17)
FLMJM22017p.abundance<-PredictGAM(FLMJM2abundance.sig,FL17)
FLFAM22017p.abundance<-PredictGAM(FLFAM2abundance.sig,FL17)
FLMAM22017p.abundance<-PredictGAM(FLMAM2abundance.sig,FL17)
SPFJM22017p.abundance<-PredictGAM(SPFJM2abundance.sig,SP17)
SPMJM22017p.abundance<-PredictGAM(SPMJM2abundance.sig,SP17)
SPFAM22017p.abundance<-PredictGAM(SPFAM2abundance.sig,SP17)
SPMAM22017p.abundance<-PredictGAM(SPMAM2abundance.sig,SP17)
###WEST####
FLFJW22017p.abundance<-PredictGAM(FLFJW2abundance.sig,FL17)
FLMJW22017p.abundance<-PredictGAM(FLMJW2abundance.sig,FL17)
FLFAW22017p.abundance<-PredictGAM(FLFAW2abundance.sig,FL17)
FLMAW22017p.abundance<-PredictGAM(FLMAW2abundance.sig,FL17)
SPFJW22017p.abundance<-PredictGAM(SPFJW2abundance.sig,SP17)
SPMJW22017p.abundance<-PredictGAM(SPMJW2abundance.sig,SP17)
SPFAW22017p.abundance<-PredictGAM(SPFAW2abundance.sig,SP17)
SPMAW22017p.abundance<-PredictGAM(SPMAW2abundance.sig,SP17)
###Namechange####
namechange<- function(hindyear, groupp.abundance){
  
  hindyear<-data.frame(hindyear,groupp.abundance)
  names(hindyear)[10]<- "p.abundance"
  return(hindyear)
}
FLFJE22017<-namechange(FL17,FLFJE22017p.abundance)
FLMJE22017<-namechange(FL17,FLMJE22017p.abundance)
FLFAE22017<-namechange(FL17,FLFAE22017p.abundance)
FLMAE22017<-namechange(FL17,FLMAE22017p.abundance)

SPFJE22017<-namechange(SP17,SPFJE22017p.abundance)
SPMJE22017<-namechange(SP17,SPMJE22017p.abundance)
SPFAE22017<-namechange(SP17,SPFAE22017p.abundance)
SPMAE22017<-namechange(SP17,SPMAE22017p.abundance)
###Middle2017###
FLFJM22017<-namechange(FL17,FLFJM22017p.abundance)
FLMJM22017<-namechange(FL17,FLMJM22017p.abundance)
FLFAM22017<-namechange(FL17,FLFAM22017p.abundance)
FLMAM22017<-namechange(FL17,FLMAM22017p.abundance)

SPFJM22017<-namechange(SP17,SPFJM22017p.abundance)
SPMJM22017<-namechange(SP17,SPMJM22017p.abundance)
SPFAM22017<-namechange(SP17,SPFAM22017p.abundance)
SPMAM22017<-namechange(SP17,SPMAM22017p.abundance)
###West2017###
FLFJW22017<-namechange(FL17,FLFJW22017p.abundance)
FLMJW22017<-namechange(FL17,FLMJW22017p.abundance)
FLFAW22017<-namechange(FL17,FLFAW22017p.abundance)
FLMAW22017<-namechange(FL17,FLMAW22017p.abundance)

SPFJW22017<-namechange(SP17,SPFJW22017p.abundance)
SPMJW22017<-namechange(SP17,SPMJW22017p.abundance)
SPFAW22017<-namechange(SP17,SPFAW22017p.abundance)
SPMAW22017<-namechange(SP17,SPMAW22017p.abundance)

###Cleanup####
cleanupE2<- function(groupfull){
  groupfull<-groupfull[!rowSums(groupfull[2] <42.8),]#cleaning up the data to only contain rows in the GOM
  groupfull<-groupfull[!rowSums(groupfull[2] >44.9),]
  groupfull<-groupfull[!rowSums(groupfull[1] <"-66.9"),]
  groupfull<-groupfull[!rowSums(groupfull[1] >"-68.55327"),]
}
cleanupM2<- function(groupfull){
  groupfull<-groupfull[!rowSums(groupfull[2] <42.8),]#cleaning up the data to only contain rows in the GOM
  groupfull<-groupfull[!rowSums(groupfull[2] >44.9),]
  groupfull<-groupfull[!rowSums(groupfull[1] <"-68.55327"),]
  groupfull<-groupfull[!rowSums(groupfull[1] >"-69.24226"),]
}
cleanupW2<- function(groupfull){
  groupfull<-groupfull[!rowSums(groupfull[2] <42.8),]#cleaning up the data to only contain rows in the GOM
  groupfull<-groupfull[!rowSums(groupfull[2] >44.9),]
  groupfull<-groupfull[!rowSums(groupfull[1] <"-69.24226"),]
  groupfull<-groupfull[!rowSums(groupfull[1] >"-70.8"),]
}
###cleanup east####
FLFJE22017<-cleanupE2(FLFJE22017)
FLMJE22017<-cleanupE2(FLMJE22017)
FLFAE22017<-cleanupE2(FLFAE22017)
FLMAE22017<-cleanupE2(FLMAE22017)

SPFJE22017<-cleanupE2(SPFJE22017)
SPMJE22017<-cleanupE2(SPMJE22017)
SPFAE22017<-cleanupE2(SPFAE22017)
SPMAE22017<-cleanupE2(SPMAE22017)
###cleanup Middle####
FLFJM22017<-cleanupM2(FLFJM22017)
FLMJM22017<-cleanupM2(FLMJM22017)
FLFAM22017<-cleanupM2(FLFAM22017)
FLMAM22017<-cleanupM2(FLMAM22017)

SPFJM22017<-cleanupM2(SPFJM22017)
SPMJM22017<-cleanupM2(SPMJM22017)
SPFAM22017<-cleanupM2(SPFAM22017)
SPMAM22017<-cleanupM2(SPMAM22017)
###Cleanup WEST####
FLFJW22017<-cleanupW2(FLFJW22017)
FLMJW22017<-cleanupW2(FLMJW22017)
FLFAW22017<-cleanupW2(FLFAW22017)
FLMAW22017<-cleanupW2(FLMAW22017)

SPFJW22017<-cleanupW2(SPFJW22017)
SPMJW22017<-cleanupW2(SPMJW22017)
SPFAW22017<-cleanupW2(SPFAW22017)
SPMAW22017<-cleanupW2(SPMAW22017)

#####combine EW abundance predictions and plot interpolated ^  ####

FLFJew22017=rbind(FLFJE22017,FLFJM22017,FLFJW22017)
FLMJew22017=rbind(FLMJE22017,FLMJM22017,FLMJW22017)
FLFAew22017=rbind(FLFAE22017,FLFAM22017,FLFAW22017)
FLMAew22017=rbind(FLMAE22017,FLMAM22017,FLMAW22017)
SPFJew22017=rbind(SPFJE22017,SPFJM22017,SPFJW22017)
SPMJew22017=rbind(SPMJE22017,SPMJM22017,SPMJW22017)
SPFAew22017=rbind(SPFAE22017,SPFAM22017,SPFAW22017)
SPMAew22017=rbind(SPMAE22017,SPMAM22017,SPMAW22017)
#####Creating smoothed interpolated Hindcast###

###Get raster data####
###2017####
FLFJew2RAST17 <- list()
coordinates(depth_grid_plot2017) <- ~lon + lat
for(i in 1:1){
  print(i)
  temp_data <- FLFJew22017
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFJew2RAST17[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

FLMJew2RAST17 <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMJew22017
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMJew2RAST17[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

FLFAew2RAST17 <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFAew22017
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFAew2RAST17[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

FLMAew2RAST17 <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMAew22017
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMAew2RAST17[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

SPFJew2RAST17 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFJew22017
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFJew2RAST17[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

SPMJew2RAST17 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMJew22017
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMJew2RAST17[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

SPFAew2RAST17 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFAew22017
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFAew2RAST17[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

SPMAew2RAST17 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMAew22017
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMAew2RAST17[[i]] <- raster::extract(rast, depth_grid_plot2017)
}
####Plot raster data#####
plottemp<-function(rasterdata, month,year,size){
  #jpeg(filename = paste("D://MENHtrawl/Plots/grid_NS_map.jpeg", sep=""), width=140, height=120, units = "mm", res = 600)
  par(mar=c(2,2,0,0), mfrow=c(1,1))
  plotvar=unlist(rasterdata)
  nclr=5
  plotclr <- (brewer.pal(nclr,"YlOrRd"))
  class <- classIntervals(plotvar, nclr, style="quantile")
  fix_break<-round(class$brks, digits = 2)
  for(i in 1:length(rasterdata)){
    print(i)
    plotvar <- rasterdata[[i]]
    nclr=5
    plotclr <- (brewer.pal(nclr,"YlOrRd"))
    class <- classIntervals(plotvar, nclr, style="fixed", 
                            fixedBreaks= 
                              if(size=="J")c(0,0.5,3,10,25,50,200,600) #breaks calculated by using average raw data "quantile" breaks
                            else c(0,5,15,40,65,85,115,160,250,900)) #breaks calculated by using average "qauntile" breaks
    
    colcode <- findColours(class, plotclr)
    
    start_x <- range(GAM_data$Longitude)[1]
    end_x <- range(GAM_data$Longitude)[2]
    start_y <- range(GAM_data$Latitude)[1]
    end_y <- range(GAM_data$Latitude)[2]
    plot(depth_grid_plot2017$lon, depth_grid_plot2017$lat, pch=16, col=colcode, cex=0.1, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, xlab="Longitude", ylab="Latitude")
    map(database= mainecoast, ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE, lwd=0.1)
    #plot(sa511_513, add=T)
    box()
    legend("topleft", paste(year, "-", month, sep=""), bty="n", cex=1.5)
    legend("bottomright", legend=c(names(attr(colcode, "table"))), fill=c(attr(colcode, "palette")), cex=0.9,bg="white",
           bty = "o", title="Nonstationary Abundance")
    axis(1, cex=0.5)
    axis(2, cex=0.5)
  }
  #dev.off()
}

plottemp(FLFJew2RAST17,"FLFJ","2017","J")
plottemp(FLMJew2RAST17,"FLMJ","2017","J")
plottemp(FLFAew2RAST17,"FLFA","2017","A")
plottemp(FLMAew2RAST17,"FLMA","2017","A")
plottemp(SPFJew2RAST17,"SPFJ","2017","J")
plottemp(SPMJew2RAST17,"SPMJ","2017","J")
plottemp(SPFAew2RAST17,"SPFA","2017","A")
plottemp(SPMAew2RAST17,"SPMA","2017","A")


####                                                            ####
####                                                            ####
#####Calculate and plot RELATIVE differences between NSV1 and S predicted abundancies####

###2000####

FLFJ_diff_rast_00<-merge(FLFJ2000,FLFJew2000,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#FLFJ_diff_rast_00<- cbind(FLFJ_diff_rast_00[1:8],as.data.frame(((FLFJ_diff_rast_00$p.abundance.y-FLFJ_diff_rast_00$p.abundance.x)/FLFJ_diff_rast_00$p.abundance.x)*100))
FLFJ_diff_rast_00<- cbind(FLFJ_diff_rast_00[1:8],as.data.frame(
  ifelse(((FLFJ_diff_rast_00$p.abundance.y>1.00)&(FLFJ_diff_rast_00$p.abundance.x>1.00)),
    (((FLFJ_diff_rast_00$p.abundance.y-FLFJ_diff_rast_00$p.abundance.x)/FLFJ_diff_rast_00$p.abundance.x)*100),
  (FLFJ_diff_rast_00$p.abundance.y-FLFJ_diff_rast_00$p.abundance.x))))
names(FLFJ_diff_rast_00)[9]<-"p.abundance"
FLFJ_diff_rast_00.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFJ_diff_rast_00
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFJ_diff_rast_00.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

  
FLMJ_diff_rast_00<-merge(FLMJ2000,FLMJew2000,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#FLMJ_diff_rast_00<- cbind(FLMJ_diff_rast_00[1:8],as.data.frame(((FLMJ_diff_rast_00$p.abundance.y-FLMJ_diff_rast_00$p.abundance.x)/FLMJ_diff_rast_00$p.abundance.x)*100))
FLMJ_diff_rast_00<- cbind(FLMJ_diff_rast_00[1:8],as.data.frame(
  ifelse(((FLMJ_diff_rast_00$p.abundance.y>1.00)&(FLMJ_diff_rast_00$p.abundance.x>1.00)),
         (((FLMJ_diff_rast_00$p.abundance.y-FLMJ_diff_rast_00$p.abundance.x)/FLMJ_diff_rast_00$p.abundance.x)*100),
         (FLMJ_diff_rast_00$p.abundance.y-FLMJ_diff_rast_00$p.abundance.x))))
names(FLMJ_diff_rast_00)[9]<-"p.abundance"
FLMJ_diff_rast_00.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMJ_diff_rast_00
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMJ_diff_rast_00.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLFA_diff_rast_00<-merge(FLFA2000,FLFAew2000,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#FLFA_diff_rast_00<- cbind(FLFA_diff_rast_00[1:8],as.data.frame(((FLFA_diff_rast_00$p.abundance.y-FLFA_diff_rast_00$p.abundance.x)/FLFA_diff_rast_00$p.abundance.x)*100))
FLFA_diff_rast_00<- cbind(FLFA_diff_rast_00[1:8],as.data.frame(
  ifelse(((FLFA_diff_rast_00$p.abundance.y>1.00)&(FLFA_diff_rast_00$p.abundance.x>1.00)),
         (((FLFA_diff_rast_00$p.abundance.y-FLFA_diff_rast_00$p.abundance.x)/FLFA_diff_rast_00$p.abundance.x)*100),
         (FLFA_diff_rast_00$p.abundance.y-FLFA_diff_rast_00$p.abundance.x))))
names(FLFA_diff_rast_00)[9]<-"p.abundance"
FLFA_diff_rast_00.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFA_diff_rast_00
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFA_diff_rast_00.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMA_diff_rast_00<-merge(FLMA2000,FLMAew2000,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#FLMA_diff_rast_00<- cbind(FLMA_diff_rast_00[1:8],as.data.frame(((FLMA_diff_rast_00$p.abundance.y-FLMA_diff_rast_00$p.abundance.x)/FLMA_diff_rast_00$p.abundance.x)*100))
FLMA_diff_rast_00<- cbind(FLMA_diff_rast_00[1:8],as.data.frame(
  ifelse(((FLMA_diff_rast_00$p.abundance.y>1.00)&(FLMA_diff_rast_00$p.abundance.x>1.00)),
         (((FLMA_diff_rast_00$p.abundance.y-FLMA_diff_rast_00$p.abundance.x)/FLMA_diff_rast_00$p.abundance.x)*100),
         (FLMA_diff_rast_00$p.abundance.y-FLMA_diff_rast_00$p.abundance.x))))
names(FLMA_diff_rast_00)[9]<-"p.abundance"
FLMA_diff_rast_00.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMA_diff_rast_00
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMA_diff_rast_00.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFJ_diff_rast_00<-merge(SPFJ2000,SPFJew2000,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#SPFJ_diff_rast_00<- cbind(SPFJ_diff_rast_00[1:8],as.data.frame(((SPFJ_diff_rast_00$p.abundance.y-SPFJ_diff_rast_00$p.abundance.x)/SPFJ_diff_rast_00$p.abundance.x)*100))
SPFJ_diff_rast_00<- cbind(SPFJ_diff_rast_00[1:8],as.data.frame(
  ifelse(((SPFJ_diff_rast_00$p.abundance.y>1.00)&(SPFJ_diff_rast_00$p.abundance.x>1.00)),
         (((SPFJ_diff_rast_00$p.abundance.y-SPFJ_diff_rast_00$p.abundance.x)/SPFJ_diff_rast_00$p.abundance.x)*100),
         (SPFJ_diff_rast_00$p.abundance.y-SPFJ_diff_rast_00$p.abundance.x))))
names(SPFJ_diff_rast_00)[9]<-"p.abundance"
SPFJ_diff_rast_00.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFJ_diff_rast_00
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFJ_diff_rast_00.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMJ_diff_rast_00<-merge(SPMJ2000,SPMJew2000,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#SPMJ_diff_rast_00<- cbind(SPMJ_diff_rast_00[1:8],as.data.frame(((SPMJ_diff_rast_00$p.abundance.y-SPMJ_diff_rast_00$p.abundance.x)/SPMJ_diff_rast_00$p.abundance.x)*100))
SPMJ_diff_rast_00<- cbind(SPMJ_diff_rast_00[1:8],as.data.frame(
  ifelse(((SPMJ_diff_rast_00$p.abundance.y>1.00)&(SPMJ_diff_rast_00$p.abundance.x>1.00)),
         (((SPMJ_diff_rast_00$p.abundance.y-SPMJ_diff_rast_00$p.abundance.x)/SPMJ_diff_rast_00$p.abundance.x)*100),
         (SPMJ_diff_rast_00$p.abundance.y-SPMJ_diff_rast_00$p.abundance.x))))
names(SPMJ_diff_rast_00)[9]<-"p.abundance"
SPMJ_diff_rast_00.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMJ_diff_rast_00
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMJ_diff_rast_00.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFA_diff_rast_00<-merge(SPFA2000,SPFAew2000,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#SPFA_diff_rast_00<- cbind(SPFA_diff_rast_00[1:8],as.data.frame(((SPFA_diff_rast_00$p.abundance.y-SPFA_diff_rast_00$p.abundance.x)/SPFA_diff_rast_00$p.abundance.x)*100))
SPFA_diff_rast_00<- cbind(SPFA_diff_rast_00[1:8],as.data.frame(
  ifelse(((SPFA_diff_rast_00$p.abundance.y>1.00)&(SPFA_diff_rast_00$p.abundance.x>1.00)),
         (((SPFA_diff_rast_00$p.abundance.y-SPFA_diff_rast_00$p.abundance.x)/SPFA_diff_rast_00$p.abundance.x)*100),
         (SPFA_diff_rast_00$p.abundance.y-SPFA_diff_rast_00$p.abundance.x))))
names(SPFA_diff_rast_00)[9]<-"p.abundance"
SPFA_diff_rast_00.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFA_diff_rast_00
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFA_diff_rast_00.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMA_diff_rast_00<-merge(SPMA2000,SPMAew2000,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#SPMA_diff_rast_00<- cbind(SPMA_diff_rast_00[1:8],as.data.frame(((SPMA_diff_rast_00$p.abundance.y-SPMA_diff_rast_00$p.abundance.x)/SPMA_diff_rast_00$p.abundance.x)*100))
SPMA_diff_rast_00<- cbind(SPMA_diff_rast_00[1:8],as.data.frame(
  ifelse(((SPMA_diff_rast_00$p.abundance.y>1.00)&(SPMA_diff_rast_00$p.abundance.x>1.00)),
         (((SPMA_diff_rast_00$p.abundance.y-SPMA_diff_rast_00$p.abundance.x)/SPMA_diff_rast_00$p.abundance.x)*100),
         (SPMA_diff_rast_00$p.abundance.y-SPMA_diff_rast_00$p.abundance.x))))
names(SPMA_diff_rast_00)[9]<-"p.abundance"
SPMA_diff_rast_00.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMA_diff_rast_00
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMA_diff_rast_00.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}
###2006####
FLFJ_diff_rast_06<-merge(FLFJ2006,FLFJew2006,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#FLFJ_diff_rast_06<- cbind(FLFJ_diff_rast_06[1:8],as.data.frame(((FLFJ_diff_rast_06$p.abundance.y-FLFJ_diff_rast_06$p.abundance.x)/FLFJ_diff_rast_06$p.abundance.x)*100))
FLFJ_diff_rast_06<- cbind(FLFJ_diff_rast_06[1:8],as.data.frame(
  ifelse(((FLFJ_diff_rast_06$p.abundance.y>1.00)&(FLFJ_diff_rast_06$p.abundance.x>1.00)),
         (((FLFJ_diff_rast_06$p.abundance.y-FLFJ_diff_rast_06$p.abundance.x)/FLFJ_diff_rast_06$p.abundance.x)*100),
         (FLFJ_diff_rast_06$p.abundance.y-FLFJ_diff_rast_06$p.abundance.x))))
names(FLFJ_diff_rast_06)[9]<-"p.abundance"
FLFJ_diff_rast_06.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFJ_diff_rast_06
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFJ_diff_rast_06.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMJ_diff_rast_06<-merge(FLMJ2006,FLMJew2006,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#FLMJ_diff_rast_06<- cbind(FLMJ_diff_rast_06[1:8],as.data.frame(((FLMJ_diff_rast_06$p.abundance.y-FLMJ_diff_rast_06$p.abundance.x)/FLMJ_diff_rast_06$p.abundance.x)*100))
FLMJ_diff_rast_06<- cbind(FLMJ_diff_rast_06[1:8],as.data.frame(
  ifelse(((FLMJ_diff_rast_06$p.abundance.y>1.00)&(FLMJ_diff_rast_06$p.abundance.x>1.00)),
         (((FLMJ_diff_rast_06$p.abundance.y-FLMJ_diff_rast_06$p.abundance.x)/FLMJ_diff_rast_06$p.abundance.x)*100),
         (FLMJ_diff_rast_06$p.abundance.y-FLMJ_diff_rast_06$p.abundance.x))))
names(FLMJ_diff_rast_06)[9]<-"p.abundance"
FLMJ_diff_rast_06.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMJ_diff_rast_06
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMJ_diff_rast_06.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLFA_diff_rast_06<-merge(FLFA2006,FLFAew2006,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#FLFA_diff_rast_06<- cbind(FLFA_diff_rast_06[1:8],as.data.frame(((FLFA_diff_rast_06$p.abundance.y-FLFA_diff_rast_06$p.abundance.x)/FLFA_diff_rast_06$p.abundance.x)*100))
FLFA_diff_rast_06<- cbind(FLFA_diff_rast_06[1:8],as.data.frame(
  ifelse(((FLFA_diff_rast_06$p.abundance.y>1.00)&(FLFA_diff_rast_06$p.abundance.x>1.00)),
         (((FLFA_diff_rast_06$p.abundance.y-FLFA_diff_rast_06$p.abundance.x)/FLFA_diff_rast_06$p.abundance.x)*100),
         (FLFA_diff_rast_06$p.abundance.y-FLFA_diff_rast_06$p.abundance.x))))
names(FLFA_diff_rast_06)[9]<-"p.abundance"
FLFA_diff_rast_06.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFA_diff_rast_06
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFA_diff_rast_06.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMA_diff_rast_06<-merge(FLMA2006,FLMAew2006,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#FLMA_diff_rast_06<- cbind(FLMA_diff_rast_06[1:8],as.data.frame(((FLMA_diff_rast_06$p.abundance.y-FLMA_diff_rast_06$p.abundance.x)/FLMA_diff_rast_06$p.abundance.x)*100))
FLMA_diff_rast_06<- cbind(FLMA_diff_rast_06[1:8],as.data.frame(
  ifelse(((FLMA_diff_rast_06$p.abundance.y>1.00)&(FLMA_diff_rast_06$p.abundance.x>1.00)),
         (((FLMA_diff_rast_06$p.abundance.y-FLMA_diff_rast_06$p.abundance.x)/FLMA_diff_rast_06$p.abundance.x)*100),
         (FLMA_diff_rast_06$p.abundance.y-FLMA_diff_rast_06$p.abundance.x))))
names(FLMA_diff_rast_06)[9]<-"p.abundance"
FLMA_diff_rast_06.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMA_diff_rast_06
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMA_diff_rast_06.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFJ_diff_rast_06<-merge(SPFJ2006,SPFJew2006,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#SPFJ_diff_rast_06<- cbind(SPFJ_diff_rast_06[1:8],as.data.frame(((SPFJ_diff_rast_06$p.abundance.y-SPFJ_diff_rast_06$p.abundance.x)/SPFJ_diff_rast_06$p.abundance.x)*100))
SPFJ_diff_rast_06<- cbind(SPFJ_diff_rast_06[1:8],as.data.frame(
  ifelse(((SPFJ_diff_rast_06$p.abundance.y>1.00)&(SPFJ_diff_rast_06$p.abundance.x>1.00)),
         (((SPFJ_diff_rast_06$p.abundance.y-SPFJ_diff_rast_06$p.abundance.x)/SPFJ_diff_rast_06$p.abundance.x)*100),
         (SPFJ_diff_rast_06$p.abundance.y-SPFJ_diff_rast_06$p.abundance.x))))
names(SPFJ_diff_rast_06)[9]<-"p.abundance"
SPFJ_diff_rast_06.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFJ_diff_rast_06
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFJ_diff_rast_06.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMJ_diff_rast_06<-merge(SPMJ2006,SPMJew2006,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#SPMJ_diff_rast_06<- cbind(SPMJ_diff_rast_06[1:8],as.data.frame(((SPMJ_diff_rast_06$p.abundance.y-SPMJ_diff_rast_06$p.abundance.x)/SPMJ_diff_rast_06$p.abundance.x)*100))
SPMJ_diff_rast_06<- cbind(SPMJ_diff_rast_06[1:8],as.data.frame(
  ifelse(((SPMJ_diff_rast_06$p.abundance.y>1.00)&(SPMJ_diff_rast_06$p.abundance.x>1.00)),
         (((SPMJ_diff_rast_06$p.abundance.y-SPMJ_diff_rast_06$p.abundance.x)/SPMJ_diff_rast_06$p.abundance.x)*100),
         (SPMJ_diff_rast_06$p.abundance.y-SPMJ_diff_rast_06$p.abundance.x))))
names(SPMJ_diff_rast_06)[9]<-"p.abundance"
SPMJ_diff_rast_06.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMJ_diff_rast_06
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMJ_diff_rast_06.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFA_diff_rast_06<-merge(SPFA2006,SPFAew2006,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#SPFA_diff_rast_06<- cbind(SPFA_diff_rast_06[1:8],as.data.frame(((SPFA_diff_rast_06$p.abundance.y-SPFA_diff_rast_06$p.abundance.x)/SPFA_diff_rast_06$p.abundance.x)*100))
SPFA_diff_rast_06<- cbind(SPFA_diff_rast_06[1:8],as.data.frame(
  ifelse(((SPFA_diff_rast_06$p.abundance.y>1.00)&(SPFA_diff_rast_06$p.abundance.x>1.00)),
         (((SPFA_diff_rast_06$p.abundance.y-SPFA_diff_rast_06$p.abundance.x)/SPFA_diff_rast_06$p.abundance.x)*100),
         (SPFA_diff_rast_06$p.abundance.y-SPFA_diff_rast_06$p.abundance.x))))
names(SPFA_diff_rast_06)[9]<-"p.abundance"
SPFA_diff_rast_06.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFA_diff_rast_06
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFA_diff_rast_06.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMA_diff_rast_06<-merge(SPMA2006,SPMAew2006,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#SPMA_diff_rast_06<- cbind(SPMA_diff_rast_06[1:8],as.data.frame(((SPMA_diff_rast_06$p.abundance.y-SPMA_diff_rast_06$p.abundance.x)/SPMA_diff_rast_06$p.abundance.x)*100))
SPMA_diff_rast_06<- cbind(SPMA_diff_rast_06[1:8],as.data.frame(
  ifelse(((SPMA_diff_rast_06$p.abundance.y>1.00)&(SPMA_diff_rast_06$p.abundance.x>1.00)),
         (((SPMA_diff_rast_06$p.abundance.y-SPMA_diff_rast_06$p.abundance.x)/SPMA_diff_rast_06$p.abundance.x)*100),
         (SPMA_diff_rast_06$p.abundance.y-SPMA_diff_rast_06$p.abundance.x))))
names(SPMA_diff_rast_06)[9]<-"p.abundance"
SPMA_diff_rast_06.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMA_diff_rast_06
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMA_diff_rast_06.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}
###2012####
FLFJ_diff_rast_12<-merge(FLFJ2012,FLFJew2012,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#FLFJ_diff_rast_12<- cbind(FLFJ_diff_rast_12[1:8],as.data.frame(((FLFJ_diff_rast_12$p.abundance.y-FLFJ_diff_rast_12$p.abundance.x)/FLFJ_diff_rast_12$p.abundance.x)*100))
FLFJ_diff_rast_12<- cbind(FLFJ_diff_rast_12[1:8],as.data.frame(
  ifelse(((FLFJ_diff_rast_12$p.abundance.y>1.00)&(FLFJ_diff_rast_12$p.abundance.x>1.00)),
         (((FLFJ_diff_rast_12$p.abundance.y-FLFJ_diff_rast_12$p.abundance.x)/FLFJ_diff_rast_12$p.abundance.x)*100),
         (FLFJ_diff_rast_12$p.abundance.y-FLFJ_diff_rast_12$p.abundance.x))))
names(FLFJ_diff_rast_12)[9]<-"p.abundance"
FLFJ_diff_rast_12.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFJ_diff_rast_12
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFJ_diff_rast_12.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMJ_diff_rast_12<-merge(FLMJ2012,FLMJew2012,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#FLMJ_diff_rast_12<- cbind(FLMJ_diff_rast_12[1:8],as.data.frame(((FLMJ_diff_rast_12$p.abundance.y-FLMJ_diff_rast_12$p.abundance.x)/FLMJ_diff_rast_12$p.abundance.x)*100))
FLMJ_diff_rast_12<- cbind(FLMJ_diff_rast_12[1:8],as.data.frame(
  ifelse(((FLMJ_diff_rast_12$p.abundance.y>1.00)&(FLMJ_diff_rast_12$p.abundance.x>1.00)),
         (((FLMJ_diff_rast_12$p.abundance.y-FLMJ_diff_rast_12$p.abundance.x)/FLMJ_diff_rast_12$p.abundance.x)*100),
         (FLMJ_diff_rast_12$p.abundance.y-FLMJ_diff_rast_12$p.abundance.x))))
names(FLMJ_diff_rast_12)[9]<-"p.abundance"
FLMJ_diff_rast_12.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMJ_diff_rast_12
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMJ_diff_rast_12.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLFA_diff_rast_12<-merge(FLFA2012,FLFAew2012,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#FLFA_diff_rast_12<- cbind(FLFA_diff_rast_12[1:8],as.data.frame(((FLFA_diff_rast_12$p.abundance.y-FLFA_diff_rast_12$p.abundance.x)/FLFA_diff_rast_12$p.abundance.x)*100))
FLFA_diff_rast_12<- cbind(FLFA_diff_rast_12[1:8],as.data.frame(
  ifelse(((FLFA_diff_rast_12$p.abundance.y>1.00)&(FLFA_diff_rast_12$p.abundance.x>1.00)),
         (((FLFA_diff_rast_12$p.abundance.y-FLFA_diff_rast_12$p.abundance.x)/FLFA_diff_rast_12$p.abundance.x)*100),
         (FLFA_diff_rast_12$p.abundance.y-FLFA_diff_rast_12$p.abundance.x))))
names(FLFA_diff_rast_12)[9]<-"p.abundance"
FLFA_diff_rast_12.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFA_diff_rast_12
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFA_diff_rast_12.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMA_diff_rast_12<-merge(FLMA2012,FLMAew2012,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#FLMA_diff_rast_12<- cbind(FLMA_diff_rast_12[1:8],as.data.frame(((FLMA_diff_rast_12$p.abundance.y-FLMA_diff_rast_12$p.abundance.x)/FLMA_diff_rast_12$p.abundance.x)*100))
FLMA_diff_rast_12<- cbind(FLMA_diff_rast_12[1:8],as.data.frame(
  ifelse(((FLMA_diff_rast_12$p.abundance.y>1.00)&(FLMA_diff_rast_12$p.abundance.x>1.00)),
         (((FLMA_diff_rast_12$p.abundance.y-FLMA_diff_rast_12$p.abundance.x)/FLMA_diff_rast_12$p.abundance.x)*100),
         (FLMA_diff_rast_12$p.abundance.y-FLMA_diff_rast_12$p.abundance.x))))
names(FLMA_diff_rast_12)[9]<-"p.abundance"
FLMA_diff_rast_12.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMA_diff_rast_12
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMA_diff_rast_12.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFJ_diff_rast_12<-merge(SPFJ2012,SPFJew2012,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#SPFJ_diff_rast_12<- cbind(SPFJ_diff_rast_12[1:8],as.data.frame(((SPFJ_diff_rast_12$p.abundance.y-SPFJ_diff_rast_12$p.abundance.x)/SPFJ_diff_rast_12$p.abundance.x)*100))
SPFJ_diff_rast_12<- cbind(SPFJ_diff_rast_12[1:8],as.data.frame(
  ifelse(((SPFJ_diff_rast_12$p.abundance.y>1.00)&(SPFJ_diff_rast_12$p.abundance.x>1.00)),
         (((SPFJ_diff_rast_12$p.abundance.y-SPFJ_diff_rast_12$p.abundance.x)/SPFJ_diff_rast_12$p.abundance.x)*100),
         (SPFJ_diff_rast_12$p.abundance.y-SPFJ_diff_rast_12$p.abundance.x))))
names(SPFJ_diff_rast_12)[9]<-"p.abundance"
SPFJ_diff_rast_12.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFJ_diff_rast_12
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFJ_diff_rast_12.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMJ_diff_rast_12<-merge(SPMJ2012,SPMJew2012,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#SPMJ_diff_rast_12<- cbind(SPMJ_diff_rast_12[1:8],as.data.frame(((SPMJ_diff_rast_12$p.abundance.y-SPMJ_diff_rast_12$p.abundance.x)/SPMJ_diff_rast_12$p.abundance.x)*100))
SPMJ_diff_rast_12<- cbind(SPMJ_diff_rast_12[1:8],as.data.frame(
  ifelse(((SPMJ_diff_rast_12$p.abundance.y>1.00)&(SPMJ_diff_rast_12$p.abundance.x>1.00)),
         (((SPMJ_diff_rast_12$p.abundance.y-SPMJ_diff_rast_12$p.abundance.x)/SPMJ_diff_rast_12$p.abundance.x)*100),
         (SPMJ_diff_rast_12$p.abundance.y-SPMJ_diff_rast_12$p.abundance.x))))
names(SPMJ_diff_rast_12)[9]<-"p.abundance"
SPMJ_diff_rast_12.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMJ_diff_rast_12
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMJ_diff_rast_12.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFA_diff_rast_12<-merge(SPFA2012,SPFAew2012,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#SPFA_diff_rast_12<- cbind(SPFA_diff_rast_12[1:8],as.data.frame(((SPFA_diff_rast_12$p.abundance.y-SPFA_diff_rast_12$p.abundance.x)/SPFA_diff_rast_12$p.abundance.x)*100))
SPFA_diff_rast_12<- cbind(SPFA_diff_rast_12[1:8],as.data.frame(
  ifelse(((SPFA_diff_rast_12$p.abundance.y>1.00)&(SPFA_diff_rast_12$p.abundance.x>1.00)),
         (((SPFA_diff_rast_12$p.abundance.y-SPFA_diff_rast_12$p.abundance.x)/SPFA_diff_rast_12$p.abundance.x)*100),
         (SPFA_diff_rast_12$p.abundance.y-SPFA_diff_rast_12$p.abundance.x))))
names(SPFA_diff_rast_12)[9]<-"p.abundance"
SPFA_diff_rast_12.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFA_diff_rast_12
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFA_diff_rast_12.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMA_diff_rast_12<-merge(SPMA2012,SPMAew2012,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#SPMA_diff_rast_12<- cbind(SPMA_diff_rast_12[1:8],as.data.frame(((SPMA_diff_rast_12$p.abundance.y-SPMA_diff_rast_12$p.abundance.x)/SPMA_diff_rast_12$p.abundance.x)*100))
SPMA_diff_rast_12<- cbind(SPMA_diff_rast_12[1:8],as.data.frame(
  ifelse(((SPMA_diff_rast_12$p.abundance.y>1.00)&(SPMA_diff_rast_12$p.abundance.x>1.00)),
         (((SPMA_diff_rast_12$p.abundance.y-SPMA_diff_rast_12$p.abundance.x)/SPMA_diff_rast_12$p.abundance.x)*100),
         (SPMA_diff_rast_12$p.abundance.y-SPMA_diff_rast_12$p.abundance.x))))
names(SPMA_diff_rast_12)[9]<-"p.abundance"
SPMA_diff_rast_12.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMA_diff_rast_12
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMA_diff_rast_12.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}
###2017####
FLFJ_diff_rast_17<-merge(FLFJ2017,FLFJew2017,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#FLFJ_diff_rast_17<- cbind(FLFJ_diff_rast_17[1:8],as.data.frame(((FLFJ_diff_rast_17$p.abundance.y-FLFJ_diff_rast_17$p.abundance.x)/FLFJ_diff_rast_17$p.abundance.x)*100))
FLFJ_diff_rast_17<- cbind(FLFJ_diff_rast_17[1:8],as.data.frame(
  ifelse(((FLFJ_diff_rast_17$p.abundance.y>1.00)&(FLFJ_diff_rast_17$p.abundance.x>1.00)),
         (((FLFJ_diff_rast_17$p.abundance.y-FLFJ_diff_rast_17$p.abundance.x)/FLFJ_diff_rast_17$p.abundance.x)*100),
         (FLFJ_diff_rast_17$p.abundance.y-FLFJ_diff_rast_17$p.abundance.x))))
names(FLFJ_diff_rast_17)[9]<-"p.abundance"
FLFJ_diff_rast_17.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFJ_diff_rast_17
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFJ_diff_rast_17.list[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

FLMJ_diff_rast_17<-merge(FLMJ2017,FLMJew2017,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#FLMJ_diff_rast_17<- cbind(FLMJ_diff_rast_17[1:8],as.data.frame(((FLMJ_diff_rast_17$p.abundance.y-FLMJ_diff_rast_17$p.abundance.x)/FLMJ_diff_rast_17$p.abundance.x)*100))
FLMJ_diff_rast_17<- cbind(FLMJ_diff_rast_17[1:8],as.data.frame(
  ifelse(((FLMJ_diff_rast_17$p.abundance.y>1.00)&(FLMJ_diff_rast_17$p.abundance.x>1.00)),
         (((FLMJ_diff_rast_17$p.abundance.y-FLMJ_diff_rast_17$p.abundance.x)/FLMJ_diff_rast_17$p.abundance.x)*100),
         (FLMJ_diff_rast_17$p.abundance.y-FLMJ_diff_rast_17$p.abundance.x))))
names(FLMJ_diff_rast_17)[9]<-"p.abundance"
FLMJ_diff_rast_17.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMJ_diff_rast_17
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMJ_diff_rast_17.list[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

FLFA_diff_rast_17<-merge(FLFA2017,FLFAew2017,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#FLFA_diff_rast_17<- cbind(FLFA_diff_rast_17[1:8],as.data.frame(((FLFA_diff_rast_17$p.abundance.y-FLFA_diff_rast_17$p.abundance.x)/FLFA_diff_rast_17$p.abundance.x)*100))
FLFA_diff_rast_17<- cbind(FLFA_diff_rast_17[1:8],as.data.frame(
  ifelse(((FLFA_diff_rast_17$p.abundance.y>1.00)&(FLFA_diff_rast_17$p.abundance.x>1.00)),
         (((FLFA_diff_rast_17$p.abundance.y-FLFA_diff_rast_17$p.abundance.x)/FLFA_diff_rast_17$p.abundance.x)*100),
         (FLFA_diff_rast_17$p.abundance.y-FLFA_diff_rast_17$p.abundance.x))))
names(FLFA_diff_rast_17)[9]<-"p.abundance"
FLFA_diff_rast_17.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFA_diff_rast_17
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFA_diff_rast_17.list[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

FLMA_diff_rast_17<-merge(FLMA2017,FLMAew2017,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#FLMA_diff_rast_17<- cbind(FLMA_diff_rast_17[1:8],as.data.frame(((FLMA_diff_rast_17$p.abundance.y-FLMA_diff_rast_17$p.abundance.x)/FLMA_diff_rast_17$p.abundance.x)*100))
FLMA_diff_rast_17<- cbind(FLMA_diff_rast_17[1:8],as.data.frame(
  ifelse(((FLMA_diff_rast_17$p.abundance.y>1.00)&(FLMA_diff_rast_17$p.abundance.x>1.00)),
         (((FLMA_diff_rast_17$p.abundance.y-FLMA_diff_rast_17$p.abundance.x)/FLMA_diff_rast_17$p.abundance.x)*100),
         (FLMA_diff_rast_17$p.abundance.y-FLMA_diff_rast_17$p.abundance.x))))
names(FLMA_diff_rast_17)[9]<-"p.abundance"
FLMA_diff_rast_17.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMA_diff_rast_17
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMA_diff_rast_17.list[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

SPFJ_diff_rast_17<-merge(SPFJ2017,SPFJew2017,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#SPFJ_diff_rast_17<- cbind(SPFJ_diff_rast_17[1:8],as.data.frame(((SPFJ_diff_rast_17$p.abundance.y-SPFJ_diff_rast_17$p.abundance.x)/SPFJ_diff_rast_17$p.abundance.x)*100))
SPFJ_diff_rast_17<- cbind(SPFJ_diff_rast_17[1:8],as.data.frame(
  ifelse(((SPFJ_diff_rast_17$p.abundance.y>1.00)&(SPFJ_diff_rast_17$p.abundance.x>1.00)),
         (((SPFJ_diff_rast_17$p.abundance.y-SPFJ_diff_rast_17$p.abundance.x)/SPFJ_diff_rast_17$p.abundance.x)*100),
         (SPFJ_diff_rast_17$p.abundance.y-SPFJ_diff_rast_17$p.abundance.x))))
names(SPFJ_diff_rast_17)[9]<-"p.abundance"
SPFJ_diff_rast_17.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFJ_diff_rast_17
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFJ_diff_rast_17.list[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

SPMJ_diff_rast_17<-merge(SPMJ2017,SPMJew2017,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#SPMJ_diff_rast_17<- cbind(SPMJ_diff_rast_17[1:8],as.data.frame(((SPMJ_diff_rast_17$p.abundance.y-SPMJ_diff_rast_17$p.abundance.x)/SPMJ_diff_rast_17$p.abundance.x)*100))
SPMJ_diff_rast_17<- cbind(SPMJ_diff_rast_17[1:8],as.data.frame(
  ifelse(((SPMJ_diff_rast_17$p.abundance.y>1.00)&(SPMJ_diff_rast_17$p.abundance.x>1.00)),
         (((SPMJ_diff_rast_17$p.abundance.y-SPMJ_diff_rast_17$p.abundance.x)/SPMJ_diff_rast_17$p.abundance.x)*100),
         (SPMJ_diff_rast_17$p.abundance.y-SPMJ_diff_rast_17$p.abundance.x))))
names(SPMJ_diff_rast_17)[9]<-"p.abundance"
SPMJ_diff_rast_17.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMJ_diff_rast_17
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMJ_diff_rast_17.list[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

SPFA_diff_rast_17<-merge(SPFA2017,SPFAew2017,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#SPFA_diff_rast_17<- cbind(SPFA_diff_rast_17[1:8],as.data.frame(((SPFA_diff_rast_17$p.abundance.y-SPFA_diff_rast_17$p.abundance.x)/SPFA_diff_rast_17$p.abundance.x)*100))
SPFA_diff_rast_17<- cbind(SPFA_diff_rast_17[1:8],as.data.frame(
  ifelse(((SPFA_diff_rast_17$p.abundance.y>1.00)&(SPFA_diff_rast_17$p.abundance.x>1.00)),
         (((SPFA_diff_rast_17$p.abundance.y-SPFA_diff_rast_17$p.abundance.x)/SPFA_diff_rast_17$p.abundance.x)*100),
         (SPFA_diff_rast_17$p.abundance.y-SPFA_diff_rast_17$p.abundance.x))))
names(SPFA_diff_rast_17)[9]<-"p.abundance"
SPFA_diff_rast_17.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFA_diff_rast_17
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFA_diff_rast_17.list[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

SPMA_diff_rast_17<-merge(SPMA2017,SPMAew2017,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#SPMA_diff_rast_17<- cbind(SPMA_diff_rast_17[1:8],as.data.frame(((SPMA_diff_rast_17$p.abundance.y-SPMA_diff_rast_17$p.abundance.x)/SPMA_diff_rast_17$p.abundance.x)*100))
SPMA_diff_rast_17<- cbind(SPMA_diff_rast_17[1:8],as.data.frame(
  ifelse(((SPMA_diff_rast_17$p.abundance.y>1.00)&(SPMA_diff_rast_17$p.abundance.x>1.00)),
         (((SPMA_diff_rast_17$p.abundance.y-SPMA_diff_rast_17$p.abundance.x)/SPMA_diff_rast_17$p.abundance.x)*100),
         (SPMA_diff_rast_17$p.abundance.y-SPMA_diff_rast_17$p.abundance.x))))
names(SPMA_diff_rast_17)[9]<-"p.abundance"
SPMA_diff_rast_17.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMA_diff_rast_17
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMA_diff_rast_17.list[[i]] <- raster::extract(rast, depth_grid_plot2017)
}
#####
  plotreldiff<-function(rasterdata, month,year,size,depth_grid_plot){
   
    par(mar=c(2,2,0,0), mfrow=c(1,1))
    plotvar=unlist(rasterdata)
    nclr=10
    plotclr <- rev(brewer.pal(nclr,"RdYlBu"))
    class <- classIntervals(plotvar, nclr, style="quantile")
    fix_break<-round(class$brks, digits = 2)
    for(i in 1:length(rasterdata)){
      print(i)
      plotvar <- rasterdata[[i]]
      nclr=10
      plotclr <- rev(brewer.pal(nclr,"RdYlBu"))
      class <- classIntervals(plotvar, nclr, style="fixed", 
                              fixedBreaks=
                                if(size=="J")c(-200,-100,-50,-25,-15,-5,5,15,25,50,100,200,500) #breaks calculated by using average nonstationary "quantile" breaks
                              else c(-200,-100,-50,-25,-15,-5,5,15,25,50,100,200,500))
                                
                                #if(size=="J")c(-100,-98,-90,-75,-50,-25,0,25,50,75,100) #breaks calculated by using average nonstationary "quantile" breaks
                              #else c(-100,-98,-90,-75,-50,-25,0,25,50,75,100)) #breaks calculated by using average "qauntile" breaks
      
      colcode <- findColours(class, plotclr)
      
      start_x <- range(GAM_data$Longitude)[1]
      end_x <- range(GAM_data$Longitude)[2]
      start_y <- range(GAM_data$Latitude)[1]
      end_y <- range(GAM_data$Latitude)[2]
      plot(depth_grid_plot$lon, depth_grid_plot$lat, pch=16, col=colcode, cex=0.1, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, xlab="Longitude", ylab="Latitude")
      map(database= mainecoast, ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE, lwd=0.1)
      #plot(sa511_513, add=T)
      box()
      legend("topleft", paste(year, "-", month, sep=""), bty="n", cex=1.5)
      legend("bottomright", legend=c(names(attr(colcode, "table"))), fill=c(attr(colcode, "palette")), cex=0.9,bg="white",
             bty = "o", title="Relative Difference %")
      axis(1, cex=0.5)
      axis(2, cex=0.5)
    }
    #dev.off()
  }

plotreldiff(FLFJ_diff_rast_00.list,"FLFJ","2000","J",depth_grid_plot000612)
plotreldiff(FLMJ_diff_rast_00.list,"FLMJ","2000","J",depth_grid_plot000612)
plotreldiff(FLFA_diff_rast_00.list,"FLFA","2000","A",depth_grid_plot000612)
plotreldiff(FLMA_diff_rast_00.list,"FLMA","2000","A",depth_grid_plot000612)
plotreldiff(SPFJ_diff_rast_00.list,"SPFJ","2000","J",depth_grid_plot000612)
plotreldiff(SPMJ_diff_rast_00.list,"SPMJ","2000","J",depth_grid_plot000612)
plotreldiff(SPFA_diff_rast_00.list,"SPFA","2000","A",depth_grid_plot000612)
plotreldiff(SPMA_diff_rast_00.list,"SPMA","2000","A",depth_grid_plot000612)

plotreldiff(FLFJ_diff_rast_06.list,"FLFJ","2006","J",depth_grid_plot000612)
plotreldiff(FLMJ_diff_rast_06.list,"FLMJ","2006","J",depth_grid_plot000612)
plotreldiff(FLFA_diff_rast_06.list,"FLFA","2006","A",depth_grid_plot000612)
plotreldiff(FLMA_diff_rast_06.list,"FLMA","2006","A",depth_grid_plot000612)
plotreldiff(SPFJ_diff_rast_06.list,"SPFJ","2006","J",depth_grid_plot000612)
plotreldiff(SPMJ_diff_rast_06.list,"SPMJ","2006","J",depth_grid_plot000612)
plotreldiff(SPFA_diff_rast_06.list,"SPFA","2006","A",depth_grid_plot000612)
plotreldiff(SPMA_diff_rast_06.list,"SPMA","2006","A",depth_grid_plot000612)

plotreldiff(FLFJ_diff_rast_12.list,"FLFJ","2012","J",depth_grid_plot000612)
plotreldiff(FLMJ_diff_rast_12.list,"FLMJ","2012","J",depth_grid_plot000612)
plotreldiff(FLFA_diff_rast_12.list,"FLFA","2012","A",depth_grid_plot000612)
plotreldiff(FLMA_diff_rast_12.list,"FLMA","2012","A",depth_grid_plot000612)
plotreldiff(SPFJ_diff_rast_12.list,"SPFJ","2012","J",depth_grid_plot000612)
plotreldiff(SPMJ_diff_rast_12.list,"SPMJ","2012","J",depth_grid_plot000612)
plotreldiff(SPFA_diff_rast_12.list,"SPFA","2012","A",depth_grid_plot000612)
plotreldiff(SPMA_diff_rast_12.list,"SPMA","2012","A",depth_grid_plot000612)

plotreldiff(FLFJ_diff_rast_17.list,"FLFJ","2017","J",depth_grid_plot2017)
plotreldiff(FLMJ_diff_rast_17.list,"FLMJ","2017","J",depth_grid_plot2017)
plotreldiff(FLFA_diff_rast_17.list,"FLFA","2017","A",depth_grid_plot2017)
plotreldiff(FLMA_diff_rast_17.list,"FLMA","2017","A",depth_grid_plot2017)
plotreldiff(SPFJ_diff_rast_17.list,"SPFJ","2017","J",depth_grid_plot2017)
plotreldiff(SPMJ_diff_rast_17.list,"SPMJ","2017","J",depth_grid_plot2017)
plotreldiff(SPFA_diff_rast_17.list,"SPFA","2017","A",depth_grid_plot2017)
plotreldiff(SPMA_diff_rast_17.list,"SPMA","2017","A",depth_grid_plot2017)
####Min Max#### 
library(huxtable)
table1reldiff00 <- hux(
  Group = c('FLFJ', 'FLMJ', 'FLFA','FLMA','SPFJ', 'SPMJ', 'SPFA','SPMA'),
  Max = c(max(na.omit(as.data.frame(FLFJ_diff_rast_00.list[[1]]))), max(na.omit(as.data.frame(FLMJ_diff_rast_00.list[[1]]))), 
          max(na.omit(as.data.frame(FLFA_diff_rast_00.list[[1]]))),
                max(na.omit(as.data.frame(FLMA_diff_rast_00.list[[1]]))),max(na.omit(as.data.frame(SPFJ_diff_rast_00.list[[1]]))), 
                max(na.omit(as.data.frame(SPMJ_diff_rast_00.list[[1]]))), max(na.omit(as.data.frame(SPFA_diff_rast_00.list[[1]]))),
                max(na.omit(as.data.frame(SPMA_diff_rast_00.list[[1]])))),
Min = c(min(na.omit(as.data.frame(FLFJ_diff_rast_00.list[[1]]))), min(na.omit(as.data.frame(FLMJ_diff_rast_00.list[[1]]))), 
        min(na.omit(as.data.frame(FLFA_diff_rast_00.list[[1]]))),
        min(na.omit(as.data.frame(FLMA_diff_rast_00.list[[1]]))),min(na.omit(as.data.frame(SPFJ_diff_rast_00.list[[1]]))), 
        min(na.omit(as.data.frame(SPMJ_diff_rast_00.list[[1]]))), min(na.omit(as.data.frame(SPFA_diff_rast_00.list[[1]]))),
        min(na.omit(as.data.frame(SPMA_diff_rast_00.list[[1]])))),
Mean = c(mean(FLFJ_diff_rast_00.list[[1]],na.rm=TRUE), mean(FLMJ_diff_rast_00.list[[1]],na.rm=TRUE), 
         mean(FLFA_diff_rast_00.list[[1]],na.rm=TRUE),
         mean(FLMA_diff_rast_00.list[[1]],na.rm=TRUE),mean(SPFJ_diff_rast_00.list[[1]],na.rm=TRUE), 
         mean(SPMJ_diff_rast_00.list[[1]],na.rm=TRUE), mean(SPFA_diff_rast_00.list[[1]],na.rm=TRUE),
         mean(SPMA_diff_rast_00.list[[1]],na.rm=TRUE)),
Med = c(median(FLFJ_diff_rast_00.list[[1]],na.rm=TRUE), median(FLMJ_diff_rast_00.list[[1]],na.rm=TRUE), 
        median(FLFA_diff_rast_00.list[[1]],na.rm=TRUE),
        median(FLMA_diff_rast_00.list[[1]],na.rm=TRUE),median(SPFJ_diff_rast_00.list[[1]],na.rm=TRUE), 
        median(SPMJ_diff_rast_00.list[[1]],na.rm=TRUE), median(SPFA_diff_rast_00.list[[1]],na.rm=TRUE),
        median(SPMA_diff_rast_00.list[[1]],na.rm=TRUE)),add_colnames = TRUE)

bold(table1reldiff00)[1,]           <- TRUE
bottom_border(table1reldiff00)[1,]  <- 0.4
align(table1reldiff00)[,2]          <- 'right'
right_padding(table1reldiff00)      <- 10
left_padding(table1reldiff00)       <- 10
width(table1reldiff00)              <- 0.30
number_format(table1reldiff00)      <- 2

table1reldiff00

table1reldiff06 <- hux(
  Group = c('FLFJ', 'FLMJ', 'FLFA','FLMA','SPFJ', 'SPMJ', 'SPFA','SPMA'),
  Max = c(max(na.omit(as.data.frame(FLFJ_diff_rast_06.list[[1]]))), max(na.omit(as.data.frame(FLMJ_diff_rast_06.list[[1]]))), 
          max(na.omit(as.data.frame(FLFA_diff_rast_06.list[[1]]))),
          max(na.omit(as.data.frame(FLMA_diff_rast_06.list[[1]]))),max(na.omit(as.data.frame(SPFJ_diff_rast_06.list[[1]]))), 
          max(na.omit(as.data.frame(SPMJ_diff_rast_06.list[[1]]))), max(na.omit(as.data.frame(SPFA_diff_rast_06.list[[1]]))),
          max(na.omit(as.data.frame(SPMA_diff_rast_06.list[[1]])))),
  Min = c(min(na.omit(as.data.frame(FLFJ_diff_rast_06.list[[1]]))), min(na.omit(as.data.frame(FLMJ_diff_rast_06.list[[1]]))), 
          min(na.omit(as.data.frame(FLFA_diff_rast_06.list[[1]]))),
          min(na.omit(as.data.frame(FLMA_diff_rast_06.list[[1]]))),min(na.omit(as.data.frame(SPFJ_diff_rast_06.list[[1]]))), 
          min(na.omit(as.data.frame(SPMJ_diff_rast_06.list[[1]]))), min(na.omit(as.data.frame(SPFA_diff_rast_06.list[[1]]))),
          min(na.omit(as.data.frame(SPMA_diff_rast_06.list[[1]])))),
  Mean = c(mean(FLFJ_diff_rast_06.list[[1]],na.rm=TRUE), mean(FLMJ_diff_rast_06.list[[1]],na.rm=TRUE), 
           mean(FLFA_diff_rast_06.list[[1]],na.rm=TRUE),
           mean(FLMA_diff_rast_06.list[[1]],na.rm=TRUE),mean(SPFJ_diff_rast_06.list[[1]],na.rm=TRUE), 
           mean(SPMJ_diff_rast_06.list[[1]],na.rm=TRUE), mean(SPFA_diff_rast_06.list[[1]],na.rm=TRUE),
           mean(SPMA_diff_rast_06.list[[1]],na.rm=TRUE)),
  Med = c(median(FLFJ_diff_rast_06.list[[1]],na.rm=TRUE), median(FLMJ_diff_rast_06.list[[1]],na.rm=TRUE), 
          median(FLFA_diff_rast_06.list[[1]],na.rm=TRUE),
          median(FLMA_diff_rast_06.list[[1]],na.rm=TRUE),median(SPFJ_diff_rast_06.list[[1]],na.rm=TRUE), 
          median(SPMJ_diff_rast_06.list[[1]],na.rm=TRUE), median(SPFA_diff_rast_06.list[[1]],na.rm=TRUE),
          median(SPMA_diff_rast_06.list[[1]],na.rm=TRUE)),add_colnames = TRUE)

bold(table1reldiff06)[1,]           <- TRUE
bottom_border(table1reldiff06)[1,]  <- 0.4
align(table1reldiff06)[,2]          <- 'right'
right_padding(table1reldiff06)      <- 10
left_padding(table1reldiff06)       <- 10
width(table1reldiff06)              <- 0.30
number_format(table1reldiff06)      <- 2

table1reldiff06

table1reldiff12 <- hux(
  Group = c('FLFJ', 'FLMJ', 'FLFA','FLMA','SPFJ', 'SPMJ', 'SPFA','SPMA'),
  Max = c(max(na.omit(as.data.frame(FLFJ_diff_rast_12.list[[1]]))), max(na.omit(as.data.frame(FLMJ_diff_rast_12.list[[1]]))), 
          max(na.omit(as.data.frame(FLFA_diff_rast_12.list[[1]]))),
          max(na.omit(as.data.frame(FLMA_diff_rast_12.list[[1]]))),max(na.omit(as.data.frame(SPFJ_diff_rast_12.list[[1]]))), 
          max(na.omit(as.data.frame(SPMJ_diff_rast_12.list[[1]]))), max(na.omit(as.data.frame(SPFA_diff_rast_12.list[[1]]))),
          max(na.omit(as.data.frame(SPMA_diff_rast_12.list[[1]])))),
  Min = c(min(na.omit(as.data.frame(FLFJ_diff_rast_12.list[[1]]))), min(na.omit(as.data.frame(FLMJ_diff_rast_12.list[[1]]))), 
          min(na.omit(as.data.frame(FLFA_diff_rast_12.list[[1]]))),
          min(na.omit(as.data.frame(FLMA_diff_rast_12.list[[1]]))),min(na.omit(as.data.frame(SPFJ_diff_rast_12.list[[1]]))), 
          min(na.omit(as.data.frame(SPMJ_diff_rast_12.list[[1]]))), min(na.omit(as.data.frame(SPFA_diff_rast_12.list[[1]]))),
          min(na.omit(as.data.frame(SPMA_diff_rast_12.list[[1]])))),
  Mean = c(mean(FLFJ_diff_rast_12.list[[1]],na.rm=TRUE), mean(FLMJ_diff_rast_12.list[[1]],na.rm=TRUE), 
           mean(FLFA_diff_rast_12.list[[1]],na.rm=TRUE),
           mean(FLMA_diff_rast_12.list[[1]],na.rm=TRUE),mean(SPFJ_diff_rast_12.list[[1]],na.rm=TRUE), 
           mean(SPMJ_diff_rast_12.list[[1]],na.rm=TRUE), mean(SPFA_diff_rast_12.list[[1]],na.rm=TRUE),
           mean(SPMA_diff_rast_12.list[[1]],na.rm=TRUE)),
  Med = c(median(FLFJ_diff_rast_12.list[[1]],na.rm=TRUE), median(FLMJ_diff_rast_12.list[[1]],na.rm=TRUE), 
          median(FLFA_diff_rast_12.list[[1]],na.rm=TRUE),
          median(FLMA_diff_rast_12.list[[1]],na.rm=TRUE),median(SPFJ_diff_rast_12.list[[1]],na.rm=TRUE), 
          median(SPMJ_diff_rast_12.list[[1]],na.rm=TRUE), median(SPFA_diff_rast_12.list[[1]],na.rm=TRUE),
          median(SPMA_diff_rast_12.list[[1]],na.rm=TRUE)),add_colnames = TRUE)

bold(table1reldiff12)[1,]           <- TRUE
bottom_border(table1reldiff12)[1,]  <- 0.4
align(table1reldiff12)[,2]          <- 'right'
right_padding(table1reldiff12)      <- 10
left_padding(table1reldiff12)       <- 10
width(table1reldiff12)              <- 0.30
number_format(table1reldiff12)      <- 2

table1reldiff12

table1reldiff17 <- hux(
  Group = c('FLFJ', 'FLMJ', 'FLFA','FLMA','SPFJ', 'SPMJ', 'SPFA','SPMA'),
  Max = c(max(na.omit(as.data.frame(FLFJ_diff_rast_17.list[[1]]))), max(na.omit(as.data.frame(FLMJ_diff_rast_17.list[[1]]))), 
          max(na.omit(as.data.frame(FLFA_diff_rast_17.list[[1]]))),
          max(na.omit(as.data.frame(FLMA_diff_rast_17.list[[1]]))),max(na.omit(as.data.frame(SPFJ_diff_rast_17.list[[1]]))), 
          max(na.omit(as.data.frame(SPMJ_diff_rast_17.list[[1]]))), max(na.omit(as.data.frame(SPFA_diff_rast_17.list[[1]]))),
          max(na.omit(as.data.frame(SPMA_diff_rast_17.list[[1]])))),
  Min = c(min(na.omit(as.data.frame(FLFJ_diff_rast_17.list[[1]]))), min(na.omit(as.data.frame(FLMJ_diff_rast_17.list[[1]]))), 
          min(na.omit(as.data.frame(FLFA_diff_rast_17.list[[1]]))),
          min(na.omit(as.data.frame(FLMA_diff_rast_17.list[[1]]))),min(na.omit(as.data.frame(SPFJ_diff_rast_17.list[[1]]))), 
          min(na.omit(as.data.frame(SPMJ_diff_rast_17.list[[1]]))), min(na.omit(as.data.frame(SPFA_diff_rast_17.list[[1]]))),
          min(na.omit(as.data.frame(SPMA_diff_rast_17.list[[1]])))),
  Mean = c(mean(FLFJ_diff_rast_17.list[[1]],na.rm=TRUE), mean(FLMJ_diff_rast_17.list[[1]],na.rm=TRUE), 
           mean(FLFA_diff_rast_17.list[[1]],na.rm=TRUE),
           mean(FLMA_diff_rast_17.list[[1]],na.rm=TRUE),mean(SPFJ_diff_rast_17.list[[1]],na.rm=TRUE), 
           mean(SPMJ_diff_rast_17.list[[1]],na.rm=TRUE), mean(SPFA_diff_rast_17.list[[1]],na.rm=TRUE),
           mean(SPMA_diff_rast_17.list[[1]],na.rm=TRUE)),
  Med = c(median(FLFJ_diff_rast_17.list[[1]],na.rm=TRUE), median(FLMJ_diff_rast_17.list[[1]],na.rm=TRUE), 
          median(FLFA_diff_rast_17.list[[1]],na.rm=TRUE),
          median(FLMA_diff_rast_17.list[[1]],na.rm=TRUE),median(SPFJ_diff_rast_17.list[[1]],na.rm=TRUE), 
          median(SPMJ_diff_rast_17.list[[1]],na.rm=TRUE), median(SPFA_diff_rast_17.list[[1]],na.rm=TRUE),
          median(SPMA_diff_rast_17.list[[1]],na.rm=TRUE)),add_colnames = TRUE)

bold(table1reldiff17)[1,]           <- TRUE
bottom_border(table1reldiff17)[1,]  <- 0.4
align(table1reldiff17)[,2]          <- 'right'
right_padding(table1reldiff17)      <- 10
left_padding(table1reldiff17)       <- 10
width(table1reldiff17)              <- 0.30
number_format(table1reldiff17)      <- 2

table1reldiff17
  
#####Calculate and plot REGULAR differences between NSV1 and S predicted abundancies####

###2000####

FLFJ_REGdiff_rast_00<-merge(FLFJ2000,FLFJew2000,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
FLFJ_REGdiff_rast_00<- cbind(FLFJ_REGdiff_rast_00[1:8],as.data.frame((FLFJ_REGdiff_rast_00$p.abundance.y-FLFJ_REGdiff_rast_00$p.abundance.x)))
names(FLFJ_REGdiff_rast_00)[9]<-"p.abundance"
FLFJ_REGdiff_rast_00.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFJ_REGdiff_rast_00
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFJ_REGdiff_rast_00.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}


FLMJ_REGdiff_rast_00<-merge(FLMJ2000,FLMJew2000,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
FLMJ_REGdiff_rast_00<- cbind(FLMJ_REGdiff_rast_00[1:8],as.data.frame((FLMJ_REGdiff_rast_00$p.abundance.y-FLMJ_REGdiff_rast_00$p.abundance.x)))
names(FLMJ_REGdiff_rast_00)[9]<-"p.abundance"
FLMJ_REGdiff_rast_00.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMJ_REGdiff_rast_00
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMJ_REGdiff_rast_00.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLFA_REGdiff_rast_00<-merge(FLFA2000,FLFAew2000,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
FLFA_REGdiff_rast_00<- cbind(FLFA_REGdiff_rast_00[1:8],as.data.frame((FLFA_REGdiff_rast_00$p.abundance.y-FLFA_REGdiff_rast_00$p.abundance.x)))
names(FLFA_REGdiff_rast_00)[9]<-"p.abundance"
FLFA_REGdiff_rast_00.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFA_REGdiff_rast_00
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFA_REGdiff_rast_00.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMA_REGdiff_rast_00<-merge(FLMA2000,FLMAew2000,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
FLMA_REGdiff_rast_00<- cbind(FLMA_REGdiff_rast_00[1:8],as.data.frame((FLMA_REGdiff_rast_00$p.abundance.y-FLMA_REGdiff_rast_00$p.abundance.x)))
names(FLMA_REGdiff_rast_00)[9]<-"p.abundance"
FLMA_REGdiff_rast_00.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMA_REGdiff_rast_00
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMA_REGdiff_rast_00.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFJ_REGdiff_rast_00<-merge(SPFJ2000,SPFJew2000,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
SPFJ_REGdiff_rast_00<- cbind(SPFJ_REGdiff_rast_00[1:8],as.data.frame((SPFJ_REGdiff_rast_00$p.abundance.y-SPFJ_REGdiff_rast_00$p.abundance.x)))
names(SPFJ_REGdiff_rast_00)[9]<-"p.abundance"
SPFJ_REGdiff_rast_00.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFJ_REGdiff_rast_00
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFJ_REGdiff_rast_00.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMJ_REGdiff_rast_00<-merge(SPMJ2000,SPMJew2000,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
SPMJ_REGdiff_rast_00<- cbind(SPMJ_REGdiff_rast_00[1:8],as.data.frame((SPMJ_REGdiff_rast_00$p.abundance.y-SPMJ_REGdiff_rast_00$p.abundance.x)))
names(SPMJ_REGdiff_rast_00)[9]<-"p.abundance"
SPMJ_REGdiff_rast_00.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMJ_REGdiff_rast_00
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMJ_REGdiff_rast_00.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFA_REGdiff_rast_00<-merge(SPFA2000,SPFAew2000,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
SPFA_REGdiff_rast_00<- cbind(SPFA_REGdiff_rast_00[1:8],as.data.frame((SPFA_REGdiff_rast_00$p.abundance.y-SPFA_REGdiff_rast_00$p.abundance.x)))
names(SPFA_REGdiff_rast_00)[9]<-"p.abundance"
SPFA_REGdiff_rast_00.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFA_REGdiff_rast_00
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFA_REGdiff_rast_00.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMA_REGdiff_rast_00<-merge(SPMA2000,SPMAew2000,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
SPMA_REGdiff_rast_00<- cbind(SPMA_REGdiff_rast_00[1:8],as.data.frame((SPMA_REGdiff_rast_00$p.abundance.y-SPMA_REGdiff_rast_00$p.abundance.x)))
names(SPMA_REGdiff_rast_00)[9]<-"p.abundance"
SPMA_REGdiff_rast_00.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMA_REGdiff_rast_00
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMA_REGdiff_rast_00.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}
###2006####
FLFJ_REGdiff_rast_06<-merge(FLFJ2006,FLFJew2006,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
FLFJ_REGdiff_rast_06<- cbind(FLFJ_REGdiff_rast_06[1:8],as.data.frame((FLFJ_REGdiff_rast_06$p.abundance.y-FLFJ_REGdiff_rast_06$p.abundance.x)))
names(FLFJ_REGdiff_rast_06)[9]<-"p.abundance"
FLFJ_REGdiff_rast_06.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFJ_REGdiff_rast_06
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFJ_REGdiff_rast_06.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMJ_REGdiff_rast_06<-merge(FLMJ2006,FLMJew2006,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
FLMJ_REGdiff_rast_06<- cbind(FLMJ_REGdiff_rast_06[1:8],as.data.frame((FLMJ_REGdiff_rast_06$p.abundance.y-FLMJ_REGdiff_rast_06$p.abundance.x)))
names(FLMJ_REGdiff_rast_06)[9]<-"p.abundance"
FLMJ_REGdiff_rast_06.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMJ_REGdiff_rast_06
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMJ_REGdiff_rast_06.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLFA_REGdiff_rast_06<-merge(FLFA2006,FLFAew2006,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
FLFA_REGdiff_rast_06<- cbind(FLFA_REGdiff_rast_06[1:8],as.data.frame((FLFA_REGdiff_rast_06$p.abundance.y-FLFA_REGdiff_rast_06$p.abundance.x)))
names(FLFA_REGdiff_rast_06)[9]<-"p.abundance"
FLFA_REGdiff_rast_06.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFA_REGdiff_rast_06
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFA_REGdiff_rast_06.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMA_REGdiff_rast_06<-merge(FLMA2006,FLMAew2006,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
FLMA_REGdiff_rast_06<- cbind(FLMA_REGdiff_rast_06[1:8],as.data.frame((FLMA_REGdiff_rast_06$p.abundance.y-FLMA_REGdiff_rast_06$p.abundance.x)))
names(FLMA_REGdiff_rast_06)[9]<-"p.abundance"
FLMA_REGdiff_rast_06.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMA_REGdiff_rast_06
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMA_REGdiff_rast_06.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFJ_REGdiff_rast_06<-merge(SPFJ2006,SPFJew2006,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
SPFJ_REGdiff_rast_06<- cbind(SPFJ_REGdiff_rast_06[1:8],as.data.frame((SPFJ_REGdiff_rast_06$p.abundance.y-SPFJ_REGdiff_rast_06$p.abundance.x)))
names(SPFJ_REGdiff_rast_06)[9]<-"p.abundance"
SPFJ_REGdiff_rast_06.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFJ_REGdiff_rast_06
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFJ_REGdiff_rast_06.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMJ_REGdiff_rast_06<-merge(SPMJ2006,SPMJew2006,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
SPMJ_REGdiff_rast_06<- cbind(SPMJ_REGdiff_rast_06[1:8],as.data.frame((SPMJ_REGdiff_rast_06$p.abundance.y-SPMJ_REGdiff_rast_06$p.abundance.x)))
names(SPMJ_REGdiff_rast_06)[9]<-"p.abundance"
SPMJ_REGdiff_rast_06.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMJ_REGdiff_rast_06
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMJ_REGdiff_rast_06.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFA_REGdiff_rast_06<-merge(SPFA2006,SPFAew2006,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
SPFA_REGdiff_rast_06<- cbind(SPFA_REGdiff_rast_06[1:8],as.data.frame((SPFA_REGdiff_rast_06$p.abundance.y-SPFA_REGdiff_rast_06$p.abundance.x)))
names(SPFA_REGdiff_rast_06)[9]<-"p.abundance"
SPFA_REGdiff_rast_06.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFA_REGdiff_rast_06
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFA_REGdiff_rast_06.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMA_REGdiff_rast_06<-merge(SPMA2006,SPMAew2006,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
SPMA_REGdiff_rast_06<- cbind(SPMA_REGdiff_rast_06[1:8],as.data.frame((SPMA_REGdiff_rast_06$p.abundance.y-SPMA_REGdiff_rast_06$p.abundance.x)))
names(SPMA_REGdiff_rast_06)[9]<-"p.abundance"
SPMA_REGdiff_rast_06.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMA_REGdiff_rast_06
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMA_REGdiff_rast_06.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}
###2012####
FLFJ_REGdiff_rast_12<-merge(FLFJ2012,FLFJew2012,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
FLFJ_REGdiff_rast_12<- cbind(FLFJ_REGdiff_rast_12[1:8],as.data.frame((FLFJ_REGdiff_rast_12$p.abundance.y-FLFJ_REGdiff_rast_12$p.abundance.x)))
names(FLFJ_REGdiff_rast_12)[9]<-"p.abundance"
FLFJ_REGdiff_rast_12.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFJ_REGdiff_rast_12
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFJ_REGdiff_rast_12.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMJ_REGdiff_rast_12<-merge(FLMJ2012,FLMJew2012,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
FLMJ_REGdiff_rast_12<- cbind(FLMJ_REGdiff_rast_12[1:8],as.data.frame((FLMJ_REGdiff_rast_12$p.abundance.y-FLMJ_REGdiff_rast_12$p.abundance.x)))
names(FLMJ_REGdiff_rast_12)[9]<-"p.abundance"
FLMJ_REGdiff_rast_12.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMJ_REGdiff_rast_12
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMJ_REGdiff_rast_12.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLFA_REGdiff_rast_12<-merge(FLFA2012,FLFAew2012,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
FLFA_REGdiff_rast_12<- cbind(FLFA_REGdiff_rast_12[1:8],as.data.frame((FLFA_REGdiff_rast_12$p.abundance.y-FLFA_REGdiff_rast_12$p.abundance.x)))
names(FLFA_REGdiff_rast_12)[9]<-"p.abundance"
FLFA_REGdiff_rast_12.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFA_REGdiff_rast_12
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFA_REGdiff_rast_12.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMA_REGdiff_rast_12<-merge(FLMA2012,FLMAew2012,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
FLMA_REGdiff_rast_12<- cbind(FLMA_REGdiff_rast_12[1:8],as.data.frame((FLMA_REGdiff_rast_12$p.abundance.y-FLMA_REGdiff_rast_12$p.abundance.x)))
names(FLMA_REGdiff_rast_12)[9]<-"p.abundance"
FLMA_REGdiff_rast_12.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMA_REGdiff_rast_12
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMA_REGdiff_rast_12.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFJ_REGdiff_rast_12<-merge(SPFJ2012,SPFJew2012,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
SPFJ_REGdiff_rast_12<- cbind(SPFJ_REGdiff_rast_12[1:8],as.data.frame((SPFJ_REGdiff_rast_12$p.abundance.y-SPFJ_REGdiff_rast_12$p.abundance.x)))
names(SPFJ_REGdiff_rast_12)[9]<-"p.abundance"
SPFJ_REGdiff_rast_12.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFJ_REGdiff_rast_12
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFJ_REGdiff_rast_12.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMJ_REGdiff_rast_12<-merge(SPMJ2012,SPMJew2012,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
SPMJ_REGdiff_rast_12<- cbind(SPMJ_REGdiff_rast_12[1:8],as.data.frame((SPMJ_REGdiff_rast_12$p.abundance.y-SPMJ_REGdiff_rast_12$p.abundance.x)))
names(SPMJ_REGdiff_rast_12)[9]<-"p.abundance"
SPMJ_REGdiff_rast_12.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMJ_REGdiff_rast_12
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMJ_REGdiff_rast_12.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFA_REGdiff_rast_12<-merge(SPFA2012,SPFAew2012,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
SPFA_REGdiff_rast_12<- cbind(SPFA_REGdiff_rast_12[1:8],as.data.frame((SPFA_REGdiff_rast_12$p.abundance.y-SPFA_REGdiff_rast_12$p.abundance.x)))
names(SPFA_REGdiff_rast_12)[9]<-"p.abundance"
SPFA_REGdiff_rast_12.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFA_REGdiff_rast_12
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFA_REGdiff_rast_12.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMA_REGdiff_rast_12<-merge(SPMA2012,SPMAew2012,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
SPMA_REGdiff_rast_12<- cbind(SPMA_REGdiff_rast_12[1:8],as.data.frame((SPMA_REGdiff_rast_12$p.abundance.y-SPMA_REGdiff_rast_12$p.abundance.x)))
names(SPMA_REGdiff_rast_12)[9]<-"p.abundance"
SPMA_REGdiff_rast_12.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMA_REGdiff_rast_12
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMA_REGdiff_rast_12.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}
###2017#####
FLFJ_REGdiff_rast_17<-merge(FLFJ2017,FLFJew2017,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
FLFJ_REGdiff_rast_17<- cbind(FLFJ_REGdiff_rast_17[1:8],as.data.frame((FLFJ_REGdiff_rast_17$p.abundance.y-FLFJ_REGdiff_rast_17$p.abundance.x)))
names(FLFJ_REGdiff_rast_17)[9]<-"p.abundance"
FLFJ_REGdiff_rast_17.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFJ_REGdiff_rast_17
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFJ_REGdiff_rast_17.list[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

FLMJ_REGdiff_rast_17<-merge(FLMJ2017,FLMJew2017,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
FLMJ_REGdiff_rast_17<- cbind(FLMJ_REGdiff_rast_17[1:8],as.data.frame((FLMJ_REGdiff_rast_17$p.abundance.y-FLMJ_REGdiff_rast_17$p.abundance.x)))
names(FLMJ_REGdiff_rast_17)[9]<-"p.abundance"
FLMJ_REGdiff_rast_17.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMJ_REGdiff_rast_17
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMJ_REGdiff_rast_17.list[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

FLFA_REGdiff_rast_17<-merge(FLFA2017,FLFAew2017,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
FLFA_REGdiff_rast_17<- cbind(FLFA_REGdiff_rast_17[1:8],as.data.frame((FLFA_REGdiff_rast_17$p.abundance.y-FLFA_REGdiff_rast_17$p.abundance.x)))
names(FLFA_REGdiff_rast_17)[9]<-"p.abundance"
FLFA_REGdiff_rast_17.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFA_REGdiff_rast_17
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFA_REGdiff_rast_17.list[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

FLMA_REGdiff_rast_17<-merge(FLMA2017,FLMAew2017,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
FLMA_REGdiff_rast_17<- cbind(FLMA_REGdiff_rast_17[1:8],as.data.frame((FLMA_REGdiff_rast_17$p.abundance.y-FLMA_REGdiff_rast_17$p.abundance.x)))
names(FLMA_REGdiff_rast_17)[9]<-"p.abundance"
FLMA_REGdiff_rast_17.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMA_REGdiff_rast_17
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMA_REGdiff_rast_17.list[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

SPFJ_REGdiff_rast_17<-merge(SPFJ2017,SPFJew2017,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
SPFJ_REGdiff_rast_17<- cbind(SPFJ_REGdiff_rast_17[1:8],as.data.frame((SPFJ_REGdiff_rast_17$p.abundance.y-SPFJ_REGdiff_rast_17$p.abundance.x)))
names(SPFJ_REGdiff_rast_17)[9]<-"p.abundance"
SPFJ_REGdiff_rast_17.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFJ_REGdiff_rast_17
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFJ_REGdiff_rast_17.list[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

SPMJ_REGdiff_rast_17<-merge(SPMJ2017,SPMJew2017,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
SPMJ_REGdiff_rast_17<- cbind(SPMJ_REGdiff_rast_17[1:8],as.data.frame((SPMJ_REGdiff_rast_17$p.abundance.y-SPMJ_REGdiff_rast_17$p.abundance.x)))
names(SPMJ_REGdiff_rast_17)[9]<-"p.abundance"
SPMJ_REGdiff_rast_17.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMJ_REGdiff_rast_17
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMJ_REGdiff_rast_17.list[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

SPFA_REGdiff_rast_17<-merge(SPFA2017,SPFAew2017,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
SPFA_REGdiff_rast_17<- cbind(SPFA_REGdiff_rast_17[1:8],as.data.frame((SPFA_REGdiff_rast_17$p.abundance.y-SPFA_REGdiff_rast_17$p.abundance.x)))
names(SPFA_REGdiff_rast_17)[9]<-"p.abundance"
SPFA_REGdiff_rast_17.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFA_REGdiff_rast_17
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFA_REGdiff_rast_17.list[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

SPMA_REGdiff_rast_17<-merge(SPMA2017,SPMAew2017,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
SPMA_REGdiff_rast_17<- cbind(SPMA_REGdiff_rast_17[1:8],as.data.frame((SPMA_REGdiff_rast_17$p.abundance.y-SPMA_REGdiff_rast_17$p.abundance.x)))
names(SPMA_REGdiff_rast_17)[9]<-"p.abundance"
SPMA_REGdiff_rast_17.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMA_REGdiff_rast_17
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMA_REGdiff_rast_17.list[[i]] <- raster::extract(rast, depth_grid_plot2017)
}
#####
plotdiff<-function(rasterdata, month,year,size,depth_grid_plot){
  
  par(mar=c(2,2,0,0), mfrow=c(1,1))
  plotvar=unlist(rasterdata)
  nclr=10
  plotclr <- rev(brewer.pal(nclr,"RdYlBu"))
  class <- classIntervals(plotvar, nclr, style="quantile")
  fix_break<-round(class$brks, digits = 2)
  for(i in 1:length(rasterdata)){
    print(i)
    plotvar <- rasterdata[[i]]
    nclr=10
    plotclr <- rev(brewer.pal(nclr,"RdYlBu"))
    class <- classIntervals(plotvar, nclr, style="fixed", 
                            fixedBreaks=
                              if(size=="J")c(-200,-100,-50,-25,-15,-5,5,15,25,50,100,200,500) #breaks calculated by using average nonstationary "quantile" breaks
                            else c(-200,-100,-50,-25,-15,-5,5,15,25,50,100,200,500))
    
    #if(size=="J")c(-100,-98,-90,-75,-50,-25,0,25,50,75,100) #breaks calculated by using average nonstationary "quantile" breaks
    #else c(-100,-98,-90,-75,-50,-25,0,25,50,75,100)) #breaks calculated by using average "qauntile" breaks
    
    colcode <- findColours(class, plotclr)
    
    start_x <- range(GAM_data$Longitude)[1]
    end_x <- range(GAM_data$Longitude)[2]
    start_y <- range(GAM_data$Latitude)[1]
    end_y <- range(GAM_data$Latitude)[2]
    plot(depth_grid_plot$lon, depth_grid_plot$lat, pch=16, col=colcode, cex=0.1, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, xlab="Longitude", ylab="Latitude")
    map(database= mainecoast, ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE, lwd=0.1)
    #plot(sa511_513, add=T)
    box()
    legend("topleft", paste(year, "-", month, sep=""), bty="n", cex=1.5)
    legend("bottomright", legend=c(names(attr(colcode, "table"))), fill=c(attr(colcode, "palette")), cex=0.9,bg="white",
           bty = "o", title="Difference #")
    axis(1, cex=0.5)
    axis(2, cex=0.5)
  }
  #dev.off()
}

plotdiff(FLFJ_REGdiff_rast_00.list,"FLFJ","2000","J",depth_grid_plot000612)
plotdiff(FLMJ_REGdiff_rast_00.list,"FLMJ","2000","J",depth_grid_plot000612)
plotdiff(FLFA_REGdiff_rast_00.list,"FLFA","2000","A",depth_grid_plot000612)
plotdiff(FLMA_REGdiff_rast_00.list,"FLMA","2000","A",depth_grid_plot000612)
plotdiff(SPFJ_REGdiff_rast_00.list,"SPFJ","2000","J",depth_grid_plot000612)
plotdiff(SPMJ_REGdiff_rast_00.list,"SPMJ","2000","J",depth_grid_plot000612)
plotdiff(SPFA_REGdiff_rast_00.list,"SPFA","2000","A",depth_grid_plot000612)
plotdiff(SPMA_REGdiff_rast_00.list,"SPMA","2000","A",depth_grid_plot000612)

plotdiff(FLFJ_REGdiff_rast_06.list,"FLFJ","2006","J",depth_grid_plot000612)
plotdiff(FLMJ_REGdiff_rast_06.list,"FLMJ","2006","J",depth_grid_plot000612)
plotdiff(FLFA_REGdiff_rast_06.list,"FLFA","2006","A",depth_grid_plot000612)
plotdiff(FLMA_REGdiff_rast_06.list,"FLMA","2006","A",depth_grid_plot000612)
plotdiff(SPFJ_REGdiff_rast_06.list,"SPFJ","2006","J",depth_grid_plot000612)
plotdiff(SPMJ_REGdiff_rast_06.list,"SPMJ","2006","J",depth_grid_plot000612)
plotdiff(SPFA_REGdiff_rast_06.list,"SPFA","2006","A",depth_grid_plot000612)
plotdiff(SPMA_REGdiff_rast_06.list,"SPMA","2006","A",depth_grid_plot000612)

plotdiff(FLFJ_REGdiff_rast_12.list,"FLFJ","2012","J",depth_grid_plot000612)
plotdiff(FLMJ_REGdiff_rast_12.list,"FLMJ","2012","J",depth_grid_plot000612)
plotdiff(FLFA_REGdiff_rast_12.list,"FLFA","2012","A",depth_grid_plot000612)
plotdiff(FLMA_REGdiff_rast_12.list,"FLMA","2012","A",depth_grid_plot000612)
plotdiff(SPFJ_REGdiff_rast_12.list,"SPFJ","2012","J",depth_grid_plot000612)
plotdiff(SPMJ_REGdiff_rast_12.list,"SPMJ","2012","J",depth_grid_plot000612)
plotdiff(SPFA_REGdiff_rast_12.list,"SPFA","2012","A",depth_grid_plot000612)
plotdiff(SPMA_REGdiff_rast_12.list,"SPMA","2012","A",depth_grid_plot000612)  
  
plotdiff(FLFJ_REGdiff_rast_17.list,"FLFJ","2017","J",depth_grid_plot2017)
plotdiff(FLMJ_REGdiff_rast_17.list,"FLMJ","2017","J",depth_grid_plot2017)
plotdiff(FLFA_REGdiff_rast_17.list,"FLFA","2017","A",depth_grid_plot2017)
plotdiff(FLMA_REGdiff_rast_17.list,"FLMA","2017","A",depth_grid_plot2017)
plotdiff(SPFJ_REGdiff_rast_17.list,"SPFJ","2017","J",depth_grid_plot2017)
plotdiff(SPMJ_REGdiff_rast_17.list,"SPMJ","2017","J",depth_grid_plot2017)
plotdiff(SPFA_REGdiff_rast_17.list,"SPFA","2017","A",depth_grid_plot2017)
plotdiff(SPMA_REGdiff_rast_17.list,"SPMA","2017","A",depth_grid_plot2017)   
  
  
####Min Max#### 
library(huxtable)
library(stats)
table1regdiff00 <- hux(
  Group = c('FLFJ', 'FLMJ', 'FLFA','FLMA','SPFJ', 'SPMJ', 'SPFA','SPMA'),
  Max = c(max(na.omit(as.data.frame(FLFJ_REGdiff_rast_00.list[[1]]))), max(na.omit(as.data.frame(FLMJ_REGdiff_rast_00.list[[1]]))), 
          max(na.omit(as.data.frame(FLFA_REGdiff_rast_00.list[[1]]))),
          max(na.omit(as.data.frame(FLMA_REGdiff_rast_00.list[[1]]))),max(na.omit(as.data.frame(SPFJ_REGdiff_rast_00.list[[1]]))), 
          max(na.omit(as.data.frame(SPMJ_REGdiff_rast_00.list[[1]]))), max(na.omit(as.data.frame(SPFA_REGdiff_rast_00.list[[1]]))),
          max(na.omit(as.data.frame(SPMA_REGdiff_rast_00.list[[1]])))),
  Min = c(min(na.omit(as.data.frame(FLFJ_REGdiff_rast_00.list[[1]]))), min(na.omit(as.data.frame(FLMJ_REGdiff_rast_00.list[[1]]))), 
          min(na.omit(as.data.frame(FLFA_REGdiff_rast_00.list[[1]]))),
          min(na.omit(as.data.frame(FLMA_REGdiff_rast_00.list[[1]]))),min(na.omit(as.data.frame(SPFJ_REGdiff_rast_00.list[[1]]))), 
          min(na.omit(as.data.frame(SPMJ_REGdiff_rast_00.list[[1]]))), min(na.omit(as.data.frame(SPFA_REGdiff_rast_00.list[[1]]))),
          min(na.omit(as.data.frame(SPMA_REGdiff_rast_00.list[[1]])))),
Mean = c(mean(FLFJ_REGdiff_rast_00.list[[1]],na.rm=TRUE), mean(FLMJ_REGdiff_rast_00.list[[1]],na.rm=TRUE), 
        mean(FLFA_REGdiff_rast_00.list[[1]],na.rm=TRUE),
        mean(FLMA_REGdiff_rast_00.list[[1]],na.rm=TRUE),mean(SPFJ_REGdiff_rast_00.list[[1]],na.rm=TRUE), 
        mean(SPMJ_REGdiff_rast_00.list[[1]],na.rm=TRUE), mean(SPFA_REGdiff_rast_00.list[[1]],na.rm=TRUE),
        mean(SPMA_REGdiff_rast_00.list[[1]],na.rm=TRUE)),
Med = c(median(FLFJ_REGdiff_rast_00.list[[1]],na.rm=TRUE), median(FLMJ_REGdiff_rast_00.list[[1]],na.rm=TRUE), 
         median(FLFA_REGdiff_rast_00.list[[1]],na.rm=TRUE),
         median(FLMA_REGdiff_rast_00.list[[1]],na.rm=TRUE),median(SPFJ_REGdiff_rast_00.list[[1]],na.rm=TRUE), 
         median(SPMJ_REGdiff_rast_00.list[[1]],na.rm=TRUE), median(SPFA_REGdiff_rast_00.list[[1]],na.rm=TRUE),
         median(SPMA_REGdiff_rast_00.list[[1]],na.rm=TRUE)),add_colnames = TRUE)

bold(table1regdiff00)[1,]           <- TRUE
bottom_border(table1regdiff00)[1,]  <- 0.4
align(table1regdiff00)[,2]          <- 'right'
right_padding(table1regdiff00)      <- 10
left_padding(table1regdiff00)       <- 10
width(table1regdiff00)              <- 0.30
number_format(table1regdiff00)      <- 2

table1regdiff00

table1regdiff06 <- hux(
  Group = c('FLFJ', 'FLMJ', 'FLFA','FLMA','SPFJ', 'SPMJ', 'SPFA','SPMA'),
  Max = c(max(na.omit(as.data.frame(FLFJ_REGdiff_rast_06.list[[1]]))), max(na.omit(as.data.frame(FLMJ_REGdiff_rast_06.list[[1]]))), 
          max(na.omit(as.data.frame(FLFA_REGdiff_rast_06.list[[1]]))),
          max(na.omit(as.data.frame(FLMA_REGdiff_rast_06.list[[1]]))),max(na.omit(as.data.frame(SPFJ_REGdiff_rast_06.list[[1]]))), 
          max(na.omit(as.data.frame(SPMJ_REGdiff_rast_06.list[[1]]))), max(na.omit(as.data.frame(SPFA_REGdiff_rast_06.list[[1]]))),
          max(na.omit(as.data.frame(SPMA_REGdiff_rast_06.list[[1]])))),
  Min = c(min(na.omit(as.data.frame(FLFJ_REGdiff_rast_06.list[[1]]))), min(na.omit(as.data.frame(FLMJ_REGdiff_rast_06.list[[1]]))), 
          min(na.omit(as.data.frame(FLFA_REGdiff_rast_06.list[[1]]))),
          min(na.omit(as.data.frame(FLMA_REGdiff_rast_06.list[[1]]))),min(na.omit(as.data.frame(SPFJ_REGdiff_rast_06.list[[1]]))), 
          min(na.omit(as.data.frame(SPMJ_REGdiff_rast_06.list[[1]]))), min(na.omit(as.data.frame(SPFA_REGdiff_rast_06.list[[1]]))),
          min(na.omit(as.data.frame(SPMA_REGdiff_rast_06.list[[1]])))),
  Mean = c(mean(FLFJ_REGdiff_rast_06.list[[1]],na.rm=TRUE), mean(FLMJ_REGdiff_rast_06.list[[1]],na.rm=TRUE), 
           mean(FLFA_REGdiff_rast_06.list[[1]],na.rm=TRUE),
           mean(FLMA_REGdiff_rast_06.list[[1]],na.rm=TRUE),mean(SPFJ_REGdiff_rast_06.list[[1]],na.rm=TRUE), 
           mean(SPMJ_REGdiff_rast_06.list[[1]],na.rm=TRUE), mean(SPFA_REGdiff_rast_06.list[[1]],na.rm=TRUE),
           mean(SPMA_REGdiff_rast_06.list[[1]],na.rm=TRUE)),
  Med = c(median(FLFJ_REGdiff_rast_06.list[[1]],na.rm=TRUE), median(FLMJ_REGdiff_rast_06.list[[1]],na.rm=TRUE), 
          median(FLFA_REGdiff_rast_06.list[[1]],na.rm=TRUE),
          median(FLMA_REGdiff_rast_06.list[[1]],na.rm=TRUE),median(SPFJ_REGdiff_rast_06.list[[1]],na.rm=TRUE), 
          median(SPMJ_REGdiff_rast_06.list[[1]],na.rm=TRUE), median(SPFA_REGdiff_rast_06.list[[1]],na.rm=TRUE),
          median(SPMA_REGdiff_rast_06.list[[1]],na.rm=TRUE)),add_colnames = TRUE)

bold(table1regdiff06)[1,]           <- TRUE
bottom_border(table1regdiff06)[1,]  <- 0.4
align(table1regdiff06)[,2]          <- 'right'
right_padding(table1regdiff06)      <- 10
left_padding(table1regdiff06)       <- 10
width(table1regdiff06)              <- 0.30
number_format(table1regdiff06)      <- 2

table1regdiff06

table1regdiff12 <- hux(
  Group = c('FLFJ', 'FLMJ', 'FLFA','FLMA','SPFJ', 'SPMJ', 'SPFA','SPMA'),
  Max = c(max(na.omit(as.data.frame(FLFJ_REGdiff_rast_12.list[[1]]))), max(na.omit(as.data.frame(FLMJ_REGdiff_rast_12.list[[1]]))), 
          max(na.omit(as.data.frame(FLFA_REGdiff_rast_12.list[[1]]))),
          max(na.omit(as.data.frame(FLMA_REGdiff_rast_12.list[[1]]))),max(na.omit(as.data.frame(SPFJ_REGdiff_rast_12.list[[1]]))), 
          max(na.omit(as.data.frame(SPMJ_REGdiff_rast_12.list[[1]]))), max(na.omit(as.data.frame(SPFA_REGdiff_rast_12.list[[1]]))),
          max(na.omit(as.data.frame(SPMA_REGdiff_rast_12.list[[1]])))),
  Min = c(min(na.omit(as.data.frame(FLFJ_REGdiff_rast_12.list[[1]]))), min(na.omit(as.data.frame(FLMJ_REGdiff_rast_12.list[[1]]))), 
          min(na.omit(as.data.frame(FLFA_REGdiff_rast_12.list[[1]]))),
          min(na.omit(as.data.frame(FLMA_REGdiff_rast_12.list[[1]]))),min(na.omit(as.data.frame(SPFJ_REGdiff_rast_12.list[[1]]))), 
          min(na.omit(as.data.frame(SPMJ_REGdiff_rast_12.list[[1]]))), min(na.omit(as.data.frame(SPFA_REGdiff_rast_12.list[[1]]))),
          min(na.omit(as.data.frame(SPMA_REGdiff_rast_12.list[[1]])))),
  Mean = c(mean(FLFJ_REGdiff_rast_12.list[[1]],na.rm=TRUE), mean(FLMJ_REGdiff_rast_12.list[[1]],na.rm=TRUE), 
           mean(FLFA_REGdiff_rast_12.list[[1]],na.rm=TRUE),
           mean(FLMA_REGdiff_rast_12.list[[1]],na.rm=TRUE),mean(SPFJ_REGdiff_rast_12.list[[1]],na.rm=TRUE), 
           mean(SPMJ_REGdiff_rast_12.list[[1]],na.rm=TRUE), mean(SPFA_REGdiff_rast_12.list[[1]],na.rm=TRUE),
           mean(SPMA_REGdiff_rast_12.list[[1]],na.rm=TRUE)),
  Med = c(median(FLFJ_REGdiff_rast_12.list[[1]],na.rm=TRUE), median(FLMJ_REGdiff_rast_12.list[[1]],na.rm=TRUE), 
          median(FLFA_REGdiff_rast_12.list[[1]],na.rm=TRUE),
          median(FLMA_REGdiff_rast_12.list[[1]],na.rm=TRUE),median(SPFJ_REGdiff_rast_12.list[[1]],na.rm=TRUE), 
          median(SPMJ_REGdiff_rast_12.list[[1]],na.rm=TRUE), median(SPFA_REGdiff_rast_12.list[[1]],na.rm=TRUE),
          median(SPMA_REGdiff_rast_12.list[[1]],na.rm=TRUE)),add_colnames = TRUE)

bold(table1regdiff12)[1,]           <- TRUE
bottom_border(table1regdiff12)[1,]  <- 0.4
align(table1regdiff12)[,2]          <- 'right'
right_padding(table1regdiff12)      <- 10
left_padding(table1regdiff12)       <- 10
width(table1regdiff12)              <- 0.30
number_format(table1regdiff12)      <- 2

table1regdiff12

table1regdiff17 <- hux(
  Group = c('FLFJ', 'FLMJ', 'FLFA','FLMA','SPFJ', 'SPMJ', 'SPFA','SPMA'),
  Max = c(max(na.omit(as.data.frame(FLFJ_REGdiff_rast_17.list[[1]]))), max(na.omit(as.data.frame(FLMJ_REGdiff_rast_17.list[[1]]))), 
          max(na.omit(as.data.frame(FLFA_REGdiff_rast_17.list[[1]]))),
          max(na.omit(as.data.frame(FLMA_REGdiff_rast_17.list[[1]]))),max(na.omit(as.data.frame(SPFJ_REGdiff_rast_17.list[[1]]))), 
          max(na.omit(as.data.frame(SPMJ_REGdiff_rast_17.list[[1]]))), max(na.omit(as.data.frame(SPFA_REGdiff_rast_17.list[[1]]))),
          max(na.omit(as.data.frame(SPMA_REGdiff_rast_17.list[[1]])))),
  Min = c(min(na.omit(as.data.frame(FLFJ_REGdiff_rast_17.list[[1]]))), min(na.omit(as.data.frame(FLMJ_REGdiff_rast_17.list[[1]]))), 
          min(na.omit(as.data.frame(FLFA_REGdiff_rast_17.list[[1]]))),
          min(na.omit(as.data.frame(FLMA_REGdiff_rast_17.list[[1]]))),min(na.omit(as.data.frame(SPFJ_REGdiff_rast_17.list[[1]]))), 
          min(na.omit(as.data.frame(SPMJ_REGdiff_rast_17.list[[1]]))), min(na.omit(as.data.frame(SPFA_REGdiff_rast_17.list[[1]]))),
          min(na.omit(as.data.frame(SPMA_REGdiff_rast_17.list[[1]])))),
  Mean = c(mean(FLFJ_REGdiff_rast_17.list[[1]],na.rm=TRUE), mean(FLMJ_REGdiff_rast_17.list[[1]],na.rm=TRUE), 
           mean(FLFA_REGdiff_rast_17.list[[1]],na.rm=TRUE),
           mean(FLMA_REGdiff_rast_17.list[[1]],na.rm=TRUE),mean(SPFJ_REGdiff_rast_17.list[[1]],na.rm=TRUE), 
           mean(SPMJ_REGdiff_rast_17.list[[1]],na.rm=TRUE), mean(SPFA_REGdiff_rast_17.list[[1]],na.rm=TRUE),
           mean(SPMA_REGdiff_rast_17.list[[1]],na.rm=TRUE)),
  Med = c(median(FLFJ_REGdiff_rast_17.list[[1]],na.rm=TRUE), median(FLMJ_REGdiff_rast_17.list[[1]],na.rm=TRUE), 
          median(FLFA_REGdiff_rast_17.list[[1]],na.rm=TRUE),
          median(FLMA_REGdiff_rast_17.list[[1]],na.rm=TRUE),median(SPFJ_REGdiff_rast_17.list[[1]],na.rm=TRUE), 
          median(SPMJ_REGdiff_rast_17.list[[1]],na.rm=TRUE), median(SPFA_REGdiff_rast_17.list[[1]],na.rm=TRUE),
          median(SPMA_REGdiff_rast_17.list[[1]],na.rm=TRUE)),add_colnames = TRUE)

bold(table1regdiff17)[1,]           <- TRUE
bottom_border(table1regdiff17)[1,]  <- 0.4
align(table1regdiff17)[,2]          <- 'right'
right_padding(table1regdiff17)      <- 10
left_padding(table1regdiff17)       <- 10
width(table1regdiff17)              <- 0.30
number_format(table1regdiff17)      <- 2

table1regdiff17
###                                                             #####
#####Calculate and plot RELATIVE differences between NSV2 and S predicted abundancies####

###2000####

FLFJ2_diff_rast_00<-merge(FLFJ2000,FLFJew22000,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#FLFJ_diff_rast_00<- cbind(FLFJ_diff_rast_00[1:8],as.data.frame(((FLFJ_diff_rast_00$p.abundance.y-FLFJ_diff_rast_00$p.abundance.x)/FLFJ_diff_rast_00$p.abundance.x)*100))
FLFJ2_diff_rast_00<- cbind(FLFJ2_diff_rast_00[1:8],as.data.frame(
  ifelse(((FLFJ2_diff_rast_00$p.abundance.y>1.00)&(FLFJ2_diff_rast_00$p.abundance.x>1.00)),
         (((FLFJ2_diff_rast_00$p.abundance.y-FLFJ2_diff_rast_00$p.abundance.x)/FLFJ2_diff_rast_00$p.abundance.x)*100),
         (FLFJ2_diff_rast_00$p.abundance.y-FLFJ2_diff_rast_00$p.abundance.x))))
names(FLFJ2_diff_rast_00)[9]<-"p.abundance"
FLFJ2_diff_rast_00.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFJ2_diff_rast_00
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFJ2_diff_rast_00.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}


FLMJ2_diff_rast_00<-merge(FLMJ2000,FLMJew22000,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#FLMJ2_diff_rast_00<- cbind(FLMJ2_diff_rast_00[1:8],as.data.frame(((FLMJ2_diff_rast_00$p.abundance.y-FLMJ2_diff_rast_00$p.abundance.x)/FLMJ2_diff_rast_00$p.abundance.x)*100))
FLMJ2_diff_rast_00<- cbind(FLMJ2_diff_rast_00[1:8],as.data.frame(
  ifelse(((FLMJ2_diff_rast_00$p.abundance.y>1.00)&(FLMJ2_diff_rast_00$p.abundance.x>1.00)),
         (((FLMJ2_diff_rast_00$p.abundance.y-FLMJ2_diff_rast_00$p.abundance.x)/FLMJ2_diff_rast_00$p.abundance.x)*100),
         (FLMJ2_diff_rast_00$p.abundance.y-FLMJ2_diff_rast_00$p.abundance.x))))
names(FLMJ2_diff_rast_00)[9]<-"p.abundance"
FLMJ2_diff_rast_00.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMJ2_diff_rast_00
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMJ2_diff_rast_00.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLFA2_diff_rast_00<-merge(FLFA2000,FLFAew22000,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#FLFA2_diff_rast_00<- cbind(FLFA2_diff_rast_00[1:8],as.data.frame(((FLFA2_diff_rast_00$p.abundance.y-FLFA2_diff_rast_00$p.abundance.x)/FLFA2_diff_rast_00$p.abundance.x)*100))
FLFA2_diff_rast_00<- cbind(FLFA2_diff_rast_00[1:8],as.data.frame(
  ifelse(((FLFA2_diff_rast_00$p.abundance.y>1.00)&(FLFA2_diff_rast_00$p.abundance.x>1.00)),
         (((FLFA2_diff_rast_00$p.abundance.y-FLFA2_diff_rast_00$p.abundance.x)/FLFA2_diff_rast_00$p.abundance.x)*100),
         (FLFA2_diff_rast_00$p.abundance.y-FLFA2_diff_rast_00$p.abundance.x))))
names(FLFA2_diff_rast_00)[9]<-"p.abundance"
FLFA2_diff_rast_00.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFA2_diff_rast_00
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row, duplicate = "mean"))
  rast <- raster(akima.smooth)
  FLFA2_diff_rast_00.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMA2_diff_rast_00<-merge(FLMA2000,FLMAew22000,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#FLMA2_diff_rast_00<- cbind(FLMA2_diff_rast_00[1:8],as.data.frame(((FLMA2_diff_rast_00$p.abundance.y-FLMA2_diff_rast_00$p.abundance.x)/FLMA2_diff_rast_00$p.abundance.x)*100))
FLMA2_diff_rast_00<- cbind(FLMA2_diff_rast_00[1:8],as.data.frame(
  ifelse(((FLMA2_diff_rast_00$p.abundance.y>1.00)&(FLMA2_diff_rast_00$p.abundance.x>1.00)),
         (((FLMA2_diff_rast_00$p.abundance.y-FLMA2_diff_rast_00$p.abundance.x)/FLMA2_diff_rast_00$p.abundance.x)*100),
         (FLMA2_diff_rast_00$p.abundance.y-FLMA2_diff_rast_00$p.abundance.x))))
names(FLMA2_diff_rast_00)[9]<-"p.abundance"
FLMA2_diff_rast_00.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMA2_diff_rast_00
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMA2_diff_rast_00.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFJ2_diff_rast_00<-merge(SPFJ2000,SPFJew22000,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#SPFJ2_diff_rast_00<- cbind(SPFJ2_diff_rast_00[1:8],as.data.frame(((SPFJ2_diff_rast_00$p.abundance.y-SPFJ2_diff_rast_00$p.abundance.x)/SPFJ2_diff_rast_00$p.abundance.x)*100))
SPFJ2_diff_rast_00<- cbind(SPFJ2_diff_rast_00[1:8],as.data.frame(
  ifelse(((SPFJ2_diff_rast_00$p.abundance.y>1.00)&(SPFJ2_diff_rast_00$p.abundance.x>1.00)),
         (((SPFJ2_diff_rast_00$p.abundance.y-SPFJ2_diff_rast_00$p.abundance.x)/SPFJ2_diff_rast_00$p.abundance.x)*100),
         (SPFJ2_diff_rast_00$p.abundance.y-SPFJ2_diff_rast_00$p.abundance.x))))
names(SPFJ2_diff_rast_00)[9]<-"p.abundance"
SPFJ2_diff_rast_00.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFJ2_diff_rast_00
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFJ2_diff_rast_00.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMJ2_diff_rast_00<-merge(SPMJ2000,SPMJew22000,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#SPMJ2_diff_rast_00<- cbind(SPMJ2_diff_rast_00[1:8],as.data.frame(((SPMJ2_diff_rast_00$p.abundance.y-SPMJ2_diff_rast_00$p.abundance.x)/SPMJ2_diff_rast_00$p.abundance.x)*100))
SPMJ2_diff_rast_00<- cbind(SPMJ2_diff_rast_00[1:8],as.data.frame(
  ifelse(((SPMJ2_diff_rast_00$p.abundance.y>1.00)&(SPMJ2_diff_rast_00$p.abundance.x>1.00)),
         (((SPMJ2_diff_rast_00$p.abundance.y-SPMJ2_diff_rast_00$p.abundance.x)/SPMJ2_diff_rast_00$p.abundance.x)*100),
         (SPMJ2_diff_rast_00$p.abundance.y-SPMJ2_diff_rast_00$p.abundance.x))))
names(SPMJ2_diff_rast_00)[9]<-"p.abundance"
SPMJ2_diff_rast_00.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMJ2_diff_rast_00
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMJ2_diff_rast_00.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFA2_diff_rast_00<-merge(SPFA2000,SPFAew22000,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#SPFA2_diff_rast_00<- cbind(SPFA2_diff_rast_00[1:8],as.data.frame(((SPFA2_diff_rast_00$p.abundance.y-SPFA2_diff_rast_00$p.abundance.x)/SPFA2_diff_rast_00$p.abundance.x)*100))
SPFA2_diff_rast_00<- cbind(SPFA2_diff_rast_00[1:8],as.data.frame(
  ifelse(((SPFA2_diff_rast_00$p.abundance.y>1.00)&(SPFA2_diff_rast_00$p.abundance.x>1.00)),
         (((SPFA2_diff_rast_00$p.abundance.y-SPFA2_diff_rast_00$p.abundance.x)/SPFA2_diff_rast_00$p.abundance.x)*100),
         (SPFA2_diff_rast_00$p.abundance.y-SPFA2_diff_rast_00$p.abundance.x))))
names(SPFA2_diff_rast_00)[9]<-"p.abundance"
SPFA2_diff_rast_00.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFA2_diff_rast_00
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFA2_diff_rast_00.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMA2_diff_rast_00<-merge(SPMA2000,SPMAew22000,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#SPMA2_diff_rast_00<- cbind(SPMA2_diff_rast_00[1:8],as.data.frame(((SPMA2_diff_rast_00$p.abundance.y-SPMA2_diff_rast_00$p.abundance.x)/SPMA2_diff_rast_00$p.abundance.x)*100))
SPMA2_diff_rast_00<- cbind(SPMA2_diff_rast_00[1:8],as.data.frame(
  ifelse(((SPMA2_diff_rast_00$p.abundance.y>1.00)&(SPMA2_diff_rast_00$p.abundance.x>1.00)),
         (((SPMA2_diff_rast_00$p.abundance.y-SPMA2_diff_rast_00$p.abundance.x)/SPMA2_diff_rast_00$p.abundance.x)*100),
         (SPMA2_diff_rast_00$p.abundance.y-SPMA2_diff_rast_00$p.abundance.x))))
names(SPMA2_diff_rast_00)[9]<-"p.abundance"
SPMA2_diff_rast_00.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMA2_diff_rast_00
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMA2_diff_rast_00.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}
###2006####
FLFJ2_diff_rast_06<-merge(FLFJ2006,FLFJew22006,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#FLFJ2_diff_rast_06<- cbind(FLFJ2_diff_rast_06[1:8],as.data.frame(((FLFJ2_diff_rast_06$p.abundance.y-FLFJ2_diff_rast_06$p.abundance.x)/FLFJ2_diff_rast_06$p.abundance.x)*100))
FLFJ2_diff_rast_06<- cbind(FLFJ2_diff_rast_06[1:8],as.data.frame(
  ifelse(((FLFJ2_diff_rast_06$p.abundance.y>1.00)&(FLFJ2_diff_rast_06$p.abundance.x>1.00)),
         (((FLFJ2_diff_rast_06$p.abundance.y-FLFJ2_diff_rast_06$p.abundance.x)/FLFJ2_diff_rast_06$p.abundance.x)*100),
         (FLFJ2_diff_rast_06$p.abundance.y-FLFJ2_diff_rast_06$p.abundance.x))))
names(FLFJ2_diff_rast_06)[9]<-"p.abundance"
FLFJ2_diff_rast_06.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFJ2_diff_rast_06
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFJ2_diff_rast_06.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMJ2_diff_rast_06<-merge(FLMJ2006,FLMJew22006,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#FLMJ2_diff_rast_06<- cbind(FLMJ2_diff_rast_06[1:8],as.data.frame(((FLMJ2_diff_rast_06$p.abundance.y-FLMJ2_diff_rast_06$p.abundance.x)/FLMJ2_diff_rast_06$p.abundance.x)*100))
FLMJ2_diff_rast_06<- cbind(FLMJ2_diff_rast_06[1:8],as.data.frame(
  ifelse(((FLMJ2_diff_rast_06$p.abundance.y>1.00)&(FLMJ2_diff_rast_06$p.abundance.x>1.00)),
         (((FLMJ2_diff_rast_06$p.abundance.y-FLMJ2_diff_rast_06$p.abundance.x)/FLMJ2_diff_rast_06$p.abundance.x)*100),
         (FLMJ2_diff_rast_06$p.abundance.y-FLMJ2_diff_rast_06$p.abundance.x))))
names(FLMJ2_diff_rast_06)[9]<-"p.abundance"
FLMJ2_diff_rast_06.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMJ2_diff_rast_06
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMJ2_diff_rast_06.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLFA2_diff_rast_06<-merge(FLFA2006,FLFAew22006,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#FLFA2_diff_rast_06<- cbind(FLFA2_diff_rast_06[1:8],as.data.frame(((FLFA2_diff_rast_06$p.abundance.y-FLFA2_diff_rast_06$p.abundance.x)/FLFA2_diff_rast_06$p.abundance.x)*100))
FLFA2_diff_rast_06<- cbind(FLFA2_diff_rast_06[1:8],as.data.frame(
  ifelse(((FLFA2_diff_rast_06$p.abundance.y>1.00)&(FLFA2_diff_rast_06$p.abundance.x>1.00)),
         (((FLFA2_diff_rast_06$p.abundance.y-FLFA2_diff_rast_06$p.abundance.x)/FLFA2_diff_rast_06$p.abundance.x)*100),
         (FLFA2_diff_rast_06$p.abundance.y-FLFA2_diff_rast_06$p.abundance.x))))
names(FLFA2_diff_rast_06)[9]<-"p.abundance"
FLFA2_diff_rast_06.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFA2_diff_rast_06
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFA2_diff_rast_06.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMA2_diff_rast_06<-merge(FLMA2006,FLMAew22006,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#FLMA2_diff_rast_06<- cbind(FLMA2_diff_rast_06[1:8],as.data.frame(((FLMA2_diff_rast_06$p.abundance.y-FLMA2_diff_rast_06$p.abundance.x)/FLMA2_diff_rast_06$p.abundance.x)*100))
FLMA2_diff_rast_06<- cbind(FLMA2_diff_rast_06[1:8],as.data.frame(
  ifelse(((FLMA2_diff_rast_06$p.abundance.y>1.00)&(FLMA2_diff_rast_06$p.abundance.x>1.00)),
         (((FLMA2_diff_rast_06$p.abundance.y-FLMA2_diff_rast_06$p.abundance.x)/FLMA2_diff_rast_06$p.abundance.x)*100),
         (FLMA2_diff_rast_06$p.abundance.y-FLMA2_diff_rast_06$p.abundance.x))))
names(FLMA2_diff_rast_06)[9]<-"p.abundance"
FLMA2_diff_rast_06.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMA2_diff_rast_06
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMA2_diff_rast_06.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFJ2_diff_rast_06<-merge(SPFJ2006,SPFJew22006,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#SPFJ2_diff_rast_06<- cbind(SPFJ2_diff_rast_06[1:8],as.data.frame(((SPFJ2_diff_rast_06$p.abundance.y-SPFJ2_diff_rast_06$p.abundance.x)/SPFJ2_diff_rast_06$p.abundance.x)*100))
SPFJ2_diff_rast_06<- cbind(SPFJ2_diff_rast_06[1:8],as.data.frame(
  ifelse(((SPFJ2_diff_rast_06$p.abundance.y>1.00)&(SPFJ2_diff_rast_06$p.abundance.x>1.00)),
         (((SPFJ2_diff_rast_06$p.abundance.y-SPFJ2_diff_rast_06$p.abundance.x)/SPFJ2_diff_rast_06$p.abundance.x)*100),
         (SPFJ2_diff_rast_06$p.abundance.y-SPFJ2_diff_rast_06$p.abundance.x))))
names(SPFJ2_diff_rast_06)[9]<-"p.abundance"
SPFJ2_diff_rast_06.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFJ2_diff_rast_06
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFJ2_diff_rast_06.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMJ2_diff_rast_06<-merge(SPMJ2006,SPMJew22006,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#SPMJ2_diff_rast_06<- cbind(SPMJ2_diff_rast_06[1:8],as.data.frame(((SPMJ2_diff_rast_06$p.abundance.y-SPMJ2_diff_rast_06$p.abundance.x)/SPMJ2_diff_rast_06$p.abundance.x)*100))
SPMJ2_diff_rast_06<- cbind(SPMJ2_diff_rast_06[1:8],as.data.frame(
  ifelse(((SPMJ2_diff_rast_06$p.abundance.y>1.00)&(SPMJ2_diff_rast_06$p.abundance.x>1.00)),
         (((SPMJ2_diff_rast_06$p.abundance.y-SPMJ2_diff_rast_06$p.abundance.x)/SPMJ2_diff_rast_06$p.abundance.x)*100),
         (SPMJ2_diff_rast_06$p.abundance.y-SPMJ2_diff_rast_06$p.abundance.x))))
names(SPMJ2_diff_rast_06)[9]<-"p.abundance"
SPMJ2_diff_rast_06.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMJ2_diff_rast_06
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMJ2_diff_rast_06.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFA2_diff_rast_06<-merge(SPFA2006,SPFAew22006,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#SPFA2_diff_rast_06<- cbind(SPFA2_diff_rast_06[1:8],as.data.frame(((SPFA2_diff_rast_06$p.abundance.y-SPFA2_diff_rast_06$p.abundance.x)/SPFA2_diff_rast_06$p.abundance.x)*100))
SPFA2_diff_rast_06<- cbind(SPFA2_diff_rast_06[1:8],as.data.frame(
  ifelse(((SPFA2_diff_rast_06$p.abundance.y>1.00)&(SPFA2_diff_rast_06$p.abundance.x>1.00)),
         (((SPFA2_diff_rast_06$p.abundance.y-SPFA2_diff_rast_06$p.abundance.x)/SPFA2_diff_rast_06$p.abundance.x)*100),
         (SPFA2_diff_rast_06$p.abundance.y-SPFA2_diff_rast_06$p.abundance.x))))
names(SPFA2_diff_rast_06)[9]<-"p.abundance"
SPFA2_diff_rast_06.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFA2_diff_rast_06
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFA2_diff_rast_06.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMA2_diff_rast_06<-merge(SPMA2006,SPMAew22006,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#SPMA2_diff_rast_06<- cbind(SPMA2_diff_rast_06[1:8],as.data.frame(((SPMA2_diff_rast_06$p.abundance.y-SPMA2_diff_rast_06$p.abundance.x)/SPMA2_diff_rast_06$p.abundance.x)*100))
SPMA2_diff_rast_06<- cbind(SPMA2_diff_rast_06[1:8],as.data.frame(
  ifelse(((SPMA2_diff_rast_06$p.abundance.y>1.00)&(SPMA2_diff_rast_06$p.abundance.x>1.00)),
         (((SPMA2_diff_rast_06$p.abundance.y-SPMA2_diff_rast_06$p.abundance.x)/SPMA2_diff_rast_06$p.abundance.x)*100),
         (SPMA2_diff_rast_06$p.abundance.y-SPMA2_diff_rast_06$p.abundance.x))))
names(SPMA2_diff_rast_06)[9]<-"p.abundance"
SPMA2_diff_rast_06.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMA2_diff_rast_06
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMA2_diff_rast_06.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}
###2012####
FLFJ2_diff_rast_12<-merge(FLFJ2012,FLFJew22012,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#FLFJ2_diff_rast_12<- cbind(FLFJ2_diff_rast_12[1:8],as.data.frame(((FLFJ2_diff_rast_12$p.abundance.y-FLFJ2_diff_rast_12$p.abundance.x)/FLFJ2_diff_rast_12$p.abundance.x)*100))
FLFJ2_diff_rast_12<- cbind(FLFJ2_diff_rast_12[1:8],as.data.frame(
  ifelse(((FLFJ2_diff_rast_12$p.abundance.y>1.00)&(FLFJ2_diff_rast_12$p.abundance.x>1.00)),
         (((FLFJ2_diff_rast_12$p.abundance.y-FLFJ2_diff_rast_12$p.abundance.x)/FLFJ2_diff_rast_12$p.abundance.x)*100),
         (FLFJ2_diff_rast_12$p.abundance.y-FLFJ2_diff_rast_12$p.abundance.x))))
names(FLFJ2_diff_rast_12)[9]<-"p.abundance"
FLFJ2_diff_rast_12.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFJ2_diff_rast_12
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFJ2_diff_rast_12.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMJ2_diff_rast_12<-merge(FLMJ2012,FLMJew22012,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#FLMJ2_diff_rast_12<- cbind(FLMJ2_diff_rast_12[1:8],as.data.frame(((FLMJ2_diff_rast_12$p.abundance.y-FLMJ2_diff_rast_12$p.abundance.x)/FLMJ2_diff_rast_12$p.abundance.x)*100))
FLMJ2_diff_rast_12<- cbind(FLMJ2_diff_rast_12[1:8],as.data.frame(
  ifelse(((FLMJ2_diff_rast_12$p.abundance.y>1.00)&(FLMJ2_diff_rast_12$p.abundance.x>1.00)),
         (((FLMJ2_diff_rast_12$p.abundance.y-FLMJ2_diff_rast_12$p.abundance.x)/FLMJ2_diff_rast_12$p.abundance.x)*100),
         (FLMJ2_diff_rast_12$p.abundance.y-FLMJ2_diff_rast_12$p.abundance.x))))
names(FLMJ2_diff_rast_12)[9]<-"p.abundance"
FLMJ2_diff_rast_12.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMJ2_diff_rast_12
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMJ2_diff_rast_12.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLFA2_diff_rast_12<-merge(FLFA2012,FLFAew22012,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#FLFA2_diff_rast_12<- cbind(FLFA2_diff_rast_12[1:8],as.data.frame(((FLFA2_diff_rast_12$p.abundance.y-FLFA2_diff_rast_12$p.abundance.x)/FLFA2_diff_rast_12$p.abundance.x)*100))
FLFA2_diff_rast_12<- cbind(FLFA2_diff_rast_12[1:8],as.data.frame(
  ifelse(((FLFA2_diff_rast_12$p.abundance.y>1.00)&(FLFA2_diff_rast_12$p.abundance.x>1.00)),
         (((FLFA2_diff_rast_12$p.abundance.y-FLFA2_diff_rast_12$p.abundance.x)/FLFA2_diff_rast_12$p.abundance.x)*100),
         (FLFA2_diff_rast_12$p.abundance.y-FLFA2_diff_rast_12$p.abundance.x))))
names(FLFA2_diff_rast_12)[9]<-"p.abundance"
FLFA2_diff_rast_12.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFA2_diff_rast_12
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFA2_diff_rast_12.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMA2_diff_rast_12<-merge(FLMA2012,FLMAew22012,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#FLMA2_diff_rast_12<- cbind(FLMA2_diff_rast_12[1:8],as.data.frame(((FLMA2_diff_rast_12$p.abundance.y-FLMA2_diff_rast_12$p.abundance.x)/FLMA2_diff_rast_12$p.abundance.x)*100))
FLMA2_diff_rast_12<- cbind(FLMA2_diff_rast_12[1:8],as.data.frame(
  ifelse(((FLMA2_diff_rast_12$p.abundance.y>1.00)&(FLMA2_diff_rast_12$p.abundance.x>1.00)),
         (((FLMA2_diff_rast_12$p.abundance.y-FLMA2_diff_rast_12$p.abundance.x)/FLMA2_diff_rast_12$p.abundance.x)*100),
         (FLMA2_diff_rast_12$p.abundance.y-FLMA2_diff_rast_12$p.abundance.x))))
names(FLMA2_diff_rast_12)[9]<-"p.abundance"
FLMA2_diff_rast_12.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMA2_diff_rast_12
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMA2_diff_rast_12.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFJ2_diff_rast_12<-merge(SPFJ2012,SPFJew22012,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#SPFJ2_diff_rast_12<- cbind(SPFJ2_diff_rast_12[1:8],as.data.frame(((SPFJ2_diff_rast_12$p.abundance.y-SPFJ2_diff_rast_12$p.abundance.x)/SPFJ2_diff_rast_12$p.abundance.x)*100))
SPFJ2_diff_rast_12<- cbind(SPFJ2_diff_rast_12[1:8],as.data.frame(
  ifelse(((SPFJ2_diff_rast_12$p.abundance.y>1.00)&(SPFJ2_diff_rast_12$p.abundance.x>1.00)),
         (((SPFJ2_diff_rast_12$p.abundance.y-SPFJ2_diff_rast_12$p.abundance.x)/SPFJ2_diff_rast_12$p.abundance.x)*100),
         (SPFJ2_diff_rast_12$p.abundance.y-SPFJ2_diff_rast_12$p.abundance.x))))
names(SPFJ2_diff_rast_12)[9]<-"p.abundance"
SPFJ2_diff_rast_12.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFJ2_diff_rast_12
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFJ2_diff_rast_12.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMJ2_diff_rast_12<-merge(SPMJ2012,SPMJew22012,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#SPMJ2_diff_rast_12<- cbind(SPMJ2_diff_rast_12[1:8],as.data.frame(((SPMJ2_diff_rast_12$p.abundance.y-SPMJ2_diff_rast_12$p.abundance.x)/SPMJ2_diff_rast_12$p.abundance.x)*100))
SPMJ2_diff_rast_12<- cbind(SPMJ2_diff_rast_12[1:8],as.data.frame(
  ifelse(((SPMJ2_diff_rast_12$p.abundance.y>1.00)&(SPMJ2_diff_rast_12$p.abundance.x>1.00)),
         (((SPMJ2_diff_rast_12$p.abundance.y-SPMJ2_diff_rast_12$p.abundance.x)/SPMJ2_diff_rast_12$p.abundance.x)*100),
         (SPMJ2_diff_rast_12$p.abundance.y-SPMJ2_diff_rast_12$p.abundance.x))))
names(SPMJ2_diff_rast_12)[9]<-"p.abundance"
SPMJ2_diff_rast_12.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMJ2_diff_rast_12
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMJ2_diff_rast_12.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFA2_diff_rast_12<-merge(SPFA2012,SPFAew22012,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#SPFA2_diff_rast_12<- cbind(SPFA2_diff_rast_12[1:8],as.data.frame(((SPFA2_diff_rast_12$p.abundance.y-SPFA2_diff_rast_12$p.abundance.x)/SPFA2_diff_rast_12$p.abundance.x)*100))
SPFA2_diff_rast_12<- cbind(SPFA2_diff_rast_12[1:8],as.data.frame(
  ifelse(((SPFA2_diff_rast_12$p.abundance.y>1.00)&(SPFA2_diff_rast_12$p.abundance.x>1.00)),
         (((SPFA2_diff_rast_12$p.abundance.y-SPFA2_diff_rast_12$p.abundance.x)/SPFA2_diff_rast_12$p.abundance.x)*100),
         (SPFA2_diff_rast_12$p.abundance.y-SPFA2_diff_rast_12$p.abundance.x))))
names(SPFA2_diff_rast_12)[9]<-"p.abundance"
SPFA2_diff_rast_12.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFA2_diff_rast_12
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFA2_diff_rast_12.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMA2_diff_rast_12<-merge(SPMA2012,SPMAew22012,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#SPMA2_diff_rast_12<- cbind(SPMA2_diff_rast_12[1:8],as.data.frame(((SPMA2_diff_rast_12$p.abundance.y-SPMA2_diff_rast_12$p.abundance.x)/SPMA2_diff_rast_12$p.abundance.x)*100))
SPMA2_diff_rast_12<- cbind(SPMA2_diff_rast_12[1:8],as.data.frame(
  ifelse(((SPMA2_diff_rast_12$p.abundance.y>1.00)&(SPMA2_diff_rast_12$p.abundance.x>1.00)),
         (((SPMA2_diff_rast_12$p.abundance.y-SPMA2_diff_rast_12$p.abundance.x)/SPMA2_diff_rast_12$p.abundance.x)*100),
         (SPMA2_diff_rast_12$p.abundance.y-SPMA2_diff_rast_12$p.abundance.x))))
names(SPMA2_diff_rast_12)[9]<-"p.abundance"
SPMA2_diff_rast_12.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMA2_diff_rast_12
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMA2_diff_rast_12.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}
###2017####
FLFJ2_diff_rast_17<-merge(FLFJ2017,FLFJew22017,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#FLFJ2_diff_rast_17<- cbind(FLFJ2_diff_rast_17[1:8],as.data.frame(((FLFJ2_diff_rast_17$p.abundance.y-FLFJ2_diff_rast_17$p.abundance.x)/FLFJ2_diff_rast_17$p.abundance.x)*100))
FLFJ2_diff_rast_17<- cbind(FLFJ2_diff_rast_17[1:8],as.data.frame(
  ifelse(((FLFJ2_diff_rast_17$p.abundance.y>1.00)&(FLFJ2_diff_rast_17$p.abundance.x>1.00)),
         (((FLFJ2_diff_rast_17$p.abundance.y-FLFJ2_diff_rast_17$p.abundance.x)/FLFJ2_diff_rast_17$p.abundance.x)*100),
         (FLFJ2_diff_rast_17$p.abundance.y-FLFJ2_diff_rast_17$p.abundance.x))))
names(FLFJ2_diff_rast_17)[9]<-"p.abundance"
FLFJ2_diff_rast_17.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFJ2_diff_rast_17
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFJ2_diff_rast_17.list[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

FLMJ2_diff_rast_17<-merge(FLMJ2017,FLMJew22017,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#FLMJ2_diff_rast_17<- cbind(FLMJ2_diff_rast_17[1:8],as.data.frame(((FLMJ2_diff_rast_17$p.abundance.y-FLMJ2_diff_rast_17$p.abundance.x)/FLMJ2_diff_rast_17$p.abundance.x)*100))
FLMJ2_diff_rast_17<- cbind(FLMJ2_diff_rast_17[1:8],as.data.frame(
  ifelse(((FLMJ2_diff_rast_17$p.abundance.y>1.00)&(FLMJ2_diff_rast_17$p.abundance.x>1.00)),
         (((FLMJ2_diff_rast_17$p.abundance.y-FLMJ2_diff_rast_17$p.abundance.x)/FLMJ2_diff_rast_17$p.abundance.x)*100),
         (FLMJ2_diff_rast_17$p.abundance.y-FLMJ2_diff_rast_17$p.abundance.x))))
names(FLMJ2_diff_rast_17)[9]<-"p.abundance"
FLMJ2_diff_rast_17.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMJ2_diff_rast_17
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMJ2_diff_rast_17.list[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

FLFA2_diff_rast_17<-merge(FLFA2017,FLFAew22017,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#FLFA2_diff_rast_17<- cbind(FLFA2_diff_rast_17[1:8],as.data.frame(((FLFA2_diff_rast_17$p.abundance.y-FLFA2_diff_rast_17$p.abundance.x)/FLFA2_diff_rast_17$p.abundance.x)*100))
FLFA2_diff_rast_17<- cbind(FLFA2_diff_rast_17[1:8],as.data.frame(
  ifelse(((FLFA2_diff_rast_17$p.abundance.y>1.00)&(FLFA2_diff_rast_17$p.abundance.x>1.00)),
         (((FLFA2_diff_rast_17$p.abundance.y-FLFA2_diff_rast_17$p.abundance.x)/FLFA2_diff_rast_17$p.abundance.x)*100),
         (FLFA2_diff_rast_17$p.abundance.y-FLFA2_diff_rast_17$p.abundance.x))))
names(FLFA2_diff_rast_17)[9]<-"p.abundance"
FLFA2_diff_rast_17.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFA2_diff_rast_17
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFA2_diff_rast_17.list[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

FLMA2_diff_rast_17<-merge(FLMA2017,FLMAew22017,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#FLMA2_diff_rast_17<- cbind(FLMA2_diff_rast_17[1:8],as.data.frame(((FLMA2_diff_rast_17$p.abundance.y-FLMA2_diff_rast_17$p.abundance.x)/FLMA2_diff_rast_17$p.abundance.x)*100))
FLMA2_diff_rast_17<- cbind(FLMA2_diff_rast_17[1:8],as.data.frame(
  ifelse(((FLMA2_diff_rast_17$p.abundance.y>1.00)&(FLMA2_diff_rast_17$p.abundance.x>1.00)),
         (((FLMA2_diff_rast_17$p.abundance.y-FLMA2_diff_rast_17$p.abundance.x)/FLMA2_diff_rast_17$p.abundance.x)*100),
         (FLMA2_diff_rast_17$p.abundance.y-FLMA2_diff_rast_17$p.abundance.x))))
names(FLMA2_diff_rast_17)[9]<-"p.abundance"
FLMA2_diff_rast_17.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMA2_diff_rast_17
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMA2_diff_rast_17.list[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

SPFJ2_diff_rast_17<-merge(SPFJ2017,SPFJew22017,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#SPFJ2_diff_rast_17<- cbind(SPFJ2_diff_rast_17[1:8],as.data.frame(((SPFJ2_diff_rast_17$p.abundance.y-SPFJ2_diff_rast_17$p.abundance.x)/SPFJ2_diff_rast_17$p.abundance.x)*100))
SPFJ2_diff_rast_17<- cbind(SPFJ2_diff_rast_17[1:8],as.data.frame(
  ifelse(((SPFJ2_diff_rast_17$p.abundance.y>1.00)&(SPFJ2_diff_rast_17$p.abundance.x>1.00)),
         (((SPFJ2_diff_rast_17$p.abundance.y-SPFJ2_diff_rast_17$p.abundance.x)/SPFJ2_diff_rast_17$p.abundance.x)*100),
         (SPFJ2_diff_rast_17$p.abundance.y-SPFJ2_diff_rast_17$p.abundance.x))))
names(SPFJ2_diff_rast_17)[9]<-"p.abundance"
SPFJ2_diff_rast_17.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFJ2_diff_rast_17
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFJ2_diff_rast_17.list[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

SPMJ2_diff_rast_17<-merge(SPMJ2017,SPMJew22017,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#SPMJ2_diff_rast_17<- cbind(SPMJ2_diff_rast_17[1:8],as.data.frame(((SPMJ2_diff_rast_17$p.abundance.y-SPMJ2_diff_rast_17$p.abundance.x)/SPMJ2_diff_rast_17$p.abundance.x)*100))
SPMJ2_diff_rast_17<- cbind(SPMJ2_diff_rast_17[1:8],as.data.frame(
  ifelse(((SPMJ2_diff_rast_17$p.abundance.y>1.00)&(SPMJ2_diff_rast_17$p.abundance.x>1.00)),
         (((SPMJ2_diff_rast_17$p.abundance.y-SPMJ2_diff_rast_17$p.abundance.x)/SPMJ2_diff_rast_17$p.abundance.x)*100),
         (SPMJ2_diff_rast_17$p.abundance.y-SPMJ2_diff_rast_17$p.abundance.x))))
names(SPMJ2_diff_rast_17)[9]<-"p.abundance"
SPMJ2_diff_rast_17.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMJ2_diff_rast_17
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMJ2_diff_rast_17.list[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

SPFA2_diff_rast_17<-merge(SPFA2017,SPFAew22017,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#SPFA2_diff_rast_17<- cbind(SPFA2_diff_rast_17[1:8],as.data.frame(((SPFA2_diff_rast_17$p.abundance.y-SPFA2_diff_rast_17$p.abundance.x)/SPFA2_diff_rast_17$p.abundance.x)*100))
SPFA2_diff_rast_17<- cbind(SPFA2_diff_rast_17[1:8],as.data.frame(
  ifelse(((SPFA2_diff_rast_17$p.abundance.y>1.00)&(SPFA2_diff_rast_17$p.abundance.x>1.00)),
         (((SPFA2_diff_rast_17$p.abundance.y-SPFA2_diff_rast_17$p.abundance.x)/SPFA2_diff_rast_17$p.abundance.x)*100),
         (SPFA2_diff_rast_17$p.abundance.y-SPFA2_diff_rast_17$p.abundance.x))))
names(SPFA2_diff_rast_17)[9]<-"p.abundance"
SPFA2_diff_rast_17.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFA2_diff_rast_17
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFA2_diff_rast_17.list[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

SPMA2_diff_rast_17<-merge(SPMA2017,SPMAew22017,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
#SPMA2_diff_rast_17<- cbind(SPMA2_diff_rast_17[1:8],as.data.frame(((SPMA2_diff_rast_17$p.abundance.y-SPMA2_diff_rast_17$p.abundance.x)/SPMA2_diff_rast_17$p.abundance.x)*100))
SPMA2_diff_rast_17<- cbind(SPMA2_diff_rast_17[1:8],as.data.frame(
  ifelse(((SPMA2_diff_rast_17$p.abundance.y>1.00)&(SPMA2_diff_rast_17$p.abundance.x>1.00)),
         (((SPMA2_diff_rast_17$p.abundance.y-SPMA2_diff_rast_17$p.abundance.x)/SPMA2_diff_rast_17$p.abundance.x)*100),
         (SPMA2_diff_rast_17$p.abundance.y-SPMA2_diff_rast_17$p.abundance.x))))
names(SPMA2_diff_rast_17)[9]<-"p.abundance"
SPMA2_diff_rast_17.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMA2_diff_rast_17
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMA2_diff_rast_17.list[[i]] <- raster::extract(rast, depth_grid_plot2017)
}
#####
plotreldiff<-function(rasterdata, month,year,size,depth_grid_plot){
  
  par(mar=c(2,2,0,0), mfrow=c(1,1))
  plotvar=unlist(rasterdata)
  nclr=10
  plotclr <- rev(brewer.pal(nclr,"RdYlBu"))
  class <- classIntervals(plotvar, nclr, style="quantile")
  fix_break<-round(class$brks, digits = 2)
  for(i in 1:length(rasterdata)){
    print(i)
    plotvar <- rasterdata[[i]]
    nclr=10
    plotclr <- rev(brewer.pal(nclr,"RdYlBu"))
    class <- classIntervals(plotvar, nclr, style="fixed", 
                            fixedBreaks=
                              if(size=="J")c(-200,-100,-50,-25,-15,-5,5,15,25,50,100,200,500) #breaks calculated by using average nonstationary "quantile" breaks
                            else c(-200,-100,-50,-25,-15,-5,5,15,25,50,100,200,500))
    
    #if(size=="J")c(-100,-98,-90,-75,-50,-25,0,25,50,75,100) #breaks calculated by using average nonstationary "quantile" breaks
    #else c(-100,-98,-90,-75,-50,-25,0,25,50,75,100)) #breaks calculated by using average "qauntile" breaks
    
    colcode <- findColours(class, plotclr)
    
    start_x <- range(GAM_data$Longitude)[1]
    end_x <- range(GAM_data$Longitude)[2]
    start_y <- range(GAM_data$Latitude)[1]
    end_y <- range(GAM_data$Latitude)[2]
    plot(depth_grid_plot$lon, depth_grid_plot$lat, pch=16, col=colcode, cex=0.1, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, xlab="Longitude", ylab="Latitude")
    map(database= mainecoast, ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE, lwd=0.1)
    #plot(sa511_513, add=T)
    box()
    legend("topleft", paste(year, "-", month, sep=""), bty="n", cex=1.5)
    legend("bottomright", legend=c(names(attr(colcode, "table"))), fill=c(attr(colcode, "palette")), cex=0.9,bg="white",
           bty = "o", title="Relative Difference %")
    axis(1, cex=0.5)
    axis(2, cex=0.5)
  }
  #dev.off()
}

plotreldiff(FLFJ2_diff_rast_00.list,"FLFJ","2000","J",depth_grid_plot000612)
plotreldiff(FLMJ2_diff_rast_00.list,"FLMJ","2000","J",depth_grid_plot000612)
plotreldiff(FLFA2_diff_rast_00.list,"FLFA","2000","A",depth_grid_plot000612)
plotreldiff(FLMA2_diff_rast_00.list,"FLMA","2000","A",depth_grid_plot000612)
plotreldiff(SPFJ2_diff_rast_00.list,"SPFJ","2000","J",depth_grid_plot000612)
plotreldiff(SPMJ2_diff_rast_00.list,"SPMJ","2000","J",depth_grid_plot000612)
plotreldiff(SPFA2_diff_rast_00.list,"SPFA","2000","A",depth_grid_plot000612)
plotreldiff(SPMA2_diff_rast_00.list,"SPMA","2000","A",depth_grid_plot000612)

plotreldiff(FLFJ2_diff_rast_06.list,"FLFJ","2006","J",depth_grid_plot000612)
plotreldiff(FLMJ2_diff_rast_06.list,"FLMJ","2006","J",depth_grid_plot000612)
plotreldiff(FLFA2_diff_rast_06.list,"FLFA","2006","A",depth_grid_plot000612)
plotreldiff(FLMA2_diff_rast_06.list,"FLMA","2006","A",depth_grid_plot000612)
plotreldiff(SPFJ2_diff_rast_06.list,"SPFJ","2006","J",depth_grid_plot000612)
plotreldiff(SPMJ2_diff_rast_06.list,"SPMJ","2006","J",depth_grid_plot000612)
plotreldiff(SPFA2_diff_rast_06.list,"SPFA","2006","A",depth_grid_plot000612)
plotreldiff(SPMA2_diff_rast_06.list,"SPMA","2006","A",depth_grid_plot000612)

plotreldiff(FLFJ2_diff_rast_12.list,"FLFJ","2012","J",depth_grid_plot000612)
plotreldiff(FLMJ2_diff_rast_12.list,"FLMJ","2012","J",depth_grid_plot000612)
plotreldiff(FLFA2_diff_rast_12.list,"FLFA","2012","A",depth_grid_plot000612)
plotreldiff(FLMA2_diff_rast_12.list,"FLMA","2012","A",depth_grid_plot000612)
plotreldiff(SPFJ2_diff_rast_12.list,"SPFJ","2012","J",depth_grid_plot000612)
plotreldiff(SPMJ2_diff_rast_12.list,"SPMJ","2012","J",depth_grid_plot000612)
plotreldiff(SPFA2_diff_rast_12.list,"SPFA","2012","A",depth_grid_plot000612)
plotreldiff(SPMA2_diff_rast_12.list,"SPMA","2012","A",depth_grid_plot000612)

plotreldiff(FLFJ2_diff_rast_17.list,"FLFJ","2017","J",depth_grid_plot2017)
plotreldiff(FLMJ2_diff_rast_17.list,"FLMJ","2017","J",depth_grid_plot2017)
plotreldiff(FLFA2_diff_rast_17.list,"FLFA","2017","A",depth_grid_plot2017)
plotreldiff(FLMA2_diff_rast_17.list,"FLMA","2017","A",depth_grid_plot2017)
plotreldiff(SPFJ2_diff_rast_17.list,"SPFJ","2017","J",depth_grid_plot2017)
plotreldiff(SPMJ2_diff_rast_17.list,"SPMJ","2017","J",depth_grid_plot2017)
plotreldiff(SPFA2_diff_rast_17.list,"SPFA","2017","A",depth_grid_plot2017)
plotreldiff(SPMA2_diff_rast_17.list,"SPMA","2017","A",depth_grid_plot2017)



####Min Max#### 
library(huxtable)
table1reldiff00 <- hux(
  Group = c('FLFJ', 'FLMJ', 'FLFA','FLMA','SPFJ', 'SPMJ', 'SPFA','SPMA'),
  Max = c(max(na.omit(as.data.frame(FLFJ2_diff_rast_00.list[[1]]))), max(na.omit(as.data.frame(FLMJ2_diff_rast_00.list[[1]]))), 
          max(na.omit(as.data.frame(FLFA2_diff_rast_00.list[[1]]))),
          max(na.omit(as.data.frame(FLMA2_diff_rast_00.list[[1]]))),max(na.omit(as.data.frame(SPFJ2_diff_rast_00.list[[1]]))), 
          max(na.omit(as.data.frame(SPMJ2_diff_rast_00.list[[1]]))), max(na.omit(as.data.frame(SPFA2_diff_rast_00.list[[1]]))),
          max(na.omit(as.data.frame(SPMA2_diff_rast_00.list[[1]])))),
  Min = c(min(na.omit(as.data.frame(FLFJ2_diff_rast_00.list[[1]]))), min(na.omit(as.data.frame(FLMJ2_diff_rast_00.list[[1]]))), 
          min(na.omit(as.data.frame(FLFA2_diff_rast_00.list[[1]]))),
          min(na.omit(as.data.frame(FLMA2_diff_rast_00.list[[1]]))),min(na.omit(as.data.frame(SPFJ2_diff_rast_00.list[[1]]))), 
          min(na.omit(as.data.frame(SPMJ2_diff_rast_00.list[[1]]))), min(na.omit(as.data.frame(SPFA2_diff_rast_00.list[[1]]))),
          min(na.omit(as.data.frame(SPMA2_diff_rast_00.list[[1]])))),
  Mean = c(mean(FLFJ2_diff_rast_00.list[[1]],na.rm=TRUE), mean(FLMJ2_diff_rast_00.list[[1]],na.rm=TRUE), 
           mean(FLFA2_diff_rast_00.list[[1]],na.rm=TRUE),
           mean(FLMA2_diff_rast_00.list[[1]],na.rm=TRUE),mean(SPFJ2_diff_rast_00.list[[1]],na.rm=TRUE), 
           mean(SPMJ2_diff_rast_00.list[[1]],na.rm=TRUE), mean(SPFA2_diff_rast_00.list[[1]],na.rm=TRUE),
           mean(SPMA2_diff_rast_00.list[[1]],na.rm=TRUE)),
  Med = c(median(FLFJ2_diff_rast_00.list[[1]],na.rm=TRUE), median(FLMJ2_diff_rast_00.list[[1]],na.rm=TRUE), 
          median(FLFA2_diff_rast_00.list[[1]],na.rm=TRUE),
          median(FLMA2_diff_rast_00.list[[1]],na.rm=TRUE),median(SPFJ2_diff_rast_00.list[[1]],na.rm=TRUE), 
          median(SPMJ2_diff_rast_00.list[[1]],na.rm=TRUE), median(SPFA2_diff_rast_00.list[[1]],na.rm=TRUE),
          median(SPMA2_diff_rast_00.list[[1]],na.rm=TRUE)),add_colnames = TRUE)

bold(table1reldiff00)[1,]           <- TRUE
bottom_border(table1reldiff00)[1,]  <- 0.4
align(table1reldiff00)[,2]          <- 'right'
right_padding(table1reldiff00)      <- 10
left_padding(table1reldiff00)       <- 10
width(table1reldiff00)              <- 0.30
number_format(table1reldiff00)      <- 2

table1reldiff00

table1reldiff06 <- hux(
  Group = c('FLFJ', 'FLMJ', 'FLFA','FLMA','SPFJ', 'SPMJ', 'SPFA','SPMA'),
  Max = c(max(na.omit(as.data.frame(FLFJ2_diff_rast_06.list[[1]]))), max(na.omit(as.data.frame(FLMJ2_diff_rast_06.list[[1]]))), 
          max(na.omit(as.data.frame(FLFA2_diff_rast_06.list[[1]]))),
          max(na.omit(as.data.frame(FLMA2_diff_rast_06.list[[1]]))),max(na.omit(as.data.frame(SPFJ2_diff_rast_06.list[[1]]))), 
          max(na.omit(as.data.frame(SPMJ2_diff_rast_06.list[[1]]))), max(na.omit(as.data.frame(SPFA2_diff_rast_06.list[[1]]))),
          max(na.omit(as.data.frame(SPMA2_diff_rast_06.list[[1]])))),
  Min = c(min(na.omit(as.data.frame(FLFJ2_diff_rast_06.list[[1]]))), min(na.omit(as.data.frame(FLMJ2_diff_rast_06.list[[1]]))), 
          min(na.omit(as.data.frame(FLFA2_diff_rast_06.list[[1]]))),
          min(na.omit(as.data.frame(FLMA2_diff_rast_06.list[[1]]))),min(na.omit(as.data.frame(SPFJ2_diff_rast_06.list[[1]]))), 
          min(na.omit(as.data.frame(SPMJ2_diff_rast_06.list[[1]]))), min(na.omit(as.data.frame(SPFA2_diff_rast_06.list[[1]]))),
          min(na.omit(as.data.frame(SPMA2_diff_rast_06.list[[1]])))),
  Mean = c(mean(FLFJ2_diff_rast_06.list[[1]],na.rm=TRUE), mean(FLMJ2_diff_rast_06.list[[1]],na.rm=TRUE), 
           mean(FLFA2_diff_rast_06.list[[1]],na.rm=TRUE),
           mean(FLMA2_diff_rast_06.list[[1]],na.rm=TRUE),mean(SPFJ2_diff_rast_06.list[[1]],na.rm=TRUE), 
           mean(SPMJ2_diff_rast_06.list[[1]],na.rm=TRUE), mean(SPFA2_diff_rast_06.list[[1]],na.rm=TRUE),
           mean(SPMA2_diff_rast_06.list[[1]],na.rm=TRUE)),
  Med = c(median(FLFJ2_diff_rast_06.list[[1]],na.rm=TRUE), median(FLMJ2_diff_rast_06.list[[1]],na.rm=TRUE), 
          median(FLFA2_diff_rast_06.list[[1]],na.rm=TRUE),
          median(FLMA2_diff_rast_06.list[[1]],na.rm=TRUE),median(SPFJ2_diff_rast_06.list[[1]],na.rm=TRUE), 
          median(SPMJ2_diff_rast_06.list[[1]],na.rm=TRUE), median(SPFA2_diff_rast_06.list[[1]],na.rm=TRUE),
          median(SPMA2_diff_rast_06.list[[1]],na.rm=TRUE)),add_colnames = TRUE)

bold(table1reldiff06)[1,]           <- TRUE
bottom_border(table1reldiff06)[1,]  <- 0.4
align(table1reldiff06)[,2]          <- 'right'
right_padding(table1reldiff06)      <- 10
left_padding(table1reldiff06)       <- 10
width(table1reldiff06)              <- 0.30
number_format(table1reldiff06)      <- 2

table1reldiff06

table1reldiff12 <- hux(
  Group = c('FLFJ', 'FLMJ', 'FLFA','FLMA','SPFJ', 'SPMJ', 'SPFA','SPMA'),
  Max = c(max(na.omit(as.data.frame(FLFJ2_diff_rast_12.list[[1]]))), max(na.omit(as.data.frame(FLMJ2_diff_rast_12.list[[1]]))), 
          max(na.omit(as.data.frame(FLFA2_diff_rast_12.list[[1]]))),
          max(na.omit(as.data.frame(FLMA2_diff_rast_12.list[[1]]))),max(na.omit(as.data.frame(SPFJ2_diff_rast_12.list[[1]]))), 
          max(na.omit(as.data.frame(SPMJ2_diff_rast_12.list[[1]]))), max(na.omit(as.data.frame(SPFA2_diff_rast_12.list[[1]]))),
          max(na.omit(as.data.frame(SPMA2_diff_rast_12.list[[1]])))),
  Min = c(min(na.omit(as.data.frame(FLFJ2_diff_rast_12.list[[1]]))), min(na.omit(as.data.frame(FLMJ2_diff_rast_12.list[[1]]))), 
          min(na.omit(as.data.frame(FLFA2_diff_rast_12.list[[1]]))),
          min(na.omit(as.data.frame(FLMA2_diff_rast_12.list[[1]]))),min(na.omit(as.data.frame(SPFJ2_diff_rast_12.list[[1]]))), 
          min(na.omit(as.data.frame(SPMJ2_diff_rast_12.list[[1]]))), min(na.omit(as.data.frame(SPFA2_diff_rast_12.list[[1]]))),
          min(na.omit(as.data.frame(SPMA2_diff_rast_12.list[[1]])))),
  Mean = c(mean(FLFJ2_diff_rast_12.list[[1]],na.rm=TRUE), mean(FLMJ2_diff_rast_12.list[[1]],na.rm=TRUE), 
           mean(FLFA2_diff_rast_12.list[[1]],na.rm=TRUE),
           mean(FLMA2_diff_rast_12.list[[1]],na.rm=TRUE),mean(SPFJ2_diff_rast_12.list[[1]],na.rm=TRUE), 
           mean(SPMJ2_diff_rast_12.list[[1]],na.rm=TRUE), mean(SPFA2_diff_rast_12.list[[1]],na.rm=TRUE),
           mean(SPMA2_diff_rast_12.list[[1]],na.rm=TRUE)),
  Med = c(median(FLFJ2_diff_rast_12.list[[1]],na.rm=TRUE), median(FLMJ2_diff_rast_12.list[[1]],na.rm=TRUE), 
          median(FLFA2_diff_rast_12.list[[1]],na.rm=TRUE),
          median(FLMA2_diff_rast_12.list[[1]],na.rm=TRUE),median(SPFJ2_diff_rast_12.list[[1]],na.rm=TRUE), 
          median(SPMJ2_diff_rast_12.list[[1]],na.rm=TRUE), median(SPFA2_diff_rast_12.list[[1]],na.rm=TRUE),
          median(SPMA2_diff_rast_12.list[[1]],na.rm=TRUE)),add_colnames = TRUE)

bold(table1reldiff12)[1,]           <- TRUE
bottom_border(table1reldiff12)[1,]  <- 0.4
align(table1reldiff12)[,2]          <- 'right'
right_padding(table1reldiff12)      <- 10
left_padding(table1reldiff12)       <- 10
width(table1reldiff12)              <- 0.30
number_format(table1reldiff12)      <- 2

table1reldiff12

table1reldiff17 <- hux(
  Group = c('FLFJ', 'FLMJ', 'FLFA','FLMA','SPFJ', 'SPMJ', 'SPFA','SPMA'),
  Max = c(max(na.omit(as.data.frame(FLFJ2_diff_rast_17.list[[1]]))), max(na.omit(as.data.frame(FLMJ2_diff_rast_17.list[[1]]))), 
          max(na.omit(as.data.frame(FLFA2_diff_rast_17.list[[1]]))),
          max(na.omit(as.data.frame(FLMA2_diff_rast_17.list[[1]]))),max(na.omit(as.data.frame(SPFJ2_diff_rast_17.list[[1]]))), 
          max(na.omit(as.data.frame(SPMJ2_diff_rast_17.list[[1]]))), max(na.omit(as.data.frame(SPFA2_diff_rast_17.list[[1]]))),
          max(na.omit(as.data.frame(SPMA2_diff_rast_17.list[[1]])))),
  Min = c(min(na.omit(as.data.frame(FLFJ2_diff_rast_17.list[[1]]))), min(na.omit(as.data.frame(FLMJ2_diff_rast_17.list[[1]]))), 
          min(na.omit(as.data.frame(FLFA2_diff_rast_17.list[[1]]))),
          min(na.omit(as.data.frame(FLMA2_diff_rast_17.list[[1]]))),min(na.omit(as.data.frame(SPFJ2_diff_rast_17.list[[1]]))), 
          min(na.omit(as.data.frame(SPMJ2_diff_rast_17.list[[1]]))), min(na.omit(as.data.frame(SPFA2_diff_rast_17.list[[1]]))),
          min(na.omit(as.data.frame(SPMA2_diff_rast_17.list[[1]])))),
  Mean = c(mean(FLFJ2_diff_rast_17.list[[1]],na.rm=TRUE), mean(FLMJ2_diff_rast_17.list[[1]],na.rm=TRUE), 
           mean(FLFA2_diff_rast_17.list[[1]],na.rm=TRUE),
           mean(FLMA2_diff_rast_17.list[[1]],na.rm=TRUE),mean(SPFJ2_diff_rast_17.list[[1]],na.rm=TRUE), 
           mean(SPMJ2_diff_rast_17.list[[1]],na.rm=TRUE), mean(SPFA2_diff_rast_17.list[[1]],na.rm=TRUE),
           mean(SPMA2_diff_rast_17.list[[1]],na.rm=TRUE)),
  Med = c(median(FLFJ2_diff_rast_17.list[[1]],na.rm=TRUE), median(FLMJ2_diff_rast_17.list[[1]],na.rm=TRUE), 
          median(FLFA2_diff_rast_17.list[[1]],na.rm=TRUE),
          median(FLMA2_diff_rast_17.list[[1]],na.rm=TRUE),median(SPFJ2_diff_rast_17.list[[1]],na.rm=TRUE), 
          median(SPMJ2_diff_rast_17.list[[1]],na.rm=TRUE), median(SPFA2_diff_rast_17.list[[1]],na.rm=TRUE),
          median(SPMA2_diff_rast_17.list[[1]],na.rm=TRUE)),add_colnames = TRUE)

bold(table1reldiff17)[1,]           <- TRUE
bottom_border(table1reldiff17)[1,]  <- 0.4
align(table1reldiff17)[,2]          <- 'right'
right_padding(table1reldiff17)      <- 10
left_padding(table1reldiff17)       <- 10
width(table1reldiff17)              <- 0.30
number_format(table1reldiff17)      <- 2

table1reldiff17

#####Calculate and plot REGULAR differences between NSV2 and S predicted abundancies####

###2000####

FLFJ2_REGdiffdiff_rast_00<-merge(FLFJ2000,FLFJew22000,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
FLFJ2_REGdiffdiff_rast_00<- cbind(FLFJ2_REGdiffdiff_rast_00[1:8],as.data.frame((FLFJ2_REGdiffdiff_rast_00$p.abundance.y-FLFJ2_REGdiffdiff_rast_00$p.abundance.x)))
names(FLFJ2_REGdiffdiff_rast_00)[9]<-"p.abundance"
FLFJ2_REGdiffdiff_rast_00.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFJ2_REGdiffdiff_rast_00
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFJ2_REGdiffdiff_rast_00.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}


FLMJ2_REGdiffdiff_rast_00<-merge(FLMJ2000,FLMJew22000,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
FLMJ2_REGdiffdiff_rast_00<- cbind(FLMJ2_REGdiffdiff_rast_00[1:8],as.data.frame((FLMJ2_REGdiffdiff_rast_00$p.abundance.y-FLMJ2_REGdiffdiff_rast_00$p.abundance.x)))
names(FLMJ2_REGdiffdiff_rast_00)[9]<-"p.abundance"
FLMJ2_REGdiffdiff_rast_00.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMJ2_REGdiffdiff_rast_00
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMJ2_REGdiffdiff_rast_00.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLFA2_REGdiffdiff_rast_00<-merge(FLFA2000,FLFAew22000,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
FLFA2_REGdiffdiff_rast_00<- cbind(FLFA2_REGdiffdiff_rast_00[1:8],as.data.frame((FLFA2_REGdiffdiff_rast_00$p.abundance.y-FLFA2_REGdiffdiff_rast_00$p.abundance.x)))
names(FLFA2_REGdiffdiff_rast_00)[9]<-"p.abundance"
FLFA2_REGdiffdiff_rast_00.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFA2_REGdiffdiff_rast_00
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row, duplicate= "mean"))
  rast <- raster(akima.smooth)
  FLFA2_REGdiffdiff_rast_00.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMA2_REGdiffdiff_rast_00<-merge(FLMA2000,FLMAew22000,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
FLMA2_REGdiffdiff_rast_00<- cbind(FLMA2_REGdiffdiff_rast_00[1:8],as.data.frame((FLMA2_REGdiffdiff_rast_00$p.abundance.y-FLMA2_REGdiffdiff_rast_00$p.abundance.x)))
names(FLMA2_REGdiffdiff_rast_00)[9]<-"p.abundance"
FLMA2_REGdiffdiff_rast_00.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMA2_REGdiffdiff_rast_00
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMA2_REGdiffdiff_rast_00.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFJ2_REGdiffdiff_rast_00<-merge(SPFJ2000,SPFJew22000,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
SPFJ2_REGdiffdiff_rast_00<- cbind(SPFJ2_REGdiffdiff_rast_00[1:8],as.data.frame((SPFJ2_REGdiffdiff_rast_00$p.abundance.y-SPFJ2_REGdiffdiff_rast_00$p.abundance.x)))
names(SPFJ2_REGdiffdiff_rast_00)[9]<-"p.abundance"
SPFJ2_REGdiffdiff_rast_00.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFJ2_REGdiffdiff_rast_00
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFJ2_REGdiffdiff_rast_00.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMJ2_REGdiffdiff_rast_00<-merge(SPMJ2000,SPMJew22000,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
SPMJ2_REGdiffdiff_rast_00<- cbind(SPMJ2_REGdiffdiff_rast_00[1:8],as.data.frame((SPMJ2_REGdiffdiff_rast_00$p.abundance.y-SPMJ2_REGdiffdiff_rast_00$p.abundance.x)))
names(SPMJ2_REGdiffdiff_rast_00)[9]<-"p.abundance"
SPMJ2_REGdiffdiff_rast_00.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMJ2_REGdiffdiff_rast_00
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMJ2_REGdiffdiff_rast_00.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFA2_REGdiffdiff_rast_00<-merge(SPFA2000,SPFAew22000,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
SPFA2_REGdiffdiff_rast_00<- cbind(SPFA2_REGdiffdiff_rast_00[1:8],as.data.frame((SPFA2_REGdiffdiff_rast_00$p.abundance.y-SPFA2_REGdiffdiff_rast_00$p.abundance.x)))
names(SPFA2_REGdiffdiff_rast_00)[9]<-"p.abundance"
SPFA2_REGdiffdiff_rast_00.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFA2_REGdiffdiff_rast_00
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFA2_REGdiffdiff_rast_00.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMA2_REGdiffdiff_rast_00<-merge(SPMA2000,SPMAew22000,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
SPMA2_REGdiffdiff_rast_00<- cbind(SPMA2_REGdiffdiff_rast_00[1:8],as.data.frame((SPMA2_REGdiffdiff_rast_00$p.abundance.y-SPMA2_REGdiffdiff_rast_00$p.abundance.x)))
names(SPMA2_REGdiffdiff_rast_00)[9]<-"p.abundance"
SPMA2_REGdiffdiff_rast_00.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMA2_REGdiffdiff_rast_00
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMA2_REGdiffdiff_rast_00.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}
###2006####
FLFJ2_REGdiffdiff_rast_06<-merge(FLFJ2006,FLFJew22006,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
FLFJ2_REGdiffdiff_rast_06<- cbind(FLFJ2_REGdiffdiff_rast_06[1:8],as.data.frame((FLFJ2_REGdiffdiff_rast_06$p.abundance.y-FLFJ2_REGdiffdiff_rast_06$p.abundance.x)))
names(FLFJ2_REGdiffdiff_rast_06)[9]<-"p.abundance"
FLFJ2_REGdiffdiff_rast_06.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFJ2_REGdiffdiff_rast_06
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFJ2_REGdiffdiff_rast_06.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMJ2_REGdiffdiff_rast_06<-merge(FLMJ2006,FLMJew22006,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
FLMJ2_REGdiffdiff_rast_06<- cbind(FLMJ2_REGdiffdiff_rast_06[1:8],as.data.frame((FLMJ2_REGdiffdiff_rast_06$p.abundance.y-FLMJ2_REGdiffdiff_rast_06$p.abundance.x)))
names(FLMJ2_REGdiffdiff_rast_06)[9]<-"p.abundance"
FLMJ2_REGdiffdiff_rast_06.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMJ2_REGdiffdiff_rast_06
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMJ2_REGdiffdiff_rast_06.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLFA2_REGdiffdiff_rast_06<-merge(FLFA2006,FLFAew22006,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
FLFA2_REGdiffdiff_rast_06<- cbind(FLFA2_REGdiffdiff_rast_06[1:8],as.data.frame((FLFA2_REGdiffdiff_rast_06$p.abundance.y-FLFA2_REGdiffdiff_rast_06$p.abundance.x)))
names(FLFA2_REGdiffdiff_rast_06)[9]<-"p.abundance"
FLFA2_REGdiffdiff_rast_06.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFA2_REGdiffdiff_rast_06
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFA2_REGdiffdiff_rast_06.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMA2_REGdiffdiff_rast_06<-merge(FLMA2006,FLMAew22006,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
FLMA2_REGdiffdiff_rast_06<- cbind(FLMA2_REGdiffdiff_rast_06[1:8],as.data.frame((FLMA2_REGdiffdiff_rast_06$p.abundance.y-FLMA2_REGdiffdiff_rast_06$p.abundance.x)))
names(FLMA2_REGdiffdiff_rast_06)[9]<-"p.abundance"
FLMA2_REGdiffdiff_rast_06.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMA2_REGdiffdiff_rast_06
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMA2_REGdiffdiff_rast_06.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFJ2_REGdiffdiff_rast_06<-merge(SPFJ2006,SPFJew22006,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
SPFJ2_REGdiffdiff_rast_06<- cbind(SPFJ2_REGdiffdiff_rast_06[1:8],as.data.frame((SPFJ2_REGdiffdiff_rast_06$p.abundance.y-SPFJ2_REGdiffdiff_rast_06$p.abundance.x)))
names(SPFJ2_REGdiffdiff_rast_06)[9]<-"p.abundance"
SPFJ2_REGdiffdiff_rast_06.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFJ2_REGdiffdiff_rast_06
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFJ2_REGdiffdiff_rast_06.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMJ2_REGdiffdiff_rast_06<-merge(SPMJ2006,SPMJew22006,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
SPMJ2_REGdiffdiff_rast_06<- cbind(SPMJ2_REGdiffdiff_rast_06[1:8],as.data.frame((SPMJ2_REGdiffdiff_rast_06$p.abundance.y-SPMJ2_REGdiffdiff_rast_06$p.abundance.x)))
names(SPMJ2_REGdiffdiff_rast_06)[9]<-"p.abundance"
SPMJ2_REGdiffdiff_rast_06.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMJ2_REGdiffdiff_rast_06
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMJ2_REGdiffdiff_rast_06.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFA2_REGdiffdiff_rast_06<-merge(SPFA2006,SPFAew22006,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
SPFA2_REGdiffdiff_rast_06<- cbind(SPFA2_REGdiffdiff_rast_06[1:8],as.data.frame((SPFA2_REGdiffdiff_rast_06$p.abundance.y-SPFA2_REGdiffdiff_rast_06$p.abundance.x)))
names(SPFA2_REGdiffdiff_rast_06)[9]<-"p.abundance"
SPFA2_REGdiffdiff_rast_06.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFA2_REGdiffdiff_rast_06
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFA2_REGdiffdiff_rast_06.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMA2_REGdiffdiff_rast_06<-merge(SPMA2006,SPMAew22006,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
SPMA2_REGdiffdiff_rast_06<- cbind(SPMA2_REGdiffdiff_rast_06[1:8],as.data.frame((SPMA2_REGdiffdiff_rast_06$p.abundance.y-SPMA2_REGdiffdiff_rast_06$p.abundance.x)))
names(SPMA2_REGdiffdiff_rast_06)[9]<-"p.abundance"
SPMA2_REGdiffdiff_rast_06.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMA2_REGdiffdiff_rast_06
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMA2_REGdiffdiff_rast_06.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}
###2012####
FLFJ2_REGdiffdiff_rast_12<-merge(FLFJ2012,FLFJew22012,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
FLFJ2_REGdiffdiff_rast_12<- cbind(FLFJ2_REGdiffdiff_rast_12[1:8],as.data.frame((FLFJ2_REGdiffdiff_rast_12$p.abundance.y-FLFJ2_REGdiffdiff_rast_12$p.abundance.x)))
names(FLFJ2_REGdiffdiff_rast_12)[9]<-"p.abundance"
FLFJ2_REGdiffdiff_rast_12.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFJ2_REGdiffdiff_rast_12
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFJ2_REGdiffdiff_rast_12.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMJ2_REGdiffdiff_rast_12<-merge(FLMJ2012,FLMJew22012,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
FLMJ2_REGdiffdiff_rast_12<- cbind(FLMJ2_REGdiffdiff_rast_12[1:8],as.data.frame((FLMJ2_REGdiffdiff_rast_12$p.abundance.y-FLMJ2_REGdiffdiff_rast_12$p.abundance.x)))
names(FLMJ2_REGdiffdiff_rast_12)[9]<-"p.abundance"
FLMJ2_REGdiffdiff_rast_12.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMJ2_REGdiffdiff_rast_12
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMJ2_REGdiffdiff_rast_12.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLFA2_REGdiffdiff_rast_12<-merge(FLFA2012,FLFAew22012,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
FLFA2_REGdiffdiff_rast_12<- cbind(FLFA2_REGdiffdiff_rast_12[1:8],as.data.frame((FLFA2_REGdiffdiff_rast_12$p.abundance.y-FLFA2_REGdiffdiff_rast_12$p.abundance.x)))
names(FLFA2_REGdiffdiff_rast_12)[9]<-"p.abundance"
FLFA2_REGdiffdiff_rast_12.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFA2_REGdiffdiff_rast_12
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFA2_REGdiffdiff_rast_12.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMA2_REGdiffdiff_rast_12<-merge(FLMA2012,FLMAew22012,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
FLMA2_REGdiffdiff_rast_12<- cbind(FLMA2_REGdiffdiff_rast_12[1:8],as.data.frame((FLMA2_REGdiffdiff_rast_12$p.abundance.y-FLMA2_REGdiffdiff_rast_12$p.abundance.x)))
names(FLMA2_REGdiffdiff_rast_12)[9]<-"p.abundance"
FLMA2_REGdiffdiff_rast_12.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMA2_REGdiffdiff_rast_12
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMA2_REGdiffdiff_rast_12.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFJ2_REGdiffdiff_rast_12<-merge(SPFJ2012,SPFJew22012,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
SPFJ2_REGdiffdiff_rast_12<- cbind(SPFJ2_REGdiffdiff_rast_12[1:8],as.data.frame((SPFJ2_REGdiffdiff_rast_12$p.abundance.y-SPFJ2_REGdiffdiff_rast_12$p.abundance.x)))
names(SPFJ2_REGdiffdiff_rast_12)[9]<-"p.abundance"
SPFJ2_REGdiffdiff_rast_12.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFJ2_REGdiffdiff_rast_12
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFJ2_REGdiffdiff_rast_12.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMJ2_REGdiffdiff_rast_12<-merge(SPMJ2012,SPMJew22012,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
SPMJ2_REGdiffdiff_rast_12<- cbind(SPMJ2_REGdiffdiff_rast_12[1:8],as.data.frame((SPMJ2_REGdiffdiff_rast_12$p.abundance.y-SPMJ2_REGdiffdiff_rast_12$p.abundance.x)))
names(SPMJ2_REGdiffdiff_rast_12)[9]<-"p.abundance"
SPMJ2_REGdiffdiff_rast_12.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMJ2_REGdiffdiff_rast_12
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMJ2_REGdiffdiff_rast_12.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFA2_REGdiffdiff_rast_12<-merge(SPFA2012,SPFAew22012,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
SPFA2_REGdiffdiff_rast_12<- cbind(SPFA2_REGdiffdiff_rast_12[1:8],as.data.frame((SPFA2_REGdiffdiff_rast_12$p.abundance.y-SPFA2_REGdiffdiff_rast_12$p.abundance.x)))
names(SPFA2_REGdiffdiff_rast_12)[9]<-"p.abundance"
SPFA2_REGdiffdiff_rast_12.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFA2_REGdiffdiff_rast_12
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFA2_REGdiffdiff_rast_12.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMA2_REGdiffdiff_rast_12<-merge(SPMA2012,SPMAew22012,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
SPMA2_REGdiffdiff_rast_12<- cbind(SPMA2_REGdiffdiff_rast_12[1:8],as.data.frame((SPMA2_REGdiffdiff_rast_12$p.abundance.y-SPMA2_REGdiffdiff_rast_12$p.abundance.x)))
names(SPMA2_REGdiffdiff_rast_12)[9]<-"p.abundance"
SPMA2_REGdiffdiff_rast_12.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMA2_REGdiffdiff_rast_12
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMA2_REGdiffdiff_rast_12.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}
###2017#####
FLFJ2_REGdiffdiff_rast_17<-merge(FLFJ2017,FLFJew22017,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
FLFJ2_REGdiffdiff_rast_17<- cbind(FLFJ2_REGdiffdiff_rast_17[1:8],as.data.frame((FLFJ2_REGdiffdiff_rast_17$p.abundance.y-FLFJ2_REGdiffdiff_rast_17$p.abundance.x)))
names(FLFJ2_REGdiffdiff_rast_17)[9]<-"p.abundance"
FLFJ2_REGdiffdiff_rast_17.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFJ2_REGdiffdiff_rast_17
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFJ2_REGdiffdiff_rast_17.list[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

FLMJ2_REGdiffdiff_rast_17<-merge(FLMJ2017,FLMJew22017,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
FLMJ2_REGdiffdiff_rast_17<- cbind(FLMJ2_REGdiffdiff_rast_17[1:8],as.data.frame((FLMJ2_REGdiffdiff_rast_17$p.abundance.y-FLMJ2_REGdiffdiff_rast_17$p.abundance.x)))
names(FLMJ2_REGdiffdiff_rast_17)[9]<-"p.abundance"
FLMJ2_REGdiffdiff_rast_17.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMJ2_REGdiffdiff_rast_17
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMJ2_REGdiffdiff_rast_17.list[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

FLFA2_REGdiffdiff_rast_17<-merge(FLFA2017,FLFAew22017,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
FLFA2_REGdiffdiff_rast_17<- cbind(FLFA2_REGdiffdiff_rast_17[1:8],as.data.frame((FLFA2_REGdiffdiff_rast_17$p.abundance.y-FLFA2_REGdiffdiff_rast_17$p.abundance.x)))
names(FLFA2_REGdiffdiff_rast_17)[9]<-"p.abundance"
FLFA2_REGdiffdiff_rast_17.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFA2_REGdiffdiff_rast_17
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFA2_REGdiffdiff_rast_17.list[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

FLMA2_REGdiffdiff_rast_17<-merge(FLMA2017,FLMAew22017,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
FLMA2_REGdiffdiff_rast_17<- cbind(FLMA2_REGdiffdiff_rast_17[1:8],as.data.frame((FLMA2_REGdiffdiff_rast_17$p.abundance.y-FLMA2_REGdiffdiff_rast_17$p.abundance.x)))
names(FLMA2_REGdiffdiff_rast_17)[9]<-"p.abundance"
FLMA2_REGdiffdiff_rast_17.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMA2_REGdiffdiff_rast_17
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMA2_REGdiffdiff_rast_17.list[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

SPFJ2_REGdiffdiff_rast_17<-merge(SPFJ2017,SPFJew22017,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
SPFJ2_REGdiffdiff_rast_17<- cbind(SPFJ2_REGdiffdiff_rast_17[1:8],as.data.frame((SPFJ2_REGdiffdiff_rast_17$p.abundance.y-SPFJ2_REGdiffdiff_rast_17$p.abundance.x)))
names(SPFJ2_REGdiffdiff_rast_17)[9]<-"p.abundance"
SPFJ2_REGdiffdiff_rast_17.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFJ2_REGdiffdiff_rast_17
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFJ2_REGdiffdiff_rast_17.list[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

SPMJ2_REGdiffdiff_rast_17<-merge(SPMJ2017,SPMJew22017,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
SPMJ2_REGdiffdiff_rast_17<- cbind(SPMJ2_REGdiffdiff_rast_17[1:8],as.data.frame((SPMJ2_REGdiffdiff_rast_17$p.abundance.y-SPMJ2_REGdiffdiff_rast_17$p.abundance.x)))
names(SPMJ2_REGdiffdiff_rast_17)[9]<-"p.abundance"
SPMJ2_REGdiffdiff_rast_17.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMJ2_REGdiffdiff_rast_17
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMJ2_REGdiffdiff_rast_17.list[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

SPFA2_REGdiffdiff_rast_17<-merge(SPFA2017,SPFAew22017,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
SPFA2_REGdiffdiff_rast_17<- cbind(SPFA2_REGdiffdiff_rast_17[1:8],as.data.frame((SPFA2_REGdiffdiff_rast_17$p.abundance.y-SPFA2_REGdiffdiff_rast_17$p.abundance.x)))
names(SPFA2_REGdiffdiff_rast_17)[9]<-"p.abundance"
SPFA2_REGdiffdiff_rast_17.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFA2_REGdiffdiff_rast_17
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFA2_REGdiffdiff_rast_17.list[[i]] <- raster::extract(rast, depth_grid_plot2017)
}

SPMA2_REGdiffdiff_rast_17<-merge(SPMA2017,SPMAew22017,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","Year","AvgDepth"))
SPMA2_REGdiffdiff_rast_17<- cbind(SPMA2_REGdiffdiff_rast_17[1:8],as.data.frame((SPMA2_REGdiffdiff_rast_17$p.abundance.y-SPMA2_REGdiffdiff_rast_17$p.abundance.x)))
names(SPMA2_REGdiffdiff_rast_17)[9]<-"p.abundance"
SPMA2_REGdiffdiff_rast_17.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMA2_REGdiffdiff_rast_17
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMA2_REGdiffdiff_rast_17.list[[i]] <- raster::extract(rast, depth_grid_plot2017)
}
#####
plotdiff<-function(rasterdata, month,year,size,depth_grid_plot){
  
  par(mar=c(2,2,0,0), mfrow=c(1,1))
  plotvar=unlist(rasterdata)
  nclr=10
  plotclr <- rev(brewer.pal(nclr,"RdYlBu"))
  class <- classIntervals(plotvar, nclr, style="quantile")
  fix_break<-round(class$brks, digits = 2)
  for(i in 1:length(rasterdata)){
    print(i)
    plotvar <- rasterdata[[i]]
    nclr=10
    plotclr <- rev(brewer.pal(nclr,"RdYlBu"))
    class <- classIntervals(plotvar, nclr, style="fixed", 
                            fixedBreaks=
                              if(size=="J")c(-200,-100,-50,-25,-15,-5,5,15,25,50,100,200,500) #breaks calculated by using average nonstationary "quantile" breaks
                            else c(-200,-100,-50,-25,-15,-5,5,15,25,50,100,200,500))
    
    #if(size=="J")c(-100,-98,-90,-75,-50,-25,0,25,50,75,100) #breaks calculated by using average nonstationary "quantile" breaks
    #else c(-100,-98,-90,-75,-50,-25,0,25,50,75,100)) #breaks calculated by using average "qauntile" breaks
    
    colcode <- findColours(class, plotclr)
    
    start_x <- range(GAM_data$Longitude)[1]
    end_x <- range(GAM_data$Longitude)[2]
    start_y <- range(GAM_data$Latitude)[1]
    end_y <- range(GAM_data$Latitude)[2]
    plot(depth_grid_plot$lon, depth_grid_plot$lat, pch=16, col=colcode, cex=0.1, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, xlab="Longitude", ylab="Latitude")
    map(database= mainecoast, ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE, lwd=0.1)
    #plot(sa511_513, add=T)
    box()
    legend("topleft", paste(year, "-", month, sep=""), bty="n", cex=1.5)
    legend("bottomright", legend=c(names(attr(colcode, "table"))), fill=c(attr(colcode, "palette")), cex=0.9,bg="white",
           bty = "o", title="Difference #")
    axis(1, cex=0.5)
    axis(2, cex=0.5)
  }
  #dev.off()
}

plotdiff(FLFJ2_REGdiffdiff_rast_00.list,"FLFJ","2000","J",depth_grid_plot000612)
plotdiff(FLMJ2_REGdiffdiff_rast_00.list,"FLMJ","2000","J",depth_grid_plot000612)
plotdiff(FLFA2_REGdiffdiff_rast_00.list,"FLFA","2000","A",depth_grid_plot000612)
plotdiff(FLMA2_REGdiffdiff_rast_00.list,"FLMA","2000","A",depth_grid_plot000612)
plotdiff(SPFJ2_REGdiffdiff_rast_00.list,"SPFJ","2000","J",depth_grid_plot000612)
plotdiff(SPMJ2_REGdiffdiff_rast_00.list,"SPMJ","2000","J",depth_grid_plot000612)
plotdiff(SPFA2_REGdiffdiff_rast_00.list,"SPFA","2000","A",depth_grid_plot000612)
plotdiff(SPMA2_REGdiffdiff_rast_00.list,"SPMA","2000","A",depth_grid_plot000612)

plotdiff(FLFJ2_REGdiffdiff_rast_06.list,"FLFJ","2006","J",depth_grid_plot000612)
plotdiff(FLMJ2_REGdiffdiff_rast_06.list,"FLMJ","2006","J",depth_grid_plot000612)
plotdiff(FLFA2_REGdiffdiff_rast_06.list,"FLFA","2006","A",depth_grid_plot000612)
plotdiff(FLMA2_REGdiffdiff_rast_06.list,"FLMA","2006","A",depth_grid_plot000612)
plotdiff(SPFJ2_REGdiffdiff_rast_06.list,"SPFJ","2006","J",depth_grid_plot000612)
plotdiff(SPMJ2_REGdiffdiff_rast_06.list,"SPMJ","2006","J",depth_grid_plot000612)
plotdiff(SPFA2_REGdiffdiff_rast_06.list,"SPFA","2006","A",depth_grid_plot000612)
plotdiff(SPMA2_REGdiffdiff_rast_06.list,"SPMA","2006","A",depth_grid_plot000612)

plotdiff(FLFJ2_REGdiffdiff_rast_12.list,"FLFJ","2012","J",depth_grid_plot000612)
plotdiff(FLMJ2_REGdiffdiff_rast_12.list,"FLMJ","2012","J",depth_grid_plot000612)
plotdiff(FLFA2_REGdiffdiff_rast_12.list,"FLFA","2012","A",depth_grid_plot000612)
plotdiff(FLMA2_REGdiffdiff_rast_12.list,"FLMA","2012","A",depth_grid_plot000612)
plotdiff(SPFJ2_REGdiffdiff_rast_12.list,"SPFJ","2012","J",depth_grid_plot000612)
plotdiff(SPMJ2_REGdiffdiff_rast_12.list,"SPMJ","2012","J",depth_grid_plot000612)
plotdiff(SPFA2_REGdiffdiff_rast_12.list,"SPFA","2012","A",depth_grid_plot000612)
plotdiff(SPMA2_REGdiffdiff_rast_12.list,"SPMA","2012","A",depth_grid_plot000612)  

plotdiff(FLFJ2_REGdiffdiff_rast_17.list,"FLFJ","2017","J",depth_grid_plot2017)
plotdiff(FLMJ2_REGdiffdiff_rast_17.list,"FLMJ","2017","J",depth_grid_plot2017)
plotdiff(FLFA2_REGdiffdiff_rast_17.list,"FLFA","2017","A",depth_grid_plot2017)
plotdiff(FLMA2_REGdiffdiff_rast_17.list,"FLMA","2017","A",depth_grid_plot2017)
plotdiff(SPFJ2_REGdiffdiff_rast_17.list,"SPFJ","2017","J",depth_grid_plot2017)
plotdiff(SPMJ2_REGdiffdiff_rast_17.list,"SPMJ","2017","J",depth_grid_plot2017)
plotdiff(SPFA2_REGdiffdiff_rast_17.list,"SPFA","2017","A",depth_grid_plot2017)
plotdiff(SPMA2_REGdiffdiff_rast_17.list,"SPMA","2017","A",depth_grid_plot2017)   









####Min Max#### 
library(huxtable)
table1regdiff00 <- hux(
  Group = c('FLFJ', 'FLMJ', 'FLFA','FLMA','SPFJ', 'SPMJ', 'SPFA','SPMA'),
  Max = c(max(na.omit(as.data.frame(FLFJ2_REGdiffdiff_rast_00.list[[1]]))), max(na.omit(as.data.frame(FLMJ2_REGdiffdiff_rast_00.list[[1]]))), 
          max(na.omit(as.data.frame(FLFA2_REGdiffdiff_rast_00.list[[1]]))),
          max(na.omit(as.data.frame(FLMA2_REGdiffdiff_rast_00.list[[1]]))),max(na.omit(as.data.frame(SPFJ2_REGdiffdiff_rast_00.list[[1]]))), 
          max(na.omit(as.data.frame(SPMJ2_REGdiffdiff_rast_00.list[[1]]))), max(na.omit(as.data.frame(SPFA2_REGdiffdiff_rast_00.list[[1]]))),
          max(na.omit(as.data.frame(SPMA2_REGdiffdiff_rast_00.list[[1]])))),
  Min = c(min(na.omit(as.data.frame(FLFJ2_REGdiffdiff_rast_00.list[[1]]))), min(na.omit(as.data.frame(FLMJ2_REGdiffdiff_rast_00.list[[1]]))), 
          min(na.omit(as.data.frame(FLFA2_REGdiffdiff_rast_00.list[[1]]))),
          min(na.omit(as.data.frame(FLMA2_REGdiffdiff_rast_00.list[[1]]))),min(na.omit(as.data.frame(SPFJ2_REGdiffdiff_rast_00.list[[1]]))), 
          min(na.omit(as.data.frame(SPMJ2_REGdiffdiff_rast_00.list[[1]]))), min(na.omit(as.data.frame(SPFA2_REGdiffdiff_rast_00.list[[1]]))),
          min(na.omit(as.data.frame(SPMA2_REGdiffdiff_rast_00.list[[1]])))),
  Mean = c(mean(FLFJ2_REGdiffdiff_rast_00.list[[1]],na.rm=TRUE), mean(FLMJ2_REGdiffdiff_rast_00.list[[1]],na.rm=TRUE), 
           mean(FLFA2_REGdiffdiff_rast_00.list[[1]],na.rm=TRUE),
           mean(FLMA2_REGdiffdiff_rast_00.list[[1]],na.rm=TRUE),mean(SPFJ2_REGdiffdiff_rast_00.list[[1]],na.rm=TRUE), 
           mean(SPMJ2_REGdiffdiff_rast_00.list[[1]],na.rm=TRUE), mean(SPFA2_REGdiffdiff_rast_00.list[[1]],na.rm=TRUE),
           mean(SPMA2_REGdiffdiff_rast_00.list[[1]],na.rm=TRUE)),
  Med = c(median(FLFJ2_REGdiffdiff_rast_00.list[[1]],na.rm=TRUE), median(FLMJ2_REGdiffdiff_rast_00.list[[1]],na.rm=TRUE), 
          median(FLFA2_REGdiffdiff_rast_00.list[[1]],na.rm=TRUE),
          median(FLMA2_REGdiffdiff_rast_00.list[[1]],na.rm=TRUE),median(SPFJ2_REGdiffdiff_rast_00.list[[1]],na.rm=TRUE), 
          median(SPMJ2_REGdiffdiff_rast_00.list[[1]],na.rm=TRUE), median(SPFA2_REGdiffdiff_rast_00.list[[1]],na.rm=TRUE),
          median(SPMA2_REGdiffdiff_rast_00.list[[1]],na.rm=TRUE)),add_colnames = TRUE)

bold(table1regdiff00)[1,]           <- TRUE
bottom_border(table1regdiff00)[1,]  <- 0.4
align(table1regdiff00)[,2]          <- 'right'
right_padding(table1regdiff00)      <- 10
left_padding(table1regdiff00)       <- 10
width(table1regdiff00)              <- 0.30
number_format(table1regdiff00)      <- 2

table1regdiff00

table1regdiff06 <- hux(
  Group = c('FLFJ', 'FLMJ', 'FLFA','FLMA','SPFJ', 'SPMJ', 'SPFA','SPMA'),
  Max = c(max(na.omit(as.data.frame(FLFJ2_REGdiffdiff_rast_06.list[[1]]))), max(na.omit(as.data.frame(FLMJ2_REGdiffdiff_rast_06.list[[1]]))), 
          max(na.omit(as.data.frame(FLFA2_REGdiffdiff_rast_06.list[[1]]))),
          max(na.omit(as.data.frame(FLMA2_REGdiffdiff_rast_06.list[[1]]))),max(na.omit(as.data.frame(SPFJ2_REGdiffdiff_rast_06.list[[1]]))), 
          max(na.omit(as.data.frame(SPMJ2_REGdiffdiff_rast_06.list[[1]]))), max(na.omit(as.data.frame(SPFA2_REGdiffdiff_rast_06.list[[1]]))),
          max(na.omit(as.data.frame(SPMA2_REGdiffdiff_rast_06.list[[1]])))),
  Min = c(min(na.omit(as.data.frame(FLFJ2_REGdiffdiff_rast_06.list[[1]]))), min(na.omit(as.data.frame(FLMJ2_REGdiffdiff_rast_06.list[[1]]))), 
          min(na.omit(as.data.frame(FLFA2_REGdiffdiff_rast_06.list[[1]]))),
          min(na.omit(as.data.frame(FLMA2_REGdiffdiff_rast_06.list[[1]]))),min(na.omit(as.data.frame(SPFJ2_REGdiffdiff_rast_06.list[[1]]))), 
          min(na.omit(as.data.frame(SPMJ2_REGdiffdiff_rast_06.list[[1]]))), min(na.omit(as.data.frame(SPFA2_REGdiffdiff_rast_06.list[[1]]))),
          min(na.omit(as.data.frame(SPMA2_REGdiffdiff_rast_06.list[[1]])))),
  Mean = c(mean(FLFJ2_REGdiffdiff_rast_06.list[[1]],na.rm=TRUE), mean(FLMJ2_REGdiffdiff_rast_06.list[[1]],na.rm=TRUE), 
           mean(FLFA2_REGdiffdiff_rast_06.list[[1]],na.rm=TRUE),
           mean(FLMA2_REGdiffdiff_rast_06.list[[1]],na.rm=TRUE),mean(SPFJ2_REGdiffdiff_rast_06.list[[1]],na.rm=TRUE), 
           mean(SPMJ2_REGdiffdiff_rast_06.list[[1]],na.rm=TRUE), mean(SPFA2_REGdiffdiff_rast_06.list[[1]],na.rm=TRUE),
           mean(SPMA2_REGdiffdiff_rast_06.list[[1]],na.rm=TRUE)),
  Med = c(median(FLFJ2_REGdiffdiff_rast_06.list[[1]],na.rm=TRUE), median(FLMJ2_REGdiffdiff_rast_06.list[[1]],na.rm=TRUE), 
          median(FLFA2_REGdiffdiff_rast_06.list[[1]],na.rm=TRUE),
          median(FLMA2_REGdiffdiff_rast_06.list[[1]],na.rm=TRUE),median(SPFJ2_REGdiffdiff_rast_06.list[[1]],na.rm=TRUE), 
          median(SPMJ2_REGdiffdiff_rast_06.list[[1]],na.rm=TRUE), median(SPFA2_REGdiffdiff_rast_06.list[[1]],na.rm=TRUE),
          median(SPMA2_REGdiffdiff_rast_06.list[[1]],na.rm=TRUE)),add_colnames = TRUE)

bold(table1regdiff06)[1,]           <- TRUE
bottom_border(table1regdiff06)[1,]  <- 0.4
align(table1regdiff06)[,2]          <- 'right'
right_padding(table1regdiff06)      <- 10
left_padding(table1regdiff06)       <- 10
width(table1regdiff06)              <- 0.30
number_format(table1regdiff06)      <- 2

table1regdiff06

table1regdiff12 <- hux(
  Group = c('FLFJ', 'FLMJ', 'FLFA','FLMA','SPFJ', 'SPMJ', 'SPFA','SPMA'),
  Max = c(max(na.omit(as.data.frame(FLFJ2_REGdiffdiff_rast_12.list[[1]]))), max(na.omit(as.data.frame(FLMJ2_REGdiffdiff_rast_12.list[[1]]))), 
          max(na.omit(as.data.frame(FLFA2_REGdiffdiff_rast_12.list[[1]]))),
          max(na.omit(as.data.frame(FLMA2_REGdiffdiff_rast_12.list[[1]]))),max(na.omit(as.data.frame(SPFJ2_REGdiffdiff_rast_12.list[[1]]))), 
          max(na.omit(as.data.frame(SPMJ2_REGdiffdiff_rast_12.list[[1]]))), max(na.omit(as.data.frame(SPFA2_REGdiffdiff_rast_12.list[[1]]))),
          max(na.omit(as.data.frame(SPMA2_REGdiffdiff_rast_12.list[[1]])))),
  Min = c(min(na.omit(as.data.frame(FLFJ2_REGdiffdiff_rast_12.list[[1]]))), min(na.omit(as.data.frame(FLMJ2_REGdiffdiff_rast_12.list[[1]]))), 
          min(na.omit(as.data.frame(FLFA2_REGdiffdiff_rast_12.list[[1]]))),
          min(na.omit(as.data.frame(FLMA2_REGdiffdiff_rast_12.list[[1]]))),min(na.omit(as.data.frame(SPFJ2_REGdiffdiff_rast_12.list[[1]]))), 
          min(na.omit(as.data.frame(SPMJ2_REGdiffdiff_rast_12.list[[1]]))), min(na.omit(as.data.frame(SPFA2_REGdiffdiff_rast_12.list[[1]]))),
          min(na.omit(as.data.frame(SPMA2_REGdiffdiff_rast_12.list[[1]])))),
  Mean = c(mean(FLFJ2_REGdiffdiff_rast_12.list[[1]],na.rm=TRUE), mean(FLMJ2_REGdiffdiff_rast_12.list[[1]],na.rm=TRUE), 
           mean(FLFA2_REGdiffdiff_rast_12.list[[1]],na.rm=TRUE),
           mean(FLMA2_REGdiffdiff_rast_12.list[[1]],na.rm=TRUE),mean(SPFJ2_REGdiffdiff_rast_12.list[[1]],na.rm=TRUE), 
           mean(SPMJ2_REGdiffdiff_rast_12.list[[1]],na.rm=TRUE), mean(SPFA2_REGdiffdiff_rast_12.list[[1]],na.rm=TRUE),
           mean(SPMA2_REGdiffdiff_rast_12.list[[1]],na.rm=TRUE)),
  Med = c(median(FLFJ2_REGdiffdiff_rast_12.list[[1]],na.rm=TRUE), median(FLMJ2_REGdiffdiff_rast_12.list[[1]],na.rm=TRUE), 
          median(FLFA2_REGdiffdiff_rast_12.list[[1]],na.rm=TRUE),
          median(FLMA2_REGdiffdiff_rast_12.list[[1]],na.rm=TRUE),median(SPFJ2_REGdiffdiff_rast_12.list[[1]],na.rm=TRUE), 
          median(SPMJ2_REGdiffdiff_rast_12.list[[1]],na.rm=TRUE), median(SPFA2_REGdiffdiff_rast_12.list[[1]],na.rm=TRUE),
          median(SPMA2_REGdiffdiff_rast_12.list[[1]],na.rm=TRUE)),add_colnames = TRUE)

bold(table1regdiff12)[1,]           <- TRUE
bottom_border(table1regdiff12)[1,]  <- 0.4
align(table1regdiff12)[,2]          <- 'right'
right_padding(table1regdiff12)      <- 10
left_padding(table1regdiff12)       <- 10
width(table1regdiff12)              <- 0.30
number_format(table1regdiff12)      <- 2

table1regdiff12

table1regdiff17 <- hux(
  Group = c('FLFJ', 'FLMJ', 'FLFA','FLMA','SPFJ', 'SPMJ', 'SPFA','SPMA'),
  Max = c(max(na.omit(as.data.frame(FLFJ2_REGdiffdiff_rast_17.list[[1]]))), max(na.omit(as.data.frame(FLMJ2_REGdiffdiff_rast_17.list[[1]]))), 
          max(na.omit(as.data.frame(FLFA2_REGdiffdiff_rast_17.list[[1]]))),
          max(na.omit(as.data.frame(FLMA2_REGdiffdiff_rast_17.list[[1]]))),max(na.omit(as.data.frame(SPFJ2_REGdiffdiff_rast_17.list[[1]]))), 
          max(na.omit(as.data.frame(SPMJ2_REGdiffdiff_rast_17.list[[1]]))), max(na.omit(as.data.frame(SPFA2_REGdiffdiff_rast_17.list[[1]]))),
          max(na.omit(as.data.frame(SPMA2_REGdiffdiff_rast_17.list[[1]])))),
  Min = c(min(na.omit(as.data.frame(FLFJ2_REGdiffdiff_rast_17.list[[1]]))), min(na.omit(as.data.frame(FLMJ2_REGdiffdiff_rast_17.list[[1]]))), 
          min(na.omit(as.data.frame(FLFA2_REGdiffdiff_rast_17.list[[1]]))),
          min(na.omit(as.data.frame(FLMA2_REGdiffdiff_rast_17.list[[1]]))),min(na.omit(as.data.frame(SPFJ2_REGdiffdiff_rast_17.list[[1]]))), 
          min(na.omit(as.data.frame(SPMJ2_REGdiffdiff_rast_17.list[[1]]))), min(na.omit(as.data.frame(SPFA2_REGdiffdiff_rast_17.list[[1]]))),
          min(na.omit(as.data.frame(SPMA2_REGdiffdiff_rast_17.list[[1]])))),
  Mean = c(mean(FLFJ2_REGdiffdiff_rast_17.list[[1]],na.rm=TRUE), mean(FLMJ2_REGdiffdiff_rast_17.list[[1]],na.rm=TRUE), 
           mean(FLFA2_REGdiffdiff_rast_17.list[[1]],na.rm=TRUE),
           mean(FLMA2_REGdiffdiff_rast_17.list[[1]],na.rm=TRUE),mean(SPFJ2_REGdiffdiff_rast_17.list[[1]],na.rm=TRUE), 
           mean(SPMJ2_REGdiffdiff_rast_17.list[[1]],na.rm=TRUE), mean(SPFA2_REGdiffdiff_rast_17.list[[1]],na.rm=TRUE),
           mean(SPMA2_REGdiffdiff_rast_17.list[[1]],na.rm=TRUE)),
  Med = c(median(FLFJ2_REGdiffdiff_rast_17.list[[1]],na.rm=TRUE), median(FLMJ2_REGdiffdiff_rast_17.list[[1]],na.rm=TRUE), 
          median(FLFA2_REGdiffdiff_rast_17.list[[1]],na.rm=TRUE),
          median(FLMA2_REGdiffdiff_rast_17.list[[1]],na.rm=TRUE),median(SPFJ2_REGdiffdiff_rast_17.list[[1]],na.rm=TRUE), 
          median(SPMJ2_REGdiffdiff_rast_17.list[[1]],na.rm=TRUE), median(SPFA2_REGdiffdiff_rast_17.list[[1]],na.rm=TRUE),
          median(SPMA2_REGdiffdiff_rast_17.list[[1]],na.rm=TRUE)),add_colnames = TRUE)

bold(table1regdiff17)[1,]           <- TRUE
bottom_border(table1regdiff17)[1,]  <- 0.4
align(table1regdiff17)[,2]          <- 'right'
right_padding(table1regdiff17)      <- 10
left_padding(table1regdiff17)       <- 10
width(table1regdiff17)              <- 0.30
number_format(table1regdiff17)      <- 2

table1regdiff17
####                                                            ####
###                                                             #####
#####Plot other environment Data####
plottemp<-function(rasterdata,size){
  par(mar=c(2,2,0,0), mfrow=c(1,1))
  plotvar=unlist(rasterdata)
  nclr=9
  plotclr <- (brewer.pal(nclr,"RdYlBu"))
  class <- classIntervals(plotvar, nclr, style="quantile")
  fix_break<-round(class$brks, digits = 2)
  for(i in 1:length(rasterdata)){
    print(i)
    plotvar <- rasterdata[[i]]
    nclr=9
    plotclr <- (brewer.pal(nclr,"RdYlBu"))
    class <- classIntervals(plotvar, nclr, style="fixed", 
                            fixedBreaks= 
                              if(size=="Depth")c(0,10,20,30,40,50,60,70,80,90,100) #breaks calculated by using average nonstationary "quantile" breaks
                            else c(-5,0,1,3,4,5,6,7,8,9,10)) #breaks calculated by using average "qauntile" breaks
    
    colcode <- findColours(class, plotclr)
    
    start_x <- range(GAM_data$Longitude)[1]
    end_x <- range(GAM_data$Longitude)[2]
    start_y <- range(GAM_data$Latitude)[1]
    end_y <- range(GAM_data$Latitude)[2]
    plot(depth_grid_plot000612$lon, depth_grid_plot000612$lat, pch=16, col=colcode, cex=0.1, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, xlab="Longitude", ylab="Latitude")
    map(database= mainecoast, ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE, lwd=0.1)
    #plot(sa511_513, add=T)
    box()
    legend("bottomright", legend=c(names(attr(colcode, "table"))), fill=c(attr(colcode, "palette")), cex=0.9,bg="white",
           bty = "o", title="Depth (m)")
    axis(1, cex=0.5)
    axis(2, cex=0.5)
  }
  #dev.off()
}
plottemp(FVcom_depth.list,"Depth")
plottemp(sedimentRAST,"sediment")

plottemp<-function(rasterdata,column,size){
  par(mar=c(2,2,0,0), mfrow=c(1,1))
  plotvar=column
  nclr=9
  plotclr <- (brewer.pal(nclr,"YlOrRd"))
  class <- classIntervals(plotvar, nclr, style="quantile")
  fix_break<-round(class$brks, digits = 2)
  for(i in 1:1){
    print(i)
    plotvar <- column
    nclr=9
    plotclr <- (brewer.pal(nclr,"YlOrRd"))
    class <- classIntervals(plotvar, nclr, style="fixed", 
                            fixedBreaks= 
                              if(size=="Depth (m)")c(0,10,20,30,40,50,75,100,125,150,175)
                          else  if(size=="Sediment (Phi)")c(-6,-4,-3,-1,1,2,4,8,10) 
                            else c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35)) #breaks calculated by using average "qauntile" breaks
    
    colcode <- findColours(class, plotclr)
    
    start_x <- range(GAM_data$Longitude)[1]
    end_x <- range(GAM_data$Longitude)[2]
    start_y <- range(GAM_data$Latitude)[1]
    end_y <- range(GAM_data$Latitude)[2]
    plot(rasterdata$Longitude, rasterdata$Latitude, pch=16, col=colcode, cex=0.7, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, xlab="Longitude", ylab="Latitude")
    map(database= mainecoast, ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE, lwd=0.1)
    #plot(sa511_513, add=T)
    box()
    legend("bottomright", legend=c(names(attr(colcode, "table"))), fill=c(attr(colcode, "palette")), cex=0.9,bg="white",
           bty = "o", title=size)
    axis(1, cex=0.5)
    axis(2, cex=0.5)
  }
  #dev.off()
}
plottemp(GAM_data,GAM_data$dist_frm_shore,"Distance Offshore (Deg)")
plottemp(FL00,FL00$dist_frm_shore,"Distance Offshore (Deg)")
plottemp(GAM_data,GAM_data$AvgDepth,"Depth (m)")
plottemp(FL00,FL00$AvgDepth,"Depth (m)")









###Creating Boxplots####
###Fall
bpflfj<-data.frame(FLFJ$catch,FLFJ.d$p.abundance,FLFJew$p.abundance,FLFJemw$p.abundance,FLFJvc.d$p.abundance)
bpflmj<-data.frame(FLMJ$catch,FLFJ.d$p.abundance,FLMJew$p.abundance,FLMJemw$p.abundance,FLMJvc.d$p.abundance)
bpflfa<-data.frame(FLFA$catch,FLFJ.d$p.abundance,FLFAew$p.abundance,FLFAemw$p.abundance,FLFAvc.d$p.abundance)
bpflma<-data.frame(FLMA$catch,FLFJ.d$p.abundance,FLMAew$p.abundance,FLMAemw$p.abundance,FLMAvc.d$p.abundance)

par(mar=c(3,4,0,0))
layout(matrix(1:4, ncol=2, byrow=FALSE))
bplot(bpflfj,ylab="Predicted Abundance",names=list("FLFJ","Stationary FLFJ","NSV1. FLFJ","NSV2. FLFJ","NSV3. FLFJ"))
bplot(bpflmj,ylab="Predicted Abundance",names=list("FLMJ","Stationary FLMJ","NSV1. FLMJ","NSV2. FLMJ","NSV3. FLMJ"))
bplot(bpflfa,ylab="Predicted Abundance",names=list("FLFA","Stationary FLFA","NSV1. FLFA","NSV2. FLFA","NSV3. FLFA"))
bplot(bpflma,ylab="Predicted Abundance",names=list("FLMA","Stationary FLMA","NSV1. FLMA","NSV2. FLMA","NSV3. FLMA"))

###spring
bpspfj<-data.frame(SPFJ$catch,SPFJ.d$p.abundance,SPFJew$p.abundance,SPFJemw$p.abundance,SPFJvc.d$p.abundance)
bpspmj<-data.frame(SPMJ$catch,SPMJ.d$p.abundance,SPMJew$p.abundance,SPMJemw$p.abundance,SPMJvc.d$p.abundance)
bpspfa<-data.frame(SPFA$catch,SPFA.d$p.abundance,SPFAew$p.abundance,SPFAemw$p.abundance,SPFAvc.d$p.abundance)
bpspma<-data.frame(SPMA$catch,SPMA.d$p.abundance,SPMAew$p.abundance,SPMAemw$p.abundance,SPMAvc.d$p.abundance)

par(mar=c(3,4,0,0))
layout(matrix(1:4, ncol=2, byrow=FALSE))
bplot(bpspfj,ylab="Predicted Abundance",names=list("SPFJ","Stationary SPFJ","NSV1. SPFJ","NSV2. SPFJ","NSV3. SPFJ"))
bplot(bpspmj,ylab="Predicted Abundance",names=list("SPMJ","Stationary SPMJ","NSV1. SPMJ","NSV2. SPMJ","NSV3. SPMJ"))
bplot(bpspfa,ylab="Predicted Abundance",names=list("SPFA","Stationary SPFA","NSV1. SPFA","NSV2. SPFA","NSV3. SPFA"))
bplot(bpspma,ylab="Predicted Abundance",names=list("SPMA","Stationary SPMA","NSV1. SPMA","NSV2. SPMA","NSV3. SPMA"))


#####make 4x4plots comparing models####
library(rgdal)
library(PBSmapping)
mainecoast= readOGR("D:/MENHTrawl/data/gis/ne_10m_coastline/ne_10m_coastline.shp")
plot4x4<-function(rasterdata, month,year,size,depth_grid){
  #jpeg(filename = paste("D://MENHtrawl/Plots/grid_NS_map.jpeg", sep=""), width=140, height=120, units = "mm", res = 600)
  #par(mar=c(2,2,0,0), mfrow=c(1,1))
  plotvar=unlist(rasterdata)
  nclr=9
  plotclr <- (brewer.pal(nclr,"YlOrRd"))
  class <- classIntervals(plotvar, nclr, style="quantile")
  fix_break<-round(class$brks, digits = 2)
  for(i in 1:length(rasterdata)){
    print(i)
    plotvar <- rasterdata[[i]]
    nclr=9
    plotclr <- (brewer.pal(nclr,"YlOrRd"))
    class <- classIntervals(plotvar, nclr, style="fixed", 
                            fixedBreaks=
                              if(size=="J")c(0,0.5,3,10,25,50,200,600) #breaks calculated by using average raw data "quantile" breaks
                            else c(0,5,15,40,65,85,115,160,250,900)) #breaks calculated by using average "qauntile" breaks
    
    colcode <- findColours(class, plotclr)
    
    start_x <- range(GAM_data$Longitude)[1]
    end_x <- range(GAM_data$Longitude)[2]
    start_y <- range(GAM_data$Latitude)[1]
    end_y <- range(GAM_data$Latitude)[2]
    plot(depth_grid$lon, depth_grid$lat, pch=16, col=colcode, cex=0.4, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, xlab="Longitude", ylab="Latitude")
    map(database = mainecoast, ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE, lwd=0.1)
  legend("topleft", paste(year, "-", month, sep=""), bty="n", cex=1.5)
    #plot(sa511_513, add=T)
    box()
    
    #legend("bottomright", legend=c(names(attr(colcode, "table"))), fill=c(attr(colcode, "palette")), cex=0.8,
           #bty = "n", title="Abundance")
    
    
    axis(1, cex=1.0)
    axis(2, cex=1.0)
  }
  #dev.off()
}
par(mar=c(2,2,0,0), mfrow=c(3,4))
plot4x4(FLFJRAST00,"FLFJ","2000","J",depth_grid_plot000612)
plot4x4(FLMJRAST00,"FLMJ","2000","J",depth_grid_plot000612)
plot4x4(FLFARAST00,"FLFA","2000","A",depth_grid_plot000612)
plot4x4(FLMARAST00,"FLMA","2000","A",depth_grid_plot000612)
plot4x4(FLFJewRAST00,"FLFJ","2000","J",depth_grid_plot000612)
plot4x4(FLMJewRAST00,"FLMJ","2000","J",depth_grid_plot000612)
plot4x4(FLFAewRAST00,"FLFA","2000","A",depth_grid_plot000612)
plot4x4(FLMAewRAST00,"FLMA","2000","A",depth_grid_plot000612)
plot4x4(FLFJew2RAST00,"FLFJ","2000","J",depth_grid_plot000612)
plot4x4(FLMJew2RAST00,"FLMJ","2000","J",depth_grid_plot000612)
plot4x4(FLFAew2RAST00,"FLFA","2000","A",depth_grid_plot000612)
plot4x4(FLMAew2RAST00,"FLMA","2000","A",depth_grid_plot000612)

par(mar=c(2,2,0,0), mfrow=c(4,4))
plot4x4(SPFJRAST00,"SPFJ","2000","J",depth_grid_plot000612)
plot4x4(SPMJRAST00,"SPMJ","2000","J",depth_grid_plot000612)
plot4x4(SPFARAST00,"SPFA","2000","A",depth_grid_plot000612)
plot4x4(SPMARAST00,"SPMA","2000","A",depth_grid_plot000612)
plot4x4(SPFJewRAST00,"SPFJ","2000","J",depth_grid_plot000612)
plot4x4(SPMJewRAST00,"SPMJ","2000","J",depth_grid_plot000612)
plot4x4(SPFAewRAST00,"SPFA","2000","A",depth_grid_plot000612)
plot4x4(SPMAewRAST00,"SPMA","2000","A",depth_grid_plot000612)
plot4x4(SPFJew2RAST00,"SPFJ","2000","J",depth_grid_plot000612)
plot4x4(SPMJew2RAST00,"SPMJ","2000","J",depth_grid_plot000612)
plot4x4(SPFAew2RAST00,"SPFA","2000","A",depth_grid_plot000612)
plot4x4(SPMAew2RAST00,"SPMA","2000","A",depth_grid_plot000612)
###2006
par(mar=c(2,2,0,0), mfrow=c(4,4))
plot4x4(FLFJRAST06,"FLFJ","2006","J",depth_grid_plot000612)
plot4x4(FLMJRAST06,"FLMJ","2006","J",depth_grid_plot000612)
plot4x4(FLFARAST06,"FLFA","2006","A",depth_grid_plot000612)
plot4x4(FLMARAST06,"FLMA","2006","A",depth_grid_plot000612)
plot4x4(FLFJewRAST06,"FLFJ","2006","J",depth_grid_plot000612)
plot4x4(FLMJewRAST06,"FLMJ","2006","J",depth_grid_plot000612)
plot4x4(FLFAewRAST06,"FLFA","2006","A",depth_grid_plot000612)
plot4x4(FLMAewRAST06,"FLMA","2006","A",depth_grid_plot000612)
plot4x4(FLFJew2RAST06,"FLFJ","2006","J",depth_grid_plot000612)
plot4x4(FLMJew2RAST06,"FLMJ","2006","J",depth_grid_plot000612)
plot4x4(FLFAew2RAST06,"FLFA","2006","A",depth_grid_plot000612)
plot4x4(FLMAew2RAST06,"FLMA","2006","A",depth_grid_plot000612)


par(mar=c(2,2,0,0), mfrow=c(4,4))
plot4x4(SPFJRAST06,"SPFJ","2006","J",depth_grid_plot000612)
plot4x4(SPMJRAST06,"SPMJ","2006","J",depth_grid_plot000612)
plot4x4(SPFARAST06,"SPFA","2006","A",depth_grid_plot000612)
plot4x4(SPMARAST06,"SPMA","2006","A",depth_grid_plot000612)
plot4x4(SPFJewRAST06,"SPFJ","2006","J",depth_grid_plot000612)
plot4x4(SPMJewRAST06,"SPMJ","2006","J",depth_grid_plot000612)
plot4x4(SPFAewRAST06,"SPFA","2006","A",depth_grid_plot000612)
plot4x4(SPMAewRAST06,"SPMA","2006","A",depth_grid_plot000612)
plot4x4(SPFJew2RAST06,"SPFJ","2006","J",depth_grid_plot000612)
plot4x4(SPMJew2RAST06,"SPMJ","2006","J",depth_grid_plot000612)
plot4x4(SPFAew2RAST06,"SPFA","2006","A",depth_grid_plot000612)
plot4x4(SPMAew2RAST06,"SPMA","2006","A",depth_grid_plot000612)

###2012
par(mar=c(2,2,0,0), mfrow=c(4,4))
plot4x4(FLFJRAST12,"FLFJ","2012","J",depth_grid_plot000612)
plot4x4(FLMJRAST12,"FLMJ","2012","J",depth_grid_plot000612)
plot4x4(FLFARAST12,"FLFA","2012","A",depth_grid_plot000612)
plot4x4(FLMARAST12,"FLMA","2012","A",depth_grid_plot000612)
plot4x4(FLFJewRAST12,"FLFJ","2012","J",depth_grid_plot000612)
plot4x4(FLMJewRAST12,"FLMJ","2012","J",depth_grid_plot000612)
plot4x4(FLFAewRAST12,"FLFA","2012","A",depth_grid_plot000612)
plot4x4(FLMAewRAST12,"FLMA","2012","A",depth_grid_plot000612)
plot4x4(FLFJew2RAST12,"FLFJ","2012","J",depth_grid_plot000612)
plot4x4(FLMJew2RAST12,"FLMJ","2012","J",depth_grid_plot000612)
plot4x4(FLFAew2RAST12,"FLFA","2012","A",depth_grid_plot000612)
plot4x4(FLMAew2RAST12,"FLMA","2012","A",depth_grid_plot000612)


par(mar=c(2,2,0,0), mfrow=c(4,4))
plot4x4(SPFJRAST12,"SPFJ","2012","J",depth_grid_plot000612)
plot4x4(SPMJRAST12,"SPMJ","2012","J",depth_grid_plot000612)
plot4x4(SPFARAST12,"SPFA","2012","A",depth_grid_plot000612)
plot4x4(SPMARAST12,"SPMA","2012","A",depth_grid_plot000612)
plot4x4(SPFJewRAST12,"SPFJ","2012","J",depth_grid_plot000612)
plot4x4(SPMJewRAST12,"SPMJ","2012","J",depth_grid_plot000612)
plot4x4(SPFAewRAST12,"SPFA","2012","A",depth_grid_plot000612)
plot4x4(SPMAewRAST12,"SPMA","2012","A",depth_grid_plot000612)
plot4x4(SPFJew2RAST12,"SPFJ","2012","J",depth_grid_plot000612)
plot4x4(SPMJew2RAST12,"SPMJ","2012","J",depth_grid_plot000612)
plot4x4(SPFAew2RAST12,"SPFA","2012","A",depth_grid_plot000612)
plot4x4(SPMAew2RAST12,"SPMA","2012","A",depth_grid_plot000612)

###2017
par(mar=c(2,2,0,0), mfrow=c(3,4))
plot4x4(FLFJRAST17,"FLFJ","2017","J",depth_grid_plot2017)
plot4x4(FLMJRAST17,"FLMJ","2017","J",depth_grid_plot2017)
plot4x4(FLFARAST17,"FLFA","2017","A",depth_grid_plot2017)
plot4x4(FLMARAST17,"FLMA","2017","A",depth_grid_plot2017)
plot4x4(FLFJewRAST17,"FLFJ","2017","J",depth_grid_plot2017)
plot4x4(FLMJewRAST17,"FLMJ","2017","J",depth_grid_plot2017)
plot4x4(FLFAewRAST17,"FLFA","2017","A",depth_grid_plot2017)
plot4x4(FLMAewRAST17,"FLMA","2017","A",depth_grid_plot2017)
plot4x4(FLFJew2RAST17,"FLFJ","2017","J",depth_grid_plot2017)
plot4x4(FLMJew2RAST17,"FLMJ","2017","J",depth_grid_plot2017)
plot4x4(FLFAew2RAST17,"FLFA","2017","A",depth_grid_plot2017)
plot4x4(FLMAew2RAST17,"FLMA","2017","A",depth_grid_plot2017)

par(mar=c(2,2,0,0), mfrow=c(3,4))
plot4x4(SPFJRAST17,"SPFJ","2017","J",depth_grid_plot2017)
plot4x4(SPMJRAST17,"SPMJ","2017","J",depth_grid_plot2017)
plot4x4(SPFARAST17,"SPFA","2017","A",depth_grid_plot2017)
plot4x4(SPMARAST17,"SPMA","2017","A",depth_grid_plot2017)
plot4x4(SPFJewRAST17,"SPFJ","2017","J",depth_grid_plot2017)
plot4x4(SPMJewRAST17,"SPMJ","2017","J",depth_grid_plot2017)
plot4x4(SPFAewRAST17,"SPFA","2017","A",depth_grid_plot2017)
plot4x4(SPMAewRAST17,"SPMA","2017","A",depth_grid_plot2017)
plot4x4(SPFJew2RAST17,"SPFJ","2017","J",depth_grid_plot2017)
plot4x4(SPMJew2RAST17,"SPMJ","2017","J",depth_grid_plot2017)
plot4x4(SPFAew2RAST17,"SPFA","2017","A",depth_grid_plot2017)
plot4x4(SPMAew2RAST17,"SPMA","2017","A",depth_grid_plot2017)

#####make 4x4plots difference models####
library(rgdal)
library(PBSmapping)
mainecoast= readOGR("D:/MENHTrawl/data/gis/ne_10m_coastline/ne_10m_coastline.shp")
plot4x4<-function(rasterdata, month,year,size,depth_grid,title){
  #jpeg(filename = paste("D://MENHtrawl/Plots/grid_NS_map.jpeg", sep=""), width=140, height=120, units = "mm", res = 600)
  #par(mar=c(2,2,0,0), mfrow=c(1,1))
  plotvar=unlist(rasterdata)
  nclr=9
  plotclr <- (brewer.pal(nclr,"RdYlBu"))
  class <- classIntervals(plotvar, nclr, style="quantile")
  fix_break<-round(class$brks, digits = 2)
  for(i in 1:length(rasterdata)){
    print(i)
    plotvar <- rasterdata[[i]]
    nclr=9
    plotclr <- (brewer.pal(nclr,"RdYlBu"))
    class <- classIntervals(plotvar, nclr, style="fixed", 
                            fixedBreaks=
                              if(size=="J")c(-200,-100,-50,-25,-15,-5,5,15,25,50,100,200,500) #breaks calculated by using average nonstationary "quantile" breaks
                            else c(-200,-100,-50,-25,-15,-5,5,15,25,50,100,200,500))
    
    colcode <- findColours(class, plotclr)
    
    start_x <- range(GAM_data$Longitude)[1]
    end_x <- range(GAM_data$Longitude)[2]
    start_y <- range(GAM_data$Latitude)[1]
    end_y <- range(GAM_data$Latitude)[2]
    plot(depth_grid$lon, depth_grid$lat, pch=16, col=colcode, cex=0.1, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, xlab="Longitude", ylab="Latitude")
    map(database = mainecoast, ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE, lwd=0.1)
    #plot(sa511_513, add=T)
    box()
    legend("topleft", paste(year, "-", month, sep=""), bty="n", cex=1.5)
    #legend("bottomright", legend=c(names(attr(colcode, "table"))), fill=c(attr(colcode, "palette")), cex=0.8,
          # bty = "n", title=title)
    axis(1, cex=0.5)
    axis(2, cex=0.5)
  }
  #dev.off()
}
####NSV1####
par(mar=c(2,2,0,0), mfrow=c(4,4))
plot4x4(FLFJ_diff_rast_00.list,"FLFJ","2000","J",depth_grid_plot000612,"Relative Diff")
plot4x4(FLMJ_diff_rast_00.list,"FLMJ","2000","J",depth_grid_plot000612,"Relative Diff")
plot4x4(FLFA_diff_rast_00.list,"FLFA","2000","A",depth_grid_plot000612,"Relative Diff")
plot4x4(FLMA_diff_rast_00.list,"FLMA","2000","A",depth_grid_plot000612,"Relative Diff")
plot4x4(SPFJ_diff_rast_00.list,"SPFJ","2000","J",depth_grid_plot000612,"Relative Diff")
plot4x4(SPMJ_diff_rast_00.list,"SPMJ","2000","J",depth_grid_plot000612,"Relative Diff")
plot4x4(SPFA_diff_rast_00.list,"SPFA","2000","A",depth_grid_plot000612,"Relative Diff")
plot4x4(SPMA_diff_rast_00.list,"SPMA","2000","A",depth_grid_plot000612,"Relative Diff")
plot4x4(FLFJ_REGdiff_rast_00.list,"FLFJ","2000","J",depth_grid_plot000612,"Difference")
plot4x4(FLMJ_REGdiff_rast_00.list,"FLMJ","2000","J",depth_grid_plot000612,"Difference")
plot4x4(FLFA_REGdiff_rast_00.list,"FLFA","2000","A",depth_grid_plot000612,"Difference")
plot4x4(FLMA_REGdiff_rast_00.list,"FLMA","2000","A",depth_grid_plot000612,"Difference")
plot4x4(SPFJ_REGdiff_rast_00.list,"SPFJ","2000","J",depth_grid_plot000612,"Difference")
plot4x4(SPMJ_REGdiff_rast_00.list,"SPMJ","2000","J",depth_grid_plot000612,"Difference")
plot4x4(SPFA_REGdiff_rast_00.list,"SPFA","2000","A",depth_grid_plot000612,"Difference")
plot4x4(SPMA_REGdiff_rast_00.list,"SPMA","2000","A",depth_grid_plot000612,"Difference")
###2006
par(mar=c(2,2,0,0), mfrow=c(4,4))
plot4x4(FLFJ_diff_rast_06.list,"FLFJ","2006","J",depth_grid_plot000612,"Relative Diff")
plot4x4(FLMJ_diff_rast_06.list,"FLMJ","2006","J",depth_grid_plot000612,"Relative Diff")
plot4x4(FLFA_diff_rast_06.list,"FLFA","2006","A",depth_grid_plot000612,"Relative Diff")
plot4x4(FLMA_diff_rast_06.list,"FLMA","2006","A",depth_grid_plot000612,"Relative Diff")
plot4x4(SPFJ_diff_rast_06.list,"SPFJ","2006","J",depth_grid_plot000612,"Relative Diff")
plot4x4(SPMJ_diff_rast_06.list,"SPMJ","2006","J",depth_grid_plot000612,"Relative Diff")
plot4x4(SPFA_diff_rast_06.list,"SPFA","2006","A",depth_grid_plot000612,"Relative Diff")
plot4x4(SPMA_diff_rast_06.list,"SPMA","2006","A",depth_grid_plot000612,"Relative Diff")
plot4x4(FLFJ_REGdiff_rast_06.list,"FLFJ","2006","J",depth_grid_plot000612,"Difference")
plot4x4(FLMJ_REGdiff_rast_06.list,"FLMJ","2006","J",depth_grid_plot000612,"Difference")
plot4x4(FLFA_REGdiff_rast_06.list,"FLFA","2006","A",depth_grid_plot000612,"Difference")
plot4x4(FLMA_REGdiff_rast_06.list,"FLMA","2006","A",depth_grid_plot000612,"Difference")
plot4x4(SPFJ_REGdiff_rast_06.list,"SPFJ","2006","J",depth_grid_plot000612,"Difference")
plot4x4(SPMJ_REGdiff_rast_06.list,"SPMJ","2006","J",depth_grid_plot000612,"Difference")
plot4x4(SPFA_REGdiff_rast_06.list,"SPFA","2006","A",depth_grid_plot000612,"Difference")
plot4x4(SPMA_REGdiff_rast_06.list,"SPMA","2006","A",depth_grid_plot000612,"Difference")
###2012
par(mar=c(2,2,0,0), mfrow=c(4,4))
plot4x4(FLFJ_diff_rast_12.list,"FLFJ","2012","J",depth_grid_plot000612,"Relative Diff")
plot4x4(FLMJ_diff_rast_12.list,"FLMJ","2012","J",depth_grid_plot000612,"Relative Diff")
plot4x4(FLFA_diff_rast_12.list,"FLFA","2012","A",depth_grid_plot000612,"Relative Diff")
plot4x4(FLMA_diff_rast_12.list,"FLMA","2012","A",depth_grid_plot000612,"Relative Diff")
plot4x4(SPFJ_diff_rast_12.list,"SPFJ","2012","J",depth_grid_plot000612,"Relative Diff")
plot4x4(SPMJ_diff_rast_12.list,"SPMJ","2012","J",depth_grid_plot000612,"Relative Diff")
plot4x4(SPFA_diff_rast_12.list,"SPFA","2012","A",depth_grid_plot000612,"Relative Diff")
plot4x4(SPMA_diff_rast_12.list,"SPMA","2012","A",depth_grid_plot000612,"Relative Diff")
plot4x4(FLFJ_REGdiff_rast_12.list,"FLFJ","2012","J",depth_grid_plot000612,"Difference")
plot4x4(FLMJ_REGdiff_rast_12.list,"FLMJ","2012","J",depth_grid_plot000612,"Difference")
plot4x4(FLFA_REGdiff_rast_12.list,"FLFA","2012","A",depth_grid_plot000612,"Difference")
plot4x4(FLMA_REGdiff_rast_12.list,"FLMA","2012","A",depth_grid_plot000612,"Difference")
plot4x4(SPFJ_REGdiff_rast_12.list,"SPFJ","2012","J",depth_grid_plot000612,"Difference")
plot4x4(SPMJ_REGdiff_rast_12.list,"SPMJ","2012","J",depth_grid_plot000612,"Difference")
plot4x4(SPFA_REGdiff_rast_12.list,"SPFA","2012","A",depth_grid_plot000612,"Difference")
plot4x4(SPMA_REGdiff_rast_12.list,"SPMA","2012","A",depth_grid_plot000612,"Difference")
###2017
par(mar=c(2,2,0,0), mfrow=c(4,4))
plot4x4(FLFJ_diff_rast_17.list,"FLFJ","2017","J",depth_grid_plot2017,"Relative Diff")
plot4x4(FLMJ_diff_rast_17.list,"FLMJ","2017","J",depth_grid_plot2017,"Relative Diff")
plot4x4(FLFA_diff_rast_17.list,"FLFA","2017","A",depth_grid_plot2017,"Relative Diff")
plot4x4(FLMA_diff_rast_17.list,"FLMA","2017","A",depth_grid_plot2017,"Relative Diff")
plot4x4(SPFJ_diff_rast_17.list,"SPFJ","2017","J",depth_grid_plot2017,"Relative Diff")
plot4x4(SPMJ_diff_rast_17.list,"SPMJ","2017","J",depth_grid_plot2017,"Relative Diff")
plot4x4(SPFA_diff_rast_17.list,"SPFA","2017","A",depth_grid_plot2017,"Relative Diff")
plot4x4(SPMA_diff_rast_17.list,"SPMA","2017","A",depth_grid_plot2017,"Relative Diff")
plot4x4(FLFJ_REGdiff_rast_17.list,"FLFJ","2017","J",depth_grid_plot2017,"Difference")
plot4x4(FLMJ_REGdiff_rast_17.list,"FLMJ","2017","J",depth_grid_plot2017,"Difference")
plot4x4(FLFA_REGdiff_rast_17.list,"FLFA","2017","A",depth_grid_plot2017,"Difference")
plot4x4(FLMA_REGdiff_rast_17.list,"FLMA","2017","A",depth_grid_plot2017,"Difference")
plot4x4(SPFJ_REGdiff_rast_17.list,"SPFJ","2017","J",depth_grid_plot2017,"Difference")
plot4x4(SPMJ_REGdiff_rast_17.list,"SPMJ","2017","J",depth_grid_plot2017,"Difference")
plot4x4(SPFA_REGdiff_rast_17.list,"SPFA","2017","A",depth_grid_plot2017,"Difference")
plot4x4(SPMA_REGdiff_rast_17.list,"SPMA","2017","A",depth_grid_plot2017,"Difference")



####NSV2####
par(mar=c(2,2,0,0), mfrow=c(4,4))
plot4x4(FLFJ2_diff_rast_00.list,"FLFJ","2000","J",depth_grid_plot000612,"Relative Diff")
plot4x4(FLMJ2_diff_rast_00.list,"FLMJ","2000","J",depth_grid_plot000612,"Relative Diff")
plot4x4(FLFA2_diff_rast_00.list,"FLFA","2000","A",depth_grid_plot000612,"Relative Diff")
plot4x4(FLMA2_diff_rast_00.list,"FLMA","2000","A",depth_grid_plot000612,"Relative Diff")
plot4x4(SPFJ2_diff_rast_00.list,"SPFJ","2000","J",depth_grid_plot000612,"Relative Diff")
plot4x4(SPMJ2_diff_rast_00.list,"SPMJ","2000","J",depth_grid_plot000612,"Relative Diff")
plot4x4(SPFA2_diff_rast_00.list,"SPFA","2000","A",depth_grid_plot000612,"Relative Diff")
plot4x4(SPMA2_diff_rast_00.list,"SPMA","2000","A",depth_grid_plot000612,"Relative Diff")
plot4x4(FLFJ2_REGdiffdiff_rast_00.list,"FLFJ","2000","J",depth_grid_plot000612,"Difference")
plot4x4(FLMJ2_REGdiffdiff_rast_00.list,"FLMJ","2000","J",depth_grid_plot000612,"Difference")
plot4x4(FLFA2_REGdiffdiff_rast_00.list,"FLFA","2000","A",depth_grid_plot000612,"Difference")
plot4x4(FLMA2_REGdiffdiff_rast_00.list,"FLMA","2000","A",depth_grid_plot000612,"Difference")
plot4x4(SPFJ2_REGdiffdiff_rast_00.list,"SPFJ","2000","J",depth_grid_plot000612,"Difference")
plot4x4(SPMJ2_REGdiffdiff_rast_00.list,"SPMJ","2000","J",depth_grid_plot000612,"Difference")
plot4x4(SPFA2_REGdiffdiff_rast_00.list,"SPFA","2000","A",depth_grid_plot000612,"Difference")
plot4x4(SPMA2_REGdiffdiff_rast_00.list,"SPMA","2000","A",depth_grid_plot000612,"Difference")
###2006
par(mar=c(2,2,0,0), mfrow=c(4,4))
plot4x4(FLFJ2_diff_rast_06.list,"FLFJ","2006","J",depth_grid_plot000612,"Relative Diff")
plot4x4(FLMJ2_diff_rast_06.list,"FLMJ","2006","J",depth_grid_plot000612,"Relative Diff")
plot4x4(FLFA2_diff_rast_06.list,"FLFA","2006","A",depth_grid_plot000612,"Relative Diff")
plot4x4(FLMA2_diff_rast_06.list,"FLMA","2006","A",depth_grid_plot000612,"Relative Diff")
plot4x4(SPFJ2_diff_rast_06.list,"SPFJ","2006","J",depth_grid_plot000612,"Relative Diff")
plot4x4(SPMJ2_diff_rast_06.list,"SPMJ","2006","J",depth_grid_plot000612,"Relative Diff")
plot4x4(SPFA2_diff_rast_06.list,"SPFA","2006","A",depth_grid_plot000612,"Relative Diff")
plot4x4(SPMA2_diff_rast_06.list,"SPMA","2006","A",depth_grid_plot000612,"Relative Diff")
plot4x4(FLFJ2_REGdiffdiff_rast_06.list,"FLFJ","2006","J",depth_grid_plot000612,"Difference")
plot4x4(FLMJ2_REGdiffdiff_rast_06.list,"FLMJ","2006","J",depth_grid_plot000612,"Difference")
plot4x4(FLFA2_REGdiffdiff_rast_06.list,"FLFA","2006","A",depth_grid_plot000612,"Difference")
plot4x4(FLMA2_REGdiffdiff_rast_06.list,"FLMA","2006","A",depth_grid_plot000612,"Difference")
plot4x4(SPFJ2_REGdiffdiff_rast_06.list,"SPFJ","2006","J",depth_grid_plot000612,"Difference")
plot4x4(SPMJ2_REGdiffdiff_rast_06.list,"SPMJ","2006","J",depth_grid_plot000612,"Difference")
plot4x4(SPFA2_REGdiffdiff_rast_06.list,"SPFA","2006","A",depth_grid_plot000612,"Difference")
plot4x4(SPMA2_REGdiffdiff_rast_06.list,"SPMA","2006","A",depth_grid_plot000612,"Difference")
###2012
par(mar=c(2,2,0,0), mfrow=c(4,4))
plot4x4(FLFJ2_diff_rast_12.list,"FLFJ","2012","J",depth_grid_plot000612,"Relative Diff")
plot4x4(FLMJ2_diff_rast_12.list,"FLMJ","2012","J",depth_grid_plot000612,"Relative Diff")
plot4x4(FLFA2_diff_rast_12.list,"FLFA","2012","A",depth_grid_plot000612,"Relative Diff")
plot4x4(FLMA2_diff_rast_12.list,"FLMA","2012","A",depth_grid_plot000612,"Relative Diff")
plot4x4(SPFJ2_diff_rast_12.list,"SPFJ","2012","J",depth_grid_plot000612,"Relative Diff")
plot4x4(SPMJ2_diff_rast_12.list,"SPMJ","2012","J",depth_grid_plot000612,"Relative Diff")
plot4x4(SPFA2_diff_rast_12.list,"SPFA","2012","A",depth_grid_plot000612,"Relative Diff")
plot4x4(SPMA2_diff_rast_12.list,"SPMA","2012","A",depth_grid_plot000612,"Relative Diff")
plot4x4(FLFJ2_REGdiffdiff_rast_12.list,"FLFJ","2012","J",depth_grid_plot000612,"Difference")
plot4x4(FLMJ2_REGdiffdiff_rast_12.list,"FLMJ","2012","J",depth_grid_plot000612,"Difference")
plot4x4(FLFA2_REGdiffdiff_rast_12.list,"FLFA","2012","A",depth_grid_plot000612,"Difference")
plot4x4(FLMA2_REGdiffdiff_rast_12.list,"FLMA","2012","A",depth_grid_plot000612,"Difference")
plot4x4(SPFJ2_REGdiffdiff_rast_12.list,"SPFJ","2012","J",depth_grid_plot000612,"Difference")
plot4x4(SPMJ2_REGdiffdiff_rast_12.list,"SPMJ","2012","J",depth_grid_plot000612,"Difference")
plot4x4(SPFA2_REGdiffdiff_rast_12.list,"SPFA","2012","A",depth_grid_plot000612,"Difference")
plot4x4(SPMA2_REGdiffdiff_rast_12.list,"SPMA","2012","A",depth_grid_plot000612,"Difference")
###2017
par(mar=c(2,2,0,0), mfrow=c(4,4))
plot4x4(FLFJ2_diff_rast_17.list,"FLFJ","2017","J",depth_grid_plot2017,"Relative Diff")
plot4x4(FLMJ2_diff_rast_17.list,"FLMJ","2017","J",depth_grid_plot2017,"Relative Diff")
plot4x4(FLFA2_diff_rast_17.list,"FLFA","2017","A",depth_grid_plot2017,"Relative Diff")
plot4x4(FLMA2_diff_rast_17.list,"FLMA","2017","A",depth_grid_plot2017,"Relative Diff")
plot4x4(SPFJ2_diff_rast_17.list,"SPFJ","2017","J",depth_grid_plot2017,"Relative Diff")
plot4x4(SPMJ2_diff_rast_17.list,"SPMJ","2017","J",depth_grid_plot2017,"Relative Diff")
plot4x4(SPFA2_diff_rast_17.list,"SPFA","2017","A",depth_grid_plot2017,"Relative Diff")
plot4x4(SPMA2_diff_rast_17.list,"SPMA","2017","A",depth_grid_plot2017,"Relative Diff")
plot4x4(FLFJ2_REGdiffdiff_rast_17.list,"FLFJ","2017","J",depth_grid_plot2017,"Difference")
plot4x4(FLMJ2_REGdiffdiff_rast_17.list,"FLMJ","2017","J",depth_grid_plot2017,"Difference")
plot4x4(FLFA2_REGdiffdiff_rast_17.list,"FLFA","2017","A",depth_grid_plot2017,"Difference")
plot4x4(FLMA2_REGdiffdiff_rast_17.list,"FLMA","2017","A",depth_grid_plot2017,"Difference")
plot4x4(SPFJ2_REGdiffdiff_rast_17.list,"SPFJ","2017","J",depth_grid_plot2017,"Difference")
plot4x4(SPMJ2_REGdiffdiff_rast_17.list,"SPMJ","2017","J",depth_grid_plot2017,"Difference")
plot4x4(SPFA2_REGdiffdiff_rast_17.list,"SPFA","2017","A",depth_grid_plot2017,"Difference")
plot4x4(SPMA2_REGdiffdiff_rast_17.list,"SPMA","2017","A",depth_grid_plot2017,"Difference")

####plot FLFJ all plots####
library(rgdal)
library(PBSmapping)
mainecoast= readOGR("D:/MENHTrawl/data/gis/ne_10m_coastline/ne_10m_coastline.shp")
plot4x4<-function(rasterdata, month,year,size,depth_grid){
  #jpeg(filename = paste("D://MENHtrawl/Plots/grid_NS_map.jpeg", sep=""), width=140, height=120, units = "mm", res = 600)
  #par(mar=c(2,2,0,0), mfrow=c(1,1))
  plotvar=unlist(rasterdata)
  nclr=9
  plotclr <- (brewer.pal(nclr,"YlOrRd"))
  class <- classIntervals(plotvar, nclr, style="quantile")
  fix_break<-round(class$brks, digits = 2)
  for(i in 1:length(rasterdata)){
    print(i)
    plotvar <- rasterdata[[i]]
    nclr=9
    plotclr <- (brewer.pal(nclr,"YlOrRd"))
    class <- classIntervals(plotvar, nclr, style="fixed", 
                            fixedBreaks=
                              if(size=="J")c(0,0.5,3,10,25,50,200,600) #breaks calculated by using average raw data "quantile" breaks
                            else c(0,5,15,40,65,85,115,160,250,900)) #breaks calculated by using average "qauntile" breaks
    
    colcode <- findColours(class, plotclr)
    
    start_x <- range(GAM_data$Longitude)[1]
    end_x <- range(GAM_data$Longitude)[2]
    start_y <- range(GAM_data$Latitude)[1]
    end_y <- range(GAM_data$Latitude)[2]
    plot(depth_grid$lon, depth_grid$lat, pch=16, col=colcode, cex=0.1, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, xlab="Longitude", ylab="Latitude")
    map(database = mainecoast, ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE, lwd=0.1)
    #plot(sa511_513, add=T)
    box()
    legend("topleft", paste(year, "-", month, sep=""), bty="n", cex=1.5)
    legend("bottomright", legend=c(names(attr(colcode, "table"))), fill=c(attr(colcode, "palette")), cex=1.1,
           bty = "n", title="Abundance")
    axis(1, cex=0.5)
    axis(2, cex=0.5)
  }
  #dev.off()
}
par(mar=c(2,2,0,0), mfrow=c(3,4))
plot4x4(FLFJRAST00,"FLFJ","2000","J",depth_grid_plot000612)
plot4x4(FLFJRAST06,"FLFJ","2006","J",depth_grid_plot000612)
plot4x4(FLFJRAST12,"FLFJ","2012","J",depth_grid_plot000612)
plot4x4(FLFJRAST17,"FLFJ","2017","J",depth_grid_plot2017)
plot4x4(FLFJewRAST00,"FLFJ","2000","J",depth_grid_plot000612)
plot4x4(FLFJewRAST06,"FLFJ","2006","J",depth_grid_plot000612)
plot4x4(FLFJewRAST12,"FLFJ","2012","J",depth_grid_plot000612)
plot4x4(FLFJewRAST17,"FLFJ","2017","J",depth_grid_plot2017)
plot4x4(FLFJew2RAST00,"FLFJ","2000","J",depth_grid_plot000612)
plot4x4(FLFJew2RAST06,"FLFJ","2006","J",depth_grid_plot000612)
plot4x4(FLFJew2RAST12,"FLFJ","2012","J",depth_grid_plot000612)
plot4x4(FLFJew2RAST17,"FLFJ","2017","J",depth_grid_plot2017)

plot4x4<-function(rasterdata, month,year,size,depth_grid,title){
  #jpeg(filename = paste("D://MENHtrawl/Plots/grid_NS_map.jpeg", sep=""), width=140, height=120, units = "mm", res = 600)
  #par(mar=c(2,2,0,0), mfrow=c(1,1))
  plotvar=unlist(rasterdata)
  nclr=9
  plotclr <- (brewer.pal(nclr,"RdYlBu"))
  class <- classIntervals(plotvar, nclr, style="quantile")
  fix_break<-round(class$brks, digits = 2)
  for(i in 1:length(rasterdata)){
    print(i)
    plotvar <- rasterdata[[i]]
    nclr=9
    plotclr <- (brewer.pal(nclr,"RdYlBu"))
    class <- classIntervals(plotvar, nclr, style="fixed", 
                            fixedBreaks=
                              if(size=="J")c(-200,-100,-50,-25,-15,-5,5,15,25,50,100,200,500) #breaks calculated by using average nonstationary "quantile" breaks
                            else c(-200,-100,-50,-25,-15,-5,5,15,25,50,100,200,500))
    
    colcode <- findColours(class, plotclr)
    
    start_x <- range(GAM_data$Longitude)[1]
    end_x <- range(GAM_data$Longitude)[2]
    start_y <- range(GAM_data$Latitude)[1]
    end_y <- range(GAM_data$Latitude)[2]
    plot(depth_grid$lon, depth_grid$lat, pch=16, col=colcode, cex=0.1, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, xlab="Longitude", ylab="Latitude")
    map(database = mainecoast, ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE, lwd=0.1)
    #plot(sa511_513, add=T)
    box()
    legend("topleft", paste(year, "-", month, sep=""), bty="n", cex=1.5)
    legend("bottomright", legend=c(names(attr(colcode, "table"))), fill=c(attr(colcode, "palette")), cex=1.1,
           bty = "n", title=title)
    axis(1, cex=0.5)
    axis(2, cex=0.5)
  }
  #dev.off()
}

par(mar=c(2,2,0,0), mfrow=c(2,4))
plot4x4(FLFJ_diff_rast_00.list,"FLFJ","2000","J",depth_grid_plot000612,"Relative Diff")
plot4x4(FLFJ_diff_rast_06.list,"FLFJ","2006","J",depth_grid_plot000612,"Relative Diff")
plot4x4(FLFJ_diff_rast_12.list,"FLFJ","2012","J",depth_grid_plot000612,"Relative Diff")
plot4x4(FLFJ_diff_rast_17.list,"FLFJ","2017","J",depth_grid_plot2017,"Relative Diff")
#plot4x4(FLFJ_REGdiff_rast_00.list,"FLFJ","2000","J",depth_grid_plot000612,"Difference")
#plot4x4(FLFJ_REGdiff_rast_06.list,"FLFJ","2006","J",depth_grid_plot000612,"Difference")
#plot4x4(FLFJ_REGdiff_rast_12.list,"FLFJ","2012","J",depth_grid_plot000612,"Difference")
#plot4x4(FLFJ_REGdiff_rast_17.list,"FLFJ","2017","J",depth_grid_plot2017,"Difference")
plot4x4(FLFJ2_diff_rast_00.list,"FLFJ","2000","J",depth_grid_plot000612,"Relative Diff")
plot4x4(FLFJ2_diff_rast_06.list,"FLFJ","2006","J",depth_grid_plot000612,"Relative Diff")
plot4x4(FLFJ2_diff_rast_12.list,"FLFJ","2012","J",depth_grid_plot000612,"Relative Diff")
plot4x4(FLFJ2_diff_rast_17.list,"FLFJ","2017","J",depth_grid_plot2017,"Relative Diff")





####plot SPMA all plots####
par(mar=c(2,2,0,0), mfrow=c(3,4))
plot4x4(SPMARAST00,"SPMA","2000","A",depth_grid_plot000612)
plot4x4(SPMARAST06,"SPMA","2006","A",depth_grid_plot000612)
plot4x4(SPMARAST12,"SPMA","2012","A",depth_grid_plot000612)
plot4x4(SPMARAST17,"SPMA","2017","A",depth_grid_plot2017)
plot4x4(SPMAewRAST00,"SPMA","2000","A",depth_grid_plot000612)
plot4x4(SPMAewRAST06,"SPMA","2006","A",depth_grid_plot000612)
plot4x4(SPMAewRAST12,"SPMA","2012","A",depth_grid_plot000612)
plot4x4(SPMAewRAST17,"SPMA","2017","A",depth_grid_plot2017)
plot4x4(SPMAew2RAST00,"SPMA","2000","A",depth_grid_plot000612)
plot4x4(SPMAew2RAST06,"SPMA","2006","A",depth_grid_plot000612)
plot4x4(SPMAew2RAST12,"SPMA","2012","A",depth_grid_plot000612)
plot4x4(SPMAew2RAST17,"SPMA","2017","A",depth_grid_plot2017)

plot4x4<-function(rasterdata, month,year,size,depth_grid,title){
  #jpeg(filename = paste("D://MENHtrawl/Plots/grid_NS_map.jpeg", sep=""), width=140, height=120, units = "mm", res = 600)
  #par(mar=c(2,2,0,0), mfrow=c(1,1))
  plotvar=unlist(rasterdata)
  nclr=9
  plotclr <- (brewer.pal(nclr,"RdYlBu"))
  class <- classIntervals(plotvar, nclr, style="quantile")
  fix_break<-round(class$brks, digits = 2)
  for(i in 1:length(rasterdata)){
    print(i)
    plotvar <- rasterdata[[i]]
    nclr=9
    plotclr <- (brewer.pal(nclr,"RdYlBu"))
    class <- classIntervals(plotvar, nclr, style="fixed", 
                            fixedBreaks=
                              if(size=="J")c(-200,-100,-50,-25,-15,-5,5,15,25,50,100,200,500) #breaks calculated by using average nonstationary "quantile" breaks
                            else c(-200,-100,-50,-25,-15,-5,5,15,25,50,100,200,500))
    
    colcode <- findColours(class, plotclr)
    
    start_x <- range(GAM_data$Longitude)[1]
    end_x <- range(GAM_data$Longitude)[2]
    start_y <- range(GAM_data$Latitude)[1]
    end_y <- range(GAM_data$Latitude)[2]
    plot(depth_grid$lon, depth_grid$lat, pch=16, col=colcode, cex=0.1, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, xlab="Longitude", ylab="Latitude")
    map(database = mainecoast, ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE, lwd=0.1)
    #plot(sa511_513, add=T)
    box()
    legend("topleft", paste(year, "-", month, sep=""), bty="n", cex=1.5)
    #legend_order <- matrix(1:12,ncol=2,byrow = FALSE)
    #legend("bottomright",legend=c(names(attr(colcode, "table")))[legend_order],
           #fill=c(attr(colcode, "palette"))[legend_order], cex=1.1,
           #bty = "n", title=title, ncol=2)
    legend("bottomright", legend=c(names(attr(colcode, "table"))), fill=c(attr(colcode, "palette")), cex=1.1,
           bty = "n", title=title)
    axis(1, cex=0.5)
    axis(2, cex=0.5)
  }
  #dev.off()
}

par(mar=c(2,2,0,0), mfrow=c(2,4))
plot4x4(SPMA_diff_rast_00.list,"SPMA","2000","A",depth_grid_plot000612,"Relative Diff")
plot4x4(SPMA_diff_rast_06.list,"SPMA","2006","A",depth_grid_plot000612,"Relative Diff")
plot4x4(SPMA_diff_rast_12.list,"SPMA","2012","A",depth_grid_plot000612,"Relative Diff")
plot4x4(SPMA_diff_rast_17.list,"SPMA","2017","A",depth_grid_plot2017,"Relative Diff")
#plot4x4(SPMA_REGdiff_rast_00.list,"SPMA","2000","A",depth_grid_plot000612,"Difference")
#plot4x4(SPMA_REGdiff_rast_06.list,"SPMA","2006","A",depth_grid_plot000612,"Difference")
#plot4x4(SPMA_REGdiff_rast_12.list,"SPMA","2012","A",depth_grid_plot000612,"Difference")
#plot4x4(SPMA_REGdiff_rast_17.list,"SPMA","2017","A",depth_grid_plot2017,"Difference")
plot4x4(SPMA2_diff_rast_00.list,"SPMA","2000","A",depth_grid_plot000612,"Relative Diff")
plot4x4(SPMA2_diff_rast_06.list,"SPMA","2006","A",depth_grid_plot000612,"Relative Diff")
plot4x4(SPMA2_diff_rast_12.list,"SPMA","2012","A",depth_grid_plot000612,"Relative Diff")
plot4x4(SPMA2_diff_rast_17.list,"SPMA","2017","A",depth_grid_plot2017,"Relative Diff")


##### T-test for hindcast years#####
common <- function(data.frame1,data.frame2){
  common1=merge(data.frame1,data.frame2, by = c("Latitude", "Longitude"))
  t.test(common1$p.abundance.x, common1$p.abundance.y, paired = TRUE, alternative = "two.sided")
}
common(FLFJ2000,FLFJew2000)
common(FLMJ2000,FLMJew2000)
common(FLFA2000,FLFAew2000)
common(FLMA2000,FLMAew2000)
common(SPFJ2000,SPFJew2000)
common(SPMJ2000,SPMJew2000)
common(SPFA2000,SPFAew2000)
common(SPMA2000,SPMAew2000)

common(FLFJ2006,FLFJew2006)
common(FLMJ2006,FLMJew2006)
common(FLFA2006,FLFAew2006)
common(FLMA2006,FLMAew2006)
common(SPFJ2006,SPFJew2006)
common(SPMJ2006,SPMJew2006)
common(SPFA2006,SPFAew2006)
common(SPMA2006,SPMAew2006)

common(FLFJ2012,FLFJew2012)
common(FLMJ2012,FLMJew2012)
common(FLFA2012,FLFAew2012)
common(FLMA2012,FLMAew2012)
common(SPFJ2012,SPFJew2012)
common(SPMJ2012,SPMJew2012)
common(SPFA2012,SPFAew2012)
common(SPMA2012,SPMAew2012)

common(FLFJ2017,FLFJew2017)
common(FLMJ2017,FLMJew2017)
common(FLFA2017,FLFAew2017)
common(FLMA2017,FLMAew2017)
common(SPFJ2017,SPFJew2017)
common(SPMJ2017,SPMJew2017)
common(SPFA2017,SPFAew2017)
common(SPMA2017,SPMAew2017)

common(FLFJ2000,FLFJew22000)
common(FLMJ2000,FLMJew22000)
common(FLFA2000,FLFAew22000)
common(FLMA2000,FLMAew22000)
common(SPFJ2000,SPFJew22000)
common(SPMJ2000,SPMJew22000)
common(SPFA2000,SPFAew22000)
common(SPMA2000,SPMAew22000)

common(FLFJ2006,FLFJew22006)
common(FLMJ2006,FLMJew22006)
common(FLFA2006,FLFAew22006)
common(FLMA2006,FLMAew22006)
common(SPFJ2006,SPFJew22006)
common(SPMJ2006,SPMJew22006)
common(SPFA2006,SPFAew22006)
common(SPMA2006,SPMAew22006)

common(FLFJ2012,FLFJew22012)
common(FLMJ2012,FLMJew22012)
common(FLFA2012,FLFAew22012)
common(FLMA2012,FLMAew22012)
common(SPFJ2012,SPFJew22012)
common(SPMJ2012,SPMJew22012)
common(SPFA2012,SPFAew22012)
common(SPMA2012,SPMAew22012)

common(FLFJ2017,FLFJew22017)
common(FLMJ2017,FLMJew22017)
common(FLFA2017,FLFAew22017)
common(FLMA2017,FLMAew22017)
common(SPFJ2017,SPFJew22017)
common(SPMJ2017,SPMJew22017)
common(SPFA2017,SPFAew22017)
common(SPMA2017,SPMAew22017)

