
#this usused data curated from "GetFVcomData7805.R"

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
library(rgdal)#to read in OGR files


GAM_data<- read_excel("D:/MENHtrawl/Cir data/DATAREDO2.xlsx") # Read Maine-New Hampshier Bottom Trawl Survey data.
GAM_data<- na.omit(GAM_data)#Remove NAs
#######
#changes_interpolated_45S55<- read.csv("D:/MENHtrawl/Cam's forecast TS data/changes_interpolated_45S55.csv")
#changes_interpolated_45S99<- read.csv("D:/MENHtrawl/Cam's forecast TS data/changes_interpolated_45S99.csv")
#changes_interpolated_45T55<- read.csv("D:/MENHtrawl/Cam's forecast TS data/changes_interpolated_45T55.csv")
#changes_interpolated_45T99<- read.csv("D:/MENHtrawl/Cam's forecast TS data/changes_interpolated_45T99.csv")
changes_interpolated_85S55<- read.csv("D:/MENHtrawl/Cam's forecast TS data/changes_interpolated_85S55.csv")
#changes_interpolated_85S99<- read.csv("D:/MENHtrawl/Cam's forecast TS data/changes_interpolated_85S99.csv")
changes_interpolated_85T55<- read.csv("D:/MENHtrawl/Cam's forecast TS data/changes_interpolated_85T55.csv")
#changes_interpolated_85T99<- read.csv("D:/MENHtrawl/Cam's forecast TS data/changes_interpolated_85T99.csv")

#### Load depth data and distance from shore data ####
depth_grid_plot000612 <- read.csv("D://MENHtrawl/grid_depth_data000612.csv") #Depth data for years 2000,2006, and 2012

load("D://MENHtrawl/hindcast_variables/DFS000612.RData")

#####Combine salinity, latitude, longitude, distance from shore####


SP7805<-as.data.frame(cbind(depth_grid_plot000612,DFS000612))
SP7805<-data.frame(SP7805[c(2,3,6)])
names(SP7805)[1]<-"Longitude"
names(SP7805)[2]<-"Latitude"
names(SP7805)[3]<-"dist_frm_shore"
#save(SP7805, file="D://MENHtrawl/hindcast_variables/SP7805.RData")

FL7805<-as.data.frame(cbind(depth_grid_plot000612,DFS000612))
FL7805<-data.frame(FL7805[c(2,3,6)])
names(FL7805)[1]<-"Longitude"
names(FL7805)[2]<-"Latitude"
names(FL7805)[3]<-"dist_frm_shore"
#save(FL7805, file="D://MENHtrawl/hindcast_variables/FL7805.RData")


######get sediment data####
load("D://MENHtrawl/hindcast_variables/sediment0012.RData")

SP7805<-cbind(SP7805,sediment0012[1])
FL7805<-cbind(FL7805,sediment0012[1])

names(SP7805)[4]<-"sediment"
names(FL7805)[4]<-"sediment"


load("D://MENHtrawl/FVCOM_csv/FVdepth.RData")
colnames(FVdepth)<-c("Longitude","Latitude","AvgDepth")

SP7805<-merge(SP7805,FVdepth,by=c("Longitude","Latitude"))
FL7805<-merge(FL7805,FVdepth,by=c("Longitude","Latitude"))

#save(SP7805, file="D://MENHtrawl/hindcast_variables/SP7805.RData")
#save(FL7805, file="D://MENHtrawl/hindcast_variables/FL7805.RData")
######



load(file="D://MENHtrawl/Forecast_SDM1/avg.FLtemp.7805.RData") #fvcom data made in "GetFVcomData7805.R file
load(file="D://MENHtrawl/Forecast_SDM1/avg.SPtemp.7805.RData")

load(file="D://MENHtrawl/Forecast_SDM1/avg.FLsal.7805.RData") #fvcom data made in "GetFVcomData7805.R file
load(file="D://MENHtrawl/Forecast_SDM1/avg.SPsal.7805.RData")

####adding temp/sal####

SP7805_2<-cbind(depth_grid_plot000612[2:3],avg.SPtemp.7805)
FL7805_2<-cbind(depth_grid_plot000612[2:3],avg.FLtemp.7805)

SP7805_2<-cbind(SP7805_2,avg.SPsal.7805)
FL7805_2<-cbind(FL7805_2,avg.FLsal.7805)

names(SP7805_2)[1]<-"Longitude"
names(SP7805_2)[2]<-"Latitude"
names(FL7805_2)[1]<-"Longitude"
names(FL7805_2)[2]<-"Latitude"

names(SP7805_2)[4]<-"Bottom_Salinity_psu"
names(SP7805_2)[3]<-"Bottom_WaterTempDegC"
names(FL7805_2)[4]<-"Bottom_Salinity_psu"
names(FL7805_2)[3]<-"Bottom_WaterTempDegC"


#test1<-merge(SP7805_2,SP7805,by=c("Longitude","Latitude"))
#test2<-merge(FL7805_2,FL7805,by=c("Longitude","Latitude"))

Springfinal7805<-merge(SP7805_2,SP7805,by=c("Longitude","Latitude"))
Fallfinal7805<-merge(FL7805_2,FL7805,by=c("Longitude","Latitude"))


#save(SP7805_2, file="D://MENHtrawl/Forecast_SDM1/SP7805_2.RData")
#save(FL7805_2, file="D://MENHtrawl/Forecast_SDM1/FL7805_2.RData")

#save(Springfinal7805, file="D://MENHtrawl/Forecast_SDM1/Springfinal7805.RData")
#save(Fallfinal7805, file="D://MENHtrawl/Forecast_SDM1/Fallfinal7805.RData")


SP7805_2= Springfinal7805
FL7805_2= Fallfinal7805

copySP7805= SP7805_2
copyFL7805= FL7805_2

####spring temp####
SP78051<- SP7805_2[!is.na(SP7805_2$Bottom_WaterTempDegC), ]
spring79_05 <- list()
coordinates(depth_grid_plot000612) <- ~lon + lat
for(i in 1:1){
  print(i)
  temp_data <- SP78051
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, Bottom_WaterTempDegC, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  spring79_05[[i]] <- raster::extract(rast, depth_grid_plot000612)
}
depth_grid_plot000612 <- read.csv("D://MENHtrawl/grid_depth_data000612.csv") #Depth data for years 2000,2006, and 2012
cbindrightlocations<-cbind(depth_grid_plot000612[2:3],spring79_05[[1]])
names(cbindrightlocations)[1]<-"Longitude"
names(cbindrightlocations)[2]<-"Latitude"
SP7805_2<- merge(SP7805,cbindrightlocations,by=c("Longitude","Latitude"))
names(SP7805_2)[6]<-"Bottom_WaterTempDegC"
#




############fall temp####


FL78051<- FL7805_2[!is.na(FL7805_2$Bottom_WaterTempDegC), ]
fall79_05 <- list()
coordinates(depth_grid_plot000612) <- ~lon + lat
for(i in 1:1){
  print(i)
  temp_data <- FL78051
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, Bottom_WaterTempDegC, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  fall79_05[[i]] <- raster::extract(rast, depth_grid_plot000612)
}
depth_grid_plot000612 <- read.csv("D://MENHtrawl/grid_depth_data000612.csv") #Depth data for years 2000,2006, and 2012
cbindrightlocations<-cbind(depth_grid_plot000612[2:3],fall79_05[[1]])
names(cbindrightlocations)[1]<-"Longitude"
names(cbindrightlocations)[2]<-"Latitude"
FL7805_2<- merge(FL7805,cbindrightlocations,by=c("Longitude","Latitude"))
names(FL7805_2)[6]<-"Bottom_WaterTempDegC"
#





##########salinity###########

SP78051<- copySP7805[!is.na(copySP7805$Bottom_Salinity_psu), ]
Sspring79_05 <- list()
coordinates(depth_grid_plot000612) <- ~lon + lat
for(i in 1:1){
  print(i)
  temp_data <- SP78051
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, Bottom_Salinity_psu, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  Sspring79_05[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

depth_grid_plot000612 <- read.csv("D://MENHtrawl/grid_depth_data000612.csv") #Depth data for years 2000,2006, and 2012
cbindrightlocations<-cbind(depth_grid_plot000612[2:3],Sspring79_05[[1]])
names(cbindrightlocations)[1]<-"Longitude"
names(cbindrightlocations)[2]<-"Latitude"
SP7805_2<- merge(SP7805_2,cbindrightlocations,by=c("Longitude","Latitude"))
names(SP7805_2)[7]<-"Bottom_Salinity_psu"
#

######salinity fall####
#FL7805_2<-cbind(FL7805_2,FLfvcom7805$`unlist(avg.sal.FL7805[[1]])`)
#names(FL7805_2)[7]<-"Bottom_Salinity_psu"

FL78051<- copyFL7805[!is.na(copyFL7805$Bottom_Salinity_psu), ]

Sfall79_05 <- list()
coordinates(depth_grid_plot000612) <- ~lon + lat
for(i in 1:1){
  print(i)
  temp_data <- FL78051
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, Bottom_Salinity_psu, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  Sfall79_05[[i]] <- raster::extract(rast, depth_grid_plot000612)
}
depth_grid_plot000612 <- read.csv("D://MENHtrawl/grid_depth_data000612.csv") #Depth data for years 2000,2006, and 2012
cbindrightlocations<-cbind(depth_grid_plot000612[2:3],Sfall79_05[[1]])
names(cbindrightlocations)[1]<-"Longitude"
names(cbindrightlocations)[2]<-"Latitude"
FL7805_2<- merge(FL7805_2,cbindrightlocations,by=c("Longitude","Latitude"))
names(FL7805_2)[7]<-"Bottom_Salinity_psu"
#


#save(SP7805_2, file="D://MENHtrawl/Forecast_SDM1/SP2855.RData") ##earlier forecast period
#save(FL7805_2, file="D://MENHtrawl/Forecast_SDM1/FL2855.RData")

#save(SP7805_2, file="D://MENHtrawl/Forecast_SDM1/SP7805_2.RData") ###later forecast period
#save(FL7805_2, file="D://MENHtrawl/Forecast_SDM1/FL7805_2.RData")





####START HERE#################################################
source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}

source_lines("D://MENHtrawl/GAM.R",1:245)
source_lines("D://MENHtrawl/GAM nonstationary.R", 1:481)
source_lines("D://MENHtrawl/GAM nonstationary EMW.R", 1:758)


depth_grid_plot000612 <- read.csv("D://MENHtrawl/grid_depth_data000612.csv") #Depth data for years 2000,2006, and 2012

#changes_interpolated_45S55<- read.csv("D:/MENHtrawl/Cam's forecast TS data/changes_interpolated_45S55.csv")
#changes_interpolated_45S99<- read.csv("D:/MENHtrawl/Cam's forecast TS data/changes_interpolated_45S99.csv")
#changes_interpolated_45T55<- read.csv("D:/MENHtrawl/Cam's forecast TS data/changes_interpolated_45T55.csv")
#changes_interpolated_45T99<- read.csv("D:/MENHtrawl/Cam's forecast TS data/changes_interpolated_45T99.csv")
changes_interpolated_85S55<- read.csv("D:/MENHtrawl/Cam's forecast TS data/changes_interpolated_85S55.csv")
#changes_interpolated_85S99<- read.csv("D:/MENHtrawl/Cam's forecast TS data/changes_interpolated_85S99.csv")
changes_interpolated_85T55<- read.csv("D:/MENHtrawl/Cam's forecast TS data/changes_interpolated_85T55.csv")
#changes_interpolated_85T99<- read.csv("D:/MENHtrawl/Cam's forecast TS data/changes_interpolated_85T99.csv")

load("D://MENHtrawl/Forecast_SDM1/SP7805_2.RData")
load("D://MENHtrawl/Forecast_SDM1/FL7805_2.RData")

#######################

###get raster data for Fall####
###45_55####
RAST45S55 <- list()
coordinates(depth_grid_plot000612) <- ~lon + lat
for(i in 1:1){
  print(i)
  temp_data <- changes_interpolated_45S55
  rast_col <- ceiling((range(temp_data$x)[2]-range(temp_data$x)[1])/0.01)
  rast_row <- ceiling((range(temp_data$y)[2]-range(temp_data$y)[1])/0.01)
  akima.smooth <- with(temp_data, interp(x,y, data, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  RAST45S55[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FL45S55<- cbind(FL7805,RAST45S55[[1]])

RAST45T55 <- list()
coordinates(depth_grid_plot000612) <- ~lon + lat
for(i in 1:1){
  print(i)
  temp_data <- changes_interpolated_45T55
  rast_col <- ceiling((range(temp_data$x)[2]-range(temp_data$x)[1])/0.01)
  rast_row <- ceiling((range(temp_data$y)[2]-range(temp_data$y)[1])/0.01)
  akima.smooth <- with(temp_data, interp(x,y, data, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  RAST45T55[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FL45T55<- cbind(FL45S55,RAST45T55[[1]])

FL45T55$TEMP4.5=rowSums(cbind(FL45T55$Bottom_WaterTempDegC,FL45T55$`RAST45T55[[1]]`),na.rm=TRUE)
FL45T55$SAL4.5=rowSums(cbind(FL45T55$Bottom_Salinity_psu,FL45T55$`RAST45S55[[1]]`),na.rm=TRUE)


####45_99#####
RAST45S99 <- list()
coordinates(depth_grid_plot000612) <- ~lon + lat
for(i in 1:1){
  print(i)
  temp_data <- changes_interpolated_45S99
  rast_col <- ceiling((range(temp_data$x)[2]-range(temp_data$x)[1])/0.01)
  rast_row <- ceiling((range(temp_data$y)[2]-range(temp_data$y)[1])/0.01)
  akima.smooth <- with(temp_data, interp(x,y, data, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  RAST45S99[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FL45S99<- cbind(FL7805,RAST45S99[[1]])

RAST45T99 <- list()
coordinates(depth_grid_plot000612) <- ~lon + lat
for(i in 1:1){
  print(i)
  temp_data <- changes_interpolated_45T99
  rast_col <- ceiling((range(temp_data$x)[2]-range(temp_data$x)[1])/0.01)
  rast_row <- ceiling((range(temp_data$y)[2]-range(temp_data$y)[1])/0.01)
  akima.smooth <- with(temp_data, interp(x,y, data, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  RAST45T99[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FL45T99<- cbind(FL45S99,RAST45T99[[1]])

FL45T99$TEMP4.5=rowSums(cbind(FL45T99$Bottom_WaterTempDegC,FL45T99$`RAST45T99[[1]]`),na.rm=TRUE)
FL45T99$SAL4.5=rowSums(cbind(FL45T99$Bottom_Salinity_psu,FL45T99$`RAST45S99[[1]]`),na.rm=TRUE)

###85_55####
RAST85S55 <- list()
coordinates(depth_grid_plot000612) <- ~lon + lat
for(i in 1:1){
  print(i)
  temp_data <- changes_interpolated_85S55
  rast_col <- ceiling((range(temp_data$x)[2]-range(temp_data$x)[1])/0.01)
  rast_row <- ceiling((range(temp_data$y)[2]-range(temp_data$y)[1])/0.01)
  akima.smooth <- with(temp_data, interp(x,y, data, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  RAST85S55[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

depth_grid_plot000612 <- read.csv("D://MENHtrawl/grid_depth_data000612.csv") #Depth data for years 2000,2006, and 2012
cbindrightlocations<-cbind(depth_grid_plot000612[2:3],RAST85S55[[1]])
names(cbindrightlocations)[1]<-"Longitude"
names(cbindrightlocations)[2]<-"Latitude"
FL85S55_2<- merge(FL7805_2,cbindrightlocations,by=c("Longitude","Latitude"))
names(FL85S55_2)[8]<-"anolsal"




RAST85T55 <- list()
coordinates(depth_grid_plot000612) <- ~lon + lat
for(i in 1:1){
  print(i)
  temp_data <- changes_interpolated_85T55
  rast_col <- ceiling((range(temp_data$x)[2]-range(temp_data$x)[1])/0.01)
  rast_row <- ceiling((range(temp_data$y)[2]-range(temp_data$y)[1])/0.01)
  akima.smooth <- with(temp_data, interp(x,y, data, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  RAST85T55[[i]] <- raster::extract(rast, depth_grid_plot000612)
}


depth_grid_plot000612 <- read.csv("D://MENHtrawl/grid_depth_data000612.csv") #Depth data for years 2000,2006, and 2012
cbindrightlocations<-cbind(depth_grid_plot000612[2:3],RAST85T55[[1]])
names(cbindrightlocations)[1]<-"Longitude"
names(cbindrightlocations)[2]<-"Latitude"
FL85T55_2<- merge(FL85S55_2,cbindrightlocations,by=c("Longitude","Latitude"))
names(FL85T55_2)[9]<-"anoltemp"


FL85T55_2$TEMP8.5=rowSums(cbind(FL85T55_2$Bottom_WaterTempDegC,FL85T55_2$anoltemp),na.rm=TRUE)
FL85T55_2$SAL8.5=rowSums(cbind(FL85T55_2$Bottom_Salinity_psu,FL85T55_2$anolsal),na.rm=TRUE)

####85_99#####
RAST85S99 <- list()
coordinates(depth_grid_plot000612) <- ~lon + lat
for(i in 1:1){
  print(i)
  temp_data <- changes_interpolated_85S99
  rast_col <- ceiling((range(temp_data$x)[2]-range(temp_data$x)[1])/0.01)
  rast_row <- ceiling((range(temp_data$y)[2]-range(temp_data$y)[1])/0.01)
  akima.smooth <- with(temp_data, interp(x,y, data, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  RAST85S99[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

depth_grid_plot000612 <- read.csv("D://MENHtrawl/grid_depth_data000612.csv") #Depth data for years 2000,2006, and 2012
cbindrightlocations<-cbind(depth_grid_plot000612[2:3],RAST85S99[[1]])
names(cbindrightlocations)[1]<-"Longitude"
names(cbindrightlocations)[2]<-"Latitude"
FL85S99_2<- merge(FL7805_2,cbindrightlocations,by=c("Longitude","Latitude"))
names(FL85S99_2)[8]<-"anolsal"




RAST85T99 <- list()
coordinates(depth_grid_plot000612) <- ~lon + lat
for(i in 1:1){
  print(i)
  temp_data <- changes_interpolated_85T99
  rast_col <- ceiling((range(temp_data$x)[2]-range(temp_data$x)[1])/0.01)
  rast_row <- ceiling((range(temp_data$y)[2]-range(temp_data$y)[1])/0.01)
  akima.smooth <- with(temp_data, interp(x,y, data, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  RAST85T99[[i]] <- raster::extract(rast, depth_grid_plot000612)
}


depth_grid_plot000612 <- read.csv("D://MENHtrawl/grid_depth_data000612.csv") #Depth data for years 2000,2006, and 2012
cbindrightlocations<-cbind(depth_grid_plot000612[2:3],RAST85T99[[1]])
names(cbindrightlocations)[1]<-"Longitude"
names(cbindrightlocations)[2]<-"Latitude"
FL85T99_2<- merge(FL85S99_2,cbindrightlocations,by=c("Longitude","Latitude"))
names(FL85T99_2)[9]<-"anoltemp"


FL85T99_2$TEMP8.5=rowSums(cbind(FL85T99_2$Bottom_WaterTempDegC,FL85T99_2$anoltemp),na.rm=TRUE)
FL85T99_2$SAL8.5=rowSums(cbind(FL85T99_2$Bottom_Salinity_psu,FL85T99_2$anolsal),na.rm=TRUE)

#####Get raster data for spring#####
###45_55####
SP45S55<- cbind(SP7805,RAST45S55[[1]])
SP45T55<- cbind(SP45S55,RAST45T55[[1]])

SP45T55$TEMP4.5=rowSums(cbind(SP45T55$Bottom_WaterTempDegC,SP45T55$`RAST45T55[[1]]`),na.rm=TRUE)
SP45T55$SAL4.5=rowSums(cbind(SP45T55$Bottom_Salinity_psu,SP45T55$`RAST45S55[[1]]`),na.rm=TRUE)

####45_99#####
SP45S99<- cbind(SP7805,RAST45S99[[1]])
SP45T99<- cbind(SP45S99,RAST45T99[[1]])

SP45T99$TEMP4.5=rowSums(cbind(SP45T99$Bottom_WaterTempDegC,SP45T99$`RAST45T99[[1]]`),na.rm=TRUE)
SP45T99$SAL4.5=rowSums(cbind(SP45T99$Bottom_Salinity_psu,SP45T99$`RAST45S99[[1]]`),na.rm=TRUE)

###85_55####
depth_grid_plot000612 <- read.csv("D://MENHtrawl/grid_depth_data000612.csv") #Depth data for years 2000,2006, and 2012
cbindrightlocations<-cbind(depth_grid_plot000612[2:3],RAST85S55[[1]])
names(cbindrightlocations)[1]<-"Longitude"
names(cbindrightlocations)[2]<-"Latitude"
SP85S55_2<- merge(SP7805_2,cbindrightlocations,by=c("Longitude","Latitude"))
names(SP85S55_2)[8]<-"anolsal"

depth_grid_plot000612 <- read.csv("D://MENHtrawl/grid_depth_data000612.csv") #Depth data for years 2000,2006, and 2012
cbindrightlocations<-cbind(depth_grid_plot000612[2:3],RAST85T55[[1]])
names(cbindrightlocations)[1]<-"Longitude"
names(cbindrightlocations)[2]<-"Latitude"
SP85T55_2<- merge(SP85S55_2,cbindrightlocations,by=c("Longitude","Latitude"))
names(SP85T55_2)[9]<-"anoltemp"


SP85T55_2$TEMP8.5=rowSums(cbind(SP85T55_2$Bottom_WaterTempDegC,SP85T55_2$anoltemp),na.rm=TRUE)
SP85T55_2$SAL8.5=rowSums(cbind(SP85T55_2$Bottom_Salinity_psu,SP85T55_2$anolsal),na.rm=TRUE)

####85_99#####
depth_grid_plot000612 <- read.csv("D://MENHtrawl/grid_depth_data000612.csv") #Depth data for years 2000,2006, and 2012
cbindrightlocations<-cbind(depth_grid_plot000612[2:3],RAST85S99[[1]])
names(cbindrightlocations)[1]<-"Longitude"
names(cbindrightlocations)[2]<-"Latitude"
SP85S99_2<- merge(SP7805_2,cbindrightlocations,by=c("Longitude","Latitude"))
names(SP85S99_2)[8]<-"anolsal"

depth_grid_plot000612 <- read.csv("D://MENHtrawl/grid_depth_data000612.csv") #Depth data for years 2000,2006, and 2012
cbindrightlocations<-cbind(depth_grid_plot000612[2:3],RAST85T99[[1]])
names(cbindrightlocations)[1]<-"Longitude"
names(cbindrightlocations)[2]<-"Latitude"
SP85T99_2<- merge(SP85S99_2,cbindrightlocations,by=c("Longitude","Latitude"))
names(SP85T99_2)[9]<-"anoltemp"


SP85T99_2$TEMP8.5=rowSums(cbind(SP85T99_2$Bottom_WaterTempDegC,SP85T99_2$anoltemp),na.rm=TRUE)
SP85T99_2$SAL8.5=rowSums(cbind(SP85T99_2$Bottom_Salinity_psu,SP85T99_2$anolsal),na.rm=TRUE)


#####Save forecast data####
#save(FL45T55, file="D://MENHtrawl/forecast_variables/FL45T55.RData")
#save(FL45T99, file="D://MENHtrawl/forecast_variables/FL45T99.RData")
save(FL85T55_2, file="D://MENHtrawl/forecast_variables/FL85T55.RData")
#save(FL85T99_2, file="D://MENHtrawl/Forecast_SDM1/FL85T99_3.RData") #SP85T99_2 is for years 1978-2005, FL85T99_2 is for 2017, _3 if for jamie's fvcom data but labed in R as _2

#save(SP45T55, file="D://MENHtrawl/forecast_variables/SP45T55.RData")
#save(SP45T99, file="D://MENHtrawl/forecast_variables/SP45T99.RData")
save(SP85T55_2, file="D://MENHtrawl/forecast_variables/SP85T55.RData")
#save(SP85T99_2, file="D://MENHtrawl/Forecast_SDM1/SP85T99_3.RData")


####START HERE####################################################################################
source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}

source_lines("D://MENHtrawl/GAM.R",1:245)
source_lines("D://MENHtrawl/GAM nonstationary.R", 1:481)
source_lines("D://MENHtrawl/GAM nonstationary EMW.R", 1:758)
source_lines("D://MENHtrawl/Hindcast GAM.R", 205:2465)


#SP85T99_2 is for years 1978-2005, FL85T99_2 is for 2017
#load("D://MENHtrawl/Forecast_SDM1/SP7805_2.RData")
#load("D://MENHtrawl/Forecast_SDM1/FL7805_2.RData")

load("D://MENHtrawl/forecast_variables/SP85T55.RData")
load("D://MENHtrawl/forecast_variables/FL85T55.RData")

#load("D://MENHtrawl/forecast_variables/FL45T55.RData")
#load("D://MENHtrawl/forecast_variables/FL45T99.RData")
#load("D://MENHtrawl/forecast_variables/FL85T55.RData")
#load("D://MENHtrawl/Forecast_SDM1/FL85T99_3.RData")
#FL85T99_2=SP85T99_2
#load("D://MENHtrawl/forecast_variables/SP45T55.RData")
#load("D://MENHtrawl/forecast_variables/SP45T99.RData")
#load("D://MENHtrawl/forecast_variables/SP85T55.RData")
#load("D://MENHtrawl/Forecast_SDM1/SP85T99_3.RData")
depth_grid_plot000612 <- read.csv("D://MENHtrawl/grid_depth_data000612.csv")#depth data for 2017



FL85T99_2<-FL85T55_2
SP85T99_2<-SP85T55_2
#FL45T55<-FL45T55[ -c(3:4,8:9) ]
#  FL45T55 <- FL45T55[c(1,2,6,7,3,4,5)]
#  names(FL45T55)[3]<-"Bottom_WaterTempDegC"
#  names(FL45T55)[4]<-"Bottom_Salinity_psu"
#FL45T99<-FL45T99[ -c(3:4,8:9) ]
#  FL45T99 <- FL45T99[c(1,2,6,7,3,4,5)]
#  names(FL45T99)[3]<-"Bottom_WaterTempDegC"
#  names(FL45T99)[4]<-"Bottom_Salinity_psu"
#FL85T55<-FL85T55[ -c(3:4,8:9) ]
#  FL85T55 <- FL85T55[c(1,2,6,7,3,4,5)]
#  names(FL85T55)[3]<-"Bottom_WaterTempDegC"
#  names(FL85T55)[4]<-"Bottom_Salinity_psu"
FL85T99_2<-FL85T99_2[ -c(6:9) ]
FL85T99_2 <- FL85T99_2[c(1,2,6,7,3,4,5)]
names(FL85T99_2)[3]<-"Bottom_WaterTempDegC"
names(FL85T99_2)[4]<-"Bottom_Salinity_psu"

#SP45T55<-SP45T55[ -c(3:4,8:9) ]
#  SP45T55 <- SP45T55[c(1,2,6,7,3,4,5)]
#  names(SP45T55)[3]<-"Bottom_WaterTempDegC"
#  names(SP45T55)[4]<-"Bottom_Salinity_psu"
#SP45T99<-SP45T99[ -c(3:4,8:9) ]
#  SP45T99 <- SP45T99[c(1,2,6,7,3,4,5)]
#  names(SP45T99)[3]<-"Bottom_WaterTempDegC"
#  names(SP45T99)[4]<-"Bottom_Salinity_psu"
#SP85T55<-SP85T55[ -c(3:4,8:9) ]
#  SP85T55 <- SP85T55[c(1,2,6,7,3,4,5)]
#  names(SP85T55)[3]<-"Bottom_WaterTempDegC"
#  names(SP85T55)[4]<-"Bottom_Salinity_psu"
SP85T99_2<-SP85T99_2[ -c(6:9) ]
SP85T99_2 <- SP85T99_2[c(1,2,6,7,3,4,5)]
names(SP85T99_2)[3]<-"Bottom_WaterTempDegC"
names(SP85T99_2)[4]<-"Bottom_Salinity_psu"

library(Jmisc)
###Adding year column/ cleanup############
#FL45T55<-addCol(FL45T55, Year=2042)
#FL45T99<-addCol(FL45T99, Year=2086)
#FL85T55<-addCol(FL85T55, Year=2042)
#FL85T99_2<-addCol(FL85T99_2, Year=2086)

#SP45T55<-addCol(SP45T55, Year=2042)
#SP45T99<-addCol(SP45T99, Year=2086)
#SP85T55<-addCol(SP85T55, Year=2042)
#SP85T99_2<-addCol(SP85T99_2, Year=2086)

#FL45T55$Year<-as.numeric(as.character(FL45T55$Year))
#FL45T99$Year<-as.numeric(as.character(FL45T99$Year))
#FL85T55$Year<-as.numeric(as.character(FL85T55$Year))
#FL85T99_2$Year<-as.numeric(as.character(FL85T99_2$Year))

#SP45T55$Year<-as.numeric(as.character(SP45T55$Year))
#SP45T99$Year<-as.numeric(as.character(SP45T99$Year))
#SP85T55$Year<-as.numeric(as.character(SP85T55$Year))
#SP85T99_2$Year<-as.numeric(as.character(SP85T99_2$Year))

cleanup<- function(groupfull){
  groupfull<-groupfull[!rowSums(groupfull[2] <42.8),]#cleaning up the data to only contain rows in the GOM
  groupfull<-groupfull[!rowSums(groupfull[2] >44.9),]
  groupfull<-groupfull[!rowSums(groupfull[1] <"-66.9"),]
  groupfull<-groupfull[!rowSums(groupfull[1] >"-70.8"),]
}

#FL45T55<-cleanup(FL45T55)
#FL45T99<-cleanup(FL45T99)
#FL85T55<-cleanup(FL85T55)
FL85T99_2<-cleanup(FL85T99_2)

#SP45T55<-cleanup(SP45T55)
#SP45T99<-cleanup(SP45T99)
#SP85T55<-cleanup(SP85T55)
SP85T99_2<-cleanup(SP85T99_2)

#FL45T55<-FL45T55[complete.cases(FL45T55), ]
#FL45T99<-FL45T99[complete.cases(FL45T99), ]
#FL85T55<-FL85T55[complete.cases(FL85T55), ]
FL85T99_2<-FL85T99_2[complete.cases(FL85T99_2), ]

#SP45T55<-SP45T55[complete.cases(SP45T55), ]
#SP45T99<-SP45T99[complete.cases(SP45T99), ]
#SP85T55<-SP85T55[complete.cases(SP85T55), ]
SP85T99_2<-SP85T99_2[complete.cases(SP85T99_2), ]


####12 Miles offhsore####

###assuming the earth is round,
#One radian is the angle for a line 3,958.761 miles long, so the angle for a line 12 miles long
#is 12/3,958.761 radians. 0.003031rad × 180/p = 0.1737°.

Miles12<- function(groupfull){
  groupfull<-groupfull[!rowSums(groupfull[5] >0.4776236),]#cleaning up the data to only contain rows up to 12miles offshore
}

#FL45T55<-Miles12(FL45T55)
#FL45T99<-Miles12(FL45T99)
#FL85T55<-Miles12(FL85T55)
FL85T99_2<-Miles12(FL85T99_2)

#SP45T55<-Miles12(SP45T55)
#SP45T99<-Miles12(SP45T99)
#SP85T55<-Miles12(SP85T55)
SP85T99_2<-Miles12(SP85T99_2)


######FORECAST STATIONARY INTERPOLATED 2099#####   
# remember to only have Gam.r running and not all of the other gam model data bc R gets confused
PredictGAM<- function(groupabundance.sig,group){
  
  predict.gam(groupabundance.sig, newdata=group, type="response")
}
FLFA85p.abundance<-PredictGAM(FLFAabundance.sig,FL85T99_2)
FLMA85p.abundance<-PredictGAM(FLMAabundance.sig,FL85T99_2)
FLFJ85p.abundance<-PredictGAM(FLFJabundance.sig,FL85T99_2)
FLMJ85p.abundance<-PredictGAM(FLMJabundance.sig,FL85T99_2)

SPFJ85p.abundance<-PredictGAM(SPFJabundance.sig,SP85T99_2)
SPMJ85p.abundance<-PredictGAM(SPMJabundance.sig,SP85T99_2)
SPFA85p.abundance<-PredictGAM(SPFAabundance.sig,SP85T99_2)
SPMA85p.abundance<-PredictGAM(SPMAabundance.sig,SP85T99_2)



namechange<- function(hindyear, groupp.abundance){
  
  hindyear<-data.frame(hindyear,groupp.abundance)
  names(hindyear)[8]<- "p.abundance"
  return(hindyear)
}
FLFJ8.5<-namechange(FL85T99_2,FLFJ85p.abundance)
FLMJ8.5<-namechange(FL85T99_2,FLMJ85p.abundance)
FLFA8.5<-namechange(FL85T99_2,FLFA85p.abundance)
FLMA8.5<-namechange(FL85T99_2,FLMA85p.abundance)

SPFJ8.5<-namechange(SP85T99_2,SPFJ85p.abundance)
SPMJ8.5<-namechange(SP85T99_2,SPMJ85p.abundance)
SPFA8.5<-namechange(SP85T99_2,SPFA85p.abundance)
SPMA8.5<-namechange(SP85T99_2,SPMA85p.abundance)

###Get raster data####
FLFJ8.5RAST <- list()
coordinates(depth_grid_plot000612) <- ~lon + lat
for(i in 1:1){
  print(i)
  temp_data <- FLFJ8.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFJ8.5RAST[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMJ8.5RAST <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMJ8.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMJ8.5RAST[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLFA8.5RAST <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFA8.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFA8.5RAST[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMA8.5RAST <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMA8.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMA8.5RAST[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFJ8.5RAST <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFJ8.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFJ8.5RAST[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMJ8.5RAST <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMJ8.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMJ8.5RAST[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFA8.5RAST <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFA8.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFA8.5RAST[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMA8.5RAST <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMA8.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMA8.5RAST [[i]] <- raster::extract(rast, depth_grid_plot000612)
}
#####Plot raster data#####
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
    plot(depth_grid_plot000612$lon, depth_grid_plot000612$lat, pch=16, col=colcode, cex=0.1, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, xlab="Longitude", ylab="Latitude")
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

plottemp(FLFJ8.5RAST,"FLFJ RCP 8.5","2072-2099","J")
plottemp(FLMJ8.5RAST,"FLMJ RCP 8.5","2072-2099","J")
plottemp(FLFA8.5RAST,"FLFA RCP 8.5","2072-2099","A")
plottemp(FLMA8.5RAST,"FLMA RCP 8.5","2072-2099","A")
plottemp(SPFJ8.5RAST,"SPFJ RCP 8.5","2072-2099","J")
plottemp(SPMJ8.5RAST,"SPMJ RCP 8.5","2072-2099","J")
plottemp(SPFA8.5RAST,"SPFA RCP 8.5","2028-2099","A")
plottemp(SPMA8.5RAST,"SPMA RCP 8.5","2072-2099","A")



####                                                    ####
###FORECAST NON_STATIONARY INTERPOLATED V1. 2099####
PredictGAM<- function(groupabundance.sig,group){
  
  predict.gam(groupabundance.sig, newdata=group, type="response")
}
###EAST####
FLFJ8.5Ep.abundance<-PredictGAM(FLFJEabundance.sig,FL85T99_2)
FLMJ8.5Ep.abundance<-PredictGAM(FLMJEabundance.sig,FL85T99_2)
FLFA8.5Ep.abundance<-PredictGAM(FLFAEabundance.sig,FL85T99_2)
FLMA8.5Ep.abundance<-PredictGAM(FLMAEabundance.sig,FL85T99_2)
SPFJ8.5Ep.abundance<-PredictGAM(SPFJEabundance.sig,SP85T99_2)
SPMJ8.5Ep.abundance<-PredictGAM(SPMJEabundance.sig,SP85T99_2)
SPFA8.5Ep.abundance<-PredictGAM(SPFAEabundance.sig,SP85T99_2)
SPMA8.5Ep.abundance<-PredictGAM(SPMAEabundance.sig,SP85T99_2)
###WEST####
FLFJ8.5Wp.abundance<-PredictGAM(FLFJWabundance.sig,FL85T99_2)
FLMJ8.5Wp.abundance<-PredictGAM(FLMJWabundance.sig,FL85T99_2)
FLFA8.5Wp.abundance<-PredictGAM(FLFAWabundance.sig,FL85T99_2)
FLMA8.5Wp.abundance<-PredictGAM(FLMAWabundance.sig,FL85T99_2)
SPFJ8.5Wp.abundance<-PredictGAM(SPFJWabundance.sig,SP85T99_2)
SPMJ8.5Wp.abundance<-PredictGAM(SPMJWabundance.sig,SP85T99_2)
SPFA8.5Wp.abundance<-PredictGAM(SPFAWabundance.sig,SP85T99_2)
SPMA8.5Wp.abundance<-PredictGAM(SPMAWabundance.sig,SP85T99_2)
###Namechange####
namechange<- function(hindyear, groupp.abundance){
  
  hindyear<-data.frame(hindyear,groupp.abundance)
  names(hindyear)[8]<- "p.abundance"
  return(hindyear)
}
FLFJ8.5E<-namechange(FL85T99_2,FLFJ8.5Ep.abundance)
FLMJ8.5E<-namechange(FL85T99_2,FLMJ8.5Ep.abundance)
FLFA8.5E<-namechange(FL85T99_2,FLFA8.5Ep.abundance)
FLMA8.5E<-namechange(FL85T99_2,FLMA8.5Ep.abundance)

SPFJ8.5E<-namechange(SP85T99_2,SPFJ8.5Ep.abundance)
SPMJ8.5E<-namechange(SP85T99_2,SPMJ8.5Ep.abundance)
SPFA8.5E<-namechange(SP85T99_2,SPFA8.5Ep.abundance)
SPMA8.5E<-namechange(SP85T99_2,SPMA8.5Ep.abundance)

###West2017###
FLFJ8.5W<-namechange(FL85T99_2,FLFJ8.5Wp.abundance)
FLMJ8.5W<-namechange(FL85T99_2,FLMJ8.5Wp.abundance)
FLFA8.5W<-namechange(FL85T99_2,FLFA8.5Wp.abundance)
FLMA8.5W<-namechange(FL85T99_2,FLMA8.5Wp.abundance)

SPFJ8.5W<-namechange(SP85T99_2,SPFJ8.5Wp.abundance)
SPMJ8.5W<-namechange(SP85T99_2,SPMJ8.5Wp.abundance)
SPFA8.5W<-namechange(SP85T99_2,SPFA8.5Wp.abundance)
SPMA8.5W<-namechange(SP85T99_2,SPMA8.5Wp.abundance)

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
FLFJ8.5E<-cleanupE(FLFJ8.5E)
FLMJ8.5E<-cleanupE(FLMJ8.5E)
FLFA8.5E<-cleanupE(FLFA8.5E)
FLMA8.5E<-cleanupE(FLMA8.5E)

SPFJ8.5E<-cleanupE(SPFJ8.5E)
SPMJ8.5E<-cleanupE(SPMJ8.5E)
SPFA8.5E<-cleanupE(SPFA8.5E)
SPMA8.5E<-cleanupE(SPMA8.5E)

###Cleanup WEST####
FLFJ8.5W<-cleanupW(FLFJ8.5W)
FLMJ8.5W<-cleanupW(FLMJ8.5W)
FLFA8.5W<-cleanupW(FLFA8.5W)
FLMA8.5W<-cleanupW(FLMA8.5W)

SPFJ8.5W<-cleanupW(SPFJ8.5W)
SPMJ8.5W<-cleanupW(SPMJ8.5W)
SPFA8.5W<-cleanupW(SPFA8.5W)
SPMA8.5W<-cleanupW(SPMA8.5W)

#####combine EW abundance predictions and plot interpolated ^  ####

FLFJ8.5ew=rbind(FLFJ8.5E,FLFJ8.5W)
FLMJ8.5ew=rbind(FLMJ8.5E,FLMJ8.5W)
FLFA8.5ew=rbind(FLFA8.5E,FLFA8.5W)
FLMA8.5ew=rbind(FLMA8.5E,FLMA8.5W)
SPFJ8.5ew=rbind(SPFJ8.5E,SPFJ8.5W)
SPMJ8.5ew=rbind(SPMJ8.5E,SPMJ8.5W)
SPFA8.5ew=rbind(SPFA8.5E,SPFA8.5W)
SPMA8.5ew=rbind(SPMA8.5E,SPMA8.5W)
#####Creating smoothed interpolated Hindcast###

###Get raster data####
###2099####
FLFJ8.5ewRAST <- list()
coordinates(depth_grid_plot000612) <- ~lon + lat
for(i in 1:1){
  print(i)
  temp_data <- FLFJ8.5ew
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row, duplicate="mean"))
  rast <- raster(akima.smooth)
  FLFJ8.5ewRAST[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMJ8.5ewRAST <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMJ8.5ew
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row, duplicate="mean"))
  rast <- raster(akima.smooth)
  FLMJ8.5ewRAST[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLFA8.5ewRAST <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFA8.5ew
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row, duplicate="mean"))
  rast <- raster(akima.smooth)
  FLFA8.5ewRAST [[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMA8.5ewRAST  <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMA8.5ew
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row, duplicate="mean"))
  rast <- raster(akima.smooth)
  FLMA8.5ewRAST[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFJ8.5ewRAST <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFJ8.5ew
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row, duplicate="mean"))
  rast <- raster(akima.smooth)
  SPFJ8.5ewRAST[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMJ8.5ewRAST <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMJ8.5ew
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row, duplicate="mean"))
  rast <- raster(akima.smooth)
  SPMJ8.5ewRAST[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFA8.5ewRAST <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFA8.5ew
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row, duplicate="mean"))
  rast <- raster(akima.smooth)
  SPFA8.5ewRAST[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMA8.5ewRAST <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMA8.5ew
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row, duplicate="mean"))
  rast <- raster(akima.smooth)
  SPMA8.5ewRAST[[i]] <- raster::extract(rast, depth_grid_plot000612)
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
    plot(depth_grid_plot000612$lon, depth_grid_plot000612$lat, pch=16, col=colcode, cex=0.1, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, xlab="Longitude", ylab="Latitude")
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

plottemp(FLFJ8.5ewRAST,"FLFJ RCP 8.5","2072-2099","J")
plottemp(FLMJ8.5ewRAST,"FLMJ RCP 8.5","2072-2099","J")
plottemp(FLFA8.5ewRAST,"FLFA RCP 8.5","2072-2099","A")
plottemp(FLMA8.5ewRAST,"FLMA RCP 8.5","2072-2099","A")
plottemp(SPFJ8.5ewRAST,"SPFJ RCP 8.5","2072-2099","J")
plottemp(SPMJ8.5ewRAST,"SPMJ RCP 8.5","2072-2099","J")
plottemp(SPFA8.5ewRAST,"SPFA RCP 8.5","2072-2099","A")
plottemp(SPMA8.5ewRAST,"SPMA RCP 8.5","2072-2099","A")


####                                                      ####
###FORECASTCAST NON_STATIONARY INTERPOLATED V2. 2099####
PredictGAM<- function(groupabundance.sig,group){
  
  predict.gam(groupabundance.sig, newdata=group, type="response")
}
###EAST####
FLFJE28.5p.abundance<-PredictGAM(FLFJE2abundance.sig,FL85T99_2)
FLMJE28.5p.abundance<-PredictGAM(FLMJE2abundance.sig,FL85T99_2)
FLFAE28.5p.abundance<-PredictGAM(FLFAE2abundance.sig,FL85T99_2)
FLMAE28.5p.abundance<-PredictGAM(FLMAE2abundance.sig,FL85T99_2)
SPFJE28.5p.abundance<-PredictGAM(SPFJE2abundance.sig,SP85T99_2)
SPMJE28.5p.abundance<-PredictGAM(SPMJE2abundance.sig,SP85T99_2)
SPFAE28.5p.abundance<-PredictGAM(SPFAE2abundance.sig,SP85T99_2)
SPMAE28.5p.abundance<-PredictGAM(SPMAE2abundance.sig,SP85T99_2)
###MIDDLE####
FLFJM28.5p.abundance<-PredictGAM(FLFJM2abundance.sig,FL85T99_2)
FLMJM28.5p.abundance<-PredictGAM(FLMJM2abundance.sig,FL85T99_2)
FLFAM28.5p.abundance<-PredictGAM(FLFAM2abundance.sig,FL85T99_2)
FLMAM28.5p.abundance<-PredictGAM(FLMAM2abundance.sig,FL85T99_2)
SPFJM28.5p.abundance<-PredictGAM(SPFJM2abundance.sig,SP85T99_2)
SPMJM28.5p.abundance<-PredictGAM(SPMJM2abundance.sig,SP85T99_2)
SPFAM28.5p.abundance<-PredictGAM(SPFAM2abundance.sig,SP85T99_2)
SPMAM28.5p.abundance<-PredictGAM(SPMAM2abundance.sig,SP85T99_2)
###WEST####
FLFJW28.5p.abundance<-PredictGAM(FLFJW2abundance.sig,FL85T99_2)
FLMJW28.5p.abundance<-PredictGAM(FLMJW2abundance.sig,FL85T99_2)
FLFAW28.5p.abundance<-PredictGAM(FLFAW2abundance.sig,FL85T99_2)
FLMAW28.5p.abundance<-PredictGAM(FLMAW2abundance.sig,FL85T99_2)
SPFJW28.5p.abundance<-PredictGAM(SPFJW2abundance.sig,SP85T99_2)
SPMJW28.5p.abundance<-PredictGAM(SPMJW2abundance.sig,SP85T99_2)
SPFAW28.5p.abundance<-PredictGAM(SPFAW2abundance.sig,SP85T99_2)
SPMAW28.5p.abundance<-PredictGAM(SPMAW2abundance.sig,SP85T99_2)
###Namechange####
namechange<- function(hindyear, groupp.abundance){
  
  hindyear<-data.frame(hindyear,groupp.abundance)
  names(hindyear)[8]<- "p.abundance"
  return(hindyear)
}
FLFJE28.5<-namechange(FL85T99_2,FLFJE28.5p.abundance)
FLMJE28.5<-namechange(FL85T99_2,FLMJE28.5p.abundance)
FLFAE28.5<-namechange(FL85T99_2,FLFAE28.5p.abundance)
FLMAE28.5<-namechange(FL85T99_2,FLMAE28.5p.abundance)

SPFJE28.5<-namechange(SP85T99_2,SPFJE28.5p.abundance)
SPMJE28.5<-namechange(SP85T99_2,SPMJE28.5p.abundance)
SPFAE28.5<-namechange(SP85T99_2,SPFAE28.5p.abundance)
SPMAE28.5<-namechange(SP85T99_2,SPMAE28.5p.abundance)
###Middle###
FLFJM28.5<-namechange(FL85T99_2,FLFJM28.5p.abundance)
FLMJM28.5<-namechange(FL85T99_2,FLMJM28.5p.abundance)
FLFAM28.5<-namechange(FL85T99_2,FLFAM28.5p.abundance)
FLMAM28.5<-namechange(FL85T99_2,FLMAM28.5p.abundance)

SPFJM28.5<-namechange(SP85T99_2,SPFJM28.5p.abundance)
SPMJM28.5<-namechange(SP85T99_2,SPMJM28.5p.abundance)
SPFAM28.5<-namechange(SP85T99_2,SPFAM28.5p.abundance)
SPMAM28.5<-namechange(SP85T99_2,SPMAM28.5p.abundance)
###West###
FLFJW28.5<-namechange(FL85T99_2,FLFJW28.5p.abundance)
FLMJW28.5<-namechange(FL85T99_2,FLMJW28.5p.abundance)
FLFAW28.5<-namechange(FL85T99_2,FLFAW28.5p.abundance)
FLMAW28.5<-namechange(FL85T99_2,FLMAW28.5p.abundance)

SPFJW28.5<-namechange(SP85T99_2,SPFJW28.5p.abundance)
SPMJW28.5<-namechange(SP85T99_2,SPMJW28.5p.abundance)
SPFAW28.5<-namechange(SP85T99_2,SPFAW28.5p.abundance)
SPMAW28.5<-namechange(SP85T99_2,SPMAW28.5p.abundance)

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
FLFJE28.5<-cleanupE2(FLFJE28.5)
FLMJE28.5<-cleanupE2(FLMJE28.5)
FLFAE28.5<-cleanupE2(FLFAE28.5)
FLMAE28.5<-cleanupE2(FLMAE28.5)

SPFJE28.5<-cleanupE2(SPFJE28.5)
SPMJE28.5<-cleanupE2(SPMJE28.5)
SPFAE28.5<-cleanupE2(SPFAE28.5)
SPMAE28.5<-cleanupE2(SPMAE28.5)
###cleanup Middle####
FLFJM28.5<-cleanupM2(FLFJM28.5)
FLMJM28.5<-cleanupM2(FLMJM28.5)
FLFAM28.5<-cleanupM2(FLFAM28.5)
FLMAM28.5<-cleanupM2(FLMAM28.5)

SPFJM28.5<-cleanupM2(SPFJM28.5)
SPMJM28.5<-cleanupM2(SPMJM28.5)
SPFAM28.5<-cleanupM2(SPFAM28.5)
SPMAM28.5<-cleanupM2(SPMAM28.5)
###Cleanup WEST####
FLFJW28.5<-cleanupW2(FLFJW28.5)
FLMJW28.5<-cleanupW2(FLMJW28.5)
FLFAW28.5<-cleanupW2(FLFAW28.5)
FLMAW28.5<-cleanupW2(FLMAW28.5)

SPFJW28.5<-cleanupW2(SPFJW28.5)
SPMJW28.5<-cleanupW2(SPMJW28.5)
SPFAW28.5<-cleanupW2(SPFAW28.5)
SPMAW28.5<-cleanupW2(SPMAW28.5)

#####combine EW abundance predictions and plot interpolated ^  ####

FLFJew28.5=rbind(FLFJE28.5,FLFJM28.5,FLFJW28.5)
FLMJew28.5=rbind(FLMJE28.5,FLMJM28.5,FLMJW28.5)
FLFAew28.5=rbind(FLFAE28.5,FLFAM28.5,FLFAW28.5)
FLMAew28.5=rbind(FLMAE28.5,FLMAM28.5,FLMAW28.5)
SPFJew28.5=rbind(SPFJE28.5,SPFJM28.5,SPFJW28.5)
SPMJew28.5=rbind(SPMJE28.5,SPMJM28.5,SPMJW28.5)
SPFAew28.5=rbind(SPFAE28.5,SPFAM28.5,SPFAW28.5)
SPMAew28.5=rbind(SPMAE28.5,SPMAM28.5,SPMAW28.5)
#####Creating smoothed interpolated Hindcast###

###Get raster data####
###2099####
FLFJew2RAST8.5 <- list()
coordinates(depth_grid_plot000612) <- ~lon + lat
for(i in 1:1){
  print(i)
  temp_data <- FLFJew28.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFJew2RAST8.5[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMJew2RAST8.5 <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMJew28.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMJew2RAST8.5[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLFAew2RAST8.5 <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFAew28.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFAew2RAST8.5[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMAew2RAST8.5 <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMAew28.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMAew2RAST8.5[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFJew2RAST8.5 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFJew28.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFJew2RAST8.5[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMJew2RAST8.5 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMJew28.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMJew2RAST8.5[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFAew2RAST8.5 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFAew28.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFAew2RAST8.5[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMAew2RAST8.5 <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMAew28.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMAew2RAST8.5[[i]] <- raster::extract(rast, depth_grid_plot000612)
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
    plot(depth_grid_plot000612$lon, depth_grid_plot000612$lat, pch=16, col=colcode, cex=0.1, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, xlab="Longitude", ylab="Latitude")
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

plottemp(FLFJew2RAST8.5,"FLFJ RCP 8.5","2072-2099","J")
plottemp(FLMJew2RAST8.5,"FLMJ RCP 8.5","2072-2099","J")
plottemp(FLFAew2RAST8.5,"FLFA RCP 8.5","2072-2099","A")
plottemp(FLMAew2RAST8.5,"FLMA RCP 8.5","2072-2099","A")
plottemp(SPFJew2RAST8.5,"SPFJ RCP 8.5","2072-2099","J")
plottemp(SPMJew2RAST8.5,"SPMJ RCP 8.5","2072-2099","J")
plottemp(SPFAew2RAST8.5,"SPFA RCP 8.5","2072-2099","A")
plottemp(SPMAew2RAST8.5,"SPMA RCP 8.5","2072-2099","A")


####                                                       ####
####
#####Calculate and plot RELATIVE differences between NSV1 and S predicted abundancies####

###2099####
FLFJ_diff_rast_8.5<-merge(FLFJ8.5,FLFJ8.5ew,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","AvgDepth"))
FLFJ_diff_rast_8.5<- cbind(FLFJ_diff_rast_8.5[1:7],as.data.frame(
  ifelse(((FLFJ_diff_rast_8.5$p.abundance.y>1.00)&(FLFJ_diff_rast_8.5$p.abundance.x>1.00)),
         (((FLFJ_diff_rast_8.5$p.abundance.y-FLFJ_diff_rast_8.5$p.abundance.x)/FLFJ_diff_rast_8.5$p.abundance.x)*100),
         (FLFJ_diff_rast_8.5$p.abundance.y-FLFJ_diff_rast_8.5$p.abundance.x))))
names(FLFJ_diff_rast_8.5)[8]<-"p.abundance"
FLFJ_diff_rast_8.5.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFJ_diff_rast_8.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFJ_diff_rast_8.5.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMJ_diff_rast_8.5<-merge(FLMJ8.5,FLMJ8.5ew,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","AvgDepth"))
FLMJ_diff_rast_8.5<- cbind(FLMJ_diff_rast_8.5[1:7],as.data.frame(
  ifelse(((FLMJ_diff_rast_8.5$p.abundance.y>1.00)&(FLMJ_diff_rast_8.5$p.abundance.x>1.00)),
         (((FLMJ_diff_rast_8.5$p.abundance.y-FLMJ_diff_rast_8.5$p.abundance.x)/FLMJ_diff_rast_8.5$p.abundance.x)*100),
         (FLMJ_diff_rast_8.5$p.abundance.y-FLMJ_diff_rast_8.5$p.abundance.x))))
names(FLMJ_diff_rast_8.5)[8]<-"p.abundance"
FLMJ_diff_rast_8.5.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMJ_diff_rast_8.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMJ_diff_rast_8.5.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLFA_diff_rast_8.5<-merge(FLFA8.5,FLFA8.5ew,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","AvgDepth"))
FLFA_diff_rast_8.5<- cbind(FLFA_diff_rast_8.5[1:7],as.data.frame(
  ifelse(((FLFA_diff_rast_8.5$p.abundance.y>1.00)&(FLFA_diff_rast_8.5$p.abundance.x>1.00)),
         (((FLFA_diff_rast_8.5$p.abundance.y-FLFA_diff_rast_8.5$p.abundance.x)/FLFA_diff_rast_8.5$p.abundance.x)*100),
         (FLFA_diff_rast_8.5$p.abundance.y-FLFA_diff_rast_8.5$p.abundance.x))))
names(FLFA_diff_rast_8.5)[8]<-"p.abundance"
FLFA_diff_rast_8.5.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFA_diff_rast_8.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFA_diff_rast_8.5.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMA_diff_rast_8.5<-merge(FLMA8.5,FLMA8.5ew,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","AvgDepth"))
FLMA_diff_rast_8.5<- cbind(FLMA_diff_rast_8.5[1:7],as.data.frame(
  ifelse(((FLMA_diff_rast_8.5$p.abundance.y>1.00)&(FLMA_diff_rast_8.5$p.abundance.x>1.00)),
         (((FLMA_diff_rast_8.5$p.abundance.y-FLMA_diff_rast_8.5$p.abundance.x)/FLMA_diff_rast_8.5$p.abundance.x)*100),
         (FLMA_diff_rast_8.5$p.abundance.y-FLMA_diff_rast_8.5$p.abundance.x))))
names(FLMA_diff_rast_8.5)[8]<-"p.abundance"
FLMA_diff_rast_8.5.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMA_diff_rast_8.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMA_diff_rast_8.5.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFJ_diff_rast_8.5<-merge(SPFJ8.5,SPFJ8.5ew,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","AvgDepth"))
SPFJ_diff_rast_8.5<- cbind(SPFJ_diff_rast_8.5[1:7],as.data.frame(
  ifelse(((SPFJ_diff_rast_8.5$p.abundance.y>1.00)&(SPFJ_diff_rast_8.5$p.abundance.x>1.00)),
         (((SPFJ_diff_rast_8.5$p.abundance.y-SPFJ_diff_rast_8.5$p.abundance.x)/SPFJ_diff_rast_8.5$p.abundance.x)*100),
         (SPFJ_diff_rast_8.5$p.abundance.y-SPFJ_diff_rast_8.5$p.abundance.x))))
names(SPFJ_diff_rast_8.5)[8]<-"p.abundance"
SPFJ_diff_rast_8.5.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFJ_diff_rast_8.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFJ_diff_rast_8.5.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMJ_diff_rast_8.5<-merge(SPMJ8.5,SPMJ8.5ew,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","AvgDepth"))
SPMJ_diff_rast_8.5<- cbind(SPMJ_diff_rast_8.5[1:7],as.data.frame(
  ifelse(((SPMJ_diff_rast_8.5$p.abundance.y>1.00)&(SPMJ_diff_rast_8.5$p.abundance.x>1.00)),
         (((SPMJ_diff_rast_8.5$p.abundance.y-SPMJ_diff_rast_8.5$p.abundance.x)/SPMJ_diff_rast_8.5$p.abundance.x)*100),
         (SPMJ_diff_rast_8.5$p.abundance.y-SPMJ_diff_rast_8.5$p.abundance.x))))
names(SPMJ_diff_rast_8.5)[8]<-"p.abundance"
SPMJ_diff_rast_8.5.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMJ_diff_rast_8.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMJ_diff_rast_8.5.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFA_diff_rast_8.5<-merge(SPFA8.5,SPFA8.5ew,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","AvgDepth"))
SPFA_diff_rast_8.5<- cbind(SPFA_diff_rast_8.5[1:7],as.data.frame(
  ifelse(((SPFA_diff_rast_8.5$p.abundance.y>1.00)&(SPFA_diff_rast_8.5$p.abundance.x>1.00)),
         (((SPFA_diff_rast_8.5$p.abundance.y-SPFA_diff_rast_8.5$p.abundance.x)/SPFA_diff_rast_8.5$p.abundance.x)*100),
         (SPFA_diff_rast_8.5$p.abundance.y-SPFA_diff_rast_8.5$p.abundance.x))))
names(SPFA_diff_rast_8.5)[8]<-"p.abundance"
SPFA_diff_rast_8.5.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFA_diff_rast_8.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFA_diff_rast_8.5.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMA_diff_rast_8.5<-merge(SPMA8.5,SPMA8.5ew,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","AvgDepth"))
SPMA_diff_rast_8.5<- cbind(SPMA_diff_rast_8.5[1:7],as.data.frame(
  ifelse(((SPMA_diff_rast_8.5$p.abundance.y>1.00)&(SPMA_diff_rast_8.5$p.abundance.x>1.00)),
         (((SPMA_diff_rast_8.5$p.abundance.y-SPMA_diff_rast_8.5$p.abundance.x)/SPMA_diff_rast_8.5$p.abundance.x)*100),
         (SPMA_diff_rast_8.5$p.abundance.y-SPMA_diff_rast_8.5$p.abundance.x))))
names(SPMA_diff_rast_8.5)[8]<-"p.abundance"
SPMA_diff_rast_8.5.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMA_diff_rast_8.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMA_diff_rast_8.5.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
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
    
    
    colcode <- findColours(class, plotclr)
    
    start_x <- range(GAM_data$Longitude)[1]
    end_x <- range(GAM_data$Longitude)[2]
    start_y <- range(GAM_data$Latitude)[1]
    end_y <- range(GAM_data$Latitude)[2]
    plot(depth_grid_plot$lon, depth_grid_plot$lat, pch=16, col=colcode, cex=0.1, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, xlab="Longitude", ylab="Latitude")
    map(database= mainecoast, ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE, lwd=0.1)
    #plot(sa511_513, add=T)
    box()
    legend("topleft", paste("RCP 8.5","-", year, "-", month, sep=""), bty="n", cex=1.5)
    legend("bottomright", legend=c(names(attr(colcode, "table"))), fill=c(attr(colcode, "palette")), cex=0.9,bg="white",
           bty = "o", title="Relative Difference %")
    axis(1, cex=0.5)
    axis(2, cex=0.5)
  }
  #dev.off()
}

plotreldiff(FLFJ_diff_rast_8.5.list,"FLFJ","2072-2099","J",depth_grid_plot000612)
plotreldiff(FLMJ_diff_rast_8.5.list,"FLMJ","2072-2099","J",depth_grid_plot000612)
plotreldiff(FLFA_diff_rast_8.5.list,"FLFA","2072-2099","A",depth_grid_plot000612)
plotreldiff(FLMA_diff_rast_8.5.list,"FLMA","2072-2099","A",depth_grid_plot000612)
plotreldiff(SPFJ_diff_rast_8.5.list,"SPFJ","2072-2099","J",depth_grid_plot000612)
plotreldiff(SPMJ_diff_rast_8.5.list,"SPMJ","2072-2099","J",depth_grid_plot000612)
plotreldiff(SPFA_diff_rast_8.5.list,"SPFA","2072-2099","A",depth_grid_plot000612)
plotreldiff(SPMA_diff_rast_8.5.list,"SPMA","2072-2099","A",depth_grid_plot000612)
####Min Max#### 
library(huxtable)

table1reldiff8.5 <- hux(
  Group = c('FLFJ', 'FLMJ', 'FLFA','FLMA','SPFJ', 'SPMJ', 'SPFA','SPMA'),
  Max = c(max(na.omit(as.data.frame(FLFJ_diff_rast_8.5.list[[1]]))), max(na.omit(as.data.frame(FLMJ_diff_rast_8.5.list[[1]]))), 
          max(na.omit(as.data.frame(FLFA_diff_rast_8.5.list[[1]]))),
          max(na.omit(as.data.frame(FLMA_diff_rast_8.5.list[[1]]))),max(na.omit(as.data.frame(SPFJ_diff_rast_8.5.list[[1]]))), 
          max(na.omit(as.data.frame(SPMJ_diff_rast_8.5.list[[1]]))), max(na.omit(as.data.frame(SPFA_diff_rast_8.5.list[[1]]))),
          max(na.omit(as.data.frame(SPMA_diff_rast_8.5.list[[1]])))),
  Min = c(min(na.omit(as.data.frame(FLFJ_diff_rast_8.5.list[[1]]))), min(na.omit(as.data.frame(FLMJ_diff_rast_8.5.list[[1]]))), 
          min(na.omit(as.data.frame(FLFA_diff_rast_8.5.list[[1]]))),
          min(na.omit(as.data.frame(FLMA_diff_rast_8.5.list[[1]]))),min(na.omit(as.data.frame(SPFJ_diff_rast_8.5.list[[1]]))), 
          min(na.omit(as.data.frame(SPMJ_diff_rast_8.5.list[[1]]))), min(na.omit(as.data.frame(SPFA_diff_rast_8.5.list[[1]]))),
          min(na.omit(as.data.frame(SPMA_diff_rast_8.5.list[[1]])))),
  Mean = c(mean(FLFJ_diff_rast_8.5.list[[1]],na.rm=TRUE), mean(FLMJ_diff_rast_8.5.list[[1]],na.rm=TRUE), 
           mean(FLFA_diff_rast_8.5.list[[1]],na.rm=TRUE),
           mean(FLMA_diff_rast_8.5.list[[1]],na.rm=TRUE),mean(SPFJ_diff_rast_8.5.list[[1]],na.rm=TRUE), 
           mean(SPMJ_diff_rast_8.5.list[[1]],na.rm=TRUE), mean(SPFA_diff_rast_8.5.list[[1]],na.rm=TRUE),
           mean(SPMA_diff_rast_8.5.list[[1]],na.rm=TRUE)),
  Med = c(median(FLFJ_diff_rast_8.5.list[[1]],na.rm=TRUE), median(FLMJ_diff_rast_8.5.list[[1]],na.rm=TRUE), 
          median(FLFA_diff_rast_8.5.list[[1]],na.rm=TRUE),
          median(FLMA_diff_rast_8.5.list[[1]],na.rm=TRUE),median(SPFJ_diff_rast_8.5.list[[1]],na.rm=TRUE), 
          median(SPMJ_diff_rast_8.5.list[[1]],na.rm=TRUE), median(SPFA_diff_rast_8.5.list[[1]],na.rm=TRUE),
          median(SPMA_diff_rast_8.5.list[[1]],na.rm=TRUE)),add_colnames = TRUE)

bold(table1reldiff8.5)[1,]           <- TRUE
bottom_border(table1reldiff8.5)[1,]  <- 0.4
align(table1reldiff8.5)[,2]          <- 'right'
right_padding(table1reldiff8.5)      <- 10
left_padding(table1reldiff8.5)       <- 10
width(table1reldiff8.5)              <- 0.30
number_format(table1reldiff8.5)      <- 2

table1reldiff8.5

#####Calculate and plot REGULAR differences between NSV1 and S predicted abundancies####

###2099#####
FLFJ_REGdiff_rast_8.5<-merge(FLFJ8.5,FLFJ8.5ew,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","AvgDepth"))
FLFJ_REGdiff_rast_8.5<- cbind(FLFJ_REGdiff_rast_8.5[1:7],as.data.frame((FLFJ_REGdiff_rast_8.5$p.abundance.y-FLFJ_REGdiff_rast_8.5$p.abundance.x)))
names(FLFJ_REGdiff_rast_8.5)[8]<-"p.abundance"
FLFJ_REGdiff_rast_8.5.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFJ_REGdiff_rast_8.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFJ_REGdiff_rast_8.5.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMJ_REGdiff_rast_8.5<-merge(FLMJ8.5,FLMJ8.5ew,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","AvgDepth"))
FLMJ_REGdiff_rast_8.5<- cbind(FLMJ_REGdiff_rast_8.5[1:7],as.data.frame((FLMJ_REGdiff_rast_8.5$p.abundance.y-FLMJ_REGdiff_rast_8.5$p.abundance.x)))
names(FLMJ_REGdiff_rast_8.5)[8]<-"p.abundance"
FLMJ_REGdiff_rast_8.5.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMJ_REGdiff_rast_8.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMJ_REGdiff_rast_8.5.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLFA_REGdiff_rast_8.5<-merge(FLFA8.5,FLFA8.5ew,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","AvgDepth"))
FLFA_REGdiff_rast_8.5<- cbind(FLFA_REGdiff_rast_8.5[1:7],as.data.frame((FLFA_REGdiff_rast_8.5$p.abundance.y-FLFA_REGdiff_rast_8.5$p.abundance.x)))
names(FLFA_REGdiff_rast_8.5)[8]<-"p.abundance"
FLFA_REGdiff_rast_8.5.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFA_REGdiff_rast_8.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFA_REGdiff_rast_8.5.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMA_REGdiff_rast_8.5<-merge(FLMA8.5,FLMA8.5ew,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","AvgDepth"))
FLMA_REGdiff_rast_8.5<- cbind(FLMA_REGdiff_rast_8.5[1:7],as.data.frame((FLMA_REGdiff_rast_8.5$p.abundance.y-FLMA_REGdiff_rast_8.5$p.abundance.x)))
names(FLMA_REGdiff_rast_8.5)[8]<-"p.abundance"
FLMA_REGdiff_rast_8.5.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMA_REGdiff_rast_8.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMA_REGdiff_rast_8.5.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFJ_REGdiff_rast_8.5<-merge(SPFJ8.5,SPFJ8.5ew,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","AvgDepth"))
SPFJ_REGdiff_rast_8.5<- cbind(SPFJ_REGdiff_rast_8.5[1:7],as.data.frame((SPFJ_REGdiff_rast_8.5$p.abundance.y-SPFJ_REGdiff_rast_8.5$p.abundance.x)))
names(SPFJ_REGdiff_rast_8.5)[8]<-"p.abundance"
SPFJ_REGdiff_rast_8.5.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFJ_REGdiff_rast_8.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFJ_REGdiff_rast_8.5.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMJ_REGdiff_rast_8.5<-merge(SPMJ8.5,SPMJ8.5ew,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","AvgDepth"))
SPMJ_REGdiff_rast_8.5<- cbind(SPMJ_REGdiff_rast_8.5[1:7],as.data.frame((SPMJ_REGdiff_rast_8.5$p.abundance.y-SPMJ_REGdiff_rast_8.5$p.abundance.x)))
names(SPMJ_REGdiff_rast_8.5)[8]<-"p.abundance"
SPMJ_REGdiff_rast_8.5.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMJ_REGdiff_rast_8.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMJ_REGdiff_rast_8.5.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFA_REGdiff_rast_8.5<-merge(SPFA8.5,SPFA8.5ew,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","AvgDepth"))
SPFA_REGdiff_rast_8.5<- cbind(SPFA_REGdiff_rast_8.5[1:7],as.data.frame((SPFA_REGdiff_rast_8.5$p.abundance.y-SPFA_REGdiff_rast_8.5$p.abundance.x)))
names(SPFA_REGdiff_rast_8.5)[8]<-"p.abundance"
SPFA_REGdiff_rast_8.5.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFA_REGdiff_rast_8.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFA_REGdiff_rast_8.5.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMA_REGdiff_rast_8.5<-merge(SPMA8.5,SPMA8.5ew,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","AvgDepth"))
SPMA_REGdiff_rast_8.5<- cbind(SPMA_REGdiff_rast_8.5[1:7],as.data.frame((SPMA_REGdiff_rast_8.5$p.abundance.y-SPMA_REGdiff_rast_8.5$p.abundance.x)))
names(SPMA_REGdiff_rast_8.5)[8]<-"p.abundance"
SPMA_REGdiff_rast_8.5.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMA_REGdiff_rast_8.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMA_REGdiff_rast_8.5.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
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

plotdiff(FLFJ_REGdiff_rast_8.5.list,"RCP 8.5 FLFJ","2072-2099","J",depth_grid_plot000612)
plotdiff(FLMJ_REGdiff_rast_8.5.list,"RCP 8.5 FLMJ","2072-2099","J",depth_grid_plot000612)
plotdiff(FLFA_REGdiff_rast_8.5.list,"RCP 8.5 FLFA","2072-2099","A",depth_grid_plot000612)
plotdiff(FLMA_REGdiff_rast_8.5.list,"RCP 8.5 FLMA","2072-2099","A",depth_grid_plot000612)
plotdiff(SPFJ_REGdiff_rast_8.5.list,"RCP 8.5 SPFJ","2072-2099","J",depth_grid_plot000612)
plotdiff(SPMJ_REGdiff_rast_8.5.list,"RCP 8.5 SPMJ","2072-2099","J",depth_grid_plot000612)
plotdiff(SPFA_REGdiff_rast_8.5.list,"RCP 8.5 SPFA","2072-2099","A",depth_grid_plot000612)
plotdiff(SPMA_REGdiff_rast_8.5.list,"RCP 8.5 SPMA","2072-2099","A",depth_grid_plot000612)   


####Min Max#### 
library(huxtable)
library(stats)

table1regdiff8.5 <- hux(
  Group = c('FLFJ', 'FLMJ', 'FLFA','FLMA','SPFJ', 'SPMJ', 'SPFA','SPMA'),
  Max = c(max(na.omit(as.data.frame(FLFJ_REGdiff_rast_8.5.list[[1]]))), max(na.omit(as.data.frame(FLMJ_REGdiff_rast_8.5.list[[1]]))), 
          max(na.omit(as.data.frame(FLFA_REGdiff_rast_8.5.list[[1]]))),
          max(na.omit(as.data.frame(FLMA_REGdiff_rast_8.5.list[[1]]))),max(na.omit(as.data.frame(SPFJ_REGdiff_rast_8.5.list[[1]]))), 
          max(na.omit(as.data.frame(SPMJ_REGdiff_rast_8.5.list[[1]]))), max(na.omit(as.data.frame(SPFA_REGdiff_rast_8.5.list[[1]]))),
          max(na.omit(as.data.frame(SPMA_REGdiff_rast_8.5.list[[1]])))),
  Min = c(min(na.omit(as.data.frame(FLFJ_REGdiff_rast_8.5.list[[1]]))), min(na.omit(as.data.frame(FLMJ_REGdiff_rast_8.5.list[[1]]))), 
          min(na.omit(as.data.frame(FLFA_REGdiff_rast_8.5.list[[1]]))),
          min(na.omit(as.data.frame(FLMA_REGdiff_rast_8.5.list[[1]]))),min(na.omit(as.data.frame(SPFJ_REGdiff_rast_8.5.list[[1]]))), 
          min(na.omit(as.data.frame(SPMJ_REGdiff_rast_8.5.list[[1]]))), min(na.omit(as.data.frame(SPFA_REGdiff_rast_8.5.list[[1]]))),
          min(na.omit(as.data.frame(SPMA_REGdiff_rast_8.5.list[[1]])))),
  Mean = c(mean(FLFJ_REGdiff_rast_8.5.list[[1]],na.rm=TRUE), mean(FLMJ_REGdiff_rast_8.5.list[[1]],na.rm=TRUE), 
           mean(FLFA_REGdiff_rast_8.5.list[[1]],na.rm=TRUE),
           mean(FLMA_REGdiff_rast_8.5.list[[1]],na.rm=TRUE),mean(SPFJ_REGdiff_rast_8.5.list[[1]],na.rm=TRUE), 
           mean(SPMJ_REGdiff_rast_8.5.list[[1]],na.rm=TRUE), mean(SPFA_REGdiff_rast_8.5.list[[1]],na.rm=TRUE),
           mean(SPMA_REGdiff_rast_8.5.list[[1]],na.rm=TRUE)),
  Med = c(median(FLFJ_REGdiff_rast_8.5.list[[1]],na.rm=TRUE), median(FLMJ_REGdiff_rast_8.5.list[[1]],na.rm=TRUE), 
          median(FLFA_REGdiff_rast_8.5.list[[1]],na.rm=TRUE),
          median(FLMA_REGdiff_rast_8.5.list[[1]],na.rm=TRUE),median(SPFJ_REGdiff_rast_8.5.list[[1]],na.rm=TRUE), 
          median(SPMJ_REGdiff_rast_8.5.list[[1]],na.rm=TRUE), median(SPFA_REGdiff_rast_8.5.list[[1]],na.rm=TRUE),
          median(SPMA_REGdiff_rast_8.5.list[[1]],na.rm=TRUE)),add_colnames = TRUE)

bold(table1regdiff8.5)[1,]           <- TRUE
bottom_border(table1regdiff8.5)[1,]  <- 0.4
align(table1regdiff8.5)[,2]          <- 'right'
right_padding(table1regdiff8.5)      <- 10
left_padding(table1regdiff8.5)       <- 10
width(table1regdiff8.5)              <- 0.30
number_format(table1regdiff8.5)      <- 2

table1regdiff8.5
###                                                             #####
#####Calculate and plot RELATIVE differences between NSV2 and S predicted abundancies####

###2099####
FLFJ2_diff_rast_8.5<-merge(FLFJ8.5,FLFJew28.5,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","AvgDepth"))
FLFJ2_diff_rast_8.5<- cbind(FLFJ2_diff_rast_8.5[1:7],as.data.frame(
  ifelse(((FLFJ2_diff_rast_8.5$p.abundance.y>1.00)&(FLFJ2_diff_rast_8.5$p.abundance.x>1.00)),
         (((FLFJ2_diff_rast_8.5$p.abundance.y-FLFJ2_diff_rast_8.5$p.abundance.x)/FLFJ2_diff_rast_8.5$p.abundance.x)*100),
         (FLFJ2_diff_rast_8.5$p.abundance.y-FLFJ2_diff_rast_8.5$p.abundance.x))))
names(FLFJ2_diff_rast_8.5)[8]<-"p.abundance"
FLFJ2_diff_rast_8.5.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFJ2_diff_rast_8.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFJ2_diff_rast_8.5.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMJ2_diff_rast_8.5<-merge(FLMJ8.5,FLMJew28.5,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","AvgDepth"))
FLMJ2_diff_rast_8.5<- cbind(FLMJ2_diff_rast_8.5[1:7],as.data.frame(
  ifelse(((FLMJ2_diff_rast_8.5$p.abundance.y>1.00)&(FLMJ2_diff_rast_8.5$p.abundance.x>1.00)),
         (((FLMJ2_diff_rast_8.5$p.abundance.y-FLMJ2_diff_rast_8.5$p.abundance.x)/FLMJ2_diff_rast_8.5$p.abundance.x)*100),
         (FLMJ2_diff_rast_8.5$p.abundance.y-FLMJ2_diff_rast_8.5$p.abundance.x))))
names(FLMJ2_diff_rast_8.5)[8]<-"p.abundance"
FLMJ2_diff_rast_8.5.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMJ2_diff_rast_8.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMJ2_diff_rast_8.5.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLFA2_diff_rast_8.5<-merge(FLFA8.5,FLFAew28.5,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","AvgDepth"))
FLFA2_diff_rast_8.5<- cbind(FLFA2_diff_rast_8.5[1:7],as.data.frame(
  ifelse(((FLFA2_diff_rast_8.5$p.abundance.y>1.00)&(FLFA2_diff_rast_8.5$p.abundance.x>1.00)),
         (((FLFA2_diff_rast_8.5$p.abundance.y-FLFA2_diff_rast_8.5$p.abundance.x)/FLFA2_diff_rast_8.5$p.abundance.x)*100),
         (FLFA2_diff_rast_8.5$p.abundance.y-FLFA2_diff_rast_8.5$p.abundance.x))))
names(FLFA2_diff_rast_8.5)[8]<-"p.abundance"
FLFA2_diff_rast_8.5.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFA2_diff_rast_8.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFA2_diff_rast_8.5.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMA2_diff_rast_8.5<-merge(FLMA8.5,FLMAew28.5,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","AvgDepth"))
FLMA2_diff_rast_8.5<- cbind(FLMA2_diff_rast_8.5[1:7],as.data.frame(
  ifelse(((FLMA2_diff_rast_8.5$p.abundance.y>1.00)&(FLMA2_diff_rast_8.5$p.abundance.x>1.00)),
         (((FLMA2_diff_rast_8.5$p.abundance.y-FLMA2_diff_rast_8.5$p.abundance.x)/FLMA2_diff_rast_8.5$p.abundance.x)*100),
         (FLMA2_diff_rast_8.5$p.abundance.y-FLMA2_diff_rast_8.5$p.abundance.x))))
names(FLMA2_diff_rast_8.5)[8]<-"p.abundance"
FLMA2_diff_rast_8.5.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMA2_diff_rast_8.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMA2_diff_rast_8.5.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFJ2_diff_rast_8.5<-merge(SPFJ8.5,SPFJew28.5,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","AvgDepth"))
SPFJ2_diff_rast_8.5<- cbind(SPFJ2_diff_rast_8.5[1:7],as.data.frame(
  ifelse(((SPFJ2_diff_rast_8.5$p.abundance.y>1.00)&(SPFJ2_diff_rast_8.5$p.abundance.x>1.00)),
         (((SPFJ2_diff_rast_8.5$p.abundance.y-SPFJ2_diff_rast_8.5$p.abundance.x)/SPFJ2_diff_rast_8.5$p.abundance.x)*100),
         (SPFJ2_diff_rast_8.5$p.abundance.y-SPFJ2_diff_rast_8.5$p.abundance.x))))
names(SPFJ2_diff_rast_8.5)[8]<-"p.abundance"
SPFJ2_diff_rast_8.5.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFJ2_diff_rast_8.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFJ2_diff_rast_8.5.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMJ2_diff_rast_8.5<-merge(SPMJ8.5,SPMJew28.5,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","AvgDepth"))
SPMJ2_diff_rast_8.5<- cbind(SPMJ2_diff_rast_8.5[1:7],as.data.frame(
  ifelse(((SPMJ2_diff_rast_8.5$p.abundance.y>1.00)&(SPMJ2_diff_rast_8.5$p.abundance.x>1.00)),
         (((SPMJ2_diff_rast_8.5$p.abundance.y-SPMJ2_diff_rast_8.5$p.abundance.x)/SPMJ2_diff_rast_8.5$p.abundance.x)*100),
         (SPMJ2_diff_rast_8.5$p.abundance.y-SPMJ2_diff_rast_8.5$p.abundance.x))))
names(SPMJ2_diff_rast_8.5)[8]<-"p.abundance"
SPMJ2_diff_rast_8.5.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMJ2_diff_rast_8.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMJ2_diff_rast_8.5.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFA2_diff_rast_8.5<-merge(SPFA8.5,SPFAew28.5,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","AvgDepth"))
SPFA2_diff_rast_8.5<- cbind(SPFA2_diff_rast_8.5[1:7],as.data.frame(
  ifelse(((SPFA2_diff_rast_8.5$p.abundance.y>1.00)&(SPFA2_diff_rast_8.5$p.abundance.x>1.00)),
         (((SPFA2_diff_rast_8.5$p.abundance.y-SPFA2_diff_rast_8.5$p.abundance.x)/SPFA2_diff_rast_8.5$p.abundance.x)*100),
         (SPFA2_diff_rast_8.5$p.abundance.y-SPFA2_diff_rast_8.5$p.abundance.x))))
names(SPFA2_diff_rast_8.5)[8]<-"p.abundance"
SPFA2_diff_rast_8.5.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFA2_diff_rast_8.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFA2_diff_rast_8.5.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMA2_diff_rast_8.5<-merge(SPMA8.5,SPMAew28.5,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","AvgDepth"))
SPMA2_diff_rast_8.5<- cbind(SPMA2_diff_rast_8.5[1:7],as.data.frame(
  ifelse(((SPMA2_diff_rast_8.5$p.abundance.y>1.00)&(SPMA2_diff_rast_8.5$p.abundance.x>1.00)),
         (((SPMA2_diff_rast_8.5$p.abundance.y-SPMA2_diff_rast_8.5$p.abundance.x)/SPMA2_diff_rast_8.5$p.abundance.x)*100),
         (SPMA2_diff_rast_8.5$p.abundance.y-SPMA2_diff_rast_8.5$p.abundance.x))))
names(SPMA2_diff_rast_8.5)[8]<-"p.abundance"
SPMA2_diff_rast_8.5.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMA2_diff_rast_8.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMA2_diff_rast_8.5.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
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

plotreldiff(FLFJ2_diff_rast_8.5.list,"RCP 8.5 FLFJ","2072-2099","J",depth_grid_plot000612)
plotreldiff(FLMJ2_diff_rast_8.5.list,"RCP 8.5 FLMJ","2072-2099","J",depth_grid_plot000612)
plotreldiff(FLFA2_diff_rast_8.5.list,"RCP 8.5 FLFA","2072-2099","A",depth_grid_plot000612)
plotreldiff(FLMA2_diff_rast_8.5.list,"RCP 8.5 FLMA","2072-2099","A",depth_grid_plot000612)
plotreldiff(SPFJ2_diff_rast_8.5.list,"RCP 8.5 SPFJ","2072-2099","J",depth_grid_plot000612)
plotreldiff(SPMJ2_diff_rast_8.5.list,"RCP 8.5 SPMJ","2072-2099","J",depth_grid_plot000612)
plotreldiff(SPFA2_diff_rast_8.5.list,"RCP 8.5 SPFA","2072-2099","A",depth_grid_plot000612)
plotreldiff(SPMA2_diff_rast_8.5.list,"RCP 8.5 SPMA","2072-2099","A",depth_grid_plot000612)

####Min Max#### 
library(huxtable)

table1reldiff8.5 <- hux(
  Group = c('FLFJ', 'FLMJ', 'FLFA','FLMA','SPFJ', 'SPMJ', 'SPFA','SPMA'),
  Max = c(max(na.omit(as.data.frame(FLFJ2_diff_rast_8.5.list[[1]]))), max(na.omit(as.data.frame(FLMJ2_diff_rast_8.5.list[[1]]))), 
          max(na.omit(as.data.frame(FLFA2_diff_rast_8.5.list[[1]]))),
          max(na.omit(as.data.frame(FLMA2_diff_rast_8.5.list[[1]]))),max(na.omit(as.data.frame(SPFJ2_diff_rast_8.5.list[[1]]))), 
          max(na.omit(as.data.frame(SPMJ2_diff_rast_8.5.list[[1]]))), max(na.omit(as.data.frame(SPFA2_diff_rast_8.5.list[[1]]))),
          max(na.omit(as.data.frame(SPMA2_diff_rast_8.5.list[[1]])))),
  Min = c(min(na.omit(as.data.frame(FLFJ2_diff_rast_8.5.list[[1]]))), min(na.omit(as.data.frame(FLMJ2_diff_rast_8.5.list[[1]]))), 
          min(na.omit(as.data.frame(FLFA2_diff_rast_8.5.list[[1]]))),
          min(na.omit(as.data.frame(FLMA2_diff_rast_8.5.list[[1]]))),min(na.omit(as.data.frame(SPFJ2_diff_rast_8.5.list[[1]]))), 
          min(na.omit(as.data.frame(SPMJ2_diff_rast_8.5.list[[1]]))), min(na.omit(as.data.frame(SPFA2_diff_rast_8.5.list[[1]]))),
          min(na.omit(as.data.frame(SPMA2_diff_rast_8.5.list[[1]])))),
  Mean = c(mean(FLFJ2_diff_rast_8.5.list[[1]],na.rm=TRUE), mean(FLMJ2_diff_rast_8.5.list[[1]],na.rm=TRUE), 
           mean(FLFA2_diff_rast_8.5.list[[1]],na.rm=TRUE),
           mean(FLMA2_diff_rast_8.5.list[[1]],na.rm=TRUE),mean(SPFJ2_diff_rast_8.5.list[[1]],na.rm=TRUE), 
           mean(SPMJ2_diff_rast_8.5.list[[1]],na.rm=TRUE), mean(SPFA2_diff_rast_8.5.list[[1]],na.rm=TRUE),
           mean(SPMA2_diff_rast_8.5.list[[1]],na.rm=TRUE)),
  Med = c(median(FLFJ2_diff_rast_8.5.list[[1]],na.rm=TRUE), median(FLMJ2_diff_rast_8.5.list[[1]],na.rm=TRUE), 
          median(FLFA2_diff_rast_8.5.list[[1]],na.rm=TRUE),
          median(FLMA2_diff_rast_8.5.list[[1]],na.rm=TRUE),median(SPFJ2_diff_rast_8.5.list[[1]],na.rm=TRUE), 
          median(SPMJ2_diff_rast_8.5.list[[1]],na.rm=TRUE), median(SPFA2_diff_rast_8.5.list[[1]],na.rm=TRUE),
          median(SPMA2_diff_rast_8.5.list[[1]],na.rm=TRUE)),add_colnames = TRUE)

bold(table1reldiff8.5)[1,]           <- TRUE
bottom_border(table1reldiff8.5)[1,]  <- 0.4
align(table1reldiff8.5)[,2]          <- 'right'
right_padding(table1reldiff8.5)      <- 10
left_padding(table1reldiff8.5)       <- 10
width(table1reldiff8.5)              <- 0.30
number_format(table1reldiff8.5)      <- 2

table1reldiff8.5

#####Calculate and plot REGULAR differences between NSV2 and S predicted abundancies####

###2099#####
FLFJ2_REGdiffdiff_rast_8.5<-merge(FLFJ8.5,FLFJew28.5,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","AvgDepth"))
FLFJ2_REGdiffdiff_rast_8.5<- cbind(FLFJ2_REGdiffdiff_rast_8.5[1:7],as.data.frame((FLFJ2_REGdiffdiff_rast_8.5$p.abundance.y-FLFJ2_REGdiffdiff_rast_8.5$p.abundance.x)))
names(FLFJ2_REGdiffdiff_rast_8.5)[8]<-"p.abundance"
FLFJ2_REGdiffdiff_rast_8.5.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFJ2_REGdiffdiff_rast_8.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFJ2_REGdiffdiff_rast_8.5.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMJ2_REGdiffdiff_rast_8.5<-merge(FLMJ8.5,FLMJew28.5,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","AvgDepth"))
FLMJ2_REGdiffdiff_rast_8.5<- cbind(FLMJ2_REGdiffdiff_rast_8.5[1:7],as.data.frame((FLMJ2_REGdiffdiff_rast_8.5$p.abundance.y-FLMJ2_REGdiffdiff_rast_8.5$p.abundance.x)))
names(FLMJ2_REGdiffdiff_rast_8.5)[8]<-"p.abundance"
FLMJ2_REGdiffdiff_rast_8.5.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMJ2_REGdiffdiff_rast_8.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMJ2_REGdiffdiff_rast_8.5.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLFA2_REGdiffdiff_rast_8.5<-merge(FLFA8.5,FLFAew28.5,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","AvgDepth"))
FLFA2_REGdiffdiff_rast_8.5<- cbind(FLFA2_REGdiffdiff_rast_8.5[1:7],as.data.frame((FLFA2_REGdiffdiff_rast_8.5$p.abundance.y-FLFA2_REGdiffdiff_rast_8.5$p.abundance.x)))
names(FLFA2_REGdiffdiff_rast_8.5)[8]<-"p.abundance"
FLFA2_REGdiffdiff_rast_8.5.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLFA2_REGdiffdiff_rast_8.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLFA2_REGdiffdiff_rast_8.5.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

FLMA2_REGdiffdiff_rast_8.5<-merge(FLMA8.5,FLMAew28.5,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","AvgDepth"))
FLMA2_REGdiffdiff_rast_8.5<- cbind(FLMA2_REGdiffdiff_rast_8.5[1:7],as.data.frame((FLMA2_REGdiffdiff_rast_8.5$p.abundance.y-FLMA2_REGdiffdiff_rast_8.5$p.abundance.x)))
names(FLMA2_REGdiffdiff_rast_8.5)[8]<-"p.abundance"
FLMA2_REGdiffdiff_rast_8.5.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- FLMA2_REGdiffdiff_rast_8.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FLMA2_REGdiffdiff_rast_8.5.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFJ2_REGdiffdiff_rast_8.5<-merge(SPFJ8.5,SPFJew28.5,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","AvgDepth"))
SPFJ2_REGdiffdiff_rast_8.5<- cbind(SPFJ2_REGdiffdiff_rast_8.5[1:7],as.data.frame((SPFJ2_REGdiffdiff_rast_8.5$p.abundance.y-SPFJ2_REGdiffdiff_rast_8.5$p.abundance.x)))
names(SPFJ2_REGdiffdiff_rast_8.5)[8]<-"p.abundance"
SPFJ2_REGdiffdiff_rast_8.5.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFJ2_REGdiffdiff_rast_8.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFJ2_REGdiffdiff_rast_8.5.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMJ2_REGdiffdiff_rast_8.5<-merge(SPMJ8.5,SPMJew28.5,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","AvgDepth"))
SPMJ2_REGdiffdiff_rast_8.5<- cbind(SPMJ2_REGdiffdiff_rast_8.5[1:7],as.data.frame((SPMJ2_REGdiffdiff_rast_8.5$p.abundance.y-SPMJ2_REGdiffdiff_rast_8.5$p.abundance.x)))
names(SPMJ2_REGdiffdiff_rast_8.5)[8]<-"p.abundance"
SPMJ2_REGdiffdiff_rast_8.5.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMJ2_REGdiffdiff_rast_8.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMJ2_REGdiffdiff_rast_8.5.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPFA2_REGdiffdiff_rast_8.5<-merge(SPFA8.5,SPFAew28.5,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","AvgDepth"))
SPFA2_REGdiffdiff_rast_8.5<- cbind(SPFA2_REGdiffdiff_rast_8.5[1:7],as.data.frame((SPFA2_REGdiffdiff_rast_8.5$p.abundance.y-SPFA2_REGdiffdiff_rast_8.5$p.abundance.x)))
names(SPFA2_REGdiffdiff_rast_8.5)[8]<-"p.abundance"
SPFA2_REGdiffdiff_rast_8.5.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPFA2_REGdiffdiff_rast_8.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPFA2_REGdiffdiff_rast_8.5.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
}

SPMA2_REGdiffdiff_rast_8.5<-merge(SPMA8.5,SPMAew28.5,by=c("Longitude","Latitude","Bottom_WaterTempDegC","Bottom_Salinity_psu","dist_frm_shore","sediment","AvgDepth"))
SPMA2_REGdiffdiff_rast_8.5<- cbind(SPMA2_REGdiffdiff_rast_8.5[1:7],as.data.frame((SPMA2_REGdiffdiff_rast_8.5$p.abundance.y-SPMA2_REGdiffdiff_rast_8.5$p.abundance.x)))
names(SPMA2_REGdiffdiff_rast_8.5)[8]<-"p.abundance"
SPMA2_REGdiffdiff_rast_8.5.list <- list()
for(i in 1:1){
  print(i)
  temp_data <- SPMA2_REGdiffdiff_rast_8.5
  rast_col <- ceiling((range(temp_data$Longitude)[2]-range(temp_data$Longitude)[1])/0.01)
  rast_row <- ceiling((range(temp_data$Latitude)[2]-range(temp_data$Latitude)[1])/0.01)
  akima.smooth <- with(temp_data, interp(Longitude,Latitude, p.abundance, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SPMA2_REGdiffdiff_rast_8.5.list[[i]] <- raster::extract(rast, depth_grid_plot000612)
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

plotdiff(FLFJ2_REGdiffdiff_rast_8.5.list,"FLFJ RPC 8.5","2072-2099","J",depth_grid_plot000612)
plotdiff(FLMJ2_REGdiffdiff_rast_8.5.list,"FLMJ RPC 8.5","2072-2099","J",depth_grid_plot000612)
plotdiff(FLFA2_REGdiffdiff_rast_8.5.list,"FLFA RPC 8.5","2072-2099","A",depth_grid_plot000612)
plotdiff(FLMA2_REGdiffdiff_rast_8.5.list,"FLMA RPC 8.5","2072-2099","A",depth_grid_plot000612)
plotdiff(SPFJ2_REGdiffdiff_rast_8.5.list,"SPFJ RPC 8.5","2072-2099","J",depth_grid_plot000612)
plotdiff(SPMJ2_REGdiffdiff_rast_8.5.list,"SPMJ RPC 8.5","2072-2099","J",depth_grid_plot000612)
plotdiff(SPFA2_REGdiffdiff_rast_8.5.list,"SPFA RPC 8.5","2072-2099","A",depth_grid_plot000612)
plotdiff(SPMA2_REGdiffdiff_rast_8.5.list,"SPMA RPC 8.5","2072-2099","A",depth_grid_plot000612)   









####Min Max#### 
library(huxtable)
library(stats)

table1regdiff8.5 <- hux(
  Group = c('FLFJ', 'FLMJ', 'FLFA','FLMA','SPFJ', 'SPMJ', 'SPFA','SPMA'),
  Max = c(max(na.omit(as.data.frame(FLFJ2_REGdiffdiff_rast_8.5.list[[1]]))), max(na.omit(as.data.frame(FLMJ2_REGdiffdiff_rast_8.5.list[[1]]))), 
          max(na.omit(as.data.frame(FLFA2_REGdiffdiff_rast_8.5.list[[1]]))),
          max(na.omit(as.data.frame(FLMA2_REGdiffdiff_rast_8.5.list[[1]]))),max(na.omit(as.data.frame(SPFJ2_REGdiffdiff_rast_8.5.list[[1]]))), 
          max(na.omit(as.data.frame(SPMJ2_REGdiffdiff_rast_8.5.list[[1]]))), max(na.omit(as.data.frame(SPFA2_REGdiffdiff_rast_8.5.list[[1]]))),
          max(na.omit(as.data.frame(SPMA2_REGdiffdiff_rast_8.5.list[[1]])))),
  Min = c(min(na.omit(as.data.frame(FLFJ2_REGdiffdiff_rast_8.5.list[[1]]))), min(na.omit(as.data.frame(FLMJ2_REGdiffdiff_rast_8.5.list[[1]]))), 
          min(na.omit(as.data.frame(FLFA2_REGdiffdiff_rast_8.5.list[[1]]))),
          min(na.omit(as.data.frame(FLMA2_REGdiffdiff_rast_8.5.list[[1]]))),min(na.omit(as.data.frame(SPFJ2_REGdiffdiff_rast_8.5.list[[1]]))), 
          min(na.omit(as.data.frame(SPMJ2_REGdiffdiff_rast_8.5.list[[1]]))), min(na.omit(as.data.frame(SPFA2_REGdiffdiff_rast_8.5.list[[1]]))),
          min(na.omit(as.data.frame(SPMA2_REGdiffdiff_rast_8.5.list[[1]])))),
  Mean = c(mean(FLFJ2_REGdiffdiff_rast_8.5.list[[1]],na.rm=TRUE), mean(FLMJ2_REGdiffdiff_rast_8.5.list[[1]],na.rm=TRUE), 
           mean(FLFA2_REGdiffdiff_rast_8.5.list[[1]],na.rm=TRUE),
           mean(FLMA2_REGdiffdiff_rast_8.5.list[[1]],na.rm=TRUE),mean(SPFJ2_REGdiffdiff_rast_8.5.list[[1]],na.rm=TRUE), 
           mean(SPMJ2_REGdiffdiff_rast_8.5.list[[1]],na.rm=TRUE), mean(SPFA2_REGdiffdiff_rast_8.5.list[[1]],na.rm=TRUE),
           mean(SPMA2_REGdiffdiff_rast_8.5.list[[1]],na.rm=TRUE)),
  Med = c(median(FLFJ2_REGdiffdiff_rast_8.5.list[[1]],na.rm=TRUE), median(FLMJ2_REGdiffdiff_rast_8.5.list[[1]],na.rm=TRUE), 
          median(FLFA2_REGdiffdiff_rast_8.5.list[[1]],na.rm=TRUE),
          median(FLMA2_REGdiffdiff_rast_8.5.list[[1]],na.rm=TRUE),median(SPFJ2_REGdiffdiff_rast_8.5.list[[1]],na.rm=TRUE), 
          median(SPMJ2_REGdiffdiff_rast_8.5.list[[1]],na.rm=TRUE), median(SPFA2_REGdiffdiff_rast_8.5.list[[1]],na.rm=TRUE),
          median(SPMA2_REGdiffdiff_rast_8.5.list[[1]],na.rm=TRUE)),add_colnames = TRUE)

bold(table1regdiff8.5)[1,]           <- TRUE
bottom_border(table1regdiff8.5)[1,]  <- 0.4
align(table1regdiff8.5)[,2]          <- 'right'
right_padding(table1regdiff8.5)      <- 10
left_padding(table1regdiff8.5)       <- 10
width(table1regdiff8.5)              <- 0.30
number_format(table1regdiff8.5)      <- 2

table1regdiff8.5
####                                                            ####
###                                                             #####

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
    plot(depth_grid$lon, depth_grid$lat, pch=16, col=colcode, cex=0.1, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, xlab="Longitude", ylab="Latitude")
    map(database = mainecoast, ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE, lwd=0.1)
    #plot(sa511_513, add=T)
    box()
    legend("topleft", paste(year, "-", month, sep=""), bty="n", cex=1.5)
    #legend("bottomright", legend=c(names(attr(colcode, "table"))), fill=c(attr(colcode, "palette")), cex=1.0,
          # bty = "n", title="Abundance")
    axis(1, cex=0.5)
    axis(2, cex=0.5)
  }
  #dev.off()
}

###2099####
par(mar=c(2,2,0,0), mfrow=c(3,4))
plot4x4(FLFJ8.5RAST,"FLFJ","2072-2099","J",depth_grid_plot000612)
plot4x4(FLMJ8.5RAST,"FLMJ","2072-2099","J",depth_grid_plot000612)
plot4x4(FLFA8.5RAST,"FLFA","2072-2099","A",depth_grid_plot000612)
plot4x4(FLMA8.5RAST,"FLMA","2072-2099","A",depth_grid_plot000612)

plot4x4(FLFJ8.5ewRAST,"FLFJ","2072-2099","J",depth_grid_plot000612)
plot4x4(FLMJ8.5ewRAST,"FLMJ","2072-2099","J",depth_grid_plot000612)
plot4x4(FLFA8.5ewRAST,"FLFA","2072-2099","A",depth_grid_plot000612)
plot4x4(FLMA8.5ewRAST,"FLMA","2072-2099","A",depth_grid_plot000612)

plot4x4(FLFJew2RAST8.5,"FLFJ","2072-2099","J",depth_grid_plot000612)
plot4x4(FLMJew2RAST8.5,"FLMJ","2072-2099","J",depth_grid_plot000612)
plot4x4(FLFAew2RAST8.5,"FLFA","2072-2099","A",depth_grid_plot000612)
plot4x4(FLMAew2RAST8.5,"FLMA","2072-2099","A",depth_grid_plot000612)

par(mar=c(2,2,0,0), mfrow=c(3,4))
plot4x4(SPFJ8.5RAST,"SPFJ","2072-2099","J",depth_grid_plot000612)
plot4x4(SPMJ8.5RAST,"SPMJ","2072-2099","J",depth_grid_plot000612)
plot4x4(SPFA8.5RAST,"SPFA","2072-2099","A",depth_grid_plot000612)
plot4x4(SPMA8.5RAST,"SPMA","2072-2099","A",depth_grid_plot000612)

plot4x4(SPFJ8.5ewRAST,"SPFJ","2072-2099","J",depth_grid_plot000612)
plot4x4(SPMJ8.5ewRAST,"SPMJ","2072-2099","J",depth_grid_plot000612)
plot4x4(SPFA8.5ewRAST,"SPFA","2072-2099","A",depth_grid_plot000612)
plot4x4(SPMA8.5ewRAST,"SPMA","2072-2099","A",depth_grid_plot000612)

plot4x4(SPFJew2RAST8.5,"SPFJ","2072-2099","J",depth_grid_plot000612)
plot4x4(SPMJew2RAST8.5,"SPMJ","2072-2099","J",depth_grid_plot000612)
plot4x4(SPFAew2RAST8.5,"SPFA","2072-2099","A",depth_grid_plot000612)
plot4x4(SPMAew2RAST8.5,"SPMA","2072-2099","A",depth_grid_plot000612)

###2055####
par(mar=c(2,2,0,0), mfrow=c(3,4))
plot4x4(FLFJ8.5RAST,"FLFJ","2028-2055","J",depth_grid_plot000612)
plot4x4(FLMJ8.5RAST,"FLMJ","2028-2055","J",depth_grid_plot000612)
plot4x4(FLFA8.5RAST,"FLFA","2028-2055","A",depth_grid_plot000612)
plot4x4(FLMA8.5RAST,"FLMA","2028-2055","A",depth_grid_plot000612)

plot4x4(FLFJ8.5ewRAST,"FLFJ","2028-2055","J",depth_grid_plot000612)
plot4x4(FLMJ8.5ewRAST,"FLMJ","2028-2055","J",depth_grid_plot000612)
plot4x4(FLFA8.5ewRAST,"FLFA","2028-2055","A",depth_grid_plot000612)
plot4x4(FLMA8.5ewRAST,"FLMA","2028-2055","A",depth_grid_plot000612)

plot4x4(FLFJew2RAST8.5,"FLFJ","2028-2055","J",depth_grid_plot000612)
plot4x4(FLMJew2RAST8.5,"FLMJ","2028-2055","J",depth_grid_plot000612)
plot4x4(FLFAew2RAST8.5,"FLFA","2028-2055","A",depth_grid_plot000612)
plot4x4(FLMAew2RAST8.5,"FLMA","2028-2055","A",depth_grid_plot000612)

par(mar=c(2,2,0,0), mfrow=c(3,4))
plot4x4(SPFJ8.5RAST,"SPFJ","2028-2055","J",depth_grid_plot000612)
plot4x4(SPMJ8.5RAST,"SPMJ","2028-2055","J",depth_grid_plot000612)
plot4x4(SPFA8.5RAST,"SPFA","2028-2055","A",depth_grid_plot000612)
plot4x4(SPMA8.5RAST,"SPMA","2028-2055","A",depth_grid_plot000612)

plot4x4(SPFJ8.5ewRAST,"SPFJ","2028-2055","J",depth_grid_plot000612)
plot4x4(SPMJ8.5ewRAST,"SPMJ","2028-2055","J",depth_grid_plot000612)
plot4x4(SPFA8.5ewRAST,"SPFA","2028-2055","A",depth_grid_plot000612)
plot4x4(SPMA8.5ewRAST,"SPMA","2028-2055","A",depth_grid_plot000612)

plot4x4(SPFJew2RAST8.5,"SPFJ","2028-2055","J",depth_grid_plot000612)
plot4x4(SPMJew2RAST8.5,"SPMJ","2028-2055","J",depth_grid_plot000612)
plot4x4(SPFAew2RAST8.5,"SPFA","2028-2055","A",depth_grid_plot000612)
plot4x4(SPMAew2RAST8.5,"SPMA","2028-2055","A",depth_grid_plot000612)

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

###2099
par(mar=c(2,2,0,0), mfrow=c(4,4))
plot4x4(FLFJ_diff_rast_8.5.list,"FLFJ","2072-2099","J",depth_grid_plot000612,"Relative Diff")
plot4x4(FLMJ_diff_rast_8.5.list,"FLMJ","2072-2099","J",depth_grid_plot000612,"Relative Diff")
plot4x4(FLFA_diff_rast_8.5.list,"FLFA","2072-2099","A",depth_grid_plot000612,"Relative Diff")
plot4x4(FLMA_diff_rast_8.5.list,"FLMA","2072-2099","A",depth_grid_plot000612,"Relative Diff")
plot4x4(SPFJ_diff_rast_8.5.list,"SPFJ","2072-2099","J",depth_grid_plot000612,"Relative Diff")
plot4x4(SPMJ_diff_rast_8.5.list,"SPMJ","2072-2099","J",depth_grid_plot000612,"Relative Diff")
plot4x4(SPFA_diff_rast_8.5.list,"SPFA","2072-2099","A",depth_grid_plot000612,"Relative Diff")
plot4x4(SPMA_diff_rast_8.5.list,"SPMA","2072-2099","A",depth_grid_plot000612,"Relative Diff")
plot4x4(FLFJ_REGdiff_rast_8.5.list,"FLFJ","2072-2099","J",depth_grid_plot000612,"Difference")
plot4x4(FLMJ_REGdiff_rast_8.5.list,"FLMJ","2072-2099","J",depth_grid_plot000612,"Difference")
plot4x4(FLFA_REGdiff_rast_8.5.list,"FLFA","2072-2099","A",depth_grid_plot000612,"Difference")
plot4x4(FLMA_REGdiff_rast_8.5.list,"FLMA","2072-2099","A",depth_grid_plot000612,"Difference")
plot4x4(SPFJ_REGdiff_rast_8.5.list,"SPFJ","2072-2099","J",depth_grid_plot000612,"Difference")
plot4x4(SPMJ_REGdiff_rast_8.5.list,"SPMJ","2072-2099","J",depth_grid_plot000612,"Difference")
plot4x4(SPFA_REGdiff_rast_8.5.list,"SPFA","2072-2099","A",depth_grid_plot000612,"Difference")
plot4x4(SPMA_REGdiff_rast_8.5.list,"SPMA","2072-2099","A",depth_grid_plot000612,"Difference")

###2055
par(mar=c(2,2,0,0), mfrow=c(4,4))
plot4x4(FLFJ_diff_rast_8.5.list,"FLFJ","2028-2055","J",depth_grid_plot000612,"Relative Diff")
plot4x4(FLMJ_diff_rast_8.5.list,"FLMJ","2028-2055","J",depth_grid_plot000612,"Relative Diff")
plot4x4(FLFA_diff_rast_8.5.list,"FLFA","2028-2055","A",depth_grid_plot000612,"Relative Diff")
plot4x4(FLMA_diff_rast_8.5.list,"FLMA","2028-2055","A",depth_grid_plot000612,"Relative Diff")
plot4x4(SPFJ_diff_rast_8.5.list,"SPFJ","2028-2055","J",depth_grid_plot000612,"Relative Diff")
plot4x4(SPMJ_diff_rast_8.5.list,"SPMJ","2028-2055","J",depth_grid_plot000612,"Relative Diff")
plot4x4(SPFA_diff_rast_8.5.list,"SPFA","2028-2055","A",depth_grid_plot000612,"Relative Diff")
plot4x4(SPMA_diff_rast_8.5.list,"SPMA","2028-2055","A",depth_grid_plot000612,"Relative Diff")
plot4x4(FLFJ_REGdiff_rast_8.5.list,"FLFJ","2028-2055","J",depth_grid_plot000612,"Difference")
plot4x4(FLMJ_REGdiff_rast_8.5.list,"FLMJ","2028-2055","J",depth_grid_plot000612,"Difference")
plot4x4(FLFA_REGdiff_rast_8.5.list,"FLFA","2028-2055","A",depth_grid_plot000612,"Difference")
plot4x4(FLMA_REGdiff_rast_8.5.list,"FLMA","2028-2055","A",depth_grid_plot000612,"Difference")
plot4x4(SPFJ_REGdiff_rast_8.5.list,"SPFJ","2028-2055","J",depth_grid_plot000612,"Difference")
plot4x4(SPMJ_REGdiff_rast_8.5.list,"SPMJ","2028-2055","J",depth_grid_plot000612,"Difference")
plot4x4(SPFA_REGdiff_rast_8.5.list,"SPFA","2028-2055","A",depth_grid_plot000612,"Difference")
plot4x4(SPMA_REGdiff_rast_8.5.list,"SPMA","2028-2055","A",depth_grid_plot000612,"Difference")

####NSV2####
###2099
par(mar=c(2,2,0,0), mfrow=c(4,4))
plot4x4(FLFJ2_diff_rast_8.5.list,"FLFJ","2072-2099","J",depth_grid_plot000612,"Relative Diff")
plot4x4(FLMJ2_diff_rast_8.5.list,"FLMJ","2072-2099","J",depth_grid_plot000612,"Relative Diff")
plot4x4(FLFA2_diff_rast_8.5.list,"FLFA","2072-2099","A",depth_grid_plot000612,"Relative Diff")
plot4x4(FLMA2_diff_rast_8.5.list,"FLMA","2072-2099","A",depth_grid_plot000612,"Relative Diff")
plot4x4(SPFJ2_diff_rast_8.5.list,"SPFJ","2072-2099","J",depth_grid_plot000612,"Relative Diff")
plot4x4(SPMJ2_diff_rast_8.5.list,"SPMJ","2072-2099","J",depth_grid_plot000612,"Relative Diff")
plot4x4(SPFA2_diff_rast_8.5.list,"SPFA","2072-2099","A",depth_grid_plot000612,"Relative Diff")
plot4x4(SPMA2_diff_rast_8.5.list,"SPMA","2072-2099","A",depth_grid_plot000612,"Relative Diff")
plot4x4(FLFJ2_REGdiffdiff_rast_8.5.list,"FLFJ","2072-2099","J",depth_grid_plot000612,"Difference")
plot4x4(FLMJ2_REGdiffdiff_rast_8.5.list,"FLMJ","2072-2099","J",depth_grid_plot000612,"Difference")
plot4x4(FLFA2_REGdiffdiff_rast_8.5.list,"FLFA","2072-2099","A",depth_grid_plot000612,"Difference")
plot4x4(FLMA2_REGdiffdiff_rast_8.5.list,"FLMA","2072-2099","A",depth_grid_plot000612,"Difference")
plot4x4(SPFJ2_REGdiffdiff_rast_8.5.list,"SPFJ","2072-2099","J",depth_grid_plot000612,"Difference")
plot4x4(SPMJ2_REGdiffdiff_rast_8.5.list,"SPMJ","2072-2099","J",depth_grid_plot000612,"Difference")
plot4x4(SPFA2_REGdiffdiff_rast_8.5.list,"SPFA","2072-2099","A",depth_grid_plot000612,"Difference")
plot4x4(SPMA2_REGdiffdiff_rast_8.5.list,"SPMA","2072-2099","A",depth_grid_plot000612,"Difference")

###2055
par(mar=c(2,2,0,0), mfrow=c(4,4))
plot4x4(FLFJ2_diff_rast_8.5.list,"FLFJ","2028-2055","J",depth_grid_plot000612,"Relative Diff")
plot4x4(FLMJ2_diff_rast_8.5.list,"FLMJ","2028-2055","J",depth_grid_plot000612,"Relative Diff")
plot4x4(FLFA2_diff_rast_8.5.list,"FLFA","2028-2055","A",depth_grid_plot000612,"Relative Diff")
plot4x4(FLMA2_diff_rast_8.5.list,"FLMA","2028-2055","A",depth_grid_plot000612,"Relative Diff")
plot4x4(SPFJ2_diff_rast_8.5.list,"SPFJ","2028-2055","J",depth_grid_plot000612,"Relative Diff")
plot4x4(SPMJ2_diff_rast_8.5.list,"SPMJ","2028-2055","J",depth_grid_plot000612,"Relative Diff")
plot4x4(SPFA2_diff_rast_8.5.list,"SPFA","2028-2055","A",depth_grid_plot000612,"Relative Diff")
plot4x4(SPMA2_diff_rast_8.5.list,"SPMA","2028-2055","A",depth_grid_plot000612,"Relative Diff")
plot4x4(FLFJ2_REGdiffdiff_rast_8.5.list,"FLFJ","2028-2055","J",depth_grid_plot000612,"Difference")
plot4x4(FLMJ2_REGdiffdiff_rast_8.5.list,"FLMJ","2028-2055","J",depth_grid_plot000612,"Difference")
plot4x4(FLFA2_REGdiffdiff_rast_8.5.list,"FLFA","2028-2055","A",depth_grid_plot000612,"Difference")
plot4x4(FLMA2_REGdiffdiff_rast_8.5.list,"FLMA","2028-2055","A",depth_grid_plot000612,"Difference")
plot4x4(SPFJ2_REGdiffdiff_rast_8.5.list,"SPFJ","2028-2055","J",depth_grid_plot000612,"Difference")
plot4x4(SPMJ2_REGdiffdiff_rast_8.5.list,"SPMJ","2028-2055","J",depth_grid_plot000612,"Difference")
plot4x4(SPFA2_REGdiffdiff_rast_8.5.list,"SPFA","2028-2055","A",depth_grid_plot000612,"Difference")
plot4x4(SPMA2_REGdiffdiff_rast_8.5.list,"SPMA","2028-2055","A",depth_grid_plot000612,"Difference")
##### creatting region columns for 8.5 relative divverences to see differences per localized region#####

FLFJ_diff_rast_8.5<- transform(FLFJ_diff_rast_8.5, Region= ifelse(FLFJ_diff_rast_8.5$Longitude>="-69.27457", 2, ifelse(FLFJ_diff_rast_8.5$Longitude<"-69.27457", 4, 5)))
FLMJ_diff_rast_8.5<- transform(FLMJ_diff_rast_8.5, Region= ifelse(FLMJ_diff_rast_8.5$Longitude>="-69.27457", 2, ifelse(FLMJ_diff_rast_8.5$Longitude<"-69.27457", 4, 5)))
FLFA_diff_rast_8.5<- transform(FLFA_diff_rast_8.5, Region= ifelse(FLFA_diff_rast_8.5$Longitude>="-69.27457", 2, ifelse(FLFA_diff_rast_8.5$Longitude<"-69.27457", 4, 5)))
FLMA_diff_rast_8.5<- transform(FLMA_diff_rast_8.5, Region= ifelse(FLMA_diff_rast_8.5$Longitude>="-69.27457", 2, ifelse(FLMA_diff_rast_8.5$Longitude<"-69.27457", 4, 5)))
SPFJ_diff_rast_8.5<- transform(SPFJ_diff_rast_8.5, Region= ifelse(SPFJ_diff_rast_8.5$Longitude>="-69.27457", 2, ifelse(SPFJ_diff_rast_8.5$Longitude<"-69.27457", 4, 5)))
SPMJ_diff_rast_8.5<- transform(SPMJ_diff_rast_8.5, Region= ifelse(SPMJ_diff_rast_8.5$Longitude>="-69.27457", 2, ifelse(SPMJ_diff_rast_8.5$Longitude<"-69.27457", 4, 5)))
SPFA_diff_rast_8.5<- transform(SPFA_diff_rast_8.5, Region= ifelse(SPFA_diff_rast_8.5$Longitude>="-69.27457", 2, ifelse(SPFA_diff_rast_8.5$Longitude<"-69.27457", 4, 5)))
SPMA_diff_rast_8.5<- transform(SPMA_diff_rast_8.5, Region= ifelse(SPMA_diff_rast_8.5$Longitude>="-69.27457", 2, ifelse(SPMA_diff_rast_8.5$Longitude<"-69.27457", 4, 5)))
###### For models 1:3 differences##########################################################
FLFJ2_diff_rast_8.5<- transform(FLFJ2_diff_rast_8.5, Region= ifelse(FLFJ2_diff_rast_8.5$Longitude>="-69.27457", 2, ifelse(FLFJ2_diff_rast_8.5$Longitude<"-69.27457"&
                                          FLFJ2_diff_rast_8.5$Longitude>="-68.58246", 3, ifelse(FLFJ2_diff_rast_8.5$Longitude<"-68.58246", 4, 5))))
FLMJ2_diff_rast_8.5<- transform(FLMJ2_diff_rast_8.5, Region= ifelse(FLMJ2_diff_rast_8.5$Longitude>="-69.27457", 2, ifelse(FLMJ2_diff_rast_8.5$Longitude<"-69.27457"&
                                          FLMJ2_diff_rast_8.5$Longitude>="-68.58246", 3, ifelse(FLMJ2_diff_rast_8.5$Longitude<"-68.58246", 4, 5))))
FLFA2_diff_rast_8.5<- transform(FLFA2_diff_rast_8.5, Region= ifelse(FLFA2_diff_rast_8.5$Longitude>="-69.27457", 2, ifelse(FLFA2_diff_rast_8.5$Longitude<"-69.27457"&
                                          FLFA2_diff_rast_8.5$Longitude>="-68.58246", 3, ifelse(FLFA2_diff_rast_8.5$Longitude<"-68.58246", 4, 5))))
FLMA2_diff_rast_8.5<- transform(FLMA2_diff_rast_8.5, Region= ifelse(FLMA2_diff_rast_8.5$Longitude>="-69.27457", 2, ifelse(FLMA2_diff_rast_8.5$Longitude<"-69.27457"&
                                          FLMA2_diff_rast_8.5$Longitude>="-68.58246", 3, ifelse(FLMA2_diff_rast_8.5$Longitude<"-68.58246", 4, 5))))
SPFJ2_diff_rast_8.5<- transform(SPFJ2_diff_rast_8.5, Region= ifelse(SPFJ2_diff_rast_8.5$Longitude>="-69.27457", 2, ifelse(SPFJ2_diff_rast_8.5$Longitude<"-69.27457"&
                                          SPFJ2_diff_rast_8.5$Longitude>="-68.58246", 3, ifelse(SPFJ2_diff_rast_8.5$Longitude<"-68.58246", 4, 5))))
SPMJ2_diff_rast_8.5<- transform(SPMJ2_diff_rast_8.5, Region= ifelse(SPMJ2_diff_rast_8.5$Longitude>="-69.27457", 2, ifelse(SPMJ2_diff_rast_8.5$Longitude<"-69.27457"&
                                          SPMJ2_diff_rast_8.5$Longitude>="-68.58246", 3, ifelse(SPMJ2_diff_rast_8.5$Longitude<"-68.58246", 4, 5))))
SPFA2_diff_rast_8.5<- transform(SPFA2_diff_rast_8.5, Region= ifelse(SPFA2_diff_rast_8.5$Longitude>="-69.27457", 2, ifelse(SPFA2_diff_rast_8.5$Longitude<"-69.27457"&
                                          SPFA2_diff_rast_8.5$Longitude>="-68.58246", 3, ifelse(SPFA2_diff_rast_8.5$Longitude<"-68.58246", 4, 5))))
SPMA2_diff_rast_8.5<- transform(SPMA2_diff_rast_8.5, Region= ifelse(SPMA2_diff_rast_8.5$Longitude>="-69.27457", 2, ifelse(SPMA2_diff_rast_8.5$Longitude<"-69.27457"&
                                          SPMA2_diff_rast_8.5$Longitude>="-68.58246", 3, ifelse(SPMA2_diff_rast_8.5$Longitude<"-68.58246", 4, 5))))

res <- function(data){
  res1<-boxplot(p.abundance ~ Region, data = data)
  return(res1$stats)}
res(FLFJ_diff_rast_8.5)
res(FLMJ_diff_rast_8.5)
res(FLFA_diff_rast_8.5)
res(FLMA_diff_rast_8.5)
res(SPFJ_diff_rast_8.5)
res(SPMJ_diff_rast_8.5)
res(SPFA_diff_rast_8.5)
res(SPMA_diff_rast_8.5)

res(FLFJ2_diff_rast_8.5)
res(FLMJ2_diff_rast_8.5)
res(FLFA2_diff_rast_8.5)
res(FLMA2_diff_rast_8.5)
res(SPFJ2_diff_rast_8.5)
res(SPMJ2_diff_rast_8.5)
res(SPFA2_diff_rast_8.5)
res(SPMA2_diff_rast_8.5)

####plot future temperature and salinity####
mainecoast= readOGR("D:/MENHTrawl/data/gis/ne_10m_coastline/ne_10m_coastline.shp")
depth_grid_plot000612 <- read.csv("D://MENHtrawl/grid_depth_data000612.csv") #Depth data for years 2000,2006, and 2012
plottemp<-function(rasterdata,column,size,year,month){
  par(mar=c(2,2,0,0), mfrow=c(1,1))
  plotvar=column
  nclr=9
  plotclr <- rev(brewer.pal(nclr,"RdYlBu"))
  class <- classIntervals(plotvar, nclr, style="quantile")
  fix_break<-round(class$brks, digits = 2)
  for(i in 1:1){
    print(i)
    plotvar <- column
    nclr=9
    plotclr <- rev(brewer.pal(nclr,"RdYlBu"))
    class <- classIntervals(plotvar, nclr, style="fixed", 
                            fixedBreaks= 
                              if(size=="Fall Temperature")c(0,7,8,8.5,9,9.5,10,10.5,11.5,13,16.15,19.5)else( #breaks calculated by using average raw data "quantile" breaks
                                if(size=="Spring Temperature")c(0,4.7,5.25,6,.5,7,7.5,8.5,9.5,10.5,11.85,12.38)else(
                                  if(size=="Fall Salinity")c(0,26,31,31.5,32,32.5,33,33.5,34,34.5,35.1,35.5)else(
                                    if(size=="Spring Salinity")c(0,26,31,31.5,32,32.5,33,33.5,34,34.5,35.1,35.6)else(
                                      if(size=="avgDepth")c(0,5,10,20,30,40,50,60,70,80,90,100,125,150)else(
                                        if(size=="sediment")c(-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10)else(
                                          if(size=="DFS")c(0,0.1,0.2,0.4,0.6,0.8,1,1.2,1.4,1.6,1.8,2.0)else(
                                            if(size=="anol")c(3.0,3.05,3.1,3.15,3.20,3.25,3.3,3.35,3.40)
                                          else c(0,1,2,3,4,5,6,7,8,9,30000))))))))) #breaks calculated by using average "qauntile" breaks

    colcode <- findColours(class, plotclr)
    
    start_x <- range(GAM_data$Longitude)[1]
    end_x <- range(GAM_data$Longitude)[2]
    start_y <- range(GAM_data$Latitude)[1]
    end_y <- range(GAM_data$Latitude)[2]
    plot(rasterdata$Longitude, rasterdata$Latitude, pch=16, col=colcode, cex=0.9, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, xlab="Longitude", ylab="Latitude")
    map(database= mainecoast, ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE, lwd=0.1)
    #plot(sa511_513, add=T)
    box()
    legend("topleft", paste(year, "-", month, sep=""), bty="n", cex=1.1)
    legend("bottomright", legend=c(names(attr(colcode, "table"))), fill=c(attr(colcode, "palette")), cex=0.9,bg="white",
           bty = "o", title=if (size=="Fall Temperature")"Bottom Temperature (C)"else(
             if(size=="Spring Temperature")"Bottom Temperature (C)"else(
               if(size=="Fall Salinity")"Bottom Salinity (psu)"else(
                 if(size=="Spring Salinity")"Bottom Salinity (psu)"
                 else "Fail"))))
    axis(1, cex=0.8)
    axis(2, cex=0.8)
  }
  #dev.off()
}

plottemp(FL85T99_2,FL85T99_2$Bottom_WaterTempDegC,"Fall Temperature","2055-2099","Fall")
plottemp(FL85T99_2,FL85T99_2$Bottom_Salinity_psu,"Fall Salinity","2055-2099","Fall")
plottemp(SP85T99_2,SP85T99_2$Bottom_WaterTempDegC,"Spring Temperature","2055-2099","Spring")
plottemp(SP85T99_2,SP85T99_2$Bottom_Salinity_psu,"Spring Salinity","2055-2099","Spring")

plottemp(FL85T55_2,FL85T55_2$Bottom_WaterTempDegC,"Fall Temperature","2028-2055","Fall")
plottemp(FL85T55_2,FL85T55_2$Bottom_Salinity_psu,"Fall Salinity","2028-2055","Fall")
plottemp(SP85T55_2,SP85T55_2$Bottom_WaterTempDegC,"Spring Temperature","2028-2055","Spring")
plottemp(SP85T55_2,SP85T55_2$Bottom_Salinity_psu,"Spring Salinity","2028-2055","Spring")

plottemp(SP85T99_2,SP85T99_2$anoltemp,"anol","2055-2099","Spring")
plottemp(FL85T99_2,FL85T99_2$anolsal,"DFS","2055-2099","Spring")

plottemp(SP7805_2,SP7805_2$AvgDepth,"avgDepth","2055-2099","Spring")
plottemp(FL7805,FL7805$AvgDepth,"avgDepth","2055-2099","Spring")
plottemp(FL7805,FL7805$dist_frm_shore,"DFS","2055-2099","Spring")
plottemp(FL7805,FL7805$sediment,"sediment","2055-2099","Spring")

plottemp(Springfinal7805,Springfinal7805$Bottom_WaterTempDegC,"Spring Temperature","2055-2099","Spring")
plottemp(Springfinal7805,Springfinal7805$Bottom_Salinity_psu,"Spring Salinity","2055-2099","Spring")

plottemp(Fallfinal7805,Fallfinal7805$Bottom_WaterTempDegC,"Fall Temperature","2055-2099","Fall")
plottemp(Fallfinal7805,Fallfinal7805$Bottom_Salinity_psu,"Fall Salinity","2055-2099","Fall")

plottemp(SP7805_2,SP7805_2$Bottom_WaterTempDegC,"Spring Temperature","2055-2099","Spring")
plottemp(FL7805_2,FL7805_2$Bottom_WaterTempDegC,"Fall Temperature","2055-2099","Fall")

plottemp(test1,test1$Bottom_WaterTempDegC,"Spring Temperature","2055-2099","Spring")
plottemp(test2,test2$Bottom_WaterTempDegC,"Fall Temperature","2055-2099","Fall")
plottemp(SPFVRAST7805temp[[1]],SPFVRAST7805temp[[1]],"Spring Temperature","2055-2099","Spring")
plottemp(SPFV1978_2005,SPFV1978_2005$Bottom_Salinity_psu,"Spring Salinity","2055-2099","Spring")
plottemp(SP7805,SP7805$`spring79_05[[1]]`,"Spring Salinity","2055-2099","Spring")

plottemp(FLFV1978_2005,FLFV1978_2005$Bottom_WaterTempDegC,"Fall Temperature","2055-2099","Fall")
plottemp(FLFV1978_2005,FLFV1978_2005$Bottom_Salinity_psu,"Fall Salinity","2055-2099","Fall")


####### plot maps for paper#####
####plot FLFJ and SPMA all plots####
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
    legend("bottomright", legend=c(names(attr(colcode, "table"))), fill=c(attr(colcode, "palette")), cex=0.9,
           bty = "n", title="Abundance")
    axis(1, cex=0.5)
    axis(2, cex=0.5)
  }
  #dev.off()
}
par(mar=c(2,2,0,0), mfrow=c(3,2))
plot4x4(FLFJ8.5RAST,"FLFJ","2072-2099","J",depth_grid_plot000612)
plot4x4(SPMA8.5RAST,"SPMA","2072-2099","A",depth_grid_plot000612)
plot4x4(FLFJ8.5ewRAST,"FLFJ","2072-2099","J",depth_grid_plot000612)
plot4x4(SPMA8.5ewRAST,"SPMA","2072-2099","A",depth_grid_plot000612)
plot4x4(FLFJew2RAST8.5,"FLFJ","2072-2099","J",depth_grid_plot000612)
plot4x4(SPMAew2RAST8.5,"SPMA","2072-2099","A",depth_grid_plot000612)
plot4x4(FLFJvcRAST8.5,"FLFJ","2072-2099","J",depth_grid_plot000612)
plot4x4(SPMAvcRAST8.5,"SPMA","2072-2099","A",depth_grid_plot000612)


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
    legend("bottomright", legend=c(names(attr(colcode, "table"))), fill=c(attr(colcode, "palette")), cex=0.9,
           bty = "n", title=title)
    axis(1, cex=0.5)
    axis(2, cex=0.5)
  }
  #dev.off()
}
par(mar=c(2,2,0,0), mfrow=c(2,2))
plot4x4(FLFJ_diff_rast_8.5.list,"FLFJ","2072-2099","J",depth_grid_plot000612,"Relative Diff")
#plot4x4(FLFJ_REGdiff_rast_8.5.list,"FLFJ","2072-2099","J",depth_grid_plot000612,"Difference")
plot4x4(SPMA_diff_rast_8.5.list,"SPMA","2072-2099","A",depth_grid_plot000612,"Relative Diff")
#plot4x4(SPMA_REGdiff_rast_8.5.list,"SPMA","2072-2099","A",depth_grid_plot000612,"Difference")
plot4x4(FLFJ2_diff_rast_8.5.list,"FLFJ","2072-2099","J",depth_grid_plot000612,"Relative Diff")
#plot4x4(FLFJ2_REGdiffdiff_rast_8.5.list,"FLFJ","2072-2099","J",depth_grid_plot000612,"Difference")
plot4x4(SPMA2_diff_rast_8.5.list,"SPMA","2072-2099","A",depth_grid_plot000612,"Relative Diff")
#plot4x4(SPMA2_REGdiffdiff_rast_8.5.list,"SPMA","2072-2099","A",depth_grid_plot000612,"Difference")
plot4x4(FLFJ3_diff_rast_8.5.list,"FLFJ","2072-2099","J",depth_grid_plot000612,"Relative Diff")
#plot4x4(FLFJ3_REDdiff_rast_8.5.list,"FLFJ","2072-2099","J",depth_grid_plot000612,"Difference")
plot4x4(SPMA3_diff_rast_8.5.list,"SPMA","2072-2099","A",depth_grid_plot000612,"Relative Diff")
#plot4x4(SPMA3_REDdiff_rast_8.5.list,"SPMA","2072-2099","A",depth_grid_plot000612,"Difference")

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
####NSV1 vs NSV2 for paper####
par(mar=c(2,2,0,0), mfrow=c(4,4))
plot4x4(FLFJ_diff_rast_8.5.list,"FLFJ","2028-2055","J",depth_grid_plot000612,"Relative Diff")
plot4x4(FLMJ_diff_rast_8.5.list,"FLMJ","2028-2055","J",depth_grid_plot000612,"Relative Diff")
plot4x4(FLFA_diff_rast_8.5.list,"FLFA","2028-2055","A",depth_grid_plot000612,"Relative Diff")
plot4x4(FLMA_diff_rast_8.5.list,"FLMA","2028-2055","A",depth_grid_plot000612,"Relative Diff")
plot4x4(FLFJ2_diff_rast_8.5.list,"FLFJ","2028-2055","J",depth_grid_plot000612,"Relative Diff")
plot4x4(FLMJ2_diff_rast_8.5.list,"FLMJ","2028-2055","J",depth_grid_plot000612,"Relative Diff")
plot4x4(FLFA2_diff_rast_8.5.list,"FLFA","2028-2055","A",depth_grid_plot000612,"Relative Diff")
plot4x4(FLMA2_diff_rast_8.5.list,"FLMA","2028-2055","A",depth_grid_plot000612,"Relative Diff")
plot4x4(SPFJ_diff_rast_8.5.list,"SPFJ","2028-2055","J",depth_grid_plot000612,"Relative Diff")
plot4x4(SPMJ_diff_rast_8.5.list,"SPMJ","2028-2055","J",depth_grid_plot000612,"Relative Diff")
plot4x4(SPFA_diff_rast_8.5.list,"SPFA","2028-2055","A",depth_grid_plot000612,"Relative Diff")
plot4x4(SPMA_diff_rast_8.5.list,"SPMA","2028-2055","A",depth_grid_plot000612,"Relative Diff")
plot4x4(SPFJ2_diff_rast_8.5.list,"SPFJ","2028-2055","J",depth_grid_plot000612,"Relative Diff")
plot4x4(SPMJ2_diff_rast_8.5.list,"SPMJ","2028-2055","J",depth_grid_plot000612,"Relative Diff")
plot4x4(SPFA2_diff_rast_8.5.list,"SPFA","2028-2055","A",depth_grid_plot000612,"Relative Diff")
plot4x4(SPMA2_diff_rast_8.5.list,"SPMA","2028-2055","A",depth_grid_plot000612,"Relative Diff")




#####Ttest for forecast data######
common <- function(data.frame1,data.frame2){
  common1=merge(data.frame1,data.frame2, by = c("Latitude", "Longitude"))
  t.test(common1$p.abundance.x, common1$p.abundance.y, paired = TRUE, alternative = "two.sided")
}

common(FLFJ8.5,FLFJ8.5ew)
common(FLMJ8.5,FLMJ8.5ew)
common(FLFA8.5,FLFA8.5ew)
common(FLMA8.5,FLMA8.5ew)
common(SPFJ8.5,SPFJ8.5ew)
common(SPMJ8.5,SPMJ8.5ew)
common(SPFA8.5,SPFA8.5ew)
common(SPMA8.5,SPMA8.5ew)

common(FLFJ8.5,FLFJew28.5)
common(FLMJ8.5,FLMJew28.5)
common(FLFA8.5,FLFAew28.5)
common(FLMA8.5,FLMAew28.5)
common(SPFJ8.5,SPFJew28.5)
common(SPMJ8.5,SPMJew28.5)
common(SPFA8.5,SPFAew28.5)
common(SPMA8.5,SPMAew28.5)
