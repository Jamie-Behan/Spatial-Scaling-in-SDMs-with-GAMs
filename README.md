# Spatial-Scaling-in-SDMs-with-GAMs
These R scripts build Generalized Additive Models (GAMs) using data from the ME-NH-Inshore Bottom Trawl Survey
The purpose of this project is to compare, explore, and evalute how changes in spatial scale, data partitioning, and consequently, assumptions of nonstationarity will affect estimates density and distribution patterns in a species distribution model (SDM).
Each file is seperated for convientence, clarification and streamlining purposes.
A description of each file includes:
"GAM.R" - Building of season, sex, and size specific GAMs using all lobster data in the GOM from the years 2000-2019. These models represent the largest spatial scale model tested.
      This file also includes code that plots American lobster density estimates and residuals at 2000-2019 tral locations
      Also included is code for running RMSE, AIC, Moran's I, and two-fold corss validation tests.
"GAM nonstationary.R" - Includes the same tests and processes as the GAM.R file, but in this file, data are separated into east and west regions of the GOM. Seperate GAMs are built for
      each localized region and then later re-combined into one dataframe of estimates that will later be used for comparison purposes.
"GAM nonstationary EMW.R" - Includes the same tests and processes as the previous two files, but instead of separating data into east and west regions, as in the "GAM nonstationary.R"
      file, further localization is tested by seperating data into three regions: east, central, and western GOM. Seperate GAMs are built for each localized region and then later 
      re-combined into one dataframe. 
      At the end of this file, there is code that begins some preliminary comparisions, such as comparing estimated GAM response curves of localized regions for specific independent
      variables.
 "Hindcast GAM.R" - Uses data sourced from FVCOM in tandem with the developed models from the previous files to predict lobster density at all locations (interpolate) within the GOM
      study area of interest (approximate trawl survey boundaries). Estimates are made for years 2000, 2006, 2012, and 2017 for each of the 3 model approaches described above. Estmated
      Density plots are made for each lobster season, sex and size, group, as well as for each hindcasted year. Absolute and relative difference plots are also made, which compares the
      largest scale estimates (from GAM.R) to each of the small scale model approach estimates (from "GAM nonstationary.R" or from "GAM nonstationary EMW.R).
 "Forecast_GAMs_with_myfvcom_data.R" - Uses data sourced from FVCOM in tandem with the developed models from the first 2 listed files to predict lobster density at all locations
      (interpolate) within the GOM. This code is similar to that of the "Himdcast GAM.R" file, except instead of using hindcasted fine scale FVCOM data, delta downscaled forecasted 
      bottom temperature and bottom salinity data are used to estimate future lobster densities and spatial distributions. This file can be used for a few different RPC scenarios or
      year periods, but the scenario used in this study was 8.5 and predictions were made for the 2028-2055 time period. Estmated density plots are made for this time period for each
      season, sex and size group Absolute and relative difference plots are also made, as in "Hindcast GAM.R".
 I cannot share the ME-NH Inshore Trawl Survey data as it is not open source and must be requested by the Maine Department of Marine Resources.
 Hoewever, If you have any questions, feel free to reach out to me at jamie.behan@maine.edu
