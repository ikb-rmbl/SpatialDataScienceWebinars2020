##Script to demo querying SDP Data Products stored 
##in Amazon S3.

##Author: Ian Breckheimer
##Updated: 2-12-2020
#install.packages(c("rgdal","raster"))

##Sets up workspace
library(rgdal)
library(raster)

##Project directory
proj_dir <- "~/code/SpatialDataScienceWebinars2020/"

##Sets working directory.
setwd(proj_dir)

##Check GDAL version (should work with GDAL 2.2.3 or later).
getGDALVersionInfo()

##Path to raster datasets. These are in the cloud, 
##but you could replace these with local file paths for local datasets (i.e. "C:/data.tif")
snow2018_path <- "/vsicurl/https://rmbl-sdp.s3.us-east-2.amazonaws.com/data_products/draft/UER_snow_depth_20180331_3m_v1.tif"
snow2019_path <- "/vsicurl/https://rmbl-sdp.s3.us-east-2.amazonaws.com/data_products/draft/UER_snow_depth_20190407_3m_v1.tif"

##Loads raster datasets.
snow2018 <- raster(snow2018_path)
snow2019 <- raster(snow2019_path)

##Loads GPS points.
plotGPS <- read.csv("./Webinar1_SDP_Intro/data/example_points.csv")

##Assigns coordinates and coordinate system. These points are in
##Geographic coordinates (lat-long). If you have points in a
##different system, it will have a unique proj 4 string that
##you can find at spatialreference.org
coordinates(plotGPS) <- ~Longitude+Latitude
proj4string(plotGPS) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

##Adds coordinates back to data frame.
plotGPS$Latitude <- coordinates(plotGPS)[,2]
plotGPS$Longitude <- coordinates(plotGPS)[,1]

##Re-projects coordinates to same system as the raster.
raster_p4string <- proj4string(snow2018)
plotGPS_tr <- spTransform(plotGPS,CRS=CRS(raster_p4string))

##Samples rasters at coordinates, with a given buffer (in m):
plotGPS_tr$SnowDepth2018_20m <- extract(snow2018,plotGPS_tr,buffer=20,
                                        fun=mean)

plotGPS_tr$SnowDepth2019_20m <- extract(snow2019,plotGPS_tr,buffer=20,
                                        fun=mean)
##Displays data
plotGPS_tr@data

##Plots measurements in different years.
plot(SnowDepth2018_20m~SnowDepth2019_20m,data=plotGPS_tr)

##