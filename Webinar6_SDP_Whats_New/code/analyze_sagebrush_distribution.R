##Script to demo querying new SDP Data Products stored 
##in Amazon S3.

##Author: Ian Breckheimer
##Updated: 4-27-2021
#install.packages(c("rgdal","raster"))

##Sets up workspace
library(rgdal)
library(raster)
library(ggplot2)
library(mgcv)

##Project directory
proj_dir <- "~/code/SpatialDataScienceWebinars2020/"

##Sets working directory.
setwd(proj_dir)

##Check GDAL version (should work with GDAL 2.2.3 or later).
getGDALVersionInfo()

##Path to raster datasets. These are in the cloud, 
##but you could replace these with local file paths for local datasets (i.e. "C:/data.tif")
snow2018_path <- "/vsicurl/https://rmbl-sdp.s3.us-east-2.amazonaws.com/data_products/released/release2/UER_snow_depth_20180331_3m_v1.tif"
srad_path <- "/vsicurl/https://rmbl-sdp.s3.us-east-2.amazonaws.com/data_products/released/release2/UER_srad_bareearth_day080_3m_v1.tif"
elev_path <- "/vsicurl/https://rmbl-sdp.s3.us-east-2.amazonaws.com/data_products/released/release3/UG_dem_3m_v1.tif"

##Loads raster datasets.
snow2018 <- raster(snow2018_path)
srad <- raster(srad_path)
elev <- raster(elev_path)

##Loads GPS points.
plotGPS <- read.csv("./Webinar6_SDP_Whats_New/data/classed_points_tree_shub_herb_supp_bare.csv")

##Assigns coordinates and coordinate system. These points are in
##Geographic coordinates (lat-long). If you have points in a
##different system, it will have a unique proj 4 string that
##you can find at spatialreference.org
coordinates(plotGPS) <- ~longitude+latitude
proj4string(plotGPS) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

##Adds coordinates back to data frame.
plotGPS$latitude <- coordinates(plotGPS)[,2]
plotGPS$longitude <- coordinates(plotGPS)[,1]

##Re-projects coordinates to same system as the raster.
raster_p4string <- proj4string(snow2018)
plotGPS_tr <- spTransform(plotGPS,CRS=CRS(raster_p4string))

##Samples rasters at coordinates, grabbing just the value of the closest pixel:
plotGPS_tr$snow2018 <- extract(snow2018,plotGPS_tr,method="simple")
plotGPS_tr$srad <- extract(srad,plotGPS_tr,method="simple")
plotGPS_tr$elev <- extract(elev,plotGPS_tr,method="simple")

##Creates a new variable indicating whether a point has sagebrush or not.
plotGPS_tr$Sagebrush <- plotGPS_tr$class == "Artemisia"
plotGPS_tr$Salix <- plotGPS_tr$class == "Salix"


##Plots sagebrush and willow points as a function of environmental
##variables.
p1 <- ggplot(plotGPS_tr@data)+
  geom_point(aes(x=srad,y=snow2018,color=Sagebrush,
                 alpha=Sagebrush),size=0.75)+
  scale_y_continuous("Snow Depth, March 31st 2018 (m)",
                     limits=c(0,3))+
  scale_x_continuous("March 21st Potential Insolation (Wh/m^2)",
                     limits=c(4000,8000))+
  theme_bw()

##Fits a species distribution model using GAM.
sb_gam <- gam(Sagebrush~s(srad,snow2018,k=9),
              family=binomial(link="log"),
              data=plotGPS_tr@data)
summary(sb_gam)
plot(sb_gam)  

##Predicts from the model.
gam_pred <- expand.grid(srad=seq(4000,8000,length.out=200),
                        snow2018=seq(0,3,length.out=200))
gam_pred$Sagebrush_prob <- predict(sb_gam,newdata=gam_pred,
                                   type="response")

##Adds predictions to the plot.
p3 <- ggplot(plotGPS_tr@data)+
  geom_raster(aes(x=srad,y=snow2018,fill=Sagebrush_prob),data=
                gam_pred)+
  geom_point(aes(x=srad,y=snow2018,color=Sagebrush,
                 alpha=Sagebrush),size=0.75)+
  scale_color_grey(start=0,end=1)+
  scale_fill_distiller(type="div",palette=1)+
  scale_y_continuous("Snow Depth, March 31st 2018 (m)",
                     limits=c(0,3))+
  scale_x_continuous("March 21st Potential Insolation (Wh/m^2)",
                     limits=c(4000,8000))+
  theme_bw()
p3
