##Script to add precise terrain-following to a UAV flight planned with
##Litchi flight planning software.
##Author: Ian Breckheimer
##Updated 1-25-2021.

##Notes: to use this script, you will first need to design a UAV flight with
##Litchi mission hub (http://flylitchi.com/hub), and export it as a .csv file 
##using the Missions > Export CSV menu option.

##Sets up workspace.
library(readr)
library(raster)
library(rgdal)

##Imports raw flight path and digital elevation model. Note that this DEM is
##a cloud-optimized geoTIFF hosted in the cloud, so you don't need to download it.
flight <- read_csv("./Webinar3_Drone_Planning/data/Deer Creek Trail Demo Flight.csv")
dem <- raster("/vsicurl/https://rmbl-sdp.s3.us-east-2.amazonaws.com/data_products/released/release1/UER_dem_1m_v2.tif")
dem_proj <- proj4string(dem)

##Converts coordinates to same coordinate system as the DEM.
waypoint_coords <- cbind(flight$longitude,flight$latitude)
waypoint_sp <- SpatialPoints(coords=waypoint_coords,
                             proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
waypoint_proj <- spTransform(waypoint_sp,CRS(dem_proj))

##Samples raster at the UAV waypoints
waypoint_elev <- extract(x=dem,y=waypoint_proj,method="simple")

##Calculates the elevation relative to waypoint 1.
waypoint_relev <- waypoint_elev - waypoint_elev[1]

##Adds the flight height
flight_height <- 110
waypoint_height <- waypoint_relev + flight_height

##Subs in the new heights into the altitute field
flight$`altitude(m)` <- waypoint_height

##Exports back to .csv format.
write_csv(flight,file="./Webinar3_Drone_Planning/data/Deer Creek Trail Demo Flight - Terrain.csv")
