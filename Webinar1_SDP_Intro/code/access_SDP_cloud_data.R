##Script to demo raster remote access to SDP Data Products stored 
##in Amazon S3.

##Author: Ian Breckheimer
##Updated: 9-20-2020

## Installs packages
#install.packages(c("rgdal","raster","sf","ggplot2","ggspatial","rasterVis","gridExtra"))

##Sets up workspace
library(rgdal)
library(raster)
library(sf)
library(ggplot2)
library(ggspatial)
library(rasterVis)
library(gridExtra)

##Project directory
proj_dir <- "~/code/SpatialDataScienceWebinars2020/"

##Sets working directory.
setwd(proj_dir)

##Check GDAL version (should work with GDAL 2.2.3 or later).
getGDALVersionInfo()

##Get data frome with all available SDP data products.
sdp_prods <- read.csv("https://www.rmbl.org/wp-content/uploads/2020/06/SDP_product_table_6_8_2020.csv")
View(sdp_prods)

##Creates raster objects from cloud-based datasets.
dem_uri <- as.character(sdp_prods$Data.URL[sdp_prods$Product=="Digital Elevation Model"])
dem_path <- paste("/vsicurl/",dem_uri,sep="")
dem <- raster(dem_path, progress='text')
dem

flow_uri <- as.character(sdp_prods$Data.URL[sdp_prods$Product=="Multi-direction Flow Accumulation"])
flow_path <- paste("/vsicurl/",flow_uri,sep="")
flow <- raster(flow_path,progress='text')
flow

water_uri <- as.character(sdp_prods$Data.URL[sdp_prods$Product=="Surface Water Cover"])
water_path <- paste("/vsicurl/",water_uri,sep="")
water <- raster(water_path,progress='text')
water

##Combines them into a raster stack.
dem_flow <- stack(dem,flow,water)
names(dem_flow) <- c("dem","flow","water")

##Gothic Townsite Extent
#plot(dem,maxpixels=5000)
#gothic_extent <- drawExtent()
gothic_extent <- extent(matrix(c(326983,328033,
                                 4313306,4314244),
                               nrow=2,byrow=TRUE))

##Subsets rasters to the area of interest.
gothic_stack <- crop(dem_flow, gothic_extent, filename=tempfile(), 
                   progress="text")

##computes derived functions of subset maps, adding output to stack
gothic_stack$slope <- terrain(gothic_stack$dem,opt="slope",
                              progress="text",filename=tempfile())
gothic_stack$aspect <- terrain(gothic_stack$dem,opt="aspect",
                              progress="text",filename=tempfile())

##Doing arbitrary arithmetic on raster layers
##does raster algebra, creating a new raster map in memory if it's small
gothic_stack$flow_log <- log(gothic_stack$flow)
inMemory(gothic_stack$flow_log)

##plots all layers
plot(gothic_stack)

##samples values at random points where all layers have data
gothic_samples <- sampleRandom(gothic_stack,size=30,
                               sp=TRUE,na.rm=TRUE)

##computes a hillshade layer for plotting
gothic_stack$hillshade <- hillShade(slope=gothic_stack$slope,
                                    aspect=gothic_stack$aspect,
                                    filename=tempfile())
plot(gothic_stack$hillshade,)
points(gothic_samples)

##resamples rasters to a coarser resolution
template_100m <- raster(crs=proj4string(gothic_stack),ext=extent(gothic_stack),resolution=100)
dem_100m <- resample(gothic_stack$dem,y=template_100m,method="bilinear")
slope_100m <- resample(gothic_stack$slope,y=template_100m, method="bilinear")
flow_log_100m <- resample(gothic_stack$flow_log,y=template_100m,method="bilinear")
coarse_stack <- stack(dem_100m,slope_100m,flow_log_100m)
names(coarse_stack) <- c("dem_100m","slope_100m","flow_log_100m")

##Plots resampled data.
plot(coarse_stack)

##samples values with the resampled data.
gothic_samples <- extract(coarse_stack,gothic_samples,method="bilinear",sp=TRUE)

##adds coordinates to data frame.
gothic_samples$east <- coordinates(gothic_samples)[,1]
gothic_samples$north <- coordinates(gothic_samples)[,2]

##plots original vs resampled data.
p1 <- ggplot(gothic_samples@data)+
  geom_point(aes(x=dem,y=dem_100m))+
  geom_abline(aes(intercept=0,slope=1))+
  ggtitle("Elevation")+
  xlab("1m-Resolution DEM")+
  ylab("100m-Resolution DEM")+
  theme_bw()

p2 <- ggplot(gothic_samples@data)+
  geom_point(aes(x=slope,y=slope_100m))+
  geom_abline(aes(intercept=0,slope=1))+
  ggtitle("Slope")+
  xlab("1m-Resolution Slope")+
  ylab("100m-Resolution Slope")+
  theme_bw()

p3 <- ggplot(gothic_samples@data)+
  geom_point(aes(x=flow_log,y=flow_log_100m))+
  geom_abline(aes(intercept=0,slope=1))+
  ggtitle("Log Flow Accum.")+
  xlab("1m-Resolution Flow")+
  ylab("100m-Resolution Flow")+
  theme_bw()

gridExtra::grid.arrange(p1,p2,p3,ncol=3)


##converts water data to polygons (this is slow).
water_poly <- st_as_sf(rasterToPolygons(gothic_stack$water))
flow_poly <- st_as_sf(rasterToPolygons(gothic_stack$flow_log,fun=function(x){x>11}))

##plots the hillshade and adds sampling points
plot(gothic_stack$hillshade,ext=gothic_extent,
     main="Gothic Sample Points",
     col=hcl.colors(n=50,palette="Grays",rev=FALSE))
plot(water_poly,add=TRUE,col="slateblue")
plot(flow_poly,add=TRUE,col="slateblue")
points(gothic_samples,pch="+",cex=1,col="white")

##makes a prettier plot with ggplot2, rasterVis, and ggspatial
gplot <- gplot(gothic_stack$hillshade,maxpixels=500000)+
  geom_raster(aes(fill=value),interpolate=TRUE)+
  scale_fill_gradient(low = 'black', high = 'white',
                      guide = guide_none()) +
  layer_spatial(water_poly,aes(color="water"))+
  layer_spatial(flow_poly,color="slateblue")+
  geom_point(aes(x=east,y=north,color="sites"),
             data=gothic_samples@data,
             shape="+",size=5)+
  scale_color_manual("Legend", guide = "legend",
                     values = c("water" = "slateblue",
                                "sites" = "white")) +
  scale_x_continuous("")+
  scale_y_continuous("")+
  coord_sf(expand=0, label_axes="--EN") +
  annotation_scale(style="ticks",width_hint=0.1,location="br",
                   tick_height=0,text_col="white",line_col="white")+
  guides(color = guide_legend(override.aes = list(fill=c("grey80","slateblue"),
                                                  linetype = c(0, 0),
                                                  shape = c(43, 15))))+
  theme_bw()+
  theme(legend.position=c(0.85,0.85))

##Writes plot to disk.
png("stream_sample_map.png",width=6,height=8,units="in",res=300)
gplot
dev.off()

##Writes subset of raster data to disk.
writeRaster(gothic_stack,filename="Gothic_Townsite_Rasters.tif",
            progress='text')
