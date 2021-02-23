##Demo script for point cloud processing using lidR

##Sets up workspace
##install.packages(c("lidR","raster","rgdal","sf","future"))
library(lidR)
library(raster)
library(rgdal)
library(sf)
library(future)
setwd("~/code/SpatialDataScienceWebinars2020")

####Basic processing demo.

##Loads point clouds
lidar_tile <- readLAS("./Webinar4_Point_Clouds/data/LiDAR_tiles/Gothic_LiDAR_pointcloud_2019_tile1.laz")
sfm_tile <- readLAS("./Webinar4_Point_Clouds/data/SfM_tiles/Gothic_SfM_pointcloud_2019_tile1.laz")
summary(lidar_tile)
summary(sfm_tile)

##Note that this is a subsampled version of the SfM cloud, the original is more than 300 pts/m2

##Visualizes LiDAR cloud with rgl.
#plot(lidar_tile, color = "Intensity", bg = "white", axis = TRUE, legend = TRUE)

##Visualizes SfM cloud (slow)
#plot(sfm_tile, color = "RGB", bg = "white", axis = TRUE)

##Classifies ground points using a CSF filter.
sfm_classed <- classify_ground(sfm_tile,last_returns=FALSE, 
                               algorithm=csf(class_threshold=0.01,
                                             cloth_resolution=0.7,
                                             rigidness=3))
##Plots ground points.
#plot(sfm_classed,color="Classification")

##Interpolates ground points to create a smooth DEM.
sfm_dem <- grid_terrain(sfm_classed,use_class=2,res=0.5,
                        algorithm=tin())

#plot(sfm_dem)

##This is a raster object, so we can write it to disk and open in a GIS program.
writeRaster(sfm_dem,"./Webinar4_Point_Clouds/data/output_DEM.tif",overwrite=TRUE)

##Plots DEM triangulation.
#plot_dtm3d(sfm_dem, bg = "white",axis=TRUE) 

##Subtracts ground elevation to get a normalized cloud.
sfm_norm <- normalize_height(sfm_classed,algorithm=tin())

##Plots normalized cloud
#plot(sfm_norm,color="Z", bg = "white",axis=TRUE,legend=TRUE)

##Creates a vegetation index to filter out structures.
sfm_norm@data$GCC <- sfm_norm@data$G / 
                    (sfm_norm@data$R + sfm_norm@data$G + sfm_norm@data$B)
#plot(sfm_norm,color="GCC",bg="white",axes=TRUE,legend=TRUE,trim=0.4)

sfm_norm <- filter_poi(sfm_norm,GCC > 0.44)

##Generates a pit-free digital surface model (takes a minute).
ht_thresholds <- c(0,0.5,1,2,5,10,15,20,25,30,40)
sfm_chm <- grid_canopy(sfm_norm,res=0.5,
                       algorithm=pitfree(thresholds = ht_thresholds,
                                          max_edge = c(0,1)))

##Finds putative tree tops.
#function for local maximum filter.
f <- function(x) {
  y <- 1.4 * (-(exp(-0.11*(x-2)) - 1)) + 2.2
  y[x < 0.5] <- 1.9
  y[x > 25] <- 3.5
  return(y)
}

heights <- seq(-5,30,0.5)
ws <- f(heights)
#plot(heights, ws, type = "l",  ylim = c(0,6))

sfm_ttops <- find_trees(sfm_norm,
                        algorithm=lmf(ws=f,hmin=1,shape="circular"))

##Visualizes on top of the CHM.
#plot(sfm_dsm, col = height.colors(50))
#plot(sfm_ttops, add = TRUE)

##Segments the DSM using the itcSegment algorithm (Dalponte 2016).
seg_alg <- dalponte2016(sfm_chm, sfm_ttops,th_tree=0.5,
                        th_seed=0.35,th_cr=0.5,max_cr=10)
sfm_seg <- segment_trees(las=sfm_norm,algorithm=seg_alg)

##Plots segmentation.
#plot(sfm_seg, bg = "white", size = 4, color = "treeID")

##Calculates individual tree metrics from point cloud.
custom_tree_metrics <- function(z, gcc) { # user-defined function
  metrics <- list(
    z_max = max(z),     # max height
    z_sd = sd(z),  # vertical variability of points
    GCC_mean=mean(gcc), # mean GCC
    GCC_sd=sd(gcc) ## GCC sd
  )
  return(metrics) # output
}

crown_metrics <- delineate_crowns(sfm_seg, 
                                   func = ~custom_tree_metrics(z = Z, 
                                                               gcc = GCC))
#spplot(crowns_metrics, "GCC_mean", col.regions = hcl.colors(30)) 
st_write(st_as_sf(crown_metrics),dsn="./Webinar4_Point_Clouds/data/crown_polygons.gpkg")

####Tiled processing demo (for clouds that can't fit in memory. #####

lidar_catalog <- readLAScatalog("./Webinar4_Point_Clouds/data/LiDAR_tiles/")
summary(lidar_catalog)
#plot(lidar_catalog)

##Catalog engine processes by "chunks", which might not be the same as the underlying
##files.
opt_chunk_size(lidar_catalog) <- 100
opt_chunk_buffer(lidar_catalog) <- 10
opt_output_files(lidar_catalog) <- "./Webinar4_Point_Clouds/data/raster_tiles/chunk_{ID}"

##Sets catalog to overwrite outputs by default.
lidar_catalog@output_options$drivers$Raster$param$overwrite <- TRUE

##Most lidar functions work on catalogs, processing by chunk.
start_time <- Sys.time()
dem <- grid_terrain(lidar_catalog,res=0.5,algorithm=tin())
end_time <- Sys.time()
print(paste("Processing time", end_time - start_time, "seconds"))

##You can set up the catalog engine to process chunks in parallel using the plan() function.
start_time <- Sys.time()
plan("future::multisession", workers = 2L)
dem <- grid_terrain(lidar_catalog,res=0.5,algorithm=tin())
plan("future::sequential")
end_time <- Sys.time()
print(paste("Processing time", end_time - start_time, "seconds"))

##By default, the tiles are merged into a single virtual raster.
dem
plot(dem)

