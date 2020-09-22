##Script to demo parallel processing of SDP Data Products stored 
##in the cloud.

##Sets up workspace
library(rgdal)
library(raster)
library(sp)
library(TileManager)
library(foreach)
library(progress)

##need to install Rtools before installing this package on windows.
library(doParallel)

##Project directory
proj_dir <- "~/code/SpatialDataScienceWebinars2020/"

##Sets working directory.
setwd(proj_dir)

##Sets raster options
rasterOptions(maxmemory=8e+9,memfrac=0.8)

##creates a raster object from a cloud-based source
##Get data frome with all available SDP data products.
sdp_prods <- read.csv("https://www.rmbl.org/wp-content/uploads/2020/06/SDP_product_table_6_8_2020.csv")
#View(sdp_prods)

##Creates raster objects from cloud-based datasets.
dem_uri <- as.character(sdp_prods$Data.URL[sdp_prods$Product=="Digital Elevation Model"])
dem_path <- paste("/vsicurl/",dem_uri,sep="")
dem <- raster(dem_path, progress='text')
dem

flow_uri <- as.character(sdp_prods$Data.URL[sdp_prods$Product=="Multi-direction Flow Accumulation"])
flow <- raster(paste("/vsicurl/",flow_uri,sep=""),
               progress='text')

##study area of interest
aoi_extent <- extent(matrix(c(325876,328251,
                                 4312941,4314552),
                               nrow=2,byrow=TRUE))

##downloads a local copy of the data for the area of interest
local_brick <- crop(stack(dem,flow),y=aoi_extent,progress='text')
names(local_brick) <- c("dem","flow")
inMemory(local_brick)


##computes a slope layer without tiling or parallelization.
##this is much faster for small areas.
start_time <- Sys.time()
dem_slope <- terrain(local_brick$dem,opt="slope",
                     progress="text")
end_time <- Sys.time()

total_elapsed <- end_time - start_time
total_elapsed

##set up the tiling scheme for processing larger areas.
tiles <- tileScheme(dem,tiledim=c(5000,5000),buffer=5)

#plot(dem,maxpixels=5000)
plot(tiles)

##create and register a parallel backend.
#cl <- makeCluster(parallel::detectCores()-2,outfile="")
cl <- makeCluster(detectCores()/2,outfile="")
registerDoParallel(cl)

##create a progress bar
#pb <- txtProgressBar(title = "Iterative training", min = 0, max = 30, style = 3)

##creates a parallel foreach loop to process each tile.
start_time <- Sys.time()
out_tiles <- foreach(i=1:length(tiles@buffs),
                     .packages=c("raster")) %dopar% {
  
  tile_extent <- extent(bbox(tiles@buffs[[i]]@Polygons[[1]]@coords))
  tile <- crop(dem,tile_extent)
  tile_slope <- terrain(tile,opt="slope",
                        progress="text")
  out_extent <- extent(bbox(tiles@nbuffs[[i]]@Polygons[[1]]@coords))
  tile_slope_crop <- crop(tile_slope,out_extent)
  
  (tile_slope_crop)
}
stopCluster(cl)

end_time <- Sys.time()

total_elapsed <- end_time - start_time
total_elapsed

##mosaics the resulting tiles.
out_tiles$ext <- aoi_extent
out_tiles$filename="dem_slope_merged.tif"
out_tiles$overwrite=TRUE
slope_merged <- do.call(merge,out_tiles)

plot(slope_merged)

