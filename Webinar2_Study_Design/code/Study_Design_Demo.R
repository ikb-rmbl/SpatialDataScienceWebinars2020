##Script to demo study design using SDP Data Products stored 
##in Amazon S3.

##Author: Ian Breckheimer
##Updated: 10-26-2020

## Installs packages
#install.packages(c("rgdal","raster","sf","ggplot2","ggspatial","rasterVis","gridExtra","lhs","pdist"))

##Sets up workspace
library(rgdal)
library(raster)
library(sf)
library(ggplot2)
library(ggspatial)
library(rasterVis)
library(gridExtra)
library(lhs)
library(pdist)

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

landcover_uri <- as.character(sdp_prods$Data.URL[sdp_prods$Product=="Basic Landcover"])
landcover_path <- paste("/vsicurl/",landcover_uri,sep="")
landcover <- raster(landcover_path,progress='text')
landcover

## Brings in a currently unreleased (beta) map of access time.
access_uri <- "https://rmbl-sdp.s3.us-east-2.amazonaws.com/data_products/draft/UER_access_time_minutes_1m_v2.tif"
access_path <- paste("/vsicurl/",access_uri,sep="")
access <- raster(access_path,progress='text')
access

## Brings in a currently unreleased (beta) map of public lands.
public_uri <- "https://rmbl-sdp.s3.us-east-2.amazonaws.com/data_products/draft/UER_USFS_BLM_lands_1m_v1.tif"
public_path <- paste("/vsicurl/",public_uri,sep="")
public <- raster(public_path,progress='text')
public

##Combines them into a raster stack.
study_stack <- stack(dem,landcover,access,public)
names(study_stack) <- c("dem","landcover","access_time","public")

##Gothic Townsite Extent
#plot(dem,maxpixels=5000)
#gothic_extent <- drawExtent()
gothic_extent <- extent(matrix(c(326983,328033,
                                 4313306,4314244),
                               nrow=2,byrow=TRUE))

##Subsets rasters to the area of interest.
gothic_stack <- crop(study_stack, gothic_extent, filename=tempfile(), 
                     progress="text")

##Computes slope and aspect rasters.
gothic_stack$slope <- terrain(gothic_stack$dem,
                              opt="slope",unit="degrees")
gothic_stack$aspect <- terrain(gothic_stack$dem,
                               opt="aspect",unit="degrees")

##Defines criteria for suitable sampling sites.
lc_suitable <- calc(gothic_stack$landcover,
                    fun=function(x){x==1|x==2}) #landcover code 1 is "evergreen forest", code 2 is "deciduous forest" 
slope_suitable <- calc(gothic_stack$slope,
                       fun=function(x){x < 45.0}) #slopes > 45 deg. are unsafe
public_suitable <- calc(gothic_stack$public,
                        fun=function(x){x==1}) #most private lands off-limits
access_suitable <- calc(gothic_stack$access_time,
                        fun=function(x){x < 30}) #more than 30 minutes to access impractical
gothic_stack$suitable <- lc_suitable * slope_suitable * public_suitable
plot(gothic_stack$suitable)

##Takes a large random sample of pixels for reference.
set.seed(42)
reference_sample <- sampleRandom(gothic_stack,size=100000,xy=TRUE,sp=TRUE,)
reference_sample@data$suitable <- as.factor(reference_sample@data$suitable)
summary(reference_sample@data$suitable)

##Converts spatial data to sf format.
reference_sf <- st_as_sf(reference_sample)

##Looks at how potentially suitable sites differ from the full distribution.
ggplot(reference_sf)+
  geom_violin(aes(y=dem,x=suitable,color=suitable),
              draw_quantiles=c(0.1,0.5,0.9),
              trim=TRUE)+
  theme_bw()

ggplot(reference_sf)+
  geom_violin(aes(y=slope,x=suitable,color=suitable),
              draw_quantiles=c(0.1,0.5,0.9),
              trim=TRUE)+
  theme_bw()

##Simple random sampling of all suitable sites.
simple_sample <- dplyr::filter(reference_sf,suitable==1) %>%
                    dplyr::sample_n(size=100)
summary(as.factor(simple_sample$landcover))

##Stratified sample with equal numbers in different landcover categories
stratified_sample <- dplyr::filter(reference_sf,suitable==1) %>%
                        dplyr::group_by(landcover) %>%
                        dplyr::sample_n(size=50)

##Plots both designs.
simple_plot <- gplot(gothic_stack$landcover,maxpixels=500000)+
                      geom_raster(aes(fill=as.factor(value)),interpolate=FALSE)+
                      layer_spatial(simple_sample)+
                      scale_fill_discrete("LC Class")+
                      ggtitle("Simple Random Sampling, n=100")+
                      scale_x_continuous("")+
                      scale_y_continuous("")+
                      coord_sf(expand=0, label_axes="--EN") +
                      theme_bw()

stratified_plot <- gplot(gothic_stack$landcover,maxpixels=500000)+
                      geom_raster(aes(fill=as.factor(value)),interpolate=FALSE)+
                      layer_spatial(stratified_sample,aes(shape=as.factor(landcover)))+
                      scale_fill_discrete("LC Class")+
                      scale_shape_discrete("LC Class")+
                      ggtitle("Stratified Random Sampling, n=100")+
                      scale_x_continuous("")+
                      scale_y_continuous("")+
                      coord_sf(expand=0, label_axes="--EN") +
                      theme_bw()                      

#### Maximizing coverage of environmental gradients with latin hypercube sampling.
library(lhs)

## We have 100 levels of 4 variables in the hypercube.
lhs_values <- as.data.frame(randomLHS(100,4))
colnames(lhs_values) <- c("elevation","aspect","x","y")
lhs_values

## Loop through each row and find the site with the smallest 
## euclidean distance to the 100 hypercube points.
suitable_sites <- dplyr::filter(reference_sf,suitable==1)
suitable_sub <- cbind(suitable_sites$dem,suitable_sites$aspect,
                      suitable_sites$x,suitable_sites$y)

## Rescales each variable to the interval 0,1.
rescale_fun <- function(x){(x - min(x))/(max(x)-min(x))}
suitable_scale <- apply(suitable_sub,FUN=rescale_fun,MARGIN=2)

## Creates a data frame with the right shape
lhs_sample <- suitable_sites[1:100,]

for(i in 1:nrow(lhs_trans)){
  print(paste("Finding most similar point for hypercube row",i))
  distances <- pdist(X=suitable_scale,Y=lhs_values[i,])
  lhs_sample[i,] <- suitable_sites[which.min(distances@dist),]
}

##Plots latin hypercube design.
lhs_plot <- gplot(gothic_stack$landcover,maxpixels=500000)+
  geom_raster(aes(fill=as.factor(value)),interpolate=FALSE)+
  layer_spatial(lhs_sample,aes(shape=as.factor(landcover)))+
  scale_fill_discrete("LC Class")+
  scale_shape_discrete("LC Class")+
  ggtitle("Latin Hypercube Sampling, n=100")+
  scale_x_continuous("")+
  scale_y_continuous("")+
  coord_sf(expand=0, label_axes="--EN") +
  theme_bw() 
lhs_plot

##Compares the three designs.
grid.arrange(simple_plot,stratified_plot,lhs_plot,ncol=3)

####Writes the three sampling designs to disk as geopackages.
write_sf(simple_sample,dsn="./Webinar2_Study_Design/output/simple_samples_n100.gpkg")
write_sf(stratified_sample,dsn="./Webinar2_Study_Design/output/stratified_samples_n100.gpkg")
write_sf(lhs_sample,dsn="./Webinar2_Study_Design/output/lhs_samples_n100.gpkg")

####Assesses the representativeness and coverage of each design compared to the sampling frame.

##Kolmogorov's D
simple_ks_elev <- ks.test(simple_sample$dem,suitable_sites$dem)
simple_ks_elev
simple_ks_asp <- ks.test(simple_sample$aspect,suitable_sites$aspect)

stratified_ks_elev <- ks.test(stratified_sample$dem,suitable_sites$dem)
stratified_ks_asp <- ks.test(stratified_sample$aspect,suitable_sites$aspect)

lhs_ks_elev <- ks.test(lhs_sample$dem,suitable_sites$dem)
lhs_ks_asp <- ks.test(lhs_sample$aspect,suitable_sites$aspect)

d_elev <- c(simple=simple_ks_elev$statistic,
            stratified=stratified_ks_elev$statistic,
            latin_hypercube=lhs_ks_elev$statistic)

d_asp <- c(simple=simple_ks_asp$statistic,
            stratified=stratified_ks_asp$statistic,
            latin_hypercube=lhs_ks_asp$statistic)

##Compare quantiles to assess feature coverage.
suitable_quantiles_elev <- quantile(suitable_sites$dem,prob=c(0.01,0.05,0.5,0.95,0.99))
suitable_quantiles_elev
simple_quantiles_elev <- quantile(simple_sample$dem,prob=c(0.01,0.05,0.5,0.95,0.99))
simple_quantiles_elev
stratified_quantiles_elev <- quantile(stratified_sample$dem,prob=c(0.01,0.05,0.5,0.95,0.99))
stratified_quantiles_elev
lhs_quantiles_elev <- quantile(lhs_sample$dem,prob=c(0.01,0.05,0.5,0.95,0.99))
lhs_quantiles_elev

##Plot to assess coverage.
simple_feature_plot <- ggplot(suitable_sites)+
                        geom_point(aes(x=dem,y=aspect),size=0.1,color="grey80")+
                        geom_point(aes(x=dem,y=aspect),size=1,color="slateblue",
                                   data=simple_sample)+
                        ggtitle("Simple Random Sampling,n=100")+
                        theme_bw()
lhs_feature_plot <- ggplot(suitable_sites)+
                        geom_point(aes(x=dem,y=aspect),size=0.1,color="grey80")+
                        geom_point(aes(x=dem,y=aspect),size=1,color="slateblue",
                                   data=lhs_sample)+
                        ggtitle("Latin Hypercube Sampling,n=100")+
                        theme_bw()
grid.arrange(simple_feature_plot,lhs_feature_plot,ncol=2)
