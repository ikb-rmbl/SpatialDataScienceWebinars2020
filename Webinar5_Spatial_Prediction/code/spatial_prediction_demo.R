####Script to demo spatial prediction using machine learning.
####Author:Ian Breckheimer
####Updated: 29 March 2021

#### Set up workspace ####
install.packages(c("raster","rgdal","sf","readr",
                   "dplyr","tibble","tidymodels",
                   "parsnip","tidymodels","keras",
                   "xgboost","kernlab","mgcv",
                   "foreach","future","doFuture"))

#### Assemble and extract features ####
library(raster)
library(sf)

##Loads training points.
train_points <- st_read("./Webinar5_Spatial_Prediction/data/UG_decid_labels.gpkg")
plot(train_points)

##makes a raster stack of cloud-based datasets representing predictors/features.
sdp_web_path <- "/vsicurl/https://rmbl-sdp.s3.us-east-2.amazonaws.com/data_products/"

ndvi2017 <- raster(paste(sdp_web_path,"draft/UG_ndvi_oct2017_1m_v1.tif",sep=""))
rgbn2017 <- brick(paste(sdp_web_path,"draft/UG_rgbn_oct2017_1m_v1.tif",sep=""))
ndvi2019 <- raster(paste(sdp_web_path,"draft/UG_ndvi_sept2019_1m_v2.tif",sep=""))
rgbn2019 <- brick(paste(sdp_web_path,"draft/UG_rgbn_sept2019_1m_v1.tif",sep=""))
cht2019 <- raster(paste(sdp_web_path,"draft/UG_canopy_height_1m_v2.tif",sep=""))
elev <- raster(paste(sdp_web_path,"draft/UG_dem_1m_v1.tif",sep=""))

predictors <- stack(ndvi2017,rgbn2017,ndvi2019,rgbn2019,cht2019,elev)
predictors

##Extract values at the points (takes a while).
#point_features <- extract(predictors,as(train_points,"Spatial"),
#                          method="simple")
#decid_labeled <- cbind(train_points,point_features)

##Impatient folks can load the data that's already extracted.
decid_labeled <- st_read("./Webinar5_Spatial_Prediction/data/UG_decid_labels_features.gpkg")

#### Fit a baseline model ####
library(mgcv)

##Does some feature engineering to create brightness-normalized colors.
plot(train_points$UG_rgbn_oct2017_1m_v1.1,
     train_points$UG_rgbn_oct2017_1m_v1.2,
     xlab="Red 2017",ylab="Green 2017",pch=".")

decid_labeled$Brightness_2019 <- rowSums(cbind(decid_labeled$UG_rgbn_sept2019_1m_v1.1,
                                               decid_labeled$UG_rgbn_sept2019_1m_v1.2,
                                               decid_labeled$UG_rgbn_sept2019_1m_v1.3,
                                               decid_labeled$UG_rgbn_sept2019_1m_v1.4))/(255*4)

decid_labeled$Brightness_2017 <- rowSums(cbind(decid_labeled$UG_rgbn_oct2017_1m_v1.1,
                                               decid_labeled$UG_rgbn_oct2017_1m_v1.2,
                                               decid_labeled$UG_rgbn_oct2017_1m_v1.3,
                                               decid_labeled$UG_rgbn_oct2017_1m_v1.4))/(255*4)

decid_labeled$Red_norm_2017 <- decid_labeled$UG_rgbn_oct2017_1m_v1.1 / (decid_labeled$Brightness_2017 * 255 * 4)
decid_labeled$Green_norm_2017 <- decid_labeled$UG_rgbn_oct2017_1m_v1.2 / (decid_labeled$Brightness_2017 * 255 * 4)
decid_labeled$Blue_norm_2017 <- decid_labeled$UG_rgbn_oct2017_1m_v1.3 / (decid_labeled$Brightness_2017 * 255 * 4)
decid_labeled$NIR_norm_2017 <- decid_labeled$UG_rgbn_oct2017_1m_v1.4 / (decid_labeled$Brightness_2017 * 255 * 4)

decid_labeled$Red_norm_2019 <- decid_labeled$UG_rgbn_sept2019_1m_v1.1 / (decid_labeled$Brightness_2019 * 255 * 4)
decid_labeled$Green_norm_2019 <- decid_labeled$UG_rgbn_sept2019_1m_v1.2 / (decid_labeled$Brightness_2019 * 255 * 4)
decid_labeled$Blue_norm_2019 <- decid_labeled$UG_rgbn_sept2019_1m_v1.3 / (decid_labeled$Brightness_2019 * 255 * 4)
decid_labeled$NIR_norm_2019 <- decid_labeled$UG_rgbn_sept2019_1m_v1.4 / (decid_labeled$Brightness_2019 * 255 * 4)

##splits into training and testing sets.
##Splits into training, testing and validation.
set.seed(42)
split_id <- runif(nrow(decid_labeled),0,1)
decid_train <- decid_labeled[split_id <= 0.7,]
decid_test <- decid_labeled[split_id > 0.7,]

##Fits a GAM model.
decid_gam <- gam(Train_Decid ~ s(Red_norm_2017,Red_norm_2019)+
                   s(Green_norm_2017,Green_norm_2019)+
                   s(Blue_norm_2017,Blue_norm_2019)+
                   s(UG_ndvi_oct2017_1m_v1,UG_ndvi_sept2019_1m_v2)+
                   s(Brightness_2017,Brightness_2019)+
                   s(UG_canopy_height_1m_v2),
                 family="binomial",data=decid_train)
summary(decid_gam)

##Writes the model to disk.
saveRDS(decid_gam,"./Webinar5_Spatial_Prediction/output/gam_fit.RDS")

##Predicts on training and held-out data.
decid_test$gam_pred <- predict(decid_gam,newdata=decid_test,type="response")
decid_test$gam_class <- decid_test$gam_pred > 0.5
decid_train$gam_pred <- predict(decid_gam,newdata=decid_train,type="response")
decid_train$gam_class <- decid_train$gam_pred> 0.5

##Views predicted values and labels.
par(mfrow=c(1,2))
plot(decid_train$gam_pred,jitter(as.numeric(decid_train$Train_Decid),amount=0.1),
     main="Training",xlab="Prediction",ylab="True Class",col=decid_train$gam_class+1)
plot(decid_test$gam_pred,jitter(as.numeric(decid_test$Train_Decid),amount=0.1),
     main="Testing",xlab="Prediction",ylab="True Class",col=decid_test$gam_class+1)

##Computes confusion matrix and accuracy.
evaluate_accuracy <- function(conf_mat){
  acc <- (conf_mat[1,1] + conf_mat[2,2]) / sum(conf_mat)
  tpr <- conf_mat[2,2] / sum(conf_mat[2,])
  tnr <- conf_mat[1,1] / sum(conf_mat[1,])
  fpr <- conf_mat[1,2] / sum(conf_mat[2,])
  fnr <- conf_mat[2,1] / sum(conf_mat[1,])
  return(data.frame("Accuracy"=acc,
                    "TruePositiveRate"=tpr,
                    "TrueNegativeRate"=tnr,
                    "FalsePositiveRate"=fpr,
                    "FalseNegativeRate"=fnr))
}

gam_train_mat <- table(decid_train$Train_Decid,
                       decid_train$gam_class)
gam_test_mat <- table(decid_test$Train_Decid,
                       decid_test$gam_class)

evaluate_accuracy(gam_train_mat)
evaluate_accuracy(gam_test_mat)

#### Fit, tune, and assess multiple models ####
library(sf)
library(readr)
library(tidymodels)
library(parsnip)
library(xgboost)
library(kernlab)

##loads data (again).
decid_labeled <- st_read("./Webinar5_Spatial_Prediction/data/UG_decid_labels_features.gpkg")

##Drops geometry column.
st_geometry(decid_labeled) <- NULL

##Makes sure the response if a factor.
decid_labeled$Train_Decid <- factor(decid_labeled$Train_Decid)

##Splits into a training and testing set.
set.seed(42)
train_test_split <- initial_split(decid_labeled,prop=0.8)
decid_train <- training(train_test_split)
decid_test <- testing(train_test_split)
write_csv(decid_test,"./Webinar5_Spatial_Prediction/data/decid_validation_data.csv")

##Sets up a "recipe" for data transformation / feature engineering.
decid_rec <- recipe(decid_train) %>%
  update_role(Train_Decid,new_role="outcome") %>%
  #update_role(ID, new_role="ID") %>%
  step_mutate(brightness_2017=(UG_rgbn_oct2017_1m_v1.1 + 
                                 UG_rgbn_oct2017_1m_v1.2 +
                                 UG_rgbn_oct2017_1m_v1.3 +
                                 UG_rgbn_oct2017_1m_v1.4)/(255*4)) %>%
  step_mutate(Pred_red2017_norm=UG_rgbn_oct2017_1m_v1.1 / (brightness_2017 * 255 * 4),
              Pred_grn2017_norm=UG_rgbn_oct2017_1m_v1.2 / (brightness_2017 * 255 * 4),
              Pred_blue2017_norm=UG_rgbn_oct2017_1m_v1.3 / (brightness_2017 * 255 * 4)) %>%
  step_mutate(brightness_2019=(UG_rgbn_sept2019_1m_v1.1 + 
                                 UG_rgbn_sept2019_1m_v1.2 +
                                 UG_rgbn_sept2019_1m_v1.3 +
                                 UG_rgbn_sept2019_1m_v1.4)/(255*4)) %>%
  step_mutate(Pred_red2019_norm=UG_rgbn_sept2019_1m_v1.1 / (brightness_2019 * 255 * 4),
              Pred_grn2019_norm=UG_rgbn_sept2019_1m_v1.2 / (brightness_2019 * 255 * 4),
              Pred_blue2019_norm=UG_rgbn_sept2019_1m_v1.3 / (brightness_2019 * 255 * 4)) %>%
  #step_rm(contains("brightness_"))%>%
  step_normalize(contains("UG_")) %>%
  step_normalize(contains("Pred_")) %>%
  update_role(contains("UG_ndvi"),new_role="predictor") %>%
  update_role(contains("UG_canopy"),new_role="predictor") %>%
  update_role(contains("Pred_"),new_role="predictor") %>%
  prep(training = decid_train, retain = TRUE)

##Does this for the test data.
train_baked <- bake(decid_rec,new_data = decid_train, all_outcomes(), 
                    all_predictors())
test_baked <- bake(decid_rec, new_data = decid_test, all_outcomes(), 
                   all_predictors())

##Tunes a boosted regression tree model with xgboost.
set.seed(57974)

##Creates a tuning spec.
xgb_tunespec <- boost_tree(trees=100,
                           mtry=tune(),
                           min_n=tune(),
                           learn_rate=tune(),
                           tree_depth=tune(),
                           loss_reduction=0,
                           sample_size=1) %>%
  set_mode("classification") %>% 
  set_engine("xgboost", verbose = 1)

##Sets up grid search.
xgb_grid <- grid_regular(mtry(range=c(2,4)),
                         learn_rate(range=c(-2.3,-0.5)),
                         tree_depth(range=c(6,8)),
                         min_n(range=c(1,3)),
                         levels=3)

##Sets up cross-validation (v sets the number of "folds" for cv, should be > 5
##for a real project.
set.seed(57974)
xgb_folds <- vfold_cv(data=train_baked,v=5)

##Sets up tuning workflow.
xgb_tune_wf <- workflow() %>%
  add_model(xgb_tunespec) %>%
  add_formula(Train_Decid ~ .)

#Tunes hyperparameters (takes a few minutes).
xgb_tune_res <- xgb_tune_wf %>%
 tune_grid(resamples=xgb_folds,
           grid=xgb_grid,
           control(verbose = TRUE))

#Collects performance metrics
xgb_tune_metrics <- xgb_tune_res %>%
 collect_metrics()
xgb_tune_best <- xgb_tune_res %>% select_best("accuracy")

#Finalizes tuned workflow.
xgb_final_wf <- xgb_tune_wf %>%
 finalize_workflow(xgb_tune_best)

#Fits tuned model to training data.
xgb_final_fit <- xgb_final_wf %>%
 fit(data=train_baked)

##Writes model to an R object.
saveRDS(xgb_final_fit,"./output/decid_xgb_trained.RDS")
#xgb_final_fit <- readRDS("./output/decid_xgb_trained.RDS")

## XGB Training results
xgb_tr_results <- 
  decid_train %>%
  bind_cols(
    predict(xgb_final_fit, new_data = train_baked),
    predict(xgb_final_fit, new_data = train_baked, type = "prob")
  )
#tr_results %>% slice(1:5)
xgb_tr_results %>% roc_auc(truth = Train_Decid, .pred_FALSE)
xgb_tr_results %>% accuracy(truth = Train_Decid, .pred_class)
xgb_tr_results %>% conf_mat(truth = Train_Decid, .pred_class)

##Does the same with support vector machine model.
set.seed(57974)

##Creates a tuning spec.
svm_tunespec <- svm_rbf(cost=tune(),
                        rbf_sigma=tune()) %>%
  set_mode("classification") %>% 
  set_engine("kernlab", verbose = 0)

##Sets up grid search.
svm_grid <- grid_regular(cost(range=c(-3,3)),
                         rbf_sigma(range=c(-3,0)),
                         levels=5)

##Sets up cross-validation.
set.seed(57974)
svm_folds <- vfold_cv(data=train_baked,v=5)

##Sets up tuning workflow.
svm_tune_wf <- workflow() %>%
  add_model(svm_tunespec) %>%
  add_formula(Train_Decid ~ .)

#Tunes hyperparameters (takes about 20 min)
svm_tune_res <- svm_tune_wf %>%
 tune_grid(resamples=svm_folds,
           grid=svm_grid)

#Collects performance metrics
svm_tune_metrics <- svm_tune_res %>%
 collect_metrics()
svm_tune_best <- svm_tune_res %>% select_best("accuracy")

#Finalizes tuned workflow.
svm_final_wf <- svm_tune_wf %>%
 finalize_workflow(svm_tune_best)

#Fits tuned model to training data.
svm_final_fit <- svm_final_wf %>%
 fit(data=train_baked)

##Writes model to an R object.
#saveRDS(svm_final_fit,"./output/decid_svm_trained.RDS")
svm_final_fit <- readRDS("./output/decid_svm_trained.RDS")

## SVM Training results
svm_tr_results <- 
  decid_train %>%
  bind_cols(
    predict(svm_final_fit, new_data = train_baked),
    predict(svm_final_fit, new_data = train_baked, type = "prob")
  )
#tr_results %>% slice(1:5)
svm_tr_results %>% roc_auc(truth = Train_Decid, .pred_FALSE)
svm_tr_results %>% accuracy(truth = Train_Decid, .pred_class)
svm_tr_results %>% conf_mat(truth = Train_Decid, .pred_class)

#### Validate models ####

## XGB Validation results
xgb_val_results <- 
  decid_test %>%
  bind_cols(
    predict(xgb_final_fit, new_data = test_baked),
    predict(xgb_final_fit, new_data = test_baked, type = "prob")
  )
#val_results %>% slice(1:5)
xgb_val_results %>% roc_auc(truth = Train_Decid, .pred_FALSE)
xgb_val_results %>% accuracy(truth = Train_Decid, .pred_class)
xgb_val_results %>% conf_mat(truth = Train_Decid, .pred_class)

svm_val_results <- 
  decid_test %>%
  bind_cols(
    predict(svm_final_fit, new_data = test_baked),
    predict(svm_final_fit, new_data = test_baked, type = "prob")
  )
#val_results %>% slice(1:5)
svm_val_results %>% roc_auc(truth = Train_Decid, .pred_FALSE)
svm_val_results %>% accuracy(truth = Train_Decid, .pred_class)
svm_val_results %>% conf_mat(truth = Train_Decid, .pred_class)

#### Large-scale prediction ####
library(raster)
library(TileManager)
library(foreach)
library(progress)
library(doParallel)
library(RhpcBLASctl)

##Loads the tuned models from the previous step.
gam_final_fit <- readRDS("./Webinar5_Spatial_Prediction/output/gam_fit.RDS")
xgb_final_fit <- readRDS("./Webinar5_Spatial_Prediction/output/gam_fit.RDS")
svm_final_fit <- readRDS("./Webinar5_Spatial_Prediction/output/gam_fit.RDS")

##Loads the extent of the predictors and selects a small test area.
ug_bound_rast <- raster("/vsicurl/https://rmbl-sdp.s3.us-east-2.amazonaws.com/data_products/draft/UG_mask_1m_v1.tif")
plot(ug_bound_rast,maxpixels=10000)
test_ext <- drawExtent()
test_rast <- crop(ug_bound_rast,test_ext)

##Sets up tiles for large-scale processing.
tiles <- tileScheme(test_rast,tiledim=c(500,500),cells=TRUE,buffer=0)

##Set the number of threads for linear algebra (fastest is usually 1)
threads <- 1

##create and register a parallel backend.
#cl <- makeCluster(parallel::detectCores()-2,outfile="")
cl <- makeCluster(4,outfile="")
registerDoParallel(cl)
overwrite_output <- FALSE
start_time <- Sys.time()
out_tiles <- foreach(i=1:length(tiles@buffs),
                     #out_tiles <- foreach(i=1028:1056,
                     .packages=c("tidymodels","RhpcBLASctl","xgboost",
                                 "kernlab","mgcv","raster")) %dopar% {
                                   
   print(paste("Inference on tile",i,"of", length(tiles@buffs),"at",Sys.time()))  
   
   ##Sets environment and disables multithreading (avoids nested parallelization)                                                                
   #setwd("/home/rstudio/IanBreckheimer/Code/NAIP_deciduousness")
   blas_set_num_threads(threads)
   omp_set_num_threads(threads)
   
   ##grabs extent of tile.
   tile_extent <- raster::extent(bbox(tiles@buffs[[i]]@Polygons[[1]]@coords))
   
   ##Plots tile.
   tile_sp <- SpatialPolygons(Srl=list(tiles@tiles[[i]]),proj4string=tiles@crs)
   #plot(tile_sp,add=TRUE,col="red")
   
   ##checks to see if tile has data, skips otherwise.
   pred_raster <- brick(crop(ug_bound_rast,tile_extent)[[1]])
   dataType(pred_raster) <- "INT2U"
   region_max <- cellStats(pred_raster,stat="max",na.rm=TRUE)
   
   ##checks to see if output files exist.
   tile_center <- paste("w",round(mean(tile_extent@xmin,tile_extent@xmax)),"_",
                        "n",round(mean(tile_extent@ymin,tile_extent@ymax)),sep="")
   outname <- paste("./output/tiles_prob/decid_prob_tiles_",tile_center,".tif",sep="")
   #outname2 <- paste("./output/tiles_gam_ht/decid_class_tiles_",tile_center,".tif",sep="")
   
   ##Removes tile scheme to save memory.
   #rm(tiles)
   
   if(region_max!=1){
     print(paste("No data in tile, skipping..."))
     next
   }else if(file.exists(outname) & overwrite_output==FALSE){
     print(paste("Output exists, skipping..."))
     next
   }else{
     
     print(paste("Extracting raster data..."))
     pred_df <- as.data.frame(crop(predictors,tile_extent,
                                   progress="text"))
     
     ##Skips prediction for pixels if they are not tall enough, green enough, or bright enough.
     pred_df$UG_canopy_height_1m_v2[is.na(pred_df$UG_canopy_height_1m_v2)] <- 0
     pred_df[pred_df$UG_canopy_height_1m_v2<0.5,] <- NA
     pred_df$UG_ndvi_sept2019_1m_v2[is.na(pred_df$UG_ndvi_sept2019_1m_v2)] <- -1
     pred_df[pred_df$UG_ndvi_sept2019_3m_v2<0.0,] <- NA
     
     ##Skips prediction for whole tiles if there are no trees.
     if(any(!is.na(pred_df$UG_canopy_height_1m_v2))==FALSE){
       next
     }
     
     ##Makes an NA indicator for prediction.
     missing <- !complete.cases(pred_df)
     
     ##Reduced dataset without NAs.
     pred_df <- pred_df[!missing,]
     
     pred_df$Train_Decid <- factor(NA)
     pred_baked <-  bake(decid_rec, new_data = pred_df, all_outcomes(), 
                         all_predictors())

     ##Removes data frame to save memory.
     rm(pred_df)
     
     print(paste("Data preparation..."))
     
     ##Does the prediction
     xgb_prob_na <- rep(NA,length(missing))
     xgb_prob_na[!missing] <- round(predict(xgb_final_fit,new_data=pred_baked,type="prob")$'.pred_TRUE' * 10000)
     print(paste("XGBoost prediction complete..."))
     svm_prob_na <- rep(NA,length(missing))
     svm_prob_na[!missing] <- round(predict(svm_final_fit,new_data=pred_baked,type="prob")$'.pred_TRUE' * 10000)
     print(paste("SVM prediction complete..."))
     gam_prob_na <- rep(NA,length(missing))
     gam_prob_na[!missing] <- round(predict(gam_final_fit,newdata=gam_baked,type="response",block.size=100000) * 10000)
     print(paste("GAM prediction complete..."))
     
     ##Reshapes into a raster object.
     out_raster <- pred_raster
     dataType(out_raster) <- "INT2U"
     
     ##Removes pred values to save memory.
     rm(pred_raster)
     
     ##Converts predictions to raster format
     values(out_raster) <- xgb_prob_na
     out_raster[[2]] <- svm_prob_na
     out_raster[[3]] <- gam_prob_na
     out_raster[[4]] <- round(calc(out_raster,fun=mean))
     out_raster[[5]] <- round(calc(out_raster,fun=sd))
     dataType(out_raster) <- "INT2U"
     names(out_raster) <- c("xgb","svm","gam","mean","sd")
     #names(out_raster) <- c("nnet","xgb")
     
     writeRaster(out_raster,filename=outname, overwrite=TRUE,datatype="INT2U",
                 options=c("COMPRESS=DEFLATE","ZLEVEL=9","PREDICTOR=2","TILED=YES"))
     
     ##remove temporary files.
     #system(paste("rm -f ",raster::tmpDir(),"*",sep=""))
   }
 }

stopCluster(cl)

end_time <- Sys.time()

total_elapsed <- end_time - start_time
total_elapsed

##Mosaics output tiles together.
library(gdalUtils)

##Builds a list of files.
tile_paths <- list.files("./Webinar5_Spatial_Prediction/output/tiles_prob/",
                         pattern=".tif$")
gdalbuildvrt(tile_paths,"./Webinar5_Spatial_Prediction/output/tile_mosaic.vrt")
