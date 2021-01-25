##Simple function to calculate ground sample distance and image overlap in UAV imagery.
##Use this function to find the distance between flight paths and airspeed that
##will allow sufficient overlap between images. In most cases > 75% overlap is required.

##Author:David Baseler, modified by Ian Breckheimer
##Modified: 2019-06-13

##Workhorse Function
calculate_overlap<-function(altitude,image_frequency,airspeed,path_distance,sensor){
  GSD=sensor$sensor_width*altitude*100/(sensor$focal_length*sensor$image_x)
  ix=GSD*sensor$image_x/100
  iy=GSD*sensor$image_y/100
  side_overlap<-((ix-path_distance)/ix)*100
  if (side_overlap<0) side_overlap=0
  airspeed_ms<-airspeed/3.6 #[km/h] to m/s
  frame_distance<-airspeed_ms/image_frequency
  forward_overlap<-((iy-frame_distance)/iy)*100
  cat(sprintf("UAV @ %.2f m (%.2f ft) \n%.1f km/h \n%.2f images per second (every %.2f m)\nGSD %.2f cm/pixel\nSide Overlap\t%.2f \nForward Overlap\t%.2f \n",altitude,altitude*3.28084,airspeed,image_frequency,frame_distance,GSD,side_overlap,forward_overlap))
}

##Camera FOV and resolution
Phantom4Pro_sensor<-data.frame(sensor_width=12.8333, focal_length= 8.604, image_x=5472, image_y=3648)
Mavic2Pro_sensor<-data.frame(sensor_width=13.2, focal_length= 10.26, image_x=5472, image_y=3648)


##Play around with function to get at least 66% overlap.
calculate_overlap(altitude=110,
                  image_frequency=0.5,
                  airspeed=39,
                  path_distance=35,
                  sensor=Mavic2Pro_sensor)



