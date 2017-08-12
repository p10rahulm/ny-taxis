
distance_between_points <- function(x1,y1,x2,y2){
  return(sqrt((x1-x2)^2 + (y1-y2)^2))
}

generate_line <- function(x1,y1,x2,y2){
  output = list("a" = (y2-y1),"b"=(x1-x2),"c" = (y1*x2-y2*x1))
  return(output)
}

is_point_above <- function(a,b,c,x,y){
  return(a*x+b*y+c >= 0)
}
parallel_line_thru_point <- function(a,b,c,x,y){
  
  output = list("a" = a,"b"=b,"c" = (-a*x-b*y))
  return(output)
}

perpendicular_line_thru_point <- function(a,b,c,x,y){
  output = list("a" = -b,"b"=a,"c" = (b*x-a*y))
  return(output)
}

distance_between_parallel_lines <- function(a1,b1,c1,a2,b2,c2){
  if(a1/b1 != a2/b2) {return("Not Parallel")}
  output = abs((c2*a1/a2 - c1))/sqrt(a1^2 + b1^2)
  return(output)
}

perpendicular_distance_from_point_to_line <- function(a,b,c,x,y){
  output <- abs(a*x + b*y +c )/sqrt(a^2 + b^2)
  return(output)
}

give_col_no <- function(mb,ln,lt,is_ss){
  if(is_ss){return(2*(110*(mb-1)+11*(ln-1)+lt))}
  else{return(2*(110*(mb-1)+11*(ln-1)+lt)-1)}
}


# ---------------------
# Intrabucket transfer
# ---------------------


give_intrabucket_path <- function(pickup_map_buckets,pickup_longitude,pickup_latitude,dropoff_map_buckets,dropoff_longitude,dropoff_latitude){
  output <- c()
  if(pickup_latitude!=dropoff_latitude){
    for(i in (pickup_latitude + (dropoff_latitude >pickup_latitude) - (dropoff_latitude<pickup_latitude)):dropoff_latitude){
      output <- c(output,give_col_no(pickup_map_buckets,pickup_longitude,i,F))
    }
  }
  if(pickup_longitude!=dropoff_longitude){
    for(i in (pickup_longitude + (dropoff_longitude >pickup_longitude) - (dropoff_longitude<pickup_longitude)):dropoff_longitude){
      output <- c(output,give_col_no(pickup_map_buckets,i,dropoff_latitude,F))
    }
  }
  # output <- output[-length(output)]
  # if(length(output)==0){return(as.numeric(NA))}
  if(length(output)==0){return(give_col_no(pickup_map_buckets,pickup_longitude,pickup_latitude,F))}
  return(output)
}

give_intrabucket_path_string <- function(pickup_map_buckets,pickup_longitude,pickup_latitude,dropoff_map_buckets,dropoff_longitude,dropoff_latitude){
  output <- c()
  if(pickup_latitude!=dropoff_latitude){
    for(i in (pickup_latitude + (dropoff_latitude >pickup_latitude) - (dropoff_latitude<pickup_latitude)):dropoff_latitude){
      output <- c(output,give_col_no(pickup_map_buckets,pickup_longitude,i,F))
    }
  }
  if(pickup_longitude!=dropoff_longitude){
    for(i in (pickup_longitude + (dropoff_longitude >pickup_longitude) - (dropoff_longitude<pickup_longitude)):dropoff_longitude){
      output <- c(output,give_col_no(pickup_map_buckets,i,dropoff_latitude,F))
    }
  }
  output <- output[-length(output)]
  if(length(output)==0){return(as.character(NA))}
  return(paste(output,collapse = "|"))
}

give_path <- function(pickup_map_buckets,pickup_longitude,pickup_latitude,
                      dropoff_map_buckets,dropoff_longitude,dropoff_latitude){
  if(pickup_map_buckets==dropoff_map_buckets){
    return(give_intrabucket_path(pickup_map_buckets,pickup_longitude,pickup_latitude,
                                 dropoff_map_buckets,dropoff_longitude,dropoff_latitude))
  }
  # Make sure pickupmap bucket < dropoff map bucket. In this case the order doesn't matter
  if(pickup_map_buckets>dropoff_map_buckets){
    temp = pickup_map_buckets
    pickup_map_buckets = dropoff_map_buckets
    dropoff_map_buckets = temp
    
    temp = pickup_longitude
    pickup_longitude = dropoff_longitude
    dropoff_longitude = temp
    
    temp = pickup_latitude
    pickup_latitude = dropoff_latitude
    dropoff_latitude = temp
  }
  
  
  if(pickup_map_buckets==6 & dropoff_map_buckets == 11){
    output = c(give_intrabucket_path(6,pickup_longitude,pickup_latitude,6,10,pickup_latitude),
               give_intrabucket_path(9,10,pickup_latitude,9,1,pickup_latitude),
               give_intrabucket_path(11,10,pickup_latitude,11,dropoff_longitude,dropoff_latitude))
    return(output)
  }
  
  if((pickup_map_buckets==4 & dropoff_map_buckets == 11)){
    output = c(give_intrabucket_path(4,pickup_longitude,pickup_latitude,4,7,1),
               give_intrabucket_path(11,10,10,11,dropoff_longitude,dropoff_latitude))
    return(output)
  }
  if((pickup_map_buckets==9 & dropoff_map_buckets == 11)){
    output = c(give_intrabucket_path(9,pickup_longitude,pickup_latitude,9,1,pickup_latitude),
               give_intrabucket_path(11,10,pickup_latitude,11,dropoff_longitude,dropoff_latitude))
    return(output)
  }
  if((pickup_map_buckets==5 & dropoff_map_buckets == 11)){
    output = c(give_intrabucket_path(5,pickup_longitude,pickup_latitude,5,1,pickup_latitude),
               give_intrabucket_path(4,10,pickup_latitude,4,7,1),
               give_intrabucket_path(11,10,10,11,dropoff_longitude,dropoff_latitude))
    return(output)
  }
  if((pickup_map_buckets==5 & dropoff_map_buckets == 11)){
    output = c(give_intrabucket_path(5,pickup_longitude,pickup_latitude,5,1,pickup_latitude),
               give_intrabucket_path(4,10,pickup_latitude,4,7,1),
               give_intrabucket_path(11,10,10,11,dropoff_longitude,dropoff_latitude))
    return(output)
  }
  if((pickup_map_buckets==6 & dropoff_map_buckets == 9)){
    output = c(give_intrabucket_path(6,pickup_longitude,pickup_latitude,6,10,pickup_latitude),
               give_intrabucket_path(9,10,pickup_latitude,9,dropoff_longitude,dropoff_latitude))
    return(output)
  }
  if((pickup_map_buckets==4 & dropoff_map_buckets == 5)){
    output = c(give_intrabucket_path(4,pickup_longitude,pickup_latitude,4,10,pickup_latitude),
               give_intrabucket_path(5,1,pickup_latitude,5,dropoff_longitude,dropoff_latitude))
    return(output)
  }
  if((pickup_map_buckets==4 & dropoff_map_buckets == 5)){
    output = c(give_intrabucket_path(4,pickup_longitude,pickup_latitude,4,10,pickup_latitude),
               give_intrabucket_path(5,1,pickup_latitude,5,dropoff_longitude,dropoff_latitude))
    return(output)
  }
  if((pickup_map_buckets==5 & dropoff_map_buckets == 6)){
    output = c(give_intrabucket_path(5,pickup_longitude,pickup_latitude,5,pickup_longitude,1),
               give_intrabucket_path(6,pickup_longitude,10,6,dropoff_longitude,dropoff_latitude))
    return(output)
  }
  if((pickup_map_buckets==4 & dropoff_map_buckets == 9)){
    output = c(give_intrabucket_path(4,pickup_longitude,pickup_latitude,4,7,1),
               give_intrabucket_path(9,1,10,9,dropoff_longitude,dropoff_latitude))
    return(output)
  }
  if((pickup_map_buckets==4 & dropoff_map_buckets == 6)){
    output = c(give_intrabucket_path(4,pickup_longitude,pickup_latitude,4,10,pickup_latitude),
               give_intrabucket_path(5,1,pickup_latitude,5,dropoff_longitude,1),
               give_intrabucket_path(6,dropoff_longitude,10,6,dropoff_longitude,dropoff_latitude))
    return(output)
  }
  if((pickup_map_buckets==5 & dropoff_map_buckets == 9)){
    output = c(give_intrabucket_path(5,pickup_longitude,pickup_latitude,5,1,1),
               give_intrabucket_path(9,10,10,9,dropoff_longitude,dropoff_latitude))
    return(output)
  }
  if((pickup_map_buckets==5 & dropoff_map_buckets == 9)){
    output = c(give_intrabucket_path(5,pickup_longitude,pickup_latitude,5,1,1),
               give_intrabucket_path(9,10,10,9,dropoff_longitude,dropoff_latitude))
    return(output)
  }
  
  
  if(dropoff_map_buckets==7){
    output = c(give_intrabucket_path(7,dropoff_longitude,dropoff_latitude,7,1,10),
               give_path(6,1,1,pickup_map_buckets,pickup_longitude,pickup_latitude))
    return(output)
  }
  if(pickup_map_buckets==7){
    output = c(give_intrabucket_path(7,pickup_longitude,pickup_latitude,7,1,10),
               give_path(6,1,1,dropoff_map_buckets,dropoff_longitude,dropoff_latitude))
    return(output)
  }
  
  if(dropoff_map_buckets==8){
    output = c(give_intrabucket_path(8,dropoff_longitude,dropoff_latitude,8,5,10),
               give_path(11,1,5,pickup_map_buckets,pickup_longitude,pickup_latitude))
    return(output)
  }
  if(pickup_map_buckets==8){
    output = c(give_intrabucket_path(8,pickup_longitude,pickup_latitude,8,5,10),
               give_path(11,1,5,dropoff_map_buckets,dropoff_longitude,dropoff_latitude))
    return(output)
  }
  
  if(dropoff_map_buckets==10){
    output = c(give_intrabucket_path(10,dropoff_longitude,dropoff_latitude,10,10,dropoff_latitude),
               give_path(11,1,dropoff_latitude,pickup_map_buckets,pickup_longitude,pickup_latitude))
    return(output)
  }
  if(pickup_map_buckets==10){
    output = c(give_intrabucket_path(10,pickup_longitude,pickup_latitude,10,10,pickup_latitude),
               give_intrabucket_path(11,1,pickup_latitude,11,dropoff_longitude,dropoff_latitude))
    return(output)
  }
  
  
  if(pickup_map_buckets==2 & dropoff_map_buckets!=3){
    output = c(give_intrabucket_path(2,pickup_longitude,pickup_latitude,2,10,7),
               give_path(10,1,7,dropoff_map_buckets,dropoff_longitude,dropoff_latitude))
    return(output)
  }
  
  if(pickup_map_buckets==2 & dropoff_map_buckets==3){
    output = c(give_intrabucket_path(2,pickup_longitude,pickup_latitude,2,pickup_longitude,10),
               give_intrabucket_path(3,pickup_longitude,1,3,dropoff_longitude,dropoff_latitude))
    return(output)
  }
  if(pickup_map_buckets==3){
    output = c(give_intrabucket_path(3,pickup_longitude,pickup_latitude,3,pickup_longitude,1),
               give_path(2,pickup_longitude,10,dropoff_map_buckets,dropoff_longitude,dropoff_latitude))
    return(output)
  }
  if(pickup_map_buckets==1){
    output = c(give_intrabucket_path(1,pickup_longitude,pickup_latitude,1,pickup_longitude,10),
               give_path(2,pickup_longitude,1,dropoff_map_buckets,dropoff_longitude,dropoff_latitude))
    return(output)
  }
  if(((pickup_map_buckets==1 |pickup_map_buckets==2|pickup_map_buckets==3)  & 
      (dropoff_map_buckets==4 |dropoff_map_buckets==5|dropoff_map_buckets==6|
       dropoff_map_buckets==7 |dropoff_map_buckets==8|dropoff_map_buckets==9))){
    output = c(give_path(pickup_map_buckets,pickup_longitude,pickup_latitude,11,5,5),
               give_path(11,5,5,dropoff_map_buckets,dropoff_longitude,dropoff_latitude))
    return(output)
  }
  
  
}


return100cols <- function(vector){
  return(c(vector,rep(as.numeric(NA),100-length(vector))))
}

# ---------------
# Split between training, test and validation
# ---------------
return_path_and_columns <- function(data_frame){
  # Basic data.table mapping
  data_frame <- data.table(data_frame)
  # test <- data.table(test)
  # validation <- data.table(validation)
  
  # ---------------
  # creating more calculative fields
  # ---------------
  
  data_frame$distance <- distance_between_points(data_frame$pickup_longitude,data_frame$pickup_latitude,
                                            data_frame$dropoff_longitude,data_frame$dropoff_latitude)
  
  # # --------------------
  # # # trip duration field in hours
  # # --------------------
  # data_frame$duration_hours <- data_frame$trip_duration/3600
  
  # --------------------
  # # time of day
  # --------------------
  data_frame$hour_of_day <- as.numeric(substr(data_frame$pickup_datetime,12,13))
  data_frame$minute <- as.numeric(substr(data_frame$pickup_datetime,15,16))
  # # --------------------
  # # # speed
  # # --------------------
  # 
  # data_frame$speed <- (data_frame$distance / data_frame$duration_hours)
  
  # --------------------
  # # distance buckets
  # --------------------
  
  data_frame$distance_buckets <- 0
  data_frame[data_frame$distance < .5,"distance_buckets"] <- 1
  data_frame[data_frame$distance < 1 & data_frame$distance >= .5,]$distance_buckets <- 2
  data_frame[data_frame$distance < 2.5 & data_frame$distance >= 1,]$distance_buckets <- 3
  data_frame[data_frame$distance < 5 & data_frame$distance >= 2.5,]$distance_buckets <- 4
  data_frame[data_frame$distance < 10 & data_frame$distance >= 5,]$distance_buckets <- 5
  data_frame[data_frame$distance < 20 & data_frame$distance >= 10,]$distance_buckets <- 6
  data_frame[data_frame$distance >= 20,]$distance_buckets <- 7
  
  # --------------------
  # # some more processing
  # --------------------
  
  data_frame$sf <- data_frame$store_and_fwd_flag =="Y"
  data_frame$store_and_fwd_flag <- NULL
  
  # ---------------
  # # Create summary buckets
  # ---------------
  
  data_frame <- data.table(data_frame)
  # ---------------------------
  # Rawdata for mapbuckets
  # ---------------------------
  mapbuckets <- read.csv("rawdata/mapbuckets_boundaries.csv")
  
  
  # ---------------------------
  # Getmapbuckets
  # ---------------------------
  # Pickup
  data_frame$pickup_map_buckets <- as.numeric(0)
  
  for(i in 1:nrow(mapbuckets)){
    data_frame[(data_frame$pickup_longitude < mapbuckets$LongMin[i] & data_frame$pickup_longitude >= mapbuckets$LongMax[i]) & 
            (data_frame$pickup_latitude <= mapbuckets$LatMax[i] & data_frame$pickup_latitude >  mapbuckets$LatMin[i]),"pickup_map_buckets" ] <- i
  }
  
  
  # Dropoff
  data_frame$dropoff_map_buckets <- as.numeric(0)
  
  for(i in 1:nrow(mapbuckets)){
    data_frame[(data_frame$dropoff_longitude < mapbuckets$LongMin[i] & data_frame$dropoff_longitude >= mapbuckets$LongMax[i]) & 
            (data_frame$dropoff_latitude <= mapbuckets$LatMax[i] & data_frame$dropoff_latitude >  mapbuckets$LatMin[i]),"dropoff_map_buckets" ] <- i
  }
  
  
  # ---------------------------
  # get pickup_map_buckets_lat+long for buckets 1-8
  # ---------------------------
  
  
  data_frame$pickup_map_buckets_lat <- as.numeric(0)
  data_frame$pickup_map_buckets_long <- as.numeric(0)
  for(i in 1:(nrow(mapbuckets)-1)){
    data_frame[data_frame$pickup_map_buckets==i,]$pickup_map_buckets_long <- ceiling((data_frame[data_frame$pickup_map_buckets==i,]$pickup_longitude-mapbuckets$LongMin[i])/((mapbuckets$LongMax[i]-mapbuckets$LongMin[i])/10))
    data_frame[data_frame$pickup_map_buckets==i,]$pickup_map_buckets_lat <- ceiling((data_frame[data_frame$pickup_map_buckets==i,]$pickup_latitude-mapbuckets$LatMin[i])/((mapbuckets$LatMax[i]-mapbuckets$LatMin[i])/10))
  }
  rm(i)
  
  # ---------------------------
  # get dropoff_map_buckets_lat+long for buckets 1-8
  # ---------------------------
  
  
  data_frame$dropoff_map_buckets_lat <- as.numeric(0)
  data_frame$dropoff_map_buckets_long <- as.numeric(0)
  for(i in 1:(nrow(mapbuckets)-1)){
    data_frame[data_frame$dropoff_map_buckets==i,]$dropoff_map_buckets_long <- ceiling((data_frame[data_frame$dropoff_map_buckets==i,]$dropoff_longitude-mapbuckets$LongMin[i])/((mapbuckets$LongMax[i]-mapbuckets$LongMin[i])/10))
    data_frame[data_frame$dropoff_map_buckets==i,]$dropoff_map_buckets_lat <- ceiling((data_frame[data_frame$dropoff_map_buckets==i,]$dropoff_latitude-mapbuckets$LatMin[i])/((mapbuckets$LatMax[i]-mapbuckets$LatMin[i])/10))
  }
  rm(i)
  
  
  
  
  # ---------------------------
  # Get Lines 9,10,11 for new jersey, brooklyn and manhattan respectively
  # ---------------------------
  
  hudson_river <- list(x1=-73.90502,y1=40.90157,x2 = -74.04785,y2 = 40.70094)
  east_river <- list(x1=-73.856652,y1=40.88322,x2 = -73.99086,y2 = 40.69521)
  south_boundary <- list(x1=-73.92476,y1=40.72319,x2 = -74.02347,y2 = 40.69821)
  
  hudson_river_line <- generate_line(hudson_river$x1,hudson_river$y1,hudson_river$x2,hudson_river$y2)
  east_river_line <- generate_line(east_river$x1,east_river$y1,east_river$x2,east_river$y2)
  south_boundary_line <- generate_line(south_boundary$x1,south_boundary$y1,south_boundary$x2,south_boundary$y2)
  
  
  # ---------------------------
  # Split New jersey away
  # ---------------------------
  # Pickups
  data_frame[data_frame$pickup_map_buckets==9 & is_point_above(hudson_river_line$a,hudson_river_line$b,hudson_river_line$c,
                                                     data_frame$pickup_longitude,data_frame$pickup_latitude),]$pickup_map_buckets <- 10
  
  # Dropoffs
  data_frame[data_frame$dropoff_map_buckets==9 & is_point_above(hudson_river_line$a,hudson_river_line$b,hudson_river_line$c,
                                                      data_frame$dropoff_longitude,data_frame$dropoff_latitude),]$dropoff_map_buckets <- 10
  
  
  # ---------------------------
  # Split manhattan away
  # ---------------------------
  # Pickups
  data_frame[data_frame$pickup_map_buckets==9 & 
          is_point_above(south_boundary_line$a,south_boundary_line$b,south_boundary_line$c,
                         data_frame$pickup_longitude,data_frame$pickup_latitude) & 
          is_point_above(east_river_line$a,east_river_line$b,east_river_line$c,
                         data_frame$pickup_longitude,data_frame$pickup_latitude),]$pickup_map_buckets <- 11
  
  
  # Dropoff
  data_frame[data_frame$dropoff_map_buckets==9 & 
          is_point_above(south_boundary_line$a,south_boundary_line$b,south_boundary_line$c,
                         data_frame$dropoff_longitude,data_frame$dropoff_latitude) & 
          is_point_above(east_river_line$a,east_river_line$b,east_river_line$c,
                         data_frame$dropoff_longitude,data_frame$dropoff_latitude),]$dropoff_map_buckets <- 11
  
  
  # ---------------------------
  # Get subbuckets for new jersey and brooklyn
  # ---------------------------
  # Pickups
  for(i in 9:10){
    longmax <- min(data_frame[data_frame$pickup_map_buckets==i,]$pickup_longitude) 
    longmin <- max(data_frame[data_frame$pickup_map_buckets==i,]$pickup_longitude) 
    latmax <- max(data_frame[data_frame$pickup_map_buckets==i,]$pickup_latitude)
    latmin <- min(data_frame[data_frame$pickup_map_buckets==i,]$pickup_latitude)
    data_frame[data_frame$pickup_map_buckets==i,]$pickup_map_buckets_long <- ceiling((data_frame[data_frame$pickup_map_buckets==i,]$pickup_longitude-longmin)/((longmax-longmin)/10))
    data_frame[data_frame$pickup_map_buckets==i,]$pickup_map_buckets_lat <- ceiling((data_frame[data_frame$pickup_map_buckets==i,]$pickup_latitude-latmin)/((latmax-latmin)/10))
  }
  
  # Dropoffs
  for(i in 9:10){
    longmax <- min(data_frame[data_frame$dropoff_map_buckets==i,]$dropoff_longitude) 
    longmin <- max(data_frame[data_frame$dropoff_map_buckets==i,]$dropoff_longitude) 
    latmax <- max(data_frame[data_frame$dropoff_map_buckets==i,]$dropoff_latitude)
    latmin <- min(data_frame[data_frame$dropoff_map_buckets==i,]$dropoff_latitude)
    data_frame[data_frame$dropoff_map_buckets==i,]$dropoff_map_buckets_long <- ceiling((data_frame[data_frame$dropoff_map_buckets==i,]$dropoff_longitude-longmin)/((longmax-longmin)/10))
    data_frame[data_frame$dropoff_map_buckets==i,]$dropoff_map_buckets_lat <- ceiling((data_frame[data_frame$dropoff_map_buckets==i,]$dropoff_latitude-latmin)/((latmax-latmin)/10))
  }
  
  
  
  rm(latmax,latmin,longmax,longmin,i)
  rm(south_boundary_line,east_river_line)
  # ---------------------------
  # Removing unnecessary points
  # ---------------------------
  # Pickups
  data_frame <- data_frame[data_frame$pickup_map_buckets!=0,]
  data_frame <- data_frame[data_frame$pickup_map_buckets == 11 | data_frame$pickup_map_buckets_lat !=0,]
  data_frame <- data_frame[data_frame$pickup_map_buckets == 11 | data_frame$pickup_map_buckets_long !=0,]
  
  # dropoffs
  data_frame <- data_frame[data_frame$dropoff_map_buckets!=0,]
  data_frame <- data_frame[data_frame$dropoff_map_buckets == 11 | data_frame$dropoff_map_buckets_lat !=0,]
  data_frame <- data_frame[data_frame$dropoff_map_buckets == 11 | data_frame$dropoff_map_buckets_long !=0,]
  
  # ---------------------------
  # Get boundary points/lines for manhattan
  # ---------------------------
  
  # Get the parallel line from hudson river
  
  
  east_manhattan <- parallel_line_thru_point(hudson_river_line$a,hudson_river_line$b,hudson_river_line$c,
                                             east_river$x1,east_river$y1)
  
  north_manhattan <- perpendicular_line_thru_point(hudson_river_line$a,hudson_river_line$b,hudson_river_line$c,
                                                   east_river$x1,east_river$y1)
  
  
  south_manhattan <- perpendicular_line_thru_point(hudson_river_line$a,hudson_river_line$b,hudson_river_line$c,
                                                   east_river$x2,east_river$y2)
  
  
  # ---------------------------
  # Generate subbuckets for Manhattan
  # ---------------------------
  
  # Pickups
  data_frame[data_frame$pickup_map_buckets == 11,]$pickup_map_buckets_long <- ceiling(perpendicular_distance_from_point_to_line(hudson_river_line$a,hudson_river_line$b,hudson_river_line$c,
                                                                                                                      data_frame[data_frame$pickup_map_buckets == 11,]$pickup_longitude,data_frame[data_frame$pickup_map_buckets == 11,]$pickup_latitude)*10/
                                                                              distance_between_parallel_lines(hudson_river_line$a,hudson_river_line$b,hudson_river_line$c,
                                                                                                              east_manhattan$a,east_manhattan$b,east_manhattan$c))
  
  
  data_frame[data_frame$pickup_map_buckets == 11,]$pickup_map_buckets_lat <- ceiling(perpendicular_distance_from_point_to_line(north_manhattan$a,north_manhattan$b,north_manhattan$c,
                                                                                                                     data_frame[data_frame$pickup_map_buckets == 11,]$pickup_longitude,data_frame[data_frame$pickup_map_buckets == 11,]$pickup_latitude)*10/
                                                                             distance_between_parallel_lines(north_manhattan$a,north_manhattan$b,north_manhattan$c,
                                                                                                             south_manhattan$a,south_manhattan$b,south_manhattan$c))
  
  
  # dropoffs
  data_frame[data_frame$dropoff_map_buckets == 11,]$dropoff_map_buckets_long <- ceiling(perpendicular_distance_from_point_to_line(hudson_river_line$a,hudson_river_line$b,hudson_river_line$c,
                                                                                                                        data_frame[data_frame$dropoff_map_buckets == 11,]$dropoff_longitude,data_frame[data_frame$dropoff_map_buckets == 11,]$dropoff_latitude)*10/
                                                                                distance_between_parallel_lines(hudson_river_line$a,hudson_river_line$b,hudson_river_line$c,
                                                                                                                east_manhattan$a,east_manhattan$b,east_manhattan$c))
  
  
  data_frame[data_frame$dropoff_map_buckets == 11,]$dropoff_map_buckets_lat <- ceiling(perpendicular_distance_from_point_to_line(north_manhattan$a,north_manhattan$b,north_manhattan$c,
                                                                                                                       data_frame[data_frame$dropoff_map_buckets == 11,]$dropoff_longitude,data_frame[data_frame$dropoff_map_buckets == 11,]$dropoff_latitude)*10/
                                                                               distance_between_parallel_lines(north_manhattan$a,north_manhattan$b,north_manhattan$c,
                                                                                                               south_manhattan$a,south_manhattan$b,south_manhattan$c))
  
  
  rm(east_manhattan,east_river,hudson_river,hudson_river_line,north_manhattan,south_boundary,south_manhattan)
  
  
  
  
  # --------------
  # checking frequency of travel between major map_buckets
  # --------------
  data_frame$is_intra_bucket <- data_frame$pickup_map_buckets == data_frame$dropoff_map_buckets
  data_frame$is_interbucket_into_11 <- 0
  
  data_frame[((data_frame$pickup_map_buckets == 11) | (data_frame$dropoff_map_buckets == 11)) & 
          (data_frame$pickup_map_buckets != data_frame$dropoff_map_bucket),]$is_interbucket_into_11 <- 1
  
  intrabucket_frequency <- as.data.frame.matrix(table(data_frame$pickup_map_buckets, data_frame$dropoff_map_buckets))
  
  
  
  
  # --------------
  # Create all possible groups and subgroups
  # --------------
  
  numcol <- max(data_frame$pickup_map_buckets,data_frame$dropoff_map_buckets)*
    max(data_frame$pickup_map_buckets_long,data_frame$dropoff_map_buckets_long)*
    max(data_frame$pickup_map_buckets_lat,data_frame$dropoff_map_buckets_lat)
  
  columnnames = rep("",2*numcol)
  i=1
  for(mb in (1:max(data_frame$pickup_map_buckets,data_frame$dropoff_map_buckets))){
    for(ln in (1:max(data_frame$pickup_map_buckets_long,data_frame$dropoff_map_buckets_long))){
      for(lt in (1:max(data_frame$pickup_map_buckets_lat,data_frame$dropoff_map_buckets_lat))){
        
        mblnlt1 <- paste0("thru_g",mb,"ln",ln,"lt",lt)
        mblnlt2 <- paste0("ss_g",mb,"ln",ln,"lt",lt)
        columnnames[i] <- mblnlt1
        columnnames[i+1] <- mblnlt2
        i = i+2
      }
    }
  }
  rm(i,mblnlt1,mblnlt2,ln,lt,mb)
  path_matrix <- Matrix(data = 0,nrow = nrow(data_frame),ncol = 2*numcol,sparse = T,byrow = F,
                        dimnames = list(as.character(seq(1,nrow(data_frame))),columnnames))
  
  
  
  
  
  # ----------------------
  # Get the start and dropoff points in the path matrix
  # ----------------------
  start_matrix <- sparseMatrix(i = 1:nrow(data_frame), j = give_col_no(data_frame$pickup_map_buckets,data_frame$pickup_map_buckets_long,data_frame$pickup_map_buckets_lat,T), 
                               x = 1,dims = c(nrow(data_frame),ncol(path_matrix)),dimnames = list(as.character(seq(1,nrow(data_frame))),columnnames)
  )
  
  drop_matrix <- sparseMatrix(i = 1:nrow(data_frame), j = give_col_no(data_frame$dropoff_map_buckets,data_frame$dropoff_map_buckets_long,data_frame$dropoff_map_buckets_lat,T), 
                              x = 1,dims = c(nrow(data_frame),ncol(path_matrix)),dimnames = list(as.character(seq(1,nrow(data_frame))),columnnames)
  )
  
  path_matrix <- path_matrix + start_matrix + drop_matrix
  rm(start_matrix,drop_matrix)
  # ----------------------
  # 
  # Get new column names
  # ----------------------
  # ----------------------
  
  # pickups fresh buckets
  # bucket_xy_mapping <- read.csv("rawdata/bucket_xy_mapping.csv")
  # data_frame$pickup_bucket_coord <- 11
  # data_frame$dropoff_bucket_coord <- 11
  # for(i in 1:nrow(bucket_xy_mapping)){
  #   data_frame$pickup_bucket_coord[data_frame$pickup_map_buckets==bucket_xy_mapping$MapBuckets[i]] <- bucket_xy_mapping$bucketcoord[i]
  #   data_frame$dropoff_bucket_coord[data_frame$dropoff_map_buckets==bucket_xy_mapping$MapBuckets[i]] <- bucket_xy_mapping$bucketcoord[i]
  # }
  # 
  # data_frame$pickup_bucket_coord[data_frame$pickup_map_buckets==8] <- 12
  # data_frame$pickup_bucket_coord[data_frame$pickup_map_buckets==7] <- 13
  # data_frame$pickup_bucket_coord[data_frame$pickup_map_buckets==2] <- 21
  # data_frame$pickup_bucket_coord[data_frame$pickup_map_buckets==10] <- 22
  # data_frame$pickup_bucket_coord[data_frame$pickup_map_buckets==11] <- 23
  # data_frame$pickup_bucket_coord[data_frame$pickup_map_buckets==9] <- 24
  # data_frame$pickup_bucket_coord[data_frame$pickup_map_buckets==6] <- 25
  # data_frame$pickup_bucket_coord[data_frame$pickup_map_buckets==3] <- 31
  # data_frame$pickup_bucket_coord[data_frame$pickup_map_buckets==4] <- 32
  # data_frame$pickup_bucket_coord[data_frame$pickup_map_buckets==5] <- 33
  # # dropoffs fresh buckets
  # data_frame$dropoff_bucket_coord <- 11
  # data_frame$dropoff_bucket_coord[data_frame$dropoff_map_buckets==1] <- 11
  # data_frame$dropoff_bucket_coord[data_frame$dropoff_map_buckets==8] <- 12
  # data_frame$dropoff_bucket_coord[data_frame$dropoff_map_buckets==7] <- 13
  # data_frame$dropoff_bucket_coord[data_frame$dropoff_map_buckets==2] <- 21
  # data_frame$dropoff_bucket_coord[data_frame$dropoff_map_buckets==10] <- 22
  # data_frame$dropoff_bucket_coord[data_frame$dropoff_map_buckets==11] <- 23
  # data_frame$dropoff_bucket_coord[data_frame$dropoff_map_buckets==9] <- 24
  # data_frame$dropoff_bucket_coord[data_frame$dropoff_map_buckets==6] <- 25
  # data_frame$dropoff_bucket_coord[data_frame$dropoff_map_buckets==3] <- 31
  # data_frame$dropoff_bucket_coord[data_frame$dropoff_map_buckets==4] <- 32
  # data_frame$dropoff_bucket_coord[data_frame$dropoff_map_buckets==5] <- 33
  
  intrabucket_frequency2 <- as.data.frame.matrix(table(data_frame$pickup_bucket_coord,data_frame$dropoff_bucket_coord))
  
  # ----------------------
  # Register all paths in a smaller dense matrix to add to the larger one later
  # ----------------------
  # path <- mapply(FUN = give_intrabucket_path,"pickup_map_buckets" = data_frame$pickup_map_buckets,"pickup_longitude" = data_frame$pickup_map_buckets_long,
  #                "pickup_latitude" = data_frame$pickup_map_buckets_lat,
  #                "dropoff_map_buckets" =  data_frame$dropoff_map_buckets,
  #                "dropoff_longitude" = data_frame$dropoff_map_buckets_long,
  #                "dropoff_latitude" = data_frame$dropoff_map_buckets_lat,SIMPLIFY = T)
  
  path <- mapply(FUN = give_path,"pickup_map_buckets" = data_frame$pickup_map_buckets,"pickup_longitude" = data_frame$pickup_map_buckets_long,
                 "pickup_latitude" = data_frame$pickup_map_buckets_lat,
                 "dropoff_map_buckets" =  data_frame$dropoff_map_buckets,
                 "dropoff_longitude" = data_frame$dropoff_map_buckets_long,
                 "dropoff_latitude" = data_frame$dropoff_map_buckets_lat,SIMPLIFY = T)
  
  path <- mapply(FUN = return100cols,vector=path,SIMPLIFY = T)
  path <- t(path)
  
  numrows <- nrow(data_frame)
  for(i in 1:100){
    add_matrix <- sparseMatrix(i = seq(1,numrows)[!is.na(path[,i])], j = path[,i][!is.na(path[,i])], 
                               x = 1,dims = c(numrows,2*numcol))
    path_matrix <- path_matrix + add_matrix
    print(i)
    rm(add_matrix)
  }
  rm(i,path)
  rm(numcol,numrows,columnnames)
  # start_matrix[1,2352:2355]
  
  return(list("data_frame" = data_frame,"path_matrix" = path_matrix))
}

get_full_training_columns <- function(data_frame_and_sparse_matrix_input){
  data_frame_input <- data_frame_and_sparse_matrix_input[[1]]
  sparse_matrix_input <- data_frame_and_sparse_matrix_input[[2]]
  # Usefule columns
  mycols <- c("passenger_count","vendor_id","distance",
              "hour_of_day","distance_buckets","sf","is_intra_bucket","is_interbucket_into_11",
              "pickup_map_buckets","pickup_map_buckets_lat","pickup_map_buckets_long",
              "dropoff_map_buckets","dropoff_map_buckets_lat","dropoff_map_buckets_long")
  
  useful_subset <- subset(data_frame_input,select = mycols)
  useful_subset <- Matrix(data=as.matrix(useful_subset),sparse = T)
  sparse_matrix_input <- cbind(useful_subset,sparse_matrix_input)
  return(sparse_matrix_input)
}