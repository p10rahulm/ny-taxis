
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
return_path_and_columns <- function(train){
  # Basic data.table mapping
  train <- data.table(train)
  # test <- data.table(test)
  # validation <- data.table(validation)
  
  # ---------------
  # creating more calculative fields
  # ---------------
  
  train$distance <- distance_between_points(train$pickup_longitude,train$pickup_latitude,
                                            train$dropoff_longitude,train$dropoff_latitude)
  
  # # --------------------
  # # # trip duration field in hours
  # # --------------------
  # train$duration_hours <- train$trip_duration/3600
  
  # --------------------
  # # time of day
  # --------------------
  train$hour_of_day <- as.numeric(substr(train$pickup_datetime,12,13))
  train$minute <- as.numeric(substr(train$pickup_datetime,15,16))
  # # --------------------
  # # # speed
  # # --------------------
  # 
  # train$speed <- (train$distance / train$duration_hours)
  
  # --------------------
  # # distance buckets
  # --------------------
  
  train$distance_buckets <- 0
  train[train$distance < .5,"distance_buckets"] <- 1
  train[train$distance < 1 & train$distance >= .5,]$distance_buckets <- 2
  train[train$distance < 2.5 & train$distance >= 1,]$distance_buckets <- 3
  train[train$distance < 5 & train$distance >= 2.5,]$distance_buckets <- 4
  train[train$distance < 10 & train$distance >= 5,]$distance_buckets <- 5
  train[train$distance < 20 & train$distance >= 10,]$distance_buckets <- 6
  train[train$distance >= 20,]$distance_buckets <- 7
  
  # --------------------
  # # some more processing
  # --------------------
  
  train$sf <- train$store_and_fwd_flag =="Y"
  train$store_and_fwd_flag <- NULL
  
  # ---------------
  # # Create summary buckets
  # ---------------
  
  train <- data.table(train)
  # ---------------------------
  # Rawdata for mapbuckets
  # ---------------------------
  mapbuckets <- read.csv("rawdata/mapbuckets_boundaries.csv")
  
  
  # ---------------------------
  # Getmapbuckets
  # ---------------------------
  # Pickup
  train$pickup_map_buckets <- as.numeric(0)
  
  for(i in 1:nrow(mapbuckets)){
    train[(train$pickup_longitude < mapbuckets$LongMin[i] & train$pickup_longitude >= mapbuckets$LongMax[i]) & 
            (train$pickup_latitude <= mapbuckets$LatMax[i] & train$pickup_latitude >  mapbuckets$LatMin[i]),"pickup_map_buckets" ] <- i
  }
  
  
  # Dropoff
  train$dropoff_map_buckets <- as.numeric(0)
  
  for(i in 1:nrow(mapbuckets)){
    train[(train$dropoff_longitude < mapbuckets$LongMin[i] & train$dropoff_longitude >= mapbuckets$LongMax[i]) & 
            (train$dropoff_latitude <= mapbuckets$LatMax[i] & train$dropoff_latitude >  mapbuckets$LatMin[i]),"dropoff_map_buckets" ] <- i
  }
  
  
  # ---------------------------
  # get pickup_map_buckets_lat+long for buckets 1-8
  # ---------------------------
  
  
  train$pickup_map_buckets_lat <- as.numeric(0)
  train$pickup_map_buckets_long <- as.numeric(0)
  for(i in 1:(nrow(mapbuckets)-1)){
    train[train$pickup_map_buckets==i,]$pickup_map_buckets_long <- ceiling((train[train$pickup_map_buckets==i,]$pickup_longitude-mapbuckets$LongMin[i])/((mapbuckets$LongMax[i]-mapbuckets$LongMin[i])/10))
    train[train$pickup_map_buckets==i,]$pickup_map_buckets_lat <- ceiling((train[train$pickup_map_buckets==i,]$pickup_latitude-mapbuckets$LatMin[i])/((mapbuckets$LatMax[i]-mapbuckets$LatMin[i])/10))
  }
  rm(i)
  
  # ---------------------------
  # get dropoff_map_buckets_lat+long for buckets 1-8
  # ---------------------------
  
  
  train$dropoff_map_buckets_lat <- as.numeric(0)
  train$dropoff_map_buckets_long <- as.numeric(0)
  for(i in 1:(nrow(mapbuckets)-1)){
    train[train$dropoff_map_buckets==i,]$dropoff_map_buckets_long <- ceiling((train[train$dropoff_map_buckets==i,]$dropoff_longitude-mapbuckets$LongMin[i])/((mapbuckets$LongMax[i]-mapbuckets$LongMin[i])/10))
    train[train$dropoff_map_buckets==i,]$dropoff_map_buckets_lat <- ceiling((train[train$dropoff_map_buckets==i,]$dropoff_latitude-mapbuckets$LatMin[i])/((mapbuckets$LatMax[i]-mapbuckets$LatMin[i])/10))
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
  train[train$pickup_map_buckets==9 & is_point_above(hudson_river_line$a,hudson_river_line$b,hudson_river_line$c,
                                                     train$pickup_longitude,train$pickup_latitude),]$pickup_map_buckets <- 10
  
  # Dropoffs
  train[train$dropoff_map_buckets==9 & is_point_above(hudson_river_line$a,hudson_river_line$b,hudson_river_line$c,
                                                      train$dropoff_longitude,train$dropoff_latitude),]$dropoff_map_buckets <- 10
  
  
  # ---------------------------
  # Split manhattan away
  # ---------------------------
  # Pickups
  train[train$pickup_map_buckets==9 & 
          is_point_above(south_boundary_line$a,south_boundary_line$b,south_boundary_line$c,
                         train$pickup_longitude,train$pickup_latitude) & 
          is_point_above(east_river_line$a,east_river_line$b,east_river_line$c,
                         train$pickup_longitude,train$pickup_latitude),]$pickup_map_buckets <- 11
  
  
  # Dropoff
  train[train$dropoff_map_buckets==9 & 
          is_point_above(south_boundary_line$a,south_boundary_line$b,south_boundary_line$c,
                         train$dropoff_longitude,train$dropoff_latitude) & 
          is_point_above(east_river_line$a,east_river_line$b,east_river_line$c,
                         train$dropoff_longitude,train$dropoff_latitude),]$dropoff_map_buckets <- 11
  
  
  # ---------------------------
  # Get subbuckets for new jersey and brooklyn
  # ---------------------------
  # Pickups
  for(i in 9:10){
    longmax <- min(train[train$pickup_map_buckets==i,]$pickup_longitude) 
    longmin <- max(train[train$pickup_map_buckets==i,]$pickup_longitude) 
    latmax <- max(train[train$pickup_map_buckets==i,]$pickup_latitude)
    latmin <- min(train[train$pickup_map_buckets==i,]$pickup_latitude)
    train[train$pickup_map_buckets==i,]$pickup_map_buckets_long <- ceiling((train[train$pickup_map_buckets==i,]$pickup_longitude-longmin)/((longmax-longmin)/10))
    train[train$pickup_map_buckets==i,]$pickup_map_buckets_lat <- ceiling((train[train$pickup_map_buckets==i,]$pickup_latitude-latmin)/((latmax-latmin)/10))
  }
  
  # Dropoffs
  for(i in 9:10){
    longmax <- min(train[train$dropoff_map_buckets==i,]$dropoff_longitude) 
    longmin <- max(train[train$dropoff_map_buckets==i,]$dropoff_longitude) 
    latmax <- max(train[train$dropoff_map_buckets==i,]$dropoff_latitude)
    latmin <- min(train[train$dropoff_map_buckets==i,]$dropoff_latitude)
    train[train$dropoff_map_buckets==i,]$dropoff_map_buckets_long <- ceiling((train[train$dropoff_map_buckets==i,]$dropoff_longitude-longmin)/((longmax-longmin)/10))
    train[train$dropoff_map_buckets==i,]$dropoff_map_buckets_lat <- ceiling((train[train$dropoff_map_buckets==i,]$dropoff_latitude-latmin)/((latmax-latmin)/10))
  }
  
  
  
  rm(latmax,latmin,longmax,longmin,i)
  rm(south_boundary_line,east_river_line)
  # ---------------------------
  # Removing unnecessary points
  # ---------------------------
  # Pickups
  train <- train[train$pickup_map_buckets!=0,]
  train <- train[train$pickup_map_buckets == 11 | train$pickup_map_buckets_lat !=0,]
  train <- train[train$pickup_map_buckets == 11 | train$pickup_map_buckets_long !=0,]
  
  # dropoffs
  train <- train[train$dropoff_map_buckets!=0,]
  train <- train[train$dropoff_map_buckets == 11 | train$dropoff_map_buckets_lat !=0,]
  train <- train[train$dropoff_map_buckets == 11 | train$dropoff_map_buckets_long !=0,]
  
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
  train[train$pickup_map_buckets == 11,]$pickup_map_buckets_long <- ceiling(perpendicular_distance_from_point_to_line(hudson_river_line$a,hudson_river_line$b,hudson_river_line$c,
                                                                                                                      train[train$pickup_map_buckets == 11,]$pickup_longitude,train[train$pickup_map_buckets == 11,]$pickup_latitude)*10/
                                                                              distance_between_parallel_lines(hudson_river_line$a,hudson_river_line$b,hudson_river_line$c,
                                                                                                              east_manhattan$a,east_manhattan$b,east_manhattan$c))
  
  
  train[train$pickup_map_buckets == 11,]$pickup_map_buckets_lat <- ceiling(perpendicular_distance_from_point_to_line(north_manhattan$a,north_manhattan$b,north_manhattan$c,
                                                                                                                     train[train$pickup_map_buckets == 11,]$pickup_longitude,train[train$pickup_map_buckets == 11,]$pickup_latitude)*10/
                                                                             distance_between_parallel_lines(north_manhattan$a,north_manhattan$b,north_manhattan$c,
                                                                                                             south_manhattan$a,south_manhattan$b,south_manhattan$c))
  
  
  # dropoffs
  train[train$dropoff_map_buckets == 11,]$dropoff_map_buckets_long <- ceiling(perpendicular_distance_from_point_to_line(hudson_river_line$a,hudson_river_line$b,hudson_river_line$c,
                                                                                                                        train[train$dropoff_map_buckets == 11,]$dropoff_longitude,train[train$dropoff_map_buckets == 11,]$dropoff_latitude)*10/
                                                                                distance_between_parallel_lines(hudson_river_line$a,hudson_river_line$b,hudson_river_line$c,
                                                                                                                east_manhattan$a,east_manhattan$b,east_manhattan$c))
  
  
  train[train$dropoff_map_buckets == 11,]$dropoff_map_buckets_lat <- ceiling(perpendicular_distance_from_point_to_line(north_manhattan$a,north_manhattan$b,north_manhattan$c,
                                                                                                                       train[train$dropoff_map_buckets == 11,]$dropoff_longitude,train[train$dropoff_map_buckets == 11,]$dropoff_latitude)*10/
                                                                               distance_between_parallel_lines(north_manhattan$a,north_manhattan$b,north_manhattan$c,
                                                                                                               south_manhattan$a,south_manhattan$b,south_manhattan$c))
  
  
  rm(east_manhattan,east_river,hudson_river,hudson_river_line,north_manhattan,south_boundary,south_manhattan)
  
  
  
  
  # --------------
  # checking frequency of travel between major map_buckets
  # --------------
  train$is_intra_bucket <- train$pickup_map_buckets == train$dropoff_map_buckets
  train$is_interbucket_into_11 <- 0
  
  train[((train$pickup_map_buckets == 11) | (train$dropoff_map_buckets == 11)) & 
          (train$pickup_map_buckets != train$dropoff_map_bucket),]$is_interbucket_into_11 <- 1
  
  intrabucket_frequency <- as.data.frame.matrix(table(train$pickup_map_buckets, train$dropoff_map_buckets))
  
  
  
  
  # --------------
  # Create all possible groups and subgroups
  # --------------
  
  numcol <- max(train$pickup_map_buckets,train$dropoff_map_buckets)*
    max(train$pickup_map_buckets_long,train$dropoff_map_buckets_long)*
    max(train$pickup_map_buckets_lat,train$dropoff_map_buckets_lat)
  
  columnnames = rep("",2*numcol)
  i=1
  for(mb in (1:max(train$pickup_map_buckets,train$dropoff_map_buckets))){
    for(ln in (1:max(train$pickup_map_buckets_long,train$dropoff_map_buckets_long))){
      for(lt in (1:max(train$pickup_map_buckets_lat,train$dropoff_map_buckets_lat))){
        
        mblnlt1 <- paste0("thru_g",mb,"ln",ln,"lt",lt)
        mblnlt2 <- paste0("ss_g",mb,"ln",ln,"lt",lt)
        columnnames[i] <- mblnlt1
        columnnames[i+1] <- mblnlt2
        i = i+2
      }
    }
  }
  rm(i,mblnlt1,mblnlt2,ln,lt,mb)
  path_matrix <- Matrix(data = 0,nrow = nrow(train),ncol = 2*numcol,sparse = T,byrow = F,
                        dimnames = list(as.character(seq(1,nrow(train))),columnnames))
  
  
  
  
  
  # ----------------------
  # Get the start and dropoff points in the path matrix
  # ----------------------
  start_matrix <- sparseMatrix(i = 1:nrow(train), j = give_col_no(train$pickup_map_buckets,train$pickup_map_buckets_long,train$pickup_map_buckets_lat,T), 
                               x = 1,dims = c(nrow(train),ncol(path_matrix)),dimnames = list(as.character(seq(1,nrow(train))),columnnames)
  )
  
  drop_matrix <- sparseMatrix(i = 1:nrow(train), j = give_col_no(train$dropoff_map_buckets,train$dropoff_map_buckets_long,train$dropoff_map_buckets_lat,T), 
                              x = 1,dims = c(nrow(train),ncol(path_matrix)),dimnames = list(as.character(seq(1,nrow(train))),columnnames)
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
  # train$pickup_bucket_coord <- 11
  # train$dropoff_bucket_coord <- 11
  # for(i in 1:nrow(bucket_xy_mapping)){
  #   train$pickup_bucket_coord[train$pickup_map_buckets==bucket_xy_mapping$MapBuckets[i]] <- bucket_xy_mapping$bucketcoord[i]
  #   train$dropoff_bucket_coord[train$dropoff_map_buckets==bucket_xy_mapping$MapBuckets[i]] <- bucket_xy_mapping$bucketcoord[i]
  # }
  # 
  # train$pickup_bucket_coord[train$pickup_map_buckets==8] <- 12
  # train$pickup_bucket_coord[train$pickup_map_buckets==7] <- 13
  # train$pickup_bucket_coord[train$pickup_map_buckets==2] <- 21
  # train$pickup_bucket_coord[train$pickup_map_buckets==10] <- 22
  # train$pickup_bucket_coord[train$pickup_map_buckets==11] <- 23
  # train$pickup_bucket_coord[train$pickup_map_buckets==9] <- 24
  # train$pickup_bucket_coord[train$pickup_map_buckets==6] <- 25
  # train$pickup_bucket_coord[train$pickup_map_buckets==3] <- 31
  # train$pickup_bucket_coord[train$pickup_map_buckets==4] <- 32
  # train$pickup_bucket_coord[train$pickup_map_buckets==5] <- 33
  # # dropoffs fresh buckets
  # train$dropoff_bucket_coord <- 11
  # train$dropoff_bucket_coord[train$dropoff_map_buckets==1] <- 11
  # train$dropoff_bucket_coord[train$dropoff_map_buckets==8] <- 12
  # train$dropoff_bucket_coord[train$dropoff_map_buckets==7] <- 13
  # train$dropoff_bucket_coord[train$dropoff_map_buckets==2] <- 21
  # train$dropoff_bucket_coord[train$dropoff_map_buckets==10] <- 22
  # train$dropoff_bucket_coord[train$dropoff_map_buckets==11] <- 23
  # train$dropoff_bucket_coord[train$dropoff_map_buckets==9] <- 24
  # train$dropoff_bucket_coord[train$dropoff_map_buckets==6] <- 25
  # train$dropoff_bucket_coord[train$dropoff_map_buckets==3] <- 31
  # train$dropoff_bucket_coord[train$dropoff_map_buckets==4] <- 32
  # train$dropoff_bucket_coord[train$dropoff_map_buckets==5] <- 33
  
  intrabucket_frequency2 <- as.data.frame.matrix(table(train$pickup_bucket_coord,train$dropoff_bucket_coord))
  
  # ----------------------
  # Register all paths in a smaller dense matrix to add to the larger one later
  # ----------------------
  # path <- mapply(FUN = give_intrabucket_path,"pickup_map_buckets" = train$pickup_map_buckets,"pickup_longitude" = train$pickup_map_buckets_long,
  #                "pickup_latitude" = train$pickup_map_buckets_lat,
  #                "dropoff_map_buckets" =  train$dropoff_map_buckets,
  #                "dropoff_longitude" = train$dropoff_map_buckets_long,
  #                "dropoff_latitude" = train$dropoff_map_buckets_lat,SIMPLIFY = T)
  
  path <- mapply(FUN = give_path,"pickup_map_buckets" = train$pickup_map_buckets,"pickup_longitude" = train$pickup_map_buckets_long,
                 "pickup_latitude" = train$pickup_map_buckets_lat,
                 "dropoff_map_buckets" =  train$dropoff_map_buckets,
                 "dropoff_longitude" = train$dropoff_map_buckets_long,
                 "dropoff_latitude" = train$dropoff_map_buckets_lat,SIMPLIFY = T)
  
  path <- mapply(FUN = return100cols,vector=path,SIMPLIFY = T)
  path <- t(path)
  
  numrows <- nrow(train)
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
  
  return(list("data_frame" = train,"path_matrix" = path_matrix))
}