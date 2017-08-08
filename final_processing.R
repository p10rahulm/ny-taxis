
# ---------------
# initialize and load libraries
# ---------------
rm(list = ls())
library(data.table)
source("distance_functions.R")
library(caTools)

library(ggplot2)
library(ggmap)

# ---------------
# # Load files afresh
# ---------------
load(file = "rawdata/train.bin")
load(file = "rawdata/test.bin")
load(file = "rawdata/validation.bin")



# ---------------
# Split between training, test and validation
# ---------------



# ---------------
# creating more calculative fields
# ---------------

train$distance <- distance_between_points(train$pickup_longitude,train$pickup_latitude,
                                               train$dropoff_longitude,train$dropoff_latitude)

# --------------------
# # trip duration field in hours
# --------------------
train$duration_hours <- train$trip_duration/3600

# --------------------
# # time of day
# --------------------
train$hour_of_day <- as.numeric(substr(train$pickup_datetime,12,13))
train$minute <- as.numeric(substr(train$pickup_datetime,15,16))
# --------------------
# # speed
# --------------------

train$speed <- (train$distance / train$duration_hours)

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
train$map_buckets <- as.integer(0)
for(i in 1:nrow(mapbuckets)){
  train[(train$pickup_longitude < mapbuckets$LongMin[i] & train$pickup_longitude >= mapbuckets$LongMax[i]) & (train$pickup_latitude <= mapbuckets$LatMax[i] & train$pickup_latitude >  mapbuckets$LatMin[i]),"map_buckets" ] <- i
}

# ---------------------------
# getmap_buckets_lat+long for buckets 1-8
# ---------------------------

train$map_buckets_lat <- as.integer(0)
train$map_buckets_long <- as.integer(0)

for(i in 1:(nrow(mapbuckets)-1)){
    train[train$map_buckets==i,]$map_buckets_long <- ceiling((train[train$map_buckets==i,]$pickup_longitude-mapbuckets$LongMin[i])/((mapbuckets$LongMax[i]-mapbuckets$LongMin[i])/10))
    train[train$map_buckets==i,]$map_buckets_lat <- ceiling((train[train$map_buckets==i,]$pickup_latitude-mapbuckets$LatMin[i])/((mapbuckets$LatMax[i]-mapbuckets$LatMin[i])/10))
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

train[train$map_buckets==9 & is_point_above(hudson_river_line$a,hudson_river_line$b,hudson_river_line$c,
                                            train$pickup_longitude,train$pickup_latitude),]$map_buckets <- 10


# ---------------------------
# Split manhattan away
# ---------------------------

train[train$map_buckets==9 & 
        is_point_above(south_boundary_line$a,south_boundary_line$b,south_boundary_line$c,
                                            train$pickup_longitude,train$pickup_latitude) & 
        is_point_above(east_river_line$a,east_river_line$b,east_river_line$c,
                       train$pickup_longitude,train$pickup_latitude),]$map_buckets <- 11

# ---------------------------
# Get subbuckets for new jersey and brooklyn
# ---------------------------

for(i in 9:10){
  longmax <- min(train[train$map_buckets==i,]$pickup_longitude) 
  longmin <- max(train[train$map_buckets==i,]$pickup_longitude) 
  latmax <- max(train[train$map_buckets==i,]$pickup_latitude)
  latmin <- min(train[train$map_buckets==i,]$pickup_latitude)
  train[train$map_buckets==i,]$map_buckets_long <- ceiling((train[train$map_buckets==i,]$pickup_longitude-longmin)/((longmax-longmin)/10))
  train[train$map_buckets==i,]$map_buckets_lat <- ceiling((train[train$map_buckets==i,]$pickup_latitude-latmin)/((latmax-latmin)/10))
}
rm(latmax,latmin,longmax,longmin,i)
rm(south_boundary_line,east_river_line)
# ---------------------------
# Removing unnecessary points
# ---------------------------
train <- train[train$map_buckets!=0,]
train <- train[train$map_buckets == 11 | train$map_buckets_lat !=0,]
train <- train[train$map_buckets == 11 | train$map_buckets_long !=0,]

# ---------------------------
# Generate subbuckets for Manhattan
# ---------------------------

# Get the parallel line from hudson river


east_manhattan <- parallel_line_thru_point(hudson_river_line$a,hudson_river_line$b,hudson_river_line$c,
                         east_river$x1,east_river$y1)

north_manhattan <- perpendicular_line_thru_point(hudson_river_line$a,hudson_river_line$b,hudson_river_line$c,
                                           east_river$x1,east_river$y1)


south_manhattan <- perpendicular_line_thru_point(hudson_river_line$a,hudson_river_line$b,hudson_river_line$c,
                                           east_river$x2,east_river$y2)




train[train$map_buckets == 11,]$map_buckets_long <- ceiling(perpendicular_distance_from_point_to_line(hudson_river_line$a,hudson_river_line$b,hudson_river_line$c,
                                                                                train[train$map_buckets == 11,]$pickup_longitude,train[train$map_buckets == 11,]$pickup_latitude)*10/
  distance_between_parallel_lines(hudson_river_line$a,hudson_river_line$b,hudson_river_line$c,
                                                                            east_manhattan$a,east_manhattan$b,east_manhattan$c))


train[train$map_buckets == 11,]$map_buckets_lat <- ceiling(perpendicular_distance_from_point_to_line(north_manhattan$a,north_manhattan$b,north_manhattan$c,
                                                                                                      train[train$map_buckets == 11,]$pickup_longitude,train[train$map_buckets == 11,]$pickup_latitude)*10/
                                                              distance_between_parallel_lines(north_manhattan$a,north_manhattan$b,north_manhattan$c,
                                                                                              south_manhattan$a,south_manhattan$b,south_manhattan$c))





# -----------------
# # Subbucket function
# -----------------
# Keep in mind, change in x is going to be change in longitude and change in y is going to be change in latitude
# i is long j is latitude

bucket_array <- function(bucket,i,j){
   if(bucket == 1){
  subbucket_x <- ceiling((i-74.24602)/(-74.04785--74.24602))*10
  subbucket_y<- ceiling((j-40.988000)/( 40.901577492-40.988000))*10
  return  (list(bucket,subbucket_x, subbucket_y))
   }
  if(bucket == 2){
    subbucket_x <- ceiling((i--74.24602)/(-74.04785--74.24602))*10
    subbucket_y <- ceiling((j- 40.901577492)/(40.700768- 40.901577492))*10
    return(list(bucket,subbucket_x, subbucket_y))
  }
  if(bucket == 3){
    subbucket_x <- ceiling((i--74.24602)/(-74.04785--74.24602))*10
    subbucket_y <- ceiling((j-40.700768)/(40.41323-40.700768))*10
    return(list(bucket,subbucket_x, subbucket_y))
  }
  if(bucket == 4){
    subbucket_x <- ceiling((i--74.04785)/(-73.915957--74.04785))*10
    subbucket_y <- ceiling((j-40.700768)/(40.41323-40.700768))*10
    return(list(bucket,subbucket_x, subbucket_y))
  }
  if(bucket == 5){
    subbucket_x <- ceiling((i--73.915957)/(-73.49034--73.915957))*10
    subbucket_y <- ceiling((j-40.700780)/(40.41323-40.700780))*10
    return(list(bucket,subbucket_x, subbucket_y))
  }
  if(bucket == 6){
    subbucket_x <- ceiling((i--73.915957)/(-73.49034--73.915957))*10
    subbucket_y <- ceiling((j- 40.901577492)/(40.700780- 40.901577492))*10
    return(list(bucket,subbucket_x, subbucket_y))
  }
  if(bucket == 7){
    subbucket_x <- ceiling((i--73.915957)/(-73.49034--73.915957))*10
    subbucket_y <- ceiling((j-40.988000)/( 40.901577492-40.988000))*10
    return(list(bucket,subbucket_x, subbucket_y))
  }
  if(bucket == 8){
    subbucket_x <- ceiling((i--74.04785)/(-74.04785--73.915957))*10
    subbucket_y <- ceiling((j-40.988000)/( 40.901577492-40.988000))*10
    return (list(bucket,subbucket_x, subbucket_y))
  }
  # if(bucket == 9){
  #   subbucket_x <- ceiling((i--74.04785)/(-73.915957--74.04785))*10
  #   subbucket_y <- ceiling((j- 40.901577492)/(40.700768- 40.901577492))*10
  #   return(bucket,subbucket_x, subbucket_y)
  # }
  else{ #for anything in bucket 9
    return (0)
  }
}

bucket_arrays <- bucket_array(train$map_buckets,train$pickup_longitude,train$pickup_latitude)


# ---------------
# # Manhattan grid
# ---------------

# variable labels:

x1 <- 40.70094
y1 <- -74.04785
x2 <- 40.90157
y2 <- -73.90502
x3 <- 40.88323
y3 <- -73.85665
x4 <- 40.69521
y4 <- -73.99086


# (py – qy)x + (qx – px)y + (pxqy – qxpy) = 0
# (y1-y2)x + (x2-x1)y + (x1*y2 - x2*y1) = 0

# lines have been GRAPHED

l1 <- function(x,y){
  # 0.0905x + 0.139188y +(-13.98926) = 0
  return ((y1-y2)*x + (x2-x1)*y +(x1*y2 - x2*y1))
}

l2 <- function(x,y){
  # perpendicular line...
  # (qx – px)*x + (py – qy)*y - ((qx – px)*x3 - (py – qy)*y3)
  # (y2-y1)*x + (x1-x2)*y - ((y1-y2)*x3 - (x2 - x1)*y3) = 0
  return ((y2-y1)*x + (x1-x2)*y - ((y1-y2)*x3 - (x2 - x1)*y3))
}

l3 <- function(x,y){
  # (x1-x2)*x - (y2-y1)*y + ((y2-y1)*y3 - (x1-x2)*x3)
  # return((x1-x2)*x - (y2-y1)*y + ((y2-y1)*y3 - (x1-x2)*x3))
  # this was wrong
  return ((y2-y3)*x + (x3-x2)*y +(x2*y3 - x3*y2) )
  
}

l4 <- function(x,y){
  # return ((x1-x2)*x - (y2-y1)*y + ((y2-y1)*y2 - (x1-x2)*x2))
  # (y2-y3)*x + (x3-x2)*y +((y2-y3)*x1 - (x3-x2)*y1) = 0
  # (y1-y2)x - (x2-x1)y + ((y1-y2)*y1 - (x2-x1)*x1) = 0
  
  # equation posed by Rahul...gives me perpendicular 
  # (x2-x1)*y -(y1-y2)*y + ((y1-y2)*y1-((x2-x1)*x1)
}

# creating matrix

lin <-as.data.frame(matrix(data=as.numeric(0), nrow = 4, ncol = 2))
colnames(lin) <- c("C_value","slope" )
rownames(lin) <- c("line 1", "line 2","line 3","line 4")

lin[1,1] <- (x1*y2 - x2*y1)
lin[1,2] <- (y1-y2)/(x1-x2)
lin[2,1] <- (y1-y2)*x3 - (x2 - x1)*y3
lin[2,2] <- (y3-y4)/(x3-x4)
lin[3,1] <- (y2-y1)*y3 - (x1-x2)*x3
lin[3,2] <- (y2-y3)/(x2-x3)
lin[4,1] <- (y2-y1)*y2 - (x1-x2)*x2
lin[4,2] <- (y4-y1)/(x4-x1)



# l1 <- function(x,y){
#   m <- ((y1-y3)/(x1-x3)) 
#   b <- y3-(m)*x3
#   return ((m*x)-y+b)
# }

line_equation <- function(line_number, x, y){
  return((lin[line_number,1]*x)+(lin[line_number,2]*y)+(lin[line_number,3]))
}







