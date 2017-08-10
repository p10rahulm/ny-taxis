# ---------------
# initialize and load libraries
# ---------------
rm(list = ls())
library(data.table)
source("functions.R")
library(caTools)
library(Matrix)

# library(ggplot2)
# library(ggmap)

# ---------------
# # Load files afresh
# ---------------

load(file = "rawdata/train.bin")
load(file = "rawdata/test.bin")
load(file = "rawdata/validation.bin")



# ---------------
# Split between training, test and validation
# ---------------

# Basic data.table mapping
train <- data.table(train)
test <- data.table(test)
validation <- data.table(validation)

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

# Moves through external mapbuckets.csv of mapbucket boundary coordinates

# Pickup:
train$pickup_map_buckets <- as.numeric(0)

for(i in 1:nrow(mapbuckets)){
  train[(train$pickup_longitude < mapbuckets$LongMin[i] & train$pickup_longitude >= mapbuckets$LongMax[i]) & 
          (train$pickup_latitude <= mapbuckets$LatMax[i] & train$pickup_latitude >  mapbuckets$LatMin[i]),"pickup_map_buckets" ] <- i
}


# Dropoff:
train$dropoff_map_buckets <- as.numeric(0)

for(i in 1:nrow(mapbuckets)){
  train[(train$dropoff_longitude < mapbuckets$LongMin[i] & train$dropoff_longitude >= mapbuckets$LongMax[i]) & 
          (train$dropoff_latitude <= mapbuckets$LatMax[i] & train$dropoff_latitude >  mapbuckets$LatMin[i]),"dropoff_map_buckets" ] <- i
}

# ---------------------------
# get pickup_map_buckets_lat+long for buckets 1-8
# ---------------------------

# initialization of pickup long/lat

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

# initialization of pickup long/lat

train$dropoff_map_buckets_lat <- as.numeric(0)
train$dropoff_map_buckets_long <- as.numeric(0)

for(i in 1:(nrow(mapbuckets)-1)){
  train[train$dropoff_map_buckets==i,]$dropoff_map_buckets_long <- ceiling((train[train$dropoff_map_buckets==i,]$dropoff_longitude-mapbuckets$LongMin[i])/((mapbuckets$LongMax[i]-mapbuckets$LongMin[i])/10))
  train[train$dropoff_map_buckets==i,]$dropoff_map_buckets_lat <- ceiling((train[train$dropoff_map_buckets==i,]$dropoff_latitude-mapbuckets$LatMin[i])/((mapbuckets$LatMax[i]-mapbuckets$LatMin[i])/10))
}

#removing unnessacary variable i

rm(i)




# ---------------------------
# Get Lines 9,10,11 for new jersey, brooklyn and manhattan respectively
# ---------------------------

# three different sets of coordinates to illustrate the boundaries of Manhattan Island...Hudson being the west boundary

hudson_river <- list(x1=-73.90502,y1=40.90157,x2 = -74.04785,y2 = 40.70094)
east_river <- list(x1=-73.856652,y1=40.88322,x2 = -73.99086,y2 = 40.69521)
south_boundary <- list(x1=-73.92476,y1=40.72319,x2 = -74.02347,y2 = 40.69821)

hudson_river_line <- generate_line(hudson_river$x1,hudson_river$y1,hudson_river$x2,hudson_river$y2)
east_river_line <- generate_line(east_river$x1,east_river$y1,east_river$x2,east_river$y2)
south_boundary_line <- generate_line(south_boundary$x1,south_boundary$y1,south_boundary$x2,south_boundary$y2)


# ---------------------------
# Split New jersey away
# ---------------------------

# Area that lies above Hudson river (New Jersey) it split away from Manhattan island

# Pickups
train[train$pickup_map_buckets==9 & is_point_above(hudson_river_line$a,hudson_river_line$b,hudson_river_line$c,
                                            train$pickup_longitude,train$pickup_latitude),]$pickup_map_buckets <- 10

# Dropoffs
train[train$dropoff_map_buckets==9 & is_point_above(hudson_river_line$a,hudson_river_line$b,hudson_river_line$c,
                                                   train$dropoff_longitude,train$dropoff_latitude),]$dropoff_map_buckets <- 10


# ---------------------------
# Split manhattan away
# ---------------------------

# Area between all three boundary lines (Hudson, East, and South) is designated as Manhattan Island

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

# Subbucketing for NJ and Brooklyn

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

# removing unnessacary variables

rm(latmax,latmin,longmax,longmin,i)
rm(south_boundary_line,east_river_line)

# ---------------------------
# Removing unnecessary points
# ---------------------------

# Pickups
train <- train[train$pickup_map_buckets!=0,]
train <- train[train$pickup_map_buckets == 11 | train$pickup_map_buckets_lat !=0,]
train <- train[train$pickup_map_buckets == 11 | train$pickup_map_buckets_long !=0,]

# Dropoffs
train <- train[train$dropoff_map_buckets!=0,]
train <- train[train$dropoff_map_buckets == 11 | train$dropoff_map_buckets_lat !=0,]
train <- train[train$dropoff_map_buckets == 11 | train$dropoff_map_buckets_long !=0,]

# ---------------------------
# Get boundary points/lines for manhattan
# ---------------------------

# Get the parallel line from hudson river


east_manhattan <- parallel_line_thru_point(hudson_river_line$a,hudson_river_line$b,hudson_river_line$c,
                                           east_river$x1,east_river$y1)

# Get perpendicular lines through east and hudson

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


#removing unnessacary variables

rm(east_manhattan,east_river,hudson_river,hudson_river_line,north_manhattan,south_boundary,south_manhattan)

# ----------------
# checking frequency of travel between major map_buckets
# ----------------

train$is_intra_bucket <- train$pickup_map_buckets == train$dropoff_map_buckets
intrabucket_frequency <- as.data.frame.matrix(table(train$pickup_map_buckets, train$dropoff_map_buckets))

# --------------
# Create all possible groups and subgroups
# --------------

#path matrix count of if a bucket passes through another or 

numcol <- max(train$pickup_map_buckets,train$dropoff_map_buckets)*
  max(train$pickup_map_buckets_long,train$dropoff_map_buckets_long)*
  max(train$pickup_map_buckets_lat,train$dropoff_map_buckets_lat)

columnnames = rep("",2*numcol)
i=1
for(mb in (1:max(train$pickup_map_buckets,train$dropoff_map_buckets))){
  for(ln in (1:max(train$pickup_map_buckets_long,train$dropoff_map_buckets_long))){
    for(lt in (1:max(train$pickup_map_buckets_lat,train$dropoff_map_buckets_lat))){
      
      mblnlt1 <- paste0("thru_g",mb,"ln",ln,"lt",lt)
      mblnlt2 <- paste0("ss_g",mb,"ln",ln,"lt",lt)    # what is this?
      columnnames[i] <- mblnlt1
      columnnames[i+1] <- mblnlt2
      i = i+2
    }
  }
}
rm(i,mblnlt1,mblnlt2,ln,lt,mb)

path_matrix <- Matrix(data = 0,nrow = nrow(train),ncol = 2*numcol,sparse = T,byrow = F,
                      dimnames = list(as.character(seq(1,nrow(train))),columnnames))
# rm(numcol,columnnames)


# for(i in 1:nrow(train)){
#   path_matrix[i,paste0("ss_g",train[i,"pickup_map_buckets"],"ln",train[i,"pickup_map_buckets_long"],"lt",train[i,"pickup_map_buckets_lat"])] <- 1
#   path_matrix[i,paste0("ss_g",train[i,"dropoff_map_buckets"],"ln",train[i,"dropoff_map_buckets_long"],"lt",train[i,"dropoff_map_buckets_lat"])] <- 1
# }

# pickupcols <- paste0("ss_g",train$pickup_map_buckets,"ln",train$pickup_map_buckets_long,"lt",train$pickup_map_buckets_lat)
# path_matrix[,paste0("ss_g",train$pickup_map_buckets,"ln",train$pickup_map_buckets_long,"lt",train$pickup_map_buckets_lat)] <- 1
# path_matrix[,paste0("ss_g",train$dropoff_map_buckets,"ln",train$dropoff_map_buckets_long,"lt",train$dropoff_map_buckets_lat)] <- 1

#sparse matrix of the starting point

start_matrix <- sparseMatrix(i = 1:nrow(train), j = give_col_no(train$pickup_map_buckets,train$pickup_map_buckets_long,train$pickup_map_buckets_lat,T), 
             x = 1,dims = c(nrow(train),ncol(path_matrix)),dimnames = list(as.character(seq(1,nrow(train))),columnnames)
            )

#sparse matrix of the drop point

drop_matrix <- sparseMatrix(i = 1:nrow(train), j = give_col_no(train$dropoff_map_buckets,train$dropoff_map_buckets_long,train$dropoff_map_buckets_lat,T), 
                             x = 1,dims = c(nrow(train),ncol(path_matrix)),dimnames = list(as.character(seq(1,nrow(train))),columnnames)
                )

path_matrix <- start_matrix + drop_matrix
rm(start_matrix,drop_matrix)

# ---------------------
# Intrabucket transfer
# ---------------------

# intrabucket movement, moves down or up and then to the right or left based on pick up and drop off point

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
  output <- output[-length(output)]
  return(output)
}


# start_matrix[1,2352:2355]


