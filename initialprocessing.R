
# ---------------
# initialize and load libraries
# ---------------
rm(list = ls())
library(data.table)
source("distance_functions.R")

# # ---------------
# # reading file
# # ---------------
# taxi_train <- read.csv("rawdata/train.csv")
# taxi_test <- read.csv("rawdata/test.csv")
# 
# taxi_train <- data.table(taxi_train)
# taxi_test <- data.table(taxi_test)
# # ---------------
# # save files as binary
# # ---------------
# save(taxi_train,file="rawdata/taxi_train.bin")
# save(taxi_test,file="rawdata/taxi_test.bin")

# ---------------
# # Load files afresh
# ---------------
load(file = "rawdata/taxi_train.bin")
load(file = "rawdata/taxi_test.bin")


# ---------------
# Create a test and training data set
# ---------------

## 75% of the sample size
smp_size <- floor(0.75 * nrow(taxi_train))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(taxi_train)), size = smp_size)

train <- taxi_train[train_ind, ]
test <- taxi_train[-train_ind, ]

save(train,file="rawdata/train.bin")
save(test,file="rawdata/test.bin")
save(validation,file="rawdata/validation.bin")


# ---------------
# # View summary
# ---------------
#
# str(taxi_train)
# str(taxi_test)
# view more summaries
# summary(taxi_train)
# creating more calculative fields
# distance field

taxi_train$distance <- sqrt((taxi_train$pickup_longitude-taxi_train$dropoff_longitude)^2+(taxi_train$pickup_latitude-taxi_train$dropoff_latitude)^2)*52.7163052

# --------------------
# # trip duration field in hours
# --------------------
taxi_train$duration_hours <- taxi_train$trip_duration/3600

# --------------------
# # time of day
# --------------------
taxi_train$hour_of_day <- as.numeric(substr(taxi_train$pickup_datetime,12,13))
taxi_train$minute <- as.numeric(substr(taxi_train$pickup_datetime,15,16))
# --------------------
# # speed
# --------------------

taxi_train$speed <- (taxi_train$distance / taxi_train$duration_hours)

# --------------------
# # distance buckets
# --------------------

taxi_train$distance_buckets <- 0
taxi_train[taxi_train$distance < .5,"distance_buckets"] <- 1
taxi_train[taxi_train$distance < 1 & taxi_train$distance >= .5,]$distance_buckets <- 2
taxi_train[taxi_train$distance < 2.5 & taxi_train$distance >= 1,]$distance_buckets <- 3
taxi_train[taxi_train$distance < 5 & taxi_train$distance >= 2.5,]$distance_buckets <- 4
taxi_train[taxi_train$distance < 10 & taxi_train$distance >= 5,]$distance_buckets <- 5
taxi_train[taxi_train$distance < 20 & taxi_train$distance >= 10,]$distance_buckets <- 6
taxi_train[taxi_train$distance >= 20,]$distance_buckets <- 7

# --------------------
# # some more processing
# --------------------

taxi_train$sf <- taxi_train$store_and_fwd_flag =="Y"
taxi_train$store_and_fwd_flag <- NULL


# # ---------------
# # subsetting
# # ---------------

# b1 <- taxi_train[taxi_train$distance_buckets == 1,"speed"]
# b1$speedB1 <- b1$speed
# b1$speed <- NULL
# 
# b2 <- taxi_train[taxi_train$distance_buckets == 2,"speed"]
# b2$speedB2 <- b2$speed
# b2$speed <- NULL
# 
# b3 <- taxi_train[taxi_train$distance_buckets == 3,"speed"]
# b3$speedB3 <- b3$speed
# b3$speed <- NULL
# 
# b4 <- taxi_train[taxi_train$distance_buckets == 4,"speed"]
# b4$speedB4 <- b4$speed
# b4$speed <- NULL
# 
# b5 <- taxi_train[taxi_train$distance_buckets == 5,"speed"]
# b5$speedB5 <- b5$speed
# b5$speed <- NULL
# 
# b6 <- taxi_train[taxi_train$distance_buckets == 6,"speed"]
# b6$speedB6 <- b6$speed
# b6$speed <- NULL
# 
# b7 <- taxi_train[taxi_train$distance_buckets == 7,"speed"]
# b7$speedB7 <- b7$speed
# b7$speed <- NULL
# 
# b1s <- summary(b1)
# b2s <- summary(b2)
# b3s <- summary(b3)
# b4s <- summary(b4)
# b5s <- summary(b5)
# b6s <- summary(b6)
# b7s <- summary(b7)
# 
# summary_dbuckets <- cbind(b1s,b2s,b3s,b4s,b5s,b6s,b7s)

# ---------------
# # Create summary buckets
# ---------------

taxi_train <- data.table(taxi_train)

# --------------------
# # distance buckets
# --------------------

dbuckets <- as.data.frame(taxi_train[,
                                             {list(
                                               "num_in_bucket" = .N,
                                               "min_speed" = min(speed),
                                               "max_speed" = max(speed),
                                               "avg_speed" = mean(speed),
                                               "median_speed" = median(speed),
                                               "p5_speed" = quantile(speed,0.05),
                                               "p15_speed" = quantile(speed,0.15),
                                               "p25_speed" = quantile(speed,0.25),
                                               "p35_speed" = quantile(speed,0.35),
                                               "p45_speed" = quantile(speed,0.45),
                                               "p55_speed" = quantile(speed,0.55),
                                               "p65_speed" = quantile(speed,0.65),
                                               "p75_speed" = quantile(speed,0.75),
                                               "p85_speed" = quantile(speed,0.85),
                                               "p95_speed" = quantile(speed,0.95)
                                             )
                                             }          
                                             ,
                                             by = list("buckets"= distance_buckets)])
dbuckets$buckets <- paste0("distance_bucket_",as.character(dbuckets$buckets))

# --------------------
# # hour of day
# --------------------

hbuckets <- as.data.frame(taxi_train[,
                                     {list(
                                       "num_in_bucket" = .N,
                                       "min_speed" = min(speed),
                                       "max_speed" = max(speed),
                                       "avg_speed" = mean(speed),
                                       "median_speed" = median(speed),
                                       "p5_speed" = quantile(speed,0.05),
                                       "p15_speed" = quantile(speed,0.15),
                                       "p25_speed" = quantile(speed,0.25),
                                       "p35_speed" = quantile(speed,0.35),
                                       "p45_speed" = quantile(speed,0.45),
                                       "p55_speed" = quantile(speed,0.55),
                                       "p65_speed" = quantile(speed,0.65),
                                       "p75_speed" = quantile(speed,0.75),
                                       "p85_speed" = quantile(speed,0.85),
                                       "p95_speed" = quantile(speed,0.95)
                                     )
                                     }
                                     ,
                                     by = list("buckets"= hour_of_day)])
hbuckets$buckets <- paste0("hour_bucket_",as.character(hbuckets$buckets))

# --------------------
# # vendor
# --------------------

vbuckets <- as.data.frame(taxi_train[,
                                     {list(
                                       "num_in_bucket" = .N,
                                       "min_speed" = min(speed),
                                       "max_speed" = max(speed),
                                       "avg_speed" = mean(speed),
                                       "median_speed" = median(speed),
                                       "p5_speed" = quantile(speed,0.05),
                                       "p15_speed" = quantile(speed,0.15),
                                       "p25_speed" = quantile(speed,0.25),
                                       "p35_speed" = quantile(speed,0.35),
                                       "p45_speed" = quantile(speed,0.45),
                                       "p55_speed" = quantile(speed,0.55),
                                       "p65_speed" = quantile(speed,0.65),
                                       "p75_speed" = quantile(speed,0.75),
                                       "p85_speed" = quantile(speed,0.85),
                                       "p95_speed" = quantile(speed,0.95)
                                     )
                                     }
                                     ,
                                     by = list("buckets"= vendor_id)])
vbuckets$buckets <- paste0("vendor_bucket_",as.character(vbuckets$buckets))

# --------------------
# # store_and_fwd_flag
# --------------------

sfbuckets <- as.data.frame(taxi_train[,
                                     {list(
                                       "num_in_bucket" = .N,
                                       "min_speed" = min(speed),
                                       "max_speed" = max(speed),
                                       "avg_speed" = mean(speed),
                                       "median_speed" = median(speed),
                                       "p5_speed" = quantile(speed,0.05),
                                       "p15_speed" = quantile(speed,0.15),
                                       "p25_speed" = quantile(speed,0.25),
                                       "p35_speed" = quantile(speed,0.35),
                                       "p45_speed" = quantile(speed,0.45),
                                       "p55_speed" = quantile(speed,0.55),
                                       "p65_speed" = quantile(speed,0.65),
                                       "p75_speed" = quantile(speed,0.75),
                                       "p85_speed" = quantile(speed,0.85),
                                       "p95_speed" = quantile(speed,0.95)
                                     )
                                     }
                                     ,
                                     by = list("buckets"= sf)])
sfbuckets$buckets <- paste0("sf_bucket_",as.character(sfbuckets$buckets))

# --------------------
# # passenger count
# --------------------

pbuckets <- as.data.frame(taxi_train[,
                                     {list(
                                       "num_in_bucket" = .N,
                                       "min_speed" = min(speed),
                                       "max_speed" = max(speed),
                                       "avg_speed" = mean(speed),
                                       "median_speed" = median(speed),
                                       "p5_speed" = quantile(speed,0.05),
                                       "p15_speed" = quantile(speed,0.15),
                                       "p25_speed" = quantile(speed,0.25),
                                       "p35_speed" = quantile(speed,0.35),
                                       "p45_speed" = quantile(speed,0.45),
                                       "p55_speed" = quantile(speed,0.55),
                                       "p65_speed" = quantile(speed,0.65),
                                       "p75_speed" = quantile(speed,0.75),
                                       "p85_speed" = quantile(speed,0.85),
                                       "p95_speed" = quantile(speed,0.95)
                                     )
                                     }
                                     ,
                                     by = list("buckets"= passenger_count)])
pbuckets$buckets <- paste0("num_passenger_bucket_",as.character(pbuckets$buckets))


all_buckets <- rbind(dbuckets,hbuckets,pbuckets,sfbuckets,vbuckets)
rm(dbuckets,hbuckets,pbuckets,sfbuckets,vbuckets)

# --------------------
# # Double Aggregation
# --------------------

dhbuckets <- as.data.frame(taxi_train[,
                                     {list(
                                       "num_in_bucket" = .N,
                                       "min_speed" = min(speed),
                                       "max_speed" = max(speed),
                                       "avg_speed" = mean(speed),
                                       "median_speed" = median(speed),
                                       "p5_speed" = quantile(speed,0.05),
                                       "p15_speed" = quantile(speed,0.15),
                                       "p25_speed" = quantile(speed,0.25),
                                       "p35_speed" = quantile(speed,0.35),
                                       "p45_speed" = quantile(speed,0.45),
                                       "p55_speed" = quantile(speed,0.55),
                                       "p65_speed" = quantile(speed,0.65),
                                       "p75_speed" = quantile(speed,0.75),
                                       "p85_speed" = quantile(speed,0.85),
                                       "p95_speed" = quantile(speed,0.95)
                                     )
                                     }
                                     ,
                                    by = list("hourbuckets"= hour_of_day,"distancebuckets"= distance_buckets)])
dhbuckets$buckets <- paste0("distance_",as.character(dhbuckets$buckets))

# -----------------------
# # Plotting with map
# -----------------------

library(ggplot2)
library(ggmap)

lon <- c(taxi_train$pickup_longitude[1:100000])
lat <- c(taxi_train$pickup_latitude[1:100000])
df <- as.data.frame(cbind(lon,lat))

mapgilbert <- get_map(location = c(lon = mean(df$lon), lat = mean(df$lat)), zoom = 11,
                      maptype = "satellite", scale = 2)

ggmap(mapgilbert) +
  geom_point(data = df, aes(x = lon, y = lat, fill = "red", alpha = 0.8), size = 2, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)

# -----------------
# # plotting on x y 
# -----------------

plot(taxi_train$pickup_longitude[1:10000])
plot(taxi_train$pickup_latitude[1:10000])

#----------------------
# # Creating long / lat pick up buckets
#----------------------
# http://www.bdcc.co.uk/Gmaps/ll_grat_v3_demo.htm

# top left: (40.85000, -74.03333)
# bottom left: (40.70000, -74.03333)
# top right: (40.85000, -73.93333)
# bottom right: (40.70000, -73.93333)


taxi_train$pickup_buckets <- 0

#row 1

taxi_train[(taxi_train$pickup_longitude < -74.01663 & taxi_train$pickup_longitude >= -74.0333) & (taxi_train$pickup_latitude < 40.850 & taxi_train$pickup_latitude >= 40.825),"pickup_buckets" ] <- 1
taxi_train[(taxi_train$pickup_longitude < -73.99999 & taxi_train$pickup_longitude >= -74.01663) & (taxi_train$pickup_latitude < 40.850 & taxi_train$pickup_latitude >= 40.825),"pickup_buckets"] <- 2
taxi_train[(taxi_train$pickup_longitude < -73.98333 & taxi_train$pickup_longitude >= -73.99999) & (taxi_train$pickup_latitude < 40.850 & taxi_train$pickup_latitude >= 40.825),"pickup_buckets"] <- 3
taxi_train[(taxi_train$pickup_longitude < -73.96663 & taxi_train$pickup_longitude >= -73.98333) & (taxi_train$pickup_latitude < 40.850 & taxi_train$pickup_latitude >= 40.825),"pickup_buckets"] <- 4
taxi_train[(taxi_train$pickup_longitude < -73.94999 & taxi_train$pickup_longitude >= -73.96663) & (taxi_train$pickup_latitude < 40.850 & taxi_train$pickup_latitude >= 40.825),"pickup_buckets"] <- 5
taxi_train[(taxi_train$pickup_longitude < -73.93333 & taxi_train$pickup_longitude >= -73.94999) & (taxi_train$pickup_latitude < 40.850 & taxi_train$pickup_latitude >= 40.825),"pickup_buckets"] <- 6

# row 2
  
taxi_train[(taxi_train$pickup_longitude < -74.01663 & taxi_train$pickup_longitude >= -74.03333) & (taxi_train$pickup_latitude < 40.825 & taxi_train$pickup_latitude >= 40.800),"pickup_buckets"] <- 7
taxi_train[(taxi_train$pickup_longitude < -73.99999 & taxi_train$pickup_longitude >= -74.01663) & (taxi_train$pickup_latitude < 40.825 & taxi_train$pickup_latitude >= 40.800),"pickup_buckets"] <- 8
taxi_train[(taxi_train$pickup_longitude < -73.98333 & taxi_train$pickup_longitude >= -73.99999) & (taxi_train$pickup_latitude < 40.825 & taxi_train$pickup_latitude >= 40.800),"pickup_buckets"] <- 9
taxi_train[(taxi_train$pickup_longitude < -73.96663 & taxi_train$pickup_longitude >= -73.98333) & (taxi_train$pickup_latitude < 40.825 & taxi_train$pickup_latitude >= 40.800),"pickup_buckets"] <- 10
taxi_train[(taxi_train$pickup_longitude < -73.94999 & taxi_train$pickup_longitude >= -73.96663) & (taxi_train$pickup_latitude < 40.825 & taxi_train$pickup_latitude >= 40.800),"pickup_buckets"] <- 11
taxi_train[(taxi_train$pickup_longitude < -73.93333 & taxi_train$pickup_longitude >= -73.94999) & (taxi_train$pickup_latitude < 40.825 & taxi_train$pickup_latitude >= 40.800),"pickup_buckets"] <- 12

#row 3
  
taxi_train[(taxi_train$pickup_longitude < -74.01663 & taxi_train$pickup_longitude >= -74.03333) & (taxi_train$pickup_latitude < 40.800 & taxi_train$pickup_latitude >= 40.775),"pickup_buckets"] <- 13
taxi_train[(taxi_train$pickup_longitude < -73.99999 & taxi_train$pickup_longitude >= -74.01663) & (taxi_train$pickup_latitude < 40.800 & taxi_train$pickup_latitude >= 40.775),"pickup_buckets"] <- 14
taxi_train[(taxi_train$pickup_longitude < -73.98333 & taxi_train$pickup_longitude >= -73.99999) & (taxi_train$pickup_latitude < 40.800 & taxi_train$pickup_latitude >= 40.775),"pickup_buckets"] <- 15
taxi_train[(taxi_train$pickup_longitude < -73.96663 & taxi_train$pickup_longitude >= -73.98333) & (taxi_train$pickup_latitude < 40.800 & taxi_train$pickup_latitude >= 40.775),"pickup_buckets"] <- 16
taxi_train[(taxi_train$pickup_longitude < -73.94999 & taxi_train$pickup_longitude >= -73.96663) & (taxi_train$pickup_latitude < 40.800 & taxi_train$pickup_latitude >= 40.775),"pickup_buckets"] <- 17
taxi_train[(taxi_train$pickup_longitude < -73.93333 & taxi_train$pickup_longitude >= -73.94999) & (taxi_train$pickup_latitude < 40.800 & taxi_train$pickup_latitude >= 40.775),"pickup_buckets"] <- 18

#row 4
  
taxi_train[(taxi_train$pickup_longitude < -74.01663 & taxi_train$pickup_longitude >= -74.03333) & (taxi_train$pickup_latitude < 40.775 & taxi_train$pickup_latitude >= 40.750),"pickup_buckets"] <- 19
taxi_train[(taxi_train$pickup_longitude < -73.99999 & taxi_train$pickup_longitude >= -74.01663) & (taxi_train$pickup_latitude < 40.775 & taxi_train$pickup_latitude >= 40.750),"pickup_buckets"] <- 20
taxi_train[(taxi_train$pickup_longitude < -73.98333 & taxi_train$pickup_longitude >= -73.99999) & (taxi_train$pickup_latitude < 40.775 & taxi_train$pickup_latitude >= 40.750),"pickup_buckets"] <- 21
taxi_train[(taxi_train$pickup_longitude < -73.96663 & taxi_train$pickup_longitude >= -73.98333) & (taxi_train$pickup_latitude < 40.775 & taxi_train$pickup_latitude >= 40.750),"pickup_buckets"] <- 22
taxi_train[(taxi_train$pickup_longitude < -73.94999 & taxi_train$pickup_longitude >= -73.96663) & (taxi_train$pickup_latitude < 40.775 & taxi_train$pickup_latitude >= 40.750),"pickup_buckets"] <- 23
taxi_train[(taxi_train$pickup_longitude < -73.93333 & taxi_train$pickup_longitude >= -73.94999) & (taxi_train$pickup_latitude < 40.775 & taxi_train$pickup_latitude >= 40.750),"pickup_buckets"] <- 24

# row 5
  
taxi_train[(taxi_train$pickup_longitude < -74.01663 & taxi_train$pickup_longitude >= -74.03333) & (taxi_train$pickup_latitude < 40.750 & taxi_train$pickup_latitude >= 40.725),"pickup_buckets"] <- 25
taxi_train[(taxi_train$pickup_longitude < -73.99999 & taxi_train$pickup_longitude >= -74.01663) & (taxi_train$pickup_latitude < 40.750 & taxi_train$pickup_latitude >= 40.725),"pickup_buckets"] <- 26
taxi_train[(taxi_train$pickup_longitude < -73.98333 & taxi_train$pickup_longitude >= -73.99999) & (taxi_train$pickup_latitude < 40.750 & taxi_train$pickup_latitude >= 40.725),"pickup_buckets"] <- 27
taxi_train[(taxi_train$pickup_longitude < -73.96663 & taxi_train$pickup_longitude >= -73.98333) & (taxi_train$pickup_latitude < 40.750 & taxi_train$pickup_latitude >= 40.725),"pickup_buckets"] <- 28
taxi_train[(taxi_train$pickup_longitude < -73.94999 & taxi_train$pickup_longitude >= -73.96663) & (taxi_train$pickup_latitude < 40.750 & taxi_train$pickup_latitude >= 40.725),"pickup_buckets"] <- 29
taxi_train[(taxi_train$pickup_longitude < -73.93333 & taxi_train$pickup_longitude >= -73.94999) & (taxi_train$pickup_latitude < 40.750 & taxi_train$pickup_latitude >= 40.725),"pickup_buckets"] <- 30

# row 6
  
taxi_train[(taxi_train$pickup_longitude < -74.01663 & taxi_train$pickup_longitude >= -74.03333) & (taxi_train$pickup_latitude < 40.725 & taxi_train$pickup_latitude >= 40.700),"pickup_buckets"] <- 31
taxi_train[(taxi_train$pickup_longitude < -73.99999 & taxi_train$pickup_longitude >= -74.01663) & (taxi_train$pickup_latitude < 40.725 & taxi_train$pickup_latitude >= 40.700),"pickup_buckets"] <- 32
taxi_train[(taxi_train$pickup_longitude < -73.98333 & taxi_train$pickup_longitude >= -73.99999) & (taxi_train$pickup_latitude < 40.725 & taxi_train$pickup_latitude >= 40.700),"pickup_buckets"] <- 33 
taxi_train[(taxi_train$pickup_longitude < -73.96663 & taxi_train$pickup_longitude >= -73.98333) & (taxi_train$pickup_latitude < 40.725 & taxi_train$pickup_latitude >= 40.700),"pickup_buckets"] <- 34
taxi_train[(taxi_train$pickup_longitude < -73.94999 & taxi_train$pickup_longitude >= -73.96663) & (taxi_train$pickup_latitude < 40.725 & taxi_train$pickup_latitude >= 40.700),"pickup_buckets"] <- 35
taxi_train[(taxi_train$pickup_longitude < -73.93333 & taxi_train$pickup_longitude >= -73.94999) & (taxi_train$pickup_latitude < 40.725 & taxi_train$pickup_latitude >= 40.700),"pickup_buckets"] <- 36

# here it is for latitude coordinates
# taxi_train[taxi_train$pickup_latitude < 40.850 & taxi_train$pickup_latitude >= 40.825,"pickup_buckets"]
# taxi_train[taxi_train$pickup_latitude < 40.825 & taxi_train$pickup_latitude >= 40.800,"pickup_buckets"]
# taxi_train[taxi_train$pickup_latitude < 40.800 & taxi_train$pickup_latitude >= 40.775,"pickup_buckets"]
# taxi_train[taxi_train$pickup_latitude < 40.775 & taxi_train$pickup_latitude >= 40.750,"pickup_buckets"]
# taxi_train[taxi_train$pickup_latitude < 40.750 & taxi_train$pickup_latitude >= 40.725,"pickup_buckets"]
# taxi_train[taxi_train$pickup_latitude < 40.725 & taxi_train$pickup_latitude >= 40.700,"pickup_buckets"]

# ----------------------
# # pick up buckets
# ----------------------

pickup_location_buckets <- as.data.frame(taxi_train[,
                                      {list(
                                        "num_in_bucket" = .N,
                                        "avg_duration_Minutes" = mean(trip_duration/60),
                                        "median_duration_Minutes" = median(trip_duration/60),
                                        "min_speed" = min(speed),
                                        "max_speed" = max(speed),
                                        "avg_speed" = mean(speed),
                                        "median_speed" = median(speed),
                                        "p5_speed" = quantile(speed,0.05),
                                        "p15_speed" = quantile(speed,0.15),
                                        "p25_speed" = quantile(speed,0.25),
                                        "p35_speed" = quantile(speed,0.35),
                                        "p45_speed" = quantile(speed,0.45),
                                        "p55_speed" = quantile(speed,0.55),
                                        "p65_speed" = quantile(speed,0.65),
                                        "p75_speed" = quantile(speed,0.75),
                                        "p85_speed" = quantile(speed,0.85),
                                        "p95_speed" = quantile(speed,0.95)
                                      )
                                      }
                                      ,
                                      by = list("pick_upbuckets"= pickup_buckets)])
pickup_location_buckets$buckets <- paste0("pickup_location_bucket_",as.character(pickup_location_buckets$buckets))

# --------------------
# pick up duration buckets                # sensory overload
# --------------------
# pudbuckets <- as.data.frame(taxi_train[,
#                                       {list(
#                                         "num_in_bucket" = .N,
#                                         "min_speed" = min(speed),
#                                         "max_speed" = max(speed),
#                                         "avg_speed" = mean(speed),
#                                         "median_speed" = median(speed),
#                                         "p5_speed" = quantile(speed,0.05),
#                                         "p15_speed" = quantile(speed,0.15),
#                                         "p25_speed" = quantile(speed,0.25),
#                                         "p35_speed" = quantile(speed,0.35),
#                                         "p45_speed" = quantile(speed,0.45),
#                                         "p55_speed" = quantile(speed,0.55),
#                                         "p65_speed" = quantile(speed,0.65),
#                                         "p75_speed" = quantile(speed,0.75),
#                                         "p85_speed" = quantile(speed,0.85),
#                                         "p95_speed" = quantile(speed,0.95)
#                                       )
#                                       }
#                                       ,
#                                       by = list("duration_buckets"= distance_buckets,"pick_up_location_buckets"= pickup_buckets)])
# pudbuckets$buckets <- paste0("pickup_duration_bucket_",as.character(pudbuckets$buckets))


# ---------------
# # Map Buckets
# ---------------

# ---------------
# # filtering pick up data by long/lat
# ---------------

taxi_train <- taxi_train[(taxi_train$pickup_longitude > -74.24602) & (taxi_train$pickup_longitude < -73.49034)]
taxi_train <- taxi_train[(taxi_train$pickup_latitude > 40.41323) & (taxi_train$pickup_latitude < 40.98800)]


taxi_train$map_buckets <- 'z'

# -----------------
# # Bucket A
# -----------------

taxi_train[(taxi_train$pickup_longitude < -74.000780 & taxi_train$pickup_longitude >= -74.24602) & (taxi_train$pickup_latitude < 40.988000 & taxi_train$pickup_latitude >= 40.87492),"map_buckets" ] <- 'a'

# -----------------
# # Bucket B
# -----------------

taxi_train[(taxi_train$pickup_longitude < -74.000780 & taxi_train$pickup_longitude >= -74.24602) & (taxi_train$pickup_latitude < 40.847492 & taxi_train$pickup_latitude >= 40.700768),"map_buckets" ] <- 'b'

# -----------------
# # Bucket C
# -----------------

taxi_train[(taxi_train$pickup_longitude < -74.000780 & taxi_train$pickup_longitude >= -74.24602) & (taxi_train$pickup_latitude < 40.700780 & taxi_train$pickup_latitude >= 40.41323),"map_buckets" ] <- 'c'

# -----------------
# # Bucket D
# -----------------

taxi_train[(taxi_train$pickup_longitude < -73.915957 & taxi_train$pickup_longitude >= -74.00780) & (taxi_train$pickup_latitude < 40.700780 & taxi_train$pickup_latitude >= 40.41323),"map_buckets" ] <- 'd'

# -----------------
# # Bucket E
# -----------------

taxi_train[(taxi_train$pickup_longitude < -73.49034 & taxi_train$pickup_longitude >= -73.915957) & (taxi_train$pickup_latitude < 40.700780 & taxi_train$pickup_latitude >= 40.41323),"map_buckets" ] <- 'e'

# -----------------
# # Bucket F
# -----------------

taxi_train[(taxi_train$pickup_longitude < -73.49034 & taxi_train$pickup_longitude >= -73.915957) & (taxi_train$pickup_latitude < 40.801191 & taxi_train$pickup_latitude >= 40.700780),"map_buckets" ] <- 'f'

# -----------------
# # Bucket G
# -----------------

taxi_train[(taxi_train$pickup_longitude < -73.49034 & taxi_train$pickup_longitude >= -73.915957) & (taxi_train$pickup_latitude < 40.988000 & taxi_train$pickup_latitude >= 40.801191),"map_buckets" ] <- 'g'

# -----------------
# # Bucket H
# -----------------

taxi_train[(taxi_train$pickup_longitude < -73.915957 & taxi_train$pickup_longitude >= -74.00780) & (taxi_train$pickup_latitude < 40.98800 & taxi_train$pickup_latitude >= 40.801191),"map_buckets" ] <- 'h'

# -----------------
# # Bucket I
# -----------------

taxi_train[(taxi_train$pickup_longitude < -73.915957 & taxi_train$pickup_longitude >= -74.00780) & (taxi_train$pickup_latitude < 40.8974924 & taxi_train$pickup_latitude >= 40.700760),"map_buckets" ] <- 'i'

# -----------------
# # Map_buckets summary
# _________________

map_buckets_summary <- as.data.frame(taxi_train[,
                                                    {list(
                                                      "num_in_bucket" = .N,
                                                      "avg_duration_Minutes" = mean(trip_duration/60),
                                                      "median_duration_Minutes" = median(trip_duration/60),
                                                      "min_speed" = min(speed),
                                                      "max_speed" = max(speed),
                                                      "avg_speed" = mean(speed),
                                                      "median_speed" = median(speed),
                                                      "p5_speed" = quantile(speed,0.05),
                                                      "p15_speed" = quantile(speed,0.15),
                                                      "p25_speed" = quantile(speed,0.25),
                                                      "p35_speed" = quantile(speed,0.35),
                                                      "p45_speed" = quantile(speed,0.45),
                                                      "p55_speed" = quantile(speed,0.55),
                                                      "p65_speed" = quantile(speed,0.65),
                                                      "p75_speed" = quantile(speed,0.75),
                                                      "p85_speed" = quantile(speed,0.85),
                                                      "p95_speed" = quantile(speed,0.95)
                                                    )
                                                    }
                                                    ,
                                                    by = list("map_upbuckets"= map_buckets)])
map_buckets_summary$buckets <- paste0("pickup_location_bucket_",as.character(map_buckets_summary$buckets))

# ---------------
# # Manhattan grid
# ---------------

# variable labels:
# Wolfram Alpha : http://www.wolframalpha.com/widgets/view.jsp?id=7b9037eb9f5f7493a73df97a38bc58e6


x1 <- 40.709748
y1 <- -74.036782
# x2 <- 40.848936
x2 <- 40.7238462472
# y2 <- -73.946282
y2 <- -74.0275879719
x3 <- 40.697260
y3 <- -73.986695

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
  # For perpendicular
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

x4 <- 40.822 # find more exact points by solving for y
y4 <- -73.905

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



# l1 <- function(x,y){    #this format works
# a_fun <- ((y3 - y1) * x)
# b_fun <- ((x3-x1) * y)
# c_fun <- ((y1*x3 - y3*x1))
#   return (a_fun + b_fun + c_fun)
# }

# slope of l1 : -4.0463636363638
# distance between (x3, y3) and (x1, y1) : 0.0596

# l2 <- function(x,y){
#   m <- ((y1-y2)/(x1-x2)) 
#   b <- y2-(m)*x2
#   return ((m*x)-y+b)
# }

# l2 <- function(x,y){    #this format works
#   a_fun <- ((y2 - y1) * x)
#   b_fun <- ((x2-x1) * y)
#   c_fun <- ((y1*x2 - y1*x2))
#   return (a_fun + b_fun + c_fun)
# }


# l3 <- function(x,y){
#   a_fun <- ((y2 - y4) * x)
#   b_fun <- ((x2-x4) * y)
#   c_fun <- ((y4*x2 - y4*x2))
# return (a_fun + b_fun + c_fun)
# }

# l3 <- function(x,y){
#   m <- ((y4-y2)/(x4-x2)) 
#   b <- y4-(m)*x4
#   return ((m*x)-y+b)
# }

# l4 <- function(x,y){
#   a_fun <- ((y3 - y4) * x)
#   b_fun <- ((x3-x4) * y)
#   c_fun <- ((y4*x3 - y4*x3))
#   return (a_fun + b_fun + c_fun)
# }

# l4 <- function(x,y){
#   m <- ((y4-y3)/(x4-x3)) 
#   b <- y3-(m)*x3
#   return ((m*x)-y+b)
# }

# -------------------
# Distance functions!
# -------------------

distance_line_point <- function(line_number, x, y){
  numerator <- line_equation(line_number,x,y)
  denominator <- sqrt(((lin[line_number,1])^2)+((lin[line_number,2])^2))
  return(numerator/denominator)
}

distance_line_line <- function(line_number, line_number_2){
  numerator <- lin[line_number_2,3] - lin[line_number,3]
  denominator <- 0
}

  
