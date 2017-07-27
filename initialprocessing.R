
# ---------------
# initialize and load libraries
# ---------------
rm(list = ls())
library(data.table)
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
# Load files afresh
# ---------------
load(file = "rawdata/taxi_train.bin")
load(file = "rawdata/taxi_test.bin")

# ---------------
# View summary
# ---------------
# str(taxi_train)
# str(taxi_test)
# view more summaries
# summary(taxi_train)
# creating more calculative fields
# distance field
taxi_train$distance <- sqrt((taxi_train$pickup_longitude-taxi_train$dropoff_longitude)^2+(taxi_train$pickup_latitude-taxi_train$dropoff_latitude)^2)*52.7163052
# trip duration field in hours
taxi_train$duration_hours <- taxi_train$trip_duration/3600
# time of day
taxi_train$hour_of_day <- as.numeric(substr(taxi_train$pickup_datetime,12,13))
taxi_train$minute <- as.numeric(substr(taxi_train$pickup_datetime,15,16))

# speed
taxi_train$speed <- (taxi_train$distance / taxi_train$duration_hours)

#distance buckets
taxi_train$distance_buckets <- 0
taxi_train[taxi_train$distance < .5,"distance_buckets"] <- 1
taxi_train[taxi_train$distance < 1 & taxi_train$distance >= .5,]$distance_buckets <- 2
taxi_train[taxi_train$distance < 2.5 & taxi_train$distance >= 1,]$distance_buckets <- 3
taxi_train[taxi_train$distance < 5 & taxi_train$distance >= 2.5,]$distance_buckets <- 4
taxi_train[taxi_train$distance < 10 & taxi_train$distance >= 5,]$distance_buckets <- 5
taxi_train[taxi_train$distance < 20 & taxi_train$distance >= 10,]$distance_buckets <- 6
taxi_train[taxi_train$distance >= 20,]$distance_buckets <- 7


# some more processing
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
# Create summary buckets
# ---------------
taxi_train <- data.table(taxi_train)

# distance buckets
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

# hour of day


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


# vendor


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


# store_and_fwd_flag


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


# passenger count


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

# Double Aggregation



# passenger count


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


