
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
str(taxi_train)
str(taxi_test)
# view more summaries
summary(taxi_train)
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


