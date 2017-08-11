rm(list=ls())
load("cleaned/train_with_path.bin")
train_input <- train_with_path[[1]]
train_path <- train_with_path[[2]]
# Usefule columns
mycols <- c("trip_duration","passenger_count","vendor_id","distance",
            "hour_of_day","distance_buckets","sf","is_intra_bucket","is_interbucket_into_11",
            "pickup_map_buckets","pickup_map_buckets_lat","pickup_map_buckets_long",
            "dropoff_map_buckets","dropoff_map_buckets_lat","dropoff_map_buckets_long")

useful_subset <- subset(train_input,select = mycols)
# useful_subset <- Matrix(as.matrix(useful_subset),sparse = T,
#                        byrow = F,dimnames = list(as.character(seq(1,nrow(train_input))),mycols))
useful_subset <- Matrix(data=as.matrix(useful_subset),sparse = T)
train_path <- cbind(useful_subset,train_path)

# train_path2 <- Matrix(train_path,sparse=F)
