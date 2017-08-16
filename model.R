rm(list=ls())
library(glmnet)
library(pROC)
source("functions.R")

# ------------------------------
# Load all files
# ------------------------------
load("cleaned/train_with_path.bin")
load("cleaned/test_with_path.bin")
load("cleaned/validation_with_path.bin")

# ------------------------------
# Get large matrices from the loaded files
# ------------------------------
train_path <- get_full_training_columns(train_with_path)
test_path <- get_full_training_columns(test_with_path)
validate_path <- get_full_training_columns(validation_with_path)

# # ------------------------------
# # Test Use GLM
# # ------------------------------
# 
# fit <- glmnet(train_path,as.numeric(train_with_path[[1]]$trip_duration))
# pred <- predict(fit, test_path, type="class")
# print(head(pred[,1:5]))
# 
# # ------------------------------------------------------------------------------------------------------------------------
# ## The reason it returns many probability sets is because glmnet fits the model for 
# ## different regularization parameters at the same time. To help us choose the best prediction set, 
# ## we can use the function cv.glmnet. This will use cross validation to find the fit with the smallest error. 
# ## Let’s call cv.glmnet and pass the results to the s paramter (the prenalty parameter) of the predict function:
# ## use cv.glmnet to find best lambda/penalty - choosing small nfolds for cv due to
# ## s is the penalty parameter
# ## http://amunategui.github.io/sparse-matrix-glmnet/
# # ------------------------------------------------------------------------------------------------------------------------

# ------------------------------
# Predict and Find area under curve
# ------------------------------
fit <- glmnet(train_path,as.numeric(train_with_path[[1]]$trip_duration))
cv <- cv.glmnet(train_path,as.numeric(train_with_path[[1]]$trip_duration),nfolds=3)
pred <- predict(fit, test_path,type="response", s=cv$lambda.min)
auc = roc(test_with_path[[1]]$trip_duration, pred[,1])
print(auc$auc)

# ------------------------------
# Recursively better the model. For now output the predicted values for validation
# ------------------------------
pred <- predict(fit, validate_path,type="response", s=cv$lambda.min)

# ------------------------------
# Predict those not predicted
# ------------------------------

load(file = "rawdata/validation.bin")
val_raw <- validation
validation <- validation_with_path[[1]]
validation <- cbind(validation,pred[,1])
validation <- subset(validation,select = c("id","distance","V2"))
validation <- merge(val_raw,validation,by = "id",all.x = T)
validation$distance <- distance_between_points(validation$pickup_longitude,validation$pickup_latitude,
                                               validation$dropoff_longitude,validation$dropoff_latitude)*4.241/6.745779e-02

train_with_path[[1]]$speed <- train_with_path[[1]]$distance/(train_with_path[[1]]$trip_duration/3600)
# train_with_path[[1]] <- data.table(train_with_path[[1]])
avg_speed_training <- mean(train_with_path[[1]]$speed)

validation$time_taken <- validation$distance/(avg_speed_training)*3600
# validation$hour_of_day <- as.numeric(substr(validation$pickup_datetime,12,13))
validation$V2[is.na(validation$V2)] <- validation$time_taken[is.na(validation$V2)]
validation$V2[validation$V2<=60] <- validation$time_taken[validation$V2<=60]

validation$trip_duration <- validation$V2

subset_to_write <- subset(validation,select = c("id","trip_duration"))

write.csv(subset_to_write,"output/submission.csv")
