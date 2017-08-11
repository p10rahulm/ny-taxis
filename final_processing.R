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


# ------------------------------
# Run the full function for train, test and validation
# ------------------------------
# Train
train_with_path <- return_path_and_columns(train)
train_input <- train_with_path[[1]]
train_path <- train_with_path[[2]]

# test
test_with_path <- return_path_and_columns(test)
test_input <- test_with_path[[1]]
test_path <- test_with_path[[2]]


# validation
validation_with_path <- return_path_and_columns(train)
validation_input <- validation_with_path[[1]]
validation_path <- validation_with_path[[2]]
