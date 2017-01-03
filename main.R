############################################
###   Tooploox: recruitment exercise
#
#
# Author:     Damian Gwozdz
# Date:       29DEC2016
# Mod. date:  -
# Input data: data.csv
############################################


##### 0. Reading packages, setting working directory----

set.seed(1234567)

rm(list = ls())

library(data.table)
library(dplyr)
library(ggplot2)
library(caret)

wd <- "F:\\Damian\\Praca\\2016\\Toolplox Data Scientist\\Tooploox_Data_Scientist_Exercise (1)\\Tooploox_Data_Scientist_Exercise"
setwd(wd)

# 1. Reading data----

data.videos <- fread(".\\Input_data\\data.csv") %>% as.data.frame
class(data.videos)
head(data.videos)
tail(data.videos)
dim(data.videos)
sum(is.na(data.videos))
qplot(apply(data.videos[,2:ncol(data.videos)], 1, mean), geom = "histogram")

##### (1) Distribution of V(168), basic stats for v({24, 72, 168})----

# (1.1) Distribution of V(168)

qplot(data.videos[, "V169"])


#### Split----

split.vec <- sample(data.videos, p = 0.9)
