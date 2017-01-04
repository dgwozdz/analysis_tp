############################################
###   TP: recruitment exercise
#
#
# Author:     DG
# Date:       29DEC2016
# Mod. date:  -
# Input data: data.csv
############################################


##### 0. Reading packages, setting working directory----

set.seed(1234567)

rm(list = ls())

# install.packages("nortest")

library(data.table)
library(dplyr)
library(ggplot2)
library(caret)
library(nortest) # Anderson-Darling test for normality
library(corrplot)

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
names(data.videos) <- c("id", paste0("V", 1:168))

##### (1) Distribution of V(168), basic stats for v({24, 72, 168})----

# (1.1) Basic stats for v(24), v(72), v(168)

summary(data.videos[, c("V24", "V72", "V168")])
boxplot(data.videos[, c("V24", "V72", "V168")])
boxplot(data.videos[, c("V24")])

# (1.2) Distribution of v(168)

qplot(data.videos[, "V168"])

# (1.3) Distribution of v(168)

qplot(log(data.videos[, "V168"]))
ad.test(log(data.videos[, "V168"]))
shapiro.test(log(data.videos[, "V168"]))
mean(log(data.videos[, "V168"]))

# (1.4) Outliers of v(168)

data.videos$ln.V168 <- log(data.videos$V168)
mean.ln.V168 <- mean(data.videos$ln.V168)
sd.ln.V168 <- sd(data.videos$ln.V168)
low.3sigma <- mean.ln.V168-sd.ln.V168*3
high.3sigma <- mean.ln.V168+sd.ln.V168*3

data.videos <- data.videos[data.videos$ln.V168>=low.3sigma &
                             data.videos$ln.V168<=high.3sigma,]

dim(data.videos)

# (1.5) Correlation between log-transformed v(1:24) and log-transformed v(168)

for(i in 1:24){
  data.videos[[paste0("ln.V", i)]] <- log(data.videos[[paste0("V", i)]])
  
  # If calculating a natural logarithm results in Inf, then log(1) is inputted
  
  data.videos[[paste0("ln.V", i)]] <- ifelse(data.videos[[paste0("ln.V", i)]] == -Inf,
                                             log(1), data.videos[[paste0("ln.V", i)]])
}

log.cor <- cor(data.videos[, c(paste0("ln.V", c(1:24, 168)))])

corrplot.mixed(log.cor, lower = "pie", upper = "number", tl.pos = "lt", 
               tl.cex = 1, number.cex=0.7)

# (1.6) Split

split.vec <- sample(1:nrow(data.videos), size = floor(0.9*nrow(data.videos)))
data.train <- data.videos[split.vec,]
data.test <- data.videos[-split.vec,]

# (1.7) OLS for v(168)

# 1.7.1 Adding logarithms for the rest of variables

for(i in 25:167){
  data.videos[[paste0("ln.V", i)]] <- log(data.videos[[paste0("V", i)]])
  
  # If calculating a natural logarithm results in Inf, then log(1) is inputted
  
  data.videos[[paste0("ln.V", i)]] <- ifelse(data.videos[[paste0("ln.V", i)]] == -Inf,
                                             log(1), data.videos[[paste0("ln.V", i)]])
}

# (1.9) Evaluation of predictors

pred.mRSE <- function(vars, target, train, test){
  
#   vars <- "ln.V1"
#   target <- "ln.V168"
#   train <- data.train
#   test <- data.test
#   i <- 1
  # rm(vars, target, train, test, i)
  
  ####################################################################
  # (I) INFO:
  #
  # Author:     DG
  # Date:       04JAN2017
  # Mod. date:  
  # Purpose:    To evaluate Oridnary Least Squares by mean Relative
  #             Squared Error (mRSE) on the test dataset.
  # Input dataset(s): train (training data), test (test data)
  #
  # 
  # (II)  PARAMETERS:
  #
  # 1.  train     - training data
  # 2.  test      - test data
  # 3.  target    - target variable
  # 4.  var       - 
  ####################################################################
  
  mRSE <- c()
  model <- pred.vec <- list()
  
  for(i in 1:length(vars)){
    var <- gsub(" ", "+", vars[i])
    formula.lm <- as.formula(paste0(target, "~", var))
    
    # OLS model
    
    model[[i]] <- lm(formula.lm, data = train)
    
    # Prediction on a test set
    
    pred.vec[[i]] <- predict(model[[i]], test)
    
    # mRSE calculation
    
    mRSE <- c(mRSE, sum((pred.vec[[i]]/test[[target]] - 1)^2) / nrow(test))
  }
  return(mRSE)
}

pred.single.var <- paste0("ln.V", 1:24)
mRSE.single.var <- pred.mRSE(pred.single.var, "ln.V168", data.train, data.test)

pred.multiple.vars <- c()
for(i in 1:24){
  pred.multiple.vars <- c(pred.multiple.vars,
                          paste0("ln.V", 1:i, collapse = " "))
}
mRSE.multiple.vars <- pred.mRSE(pred.multiple.vars, "ln.V168", data.train, data.test)

cbind(mRSE.single.var, mRSE.multiple.vars)

# (1.10) Plot the mRSE

plot(mRSE.single.var, type = "l", xlab = "Reference time (n)", ylab = "mRSE",
     main = "Performance of linear regression models for n in <1: 24> hours
      measured as mean Relative Squared Error (mRSE)", lty = 2)
grid()
lines(mRSE.multiple.vars, type = "l", col = "green")
legend("topright", "(x,y)",
       legend = list("Linear Regression", "Multiple-input Linear Regression"), 
       lty = c(2, 1),
       col = c("black", "green"),
       bg = "white")
