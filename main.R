############################################
###   TP: recruitment exercise
#
#
# Author:           DG
# Date:             29DEC2016
# Last  mod. date:  07JAN2017
# Input data:       data.csv
############################################

##### 0. Reading packages, setting working directory----

set.seed(1234567)

rm(list = ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, dplyr, ggplot2, caret, nortest, corrplot)

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

# Naming variables properly

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

# The distribution looks a bit similar to the normal one, however both Shapiro
# and Anderson Darling test suggest rejection of null hypothesis of the data 
# being distributed normally.

# (1.4) Outliers of v(168)

data.videos$ln.V168 <- log(data.videos$V168)
mean.ln.V168 <- mean(data.videos$ln.V168)
sd.ln.V168 <- sd(data.videos$ln.V168)
low.3sigma <- mean.ln.V168-sd.ln.V168*3
high.3sigma <- mean.ln.V168+sd.ln.V168*3

# Removing outliers

data.videos <- data.videos[data.videos$ln.V168>=low.3sigma &
                             data.videos$ln.V168<=high.3sigma,]

# (1.5) Correlation between log-transformed v(1:24) and log-transformed v(168)

for(i in 1:24){
  
  # Adding log-transformed V1-V24
  
  data.videos[[paste0("ln.V", i)]] <- log(data.videos[[paste0("V", i)]])
  
  # If calculating a natural logarithm results in Inf, mean from this variable
  # is inputted
  
  data.videos[[paste0("ln.V", i)]] <- ifelse(data.videos[[paste0("ln.V", i)]] == -Inf,
                                             mean(data.videos$ln.V1[data.videos$ln.V1 != -Inf]),
                                             data.videos[[paste0("ln.V", i)]])
}

mean(data.videos$ln.V1[data.videos$ln.V1 != -Inf])

log.cor <- cor(data.videos[, c(paste0("ln.V", c(1:24, 168)))])

corrplot.mixed(log.cor, lower = "pie", upper = "number", tl.pos = "lt", 
               tl.cex = 1, number.cex=0.7)

# (1.6) Split

split.vec <- sample(1:nrow(data.videos), size = floor(0.9*nrow(data.videos)))
data.train <- data.videos[split.vec,]
data.test <- data.videos[-split.vec,]

# (1.7-9) OLS, multiple-input OLS & evaluation of predictors

pred.mRSE <- function(vars, target, train, test){
  
  #####################################################################
  # (I) INFO:
  #
  # Author:     DG
  # Date:       04JAN2017
  # Mod. date:  
  # Purpose:    To evaluate Oridnary Least Squares by mean Relative
  #             Squared Error (mRSE) on the test dataset and
  #             Root Mean Square Error (RMSE) on training data set
  # Input dataset(s): train (training data), test (test data)
  # Output(s):        list containing 2 vectors:
  #                   $mRSE.test; $RMSE.train
  #
  # 
  # (II)  PARAMETERS:
  #
  # 1.  train     - training data
  # 2.  test      - test data
  # 3.  target    - target variable
  # 4.  var       - vector of variables. If multiple predictors are
  #                 specyfied, they should be named in one string
  #                 with a blank as a separator, e.g. "Var1 Var2 Var3".
  #####################################################################
  
  mRSE.test <- RMSE.train <- c()
  model <- pred.vec <- list()
  
  for(i in 1:length(vars)){
    var <- gsub(" ", "+", vars[i])
    formula.lm <- as.formula(paste0(target, "~", var))
    
    # OLS model
    
    model[[i]] <- lm(formula.lm, data = train)
    
    # RMSE on a training set
    
    RMSE.train <- c(RMSE.train,
                    sum((model[[i]]$fitted - train[[target]])^2)/nrow(train))
    
    # Prediction on a test set
    
    pred.vec[[i]] <- predict(model[[i]], test)
    
    # mRSE calculation
    
    mRSE.test <- c(mRSE.test, sum((pred.vec[[i]]/test[[target]] - 1)^2) / nrow(test))
  }
  return(list(mRSE.test = mRSE.test, RMSE.train = RMSE.train))
}

# Creating a vector with variable names

pred.single.var <- paste0("ln.V", 1:24)

# Prediction for models with single regresors

mRSE.single.var <- pred.mRSE(pred.single.var, "ln.V168", data.train, data.test)
mRSE.single.var$mRSE.test
mRSE.single.var$RMSE.train
# Checking which model resulted in minimum mRSE

which(mRSE.single.var$mRSE.test == min(mRSE.single.var$mRSE.test)) # == 24,
# therefore the model which
# minimizes mRSE is the one with log-transformed V24

which(mRSE.single.var$RMSE.train == min(mRSE.single.var$RMSE.train)) # == also 24.
# therefore model minimizing RMSE on training data set is the one with 
# log-transformed V24

# Prediction for models with multiple regresors

pred.multiple.vars <- c()
for(i in 1:24){
  pred.multiple.vars <- c(pred.multiple.vars,
                          paste0("ln.V", 1:i, collapse = " "))
}
mRSE.multiple.vars <- pred.mRSE(pred.multiple.vars, "ln.V168", data.train, data.test)

# Checking which model resulted in minimum mRSE

which(mRSE.multiple.vars$mRSE.test == min(mRSE.multiple.vars$mRSE.test)) # == 24,
# therefore the model which
# minimizes mRSE is the one with all log-transformed 24 variables

which(mRSE.multiple.vars$RMSE.train == min(mRSE.multiple.vars$RMSE.train)) # == also 24.
# therefore model minimizing RMSE on training data set is the one with 
# all log-transformed 24 variables

# (1.10) mRSE visualization----

# Preparing data for ggplot2 - melting in order to plot multiple lines on one plot

mRSE.compare <- cbind(index = seq_along(mRSE.single.var), mRSE.single.var, 
                      mRSE.multiple.vars) %>%
  data.frame %>%
  melt(id = "index")

# Adding labels for data for ggplot2

mRSE.compare[,2] <- ifelse(mRSE.compare[,2] == "mRSE.single.var",
                           "Linear Regression",
                           "Multiple-input Linear Regression")

# Plot in ggplot2

ggplot(mRSE.compare, aes(x = index, y = value, colour = variable)) + 
  geom_line(size = 1.2) +
  geom_point() +
  xlab("Reference time (n)") +
  ylab("mRSE") +
  scale_x_continuous(breaks = seq(0, 25, 3)) +
  scale_y_continuous(breaks = seq(0, 0.003, 0.0005)) +
  ggtitle("Performance of linear regression models for n in <1: 24> hours
  measured as mean Relative Squared Error (mRSE)") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title=element_blank())