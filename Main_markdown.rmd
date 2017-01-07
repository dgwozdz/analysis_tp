#### TP: recruitment exercise
* Author:           DG
* Date:             29DEC2016
* Last  mod. date:  07JAN2017
* Input data:       data.csv

## 0. Reading packages, setting working directory

```{r eval = T}
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
```

## 1. Reading data
```{r eval = T}
# setting working directory...

wd <- "F:\\Damian\\Praca\\2016\\Toolplox Data Scientist\\Tooploox_Data_Scientist_Exercise (1)\\Tooploox_Data_Scientist_Exercise"
setwd(wd)

data.videos <- fread(".\\Input_data\\data.csv") %>% as.data.frame
```

Checking the class of read object (`r class(data.videos)`), the first & last 6
observations to check if data was loaded correctly. Results are not outputted due
to their size.

```{r eval = F}
class(data.videos) # checking object class
head(data.videos) # checking the first...
tail(data.videos) # ...and last 6 observations of the data set to check if it was loaded correctly
```

```{r eval = T}
dim(data.videos) # Number of rows & columns
sum(is.na(data.videos)) # checking if there are any missing values
```

Naming variables properly: id and the number of views after i-th hour
```{r eval = T}
names(data.videos) <- c("id", paste0("V", 1:168))
```

### 1.1 Basic stats for v(24), v(72), v(168)

```{r eval = T}
summary(data.videos[, c("V24", "V72", "V168")])
boxplot(data.videos[, c("V24", "V72", "V168")])
boxplot(data.videos[, c("V24")])
```

### 1.2 Distribution of v(168)

```{r eval = T}
qplot(data.videos[, "V168"])
plot(ecdf(data.videos[, "V168"]))
```
The distribution reminds some kind of Weibull distribution?

### 1.3 Distribution of log-transformed v(168)

```{r eval = T}
qplot(log(data.videos[, "V168"]))
ad.test(log(data.videos[, "V168"]))
shapiro.test(log(data.videos[, "V168"]))
```

The distribution looks a bit similar to the normal one, however both Shapiro
and Anderson Darling test suggest rejection of null hypothesis of the data 
being distributed normally.

### 1.4 Outliers of v(168)

```{r eval = T}
data.videos$ln.V168 <- log(data.videos$V168)
mean.ln.V168 <- mean(data.videos$ln.V168)
sd.ln.V168 <- sd(data.videos$ln.V168)
low.3sigma <- mean.ln.V168-sd.ln.V168*3
high.3sigma <- mean.ln.V168+sd.ln.V168*3
```
```{r eval = T, echo = F}
nrow.before <- nrow(data.videos)
```
Removing outliers
```{r eval = T}
data.videos <- data.videos[data.videos$ln.V168>=low.3sigma &
                             data.videos$ln.V168<=high.3sigma,]
```
```{r eval = T, echo = F}
nrow.after <- nrow(data.videos)
```
As a result of outlier removal process, `r nrow.before-nrow.after` observations were removed.

### 1.5 Correlations between log-transformed v(1:24) and log-transformed v(168)

```{r eval = T}
for(i in 1:24){
  
  # Adding log-transformed V1-V24
  
  data.videos[[paste0("ln.V", i)]] <- log(data.videos[[paste0("V", i)]])
  
  # If calculating a natural logarithm results in Inf, mean from this variable
  # is inputted
  
  data.videos[[paste0("ln.V", i)]] <- ifelse(data.videos[[paste0("ln.V", i)]] == -Inf,
                                             mean(data.videos$ln.V1[data.videos$ln.V1 != -Inf]),
                                             data.videos[[paste0("ln.V", i)]])
}

log.cor <- cor(data.videos[, c(paste0("ln.V", c(1:24, 168)))])

corrplot.mixed(log.cor, lower = "pie", upper = "number", tl.pos = "lt", 
               tl.cex = 1, number.cex=0.4, pch.cex = 5)
```

### 1.6 Split

Spitting data into training (90%) and test (10%) sets:

```{r eval = T}
split.vec <- sample(1:nrow(data.videos), size = floor(0.9*nrow(data.videos)))
data.train <- data.videos[split.vec,]
data.test <- data.videos[-split.vec,]
```

### 1.7-9 OLS, multiple-input OLS & evaluation of predictors

```{r eval = T, echo = F}
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
  # 4.  vars      - vector of variables. If multiple predictors are
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
```

Creating a vector with variable names:

```{r eval = T}
(pred.single.var <- paste0("ln.V", 1:24))
```

Prediction for models with single regresors:

```{r eval = T}
mRSE.single.var <- pred.mRSE(vars = pred.single.var,
                             target = "ln.V168",
                             train = data.train,
                             test = data.test)
mRSE.single.var$mRSE.test
```

The model which minimizes mRSE is the 
`r which(mRSE.single.var$mRSE.test == min(mRSE.single.var$mRSE.test))`th one, so it 
is the one with log-transformed V24

```{r eval  = T}
mRSE.single.var$RMSE.train
```

The model which minimizes mRSE is the
` rwhich(mRSE.single.var$RMSE.train == min(mRSE.single.var$RMSE.train))``th one, so it 
is the one with log-transformed V24

Prediction for models with multiple regresors:

```{r eval  = T}
pred.multiple.vars <- c()
for(i in 1:24){
  pred.multiple.vars <- c(pred.multiple.vars,
                          paste0("ln.V", 1:i, collapse = " "))
}

mRSE.multiple.vars <- pred.mRSE(vars = pred.multiple.vars,
                                target = "ln.V168",
                                train = data.train,
                                test = data.test)

mRSE.multiple.vars$mRSE.test
```

Model resulted in minimum mRSE was the 
`r which(mRSE.multiple.vars$mRSE.test == min(mRSE.multiple.vars$mRSE.test))`th one,
therefore it contained all log-transformed 24 variables.

```{r eval = T}
mRSE.multiple.vars$RMSE.train
```

Model resulted in minimum RMSE was the 
`r which(mRSE.multiple.vars$RMSE.train == min(mRSE.multiple.vars$RMSE.train))`th one,
therefore it also contained all log-transformed 24 variables.

### 1.10 mRSE visualization----

Preparing data for ggplot2 - melting in order to plot multiple lines on one plot:

```{r eval = T}
mRSE.compare <- cbind(index = seq_along(mRSE.single.var$mRSE.test),
                      mRSE.single.var = mRSE.single.var$mRSE.test, 
                      mRSE.multiple.vars = mRSE.multiple.vars$mRSE.test) %>%
  data.frame %>%
  melt(id = "index")

head(mRSE.compare) # checking the first 6 observations
```

Adding labels for data for ggplot2:

```{r eval = T}
mRSE.compare[,2] <- ifelse(mRSE.compare[,2] == "mRSE.single.var",
                           "Linear Regression",
                           "Multiple-input Linear Regression")
head(mRSE.compare) # checking the first 6 observations
```

Plot in ggplot2:

```{r eval = T}
ggplot(mRSE.compare, aes(x = index, y = value, colour = variable)) + 
  geom_line(size = 1.2) +
  geom_point() +
  xlab("Reference time (n)") +
  ylab("mRSE") +
  scale_x_continuous(breaks = seq(0, 25, 3)) +
  # scale_y_continuous(breaks = seq(0, 0.003, 0.0005)) +
  ggtitle("Performance of linear regression models for n in <1: 24> hours
  measured as mean Relative Squared Error (mRSE)") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title=element_blank())
```