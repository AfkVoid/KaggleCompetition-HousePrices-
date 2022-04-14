library(MASS)
install.packages("car")
library(car)
install.packages("SignifReg")
library(SignifReg)
library(tidyverse)
install.packages("missForest")
library(missForest)
library(caret)
library(nnet)
#Building Dataframes
train<-read.csv("train.csv", header = TRUE)
test<-read.csv("test.csv", header = TRUE)
#Exploratory analysis
head(train)
head(test)
glimpse(train)
glimpse(test)
is.na(train)
colSums(is.na(train))
#Creating data partition (Might replace with K-Fold if time permits)
set.seed(222)
random_sample <- createDataPartition(train$SalePrice,p = 0.7, list = FALSE)
train2 <-train[random_sample,]
test2 <-train[-random_sample,]
#Cleaning Dataframe test
#Here we remove NA columns. We also convert all the columns into numeric/factor values 
test
test$Alley<- NULL
test$PoolQC<- NULL
test$Fence<- NULL
test$MiscFeature<- NULL
test[test == "NA"]<- NA
test2clean<- type.convert(test, as.is=FALSE)
#Running Missing Forest on test2clean to plug in missing values for NA
testF<-missForest(test2clean)
#Cleaning Dataframe train
#Here we remove NA columns. We also convert all the columns into numeric/factor values
train
train$Alley<- NULL
train$PoolQC<- NULL
train$Fence<- NULL
train$MiscFeature<- NULL
train[train == "NA"]<- NA
train2clean<- type.convert(train, as.is=FALSE)
#Running Missing Forest on train2clean to plug in missing values for NA
trainf<-missForest(train2clean)
#Looking at the trainf class to see values and error
trainf$ximp
trainf$OOBerror
#Checking if any columns became single valued after missing forest (This changes every time)
(l <- sapply(trainf$ximp, function(x) is.factor(x)))
m <- trainf$ximp[, l]
ifelse(n <- sapply(m, function(x) length(levels(x))) == 1, "DROP", "NODROP")
which(sapply(trainf$ximp, function(x) length(unique(x))<2))
#This changes each time (Depends on what values are chosen during missing forest)(Wait before running this)
trainf$ximp$Utilities<-NULL
#Removing outliers found using Cook's Distance
trainf$ximp<-trainf$ximp[-c(826),]
trainf$ximp<-trainf$ximp[-c(524),]
#Making data frame of clean data (***This must be re run every time we edit train2f$ximp***)
good_data<-trainf$ximp
#Building basic linear model (No transformation)
rmodel<-lm(SalePrice~.,data = good_data)
summary(rmodel)
plot(rmodel)
#applying transformations (We run summary and plot after each model to view the fit on the data)
rmodel<-lm(log(SalePrice)~log(LotFrontage)+log(LotArea)+log(X1stFlrSF)+log(GrLivArea)+.,data = good_data)
rmodel<-lm(SalePrice~log(LotFrontage)+log(LotArea)+log(X1stFlrSF)+log(GrLivArea)+.,data = good_data)
rmodel<-lm(log(SalePrice)~.,data = good_data)
#Applying Boxcox and finding optimal lambda (This is the best model so far)
out <- boxcox(lm(SalePrice~.,data = good_data))
range(out$x[out$y > max(out$y)-qchisq(0.95,1)/2])
rmodel<-lm((SalePrice)^.25~.,data = good_data)
#Looking for colinearity between the variables
alias(rmodel)
summary(rmodel)
plot(rmodel)
#checking testF so it matches train2f in terms of columns
(l <- sapply(testF$ximp, function(x) is.factor(x)))
m <- testF$ximp[, l]
ifelse(n <- sapply(m, function(x) length(levels(x))) == 1, "DROP", "NODROP")
which(sapply(testF$ximp, function(x) length(unique(x))<2))
#Dropping values where all entries are the same (This must match train2f or else predictions does not work)
#(After running this go back to the matching function for trainf and run that one then update good data)
testF$ximp$Utilities<-NULL
#Running predictions
predictions <- predict(rmodel, testF$ximp)
data.frame( R2 = R2(predictions, test $ quality),
            RMSE = RMSE(predictions, test $ quality),
            MAE = MAE(predictions, test $ quality))
#Making submission file (still need to edit in excel to match kaggle competition)
write.csv(predictions,file="pred2.csv",row.names=T)
#'LotFrontage', 'LotArea', '1stFlrSF', 'GrLivArea', 'SalePrice'