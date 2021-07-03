library(readxl)
library(dplyr)
library(ggplot2)
install.packages("tidyverse")
library(tidyverse)
install.packages("visdat")
library(visdat)
install.packages("mice")
library(mice)
install.packages("sjmisc")
library(sjmisc)
library(ggplot2)
install.packages("corrplot")
library(corrplot)
library(caret)
install.packages("glmnet")
library(glmnet)


train1 <- read.csv(file.choose())
test1 <- read.csv(file.choose())

# Combining the sets ------------------------------------------------------
test1$SalePrice <- NA
data = rbind(train1, test1)
vis_dat(data)

# Valid Missing Values and Dealing with it----------------------------------------
## checking missing values in combined set
missing_count = data.frame(sapply(data, function(x) sum(is.na(x))))
filter(missing_count, missing_count[,1] > 0)


#Fixing the missing values for which data dictionary provides a reason
data$Alley = ifelse(is.na(data$Alley), "none_available", data$Alley)
data$BsmtQual = ifelse(is.na(data$BsmtQual), "none_available", data$BsmtQual)
data$BsmtCond  = ifelse(is.na(data$BsmtCond ), "none_available", data$BsmtCond)
data$BsmtExposure  = ifelse(is.na(data$BsmtExposure ), "none_available", data$BsmtExposure)
data$BsmtFinType1 = ifelse(is.na(data$BsmtFinType1), "none_available", data$BsmtFinType1)
data$BsmtFinType2 = ifelse(is.na(data$BsmtFinType2), "none_available", data$BsmtFinType2)
data$FireplaceQu = ifelse(is.na(data$FireplaceQu), "none_available", data$FireplaceQu)
data$GarageType = ifelse(is.na(data$GarageType), "none_available", data$GarageType)
data$GarageFinish  = ifelse(is.na(data$GarageFinish ), "none_available", data$GarageFinish)
data$GarageQual = ifelse(is.na(data$GarageQual), "none_available", data$GarageQual)
data$GarageCond   = ifelse(is.na(data$GarageCond  ), "none_available", data$GarageCond)
data$PoolQC = ifelse(is.na(data$PoolQC), "none_available", data$PoolQC)
data$Fence  = ifelse(is.na(data$Fence ), "none_available", data$Fence)
data$MiscFeature    = ifelse(is.na(data$MiscFeature   ), "none_available", data$MiscFeature)




Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

data$SaleType = ifelse(is.na(data$SaleType), Mode(data$SaleType), data$SaleType)
data$GarageArea = ifelse(is.na(data$GarageArea), Mode(data$GarageArea), data$GarageArea)
data$GarageCars = ifelse(is.na(data$GarageCars), 0, data$GarageCars)
data$Functional = ifelse(is.na(data$Functional), Mode(data$Functional), data$Functional)
data$MSZoning = ifelse(is.na(data$MSZoning), Mode(data$MSZoning), data$MSZoning)
data$Exterior1st = ifelse(is.na(data$Exterior1st), Mode(data$Exterior1st), data$Exterior1st)
data$Exterior2nd = ifelse(is.na(data$Exterior2nd), Mode(data$Exterior2nd), data$Exterior2nd)
data$BsmtFinSF1 = ifelse(data$BsmtQual == "none_available", 0, data$BsmtFinSF1)
data$BsmtFinSF2 = ifelse(data$BsmtQual == "none_available", 0, data$BsmtFinSF2)
data$BsmtUnfSF = ifelse(is.na(data$BsmtUnfSF), 0, data$BsmtUnfSF)
data$TotalBsmtSF = ifelse(data$BsmtQual == "none_available", 0, data$TotalBsmtSF)
data$Electrical = ifelse(is.na(data$Electrical), Mode(data$Electrical), data$Electrical)
data$BsmtFullBath = ifelse(data$BsmtQual == "none_available", 0, data$BsmtFullBath)
data$BsmtHalfBath = ifelse(data$BsmtQual == "none_available", 0, data$BsmtHalfBath)
data$KitchenQual  = ifelse(is.na(data$KitchenQual ), Mode(data$KitchenQual ), data$KitchenQual)
data$Utilities[is.na(data$Utilities)] = "AllPub"
data$GarageYrBlt = ifelse(is.na(data$GarageType), 0, ifelse(is.na(data$GarageYrBlt), data$YearBuilt, data$GarageYrBlt))

# MICE Imputation for Lotfrontage, MsVArType and MSVarArea ----------------
missing_count = data.frame(sapply(data, function(x) sum(is.na(x))))
filter(missing_count, missing_count[,1] > 0)
md.pattern(data)
SalePrice <- drop(data$SalePrice) # assigning a variable to add back later 
data$SalePrice <- NULL # dropping SalePrice

data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], as.factor) #for consistency, all categorical variables are turned into factors
data[sapply(data, is.numeric)] <- lapply(data[sapply(data, is.numeric)], as.integer) #for consistency, all numeric variables are turned into integers

imputed_data <- mice(data, m=5, maxit=30, method='cart') #using cart method to impute categorical variable as well
completed <- merge_imputations(data,imputed_data) #merging imputations into one

columnnames <- names(completed)
for (variable in columnnames) {
   data[variable]<- completed[variable] #creating a final imputed dataframe
}

md.pattern(data)
combined = data


# Fixing some errors ----------------------------------------
#any error where houses are remodelled before built?
any(combined$YearBuilt > combined$YearRemodAdd)
any(combined$YearBuilt < combined$YearRemodAdd)

combined[which(combined$YearBuilt > combined$YearRemodAdd),] %>% 
  select(YearBuilt, YearRemodAdd)
combined[which(combined$YearBuilt > combined$YearRemodAdd),]$YearRemodAdd =combined[which(combined$YearBuilt > combined$YearRemodAdd),]$YearBuilt #fix it

#YearSold before year built - fixing that
any(combined$YrSold < combined$YearBuilt)
combined %>% filter(YrSold < YearBuilt) %>% select(Id, YearBuilt, YrSold)
combined$YrSold = ifelse(combined$YrSold < combined$YearBuilt, combined$YearBuilt, combined$YrSold)

#Incorrect Garage Year Built
combined %>% filter(GarageYrBlt > 2200) %>% select(Id, YearBuilt, YearRemodAdd, GarageYrBlt)
combined$GarageYrBlt[combined$Id == 2593] <- 2006

# Feature Engineering -----------------------------------------------------

combined$yearssinceremod = combined$YrSold - combined$YearRemodAdd 
combined$remod_dum = ifelse(combined$yearssinceremod < 1, 1, 0) #dummy

#total square foot
combined$TotalSF = combined$X1stFlrSF + combined$X2ndFlrSF + combined$TotalBsmtSF 

#Ratio of Front lot over the entire Lot area
combined$Frontageratio = combined$LotFrontage/combined$LotArea

#Ratio of Floor area over the entire Lot area
combined$Floorratio = combined$GrLivArea/combined$LotArea

#Splitting Data
combined$SalePrice <- SalePrice
house.prices.training<- subset(combined, Id <=1460)
house.prices.prediction <- subset (combined, Id >=1461)
train = subset(combined, Id <=1460)
house.prices.data = combined

#removing outlier
#GrLivArea mean is 1510
combined %>% filter(GrLivArea > 2000 & Neighborhood == "Edwards" & Id <1460) %>% select(Id, GrLivArea, SalePrice, BldgType, LotArea,	HouseStyle,	OverallQual,	OverallCond,	YearBuilt, KitchenQual, PoolQC, MSZoning)
mean(house.prices.training$GrLivArea)
house.prices.data$GrLivArea <- ifelse(house.prices.data$GrLivArea > 4000, 1510 ,house.prices.data$GrLivArea)
plot(SalePrice ~ GrLivArea, data = house.prices.data)

house.prices.testing<-subset(house.prices.data, (Id>=1001 & Id<=1460)) #withold 1000 datapoints into a "testing" data
house.prices.training<-subset(house.prices.data, Id<=1000  ) #redefine the training data
prices.testing<-subset(house.prices.data, (Id<=120)) #withold 1000 datapoints into a "testing" data
prices.training<-subset(house.prices.data, Id>=121 & Id<=1460 )

# Visualization -----------------------------------------------------------

plot(train$SalePrice ~ train$LotArea)
plot(train$SalePrice ~ train$MasVnrArea)
plot(train$SalePrice ~ train$BsmtFinSF1)
plot(train$SalePrice ~ train$BsmtFinSF2)
plot(train$SalePrice ~ train$GrLivArea)
ggplot(train, aes(x = SalePrice)) + geom_histogram() +  xlab("Sale Prices") + ggtitle("Distribution of House Sale Price")
ggplot(train, aes(x = log(SalePrice))) + geom_histogram() + xlab("Log Sale Prices") + ggtitle("Distribution of House Sale Price")
ggplot(train, aes(x = LotArea)) + geom_histogram() +  xlab("Sale Prices") + ggtitle("Distribution of House Sale Price")
ggplot(train, aes(x = log(LotArea))) + geom_histogram() +  xlab("Log Sale Prices") + ggtitle("Distribution of House Sale Price")
ggplot(train, aes(x = BsmtFinSF1)) + geom_histogram() xlab("Sale Prices") + ggtitle("Distribution of House Sale Price")
ggplot(train, aes(x = log(BsmtFinSF1))) + geom_histogram() +  xlab("Log Sale Prices") + ggtitle("Distribution of House Sale Price")
ggplot(train, aes(x = BsmtFinSF2)) + geom_histogram() xlab("Sale Prices") + ggtitle("Distribution of House Sale Price")
ggplot(train, aes(x = log(BsmtFinSF2))) + geom_histogram() +  xlab("Log Sale Prices") + ggtitle("Distribution of House Sale Price")
ggplot(train, aes(x = GrLivArea)) + geom_histogram() + xlab("GrLivArea") + ggtitle("Distribution of House GrLivArea")
ggplot(train, aes(x = log(GrLivArea))) + geom_histogram() +  xlab("Log GrLivArea") + ggtitle("Distribution of House GrLivArea")
ggplot(train, aes(x = MasVnrArea)) + geom_histogram() xlab("Sale Prices") + ggtitle("Distribution of House Sale Price")
ggplot(train, aes(x = log(MasVnrArea))) + geom_histogram() +  xlab("Log Sale Prices") + ggtitle("Distribution of House Sale Price")


correlations <- cor(na.omit((train[, unlist(lapply(train, is.numeric))])))
row_indic <- apply(correlations, 1, function(x) sum(x > 0.3 | x < -0.3) > 1)
correlations<- correlations[row_indic ,row_indic ]
corrplot(correlations, method="square")


# Modelling ---------------------------------------------------------------

##Linear Regression Models - not selected
set.seed(2)
reg2 = step(lm(log(SalePrice)~MSSubClass +	MSZoning + log(LotArea) +	LotShape + LandContour +	
                 LotConfig +	LandSlope +	Neighborhood + Condition1 +	
                 Condition2 + HouseStyle +	OverallQual +	OverallCond +	
                 YearBuilt +	YearRemodAdd +	RoofMatl +		
                 ExterCond +	Foundation +	BsmtQual +	BsmtCond +	BsmtExposure +	BsmtFinType1 +	
                 BsmtFinSF1 +	BsmtFinType2 +	BsmtFinSF2 +BsmtUnfSF +	 +	Heating +
                 HeatingQC +	CentralAir	+Electrical + LowQualFinSF	+ log(GrLivArea)+
                 BsmtFullBath	+ BsmtHalfBath +	FullBath +	HalfBath	+ BedroomAbvGr +
                 KitchenQual +	TotRmsAbvGrd	+ Functional	+ Fireplaces	+ 
                 FireplaceQu +	GarageType +	GarageFinish +	GarageCars +	
                 GarageArea +	GarageQual +	GarageCond +	+	WoodDeckSF + remod_dum + log(Floorratio) + yearssinceremod +
                 OpenPorchSF +	X3SsnPorch	+ ScreenPorch +	PoolArea	+ TotalSF + 
                 PoolQC	+ Fence +	MiscFeature +	MiscVal +	MoSold +	YrSold +	SaleType +	SaleCondition+
                 GrLivArea*Neighborhood+OverallQual*Neighborhood*LotArea +
                 OverallQual*LotArea+ log(GrLivArea)*TotRmsAbvGrd +remod_dum + log(Floorratio) + yearssinceremod +  
                 YearBuilt*Neighborhood, house.prices.data, direction = "both"))



reg2 = lm(log(SalePrice) ~ MSZoning + log(LotArea) + Street + LotConfig + 
            LandSlope + Neighborhood + Condition1 + OverallQual + OverallCond + 
            YearBuilt + YearRemodAdd + Exterior1st + MasVnrType + ExterQual + 
            ExterCond + Foundation + BsmtExposure + BsmtUnfSF +  RoofMatl +
            HeatingQC + CentralAir + X1stFlrSF + X2ndFlrSF + BsmtFullBath + TotalBsmtSF + 
            FullBath + HalfBath + KitchenAbvGr + KitchenQual + Functional + 
            Fireplaces + GarageCars + GarageArea + WoodDeckSF + EnclosedPorch + 
            ScreenPorch + PoolArea + PoolQC + SaleCondition + GrLivArea*Neighborhood+OverallQual*Neighborhood*LotArea +
            OverallQual*LotArea+ log(GrLivArea)*TotRmsAbvGrd +remod_dum + log(Floorratio) + yearssinceremod +  
            YearBuilt*Neighborhood, prices.training)

prediction = predict(reg2, prices.testing)
prediction = exp(prediction)
mean(abs(prediction-prices.testing$SalePrice)/prices.testing$SalePrice*100) #calculate and display MAPE
RMSE((prediction), (prices.testing$SalePrice))
prediction = predict(reg2, house.prices.prediction)
prediction = exp(prediction)


##LASSO - First Try
set.seed(1)
y <- log(house.prices.training$SalePrice)
x <- model.matrix(Id~MSZoning + log(LotArea) + Street + LotConfig + 
                    LandSlope + Neighborhood + Condition1 + OverallQual + OverallCond + 
                    YearBuilt + YearRemodAdd + Exterior1st + MasVnrType + ExterQual + 
                    ExterCond + Foundation + BsmtExposure + BsmtUnfSF +  RoofMatl +
                    HeatingQC + CentralAir + X1stFlrSF + X2ndFlrSF + BsmtFullBath + 
                    FullBath + HalfBath + KitchenAbvGr + KitchenQual + Functional + 
                    Fireplaces + GarageCars + GarageArea + WoodDeckSF + EnclosedPorch + 
                    ScreenPorch + PoolArea + PoolQC + SaleCondition + GrLivArea*Neighborhood+OverallQual*Neighborhood*LotArea +
                    OverallQual*LotArea+ log(GrLivArea)*X1stFlrSF*TotRmsAbvGrd + 
                    YearBuilt*Neighborhood, house.prices.data)[,-1] #0.12654 on lasso Kaggle, 23% rank
x<-cbind(house.prices.data$Id,x)

# split X into testing, trainig/holdout and prediction as before
x.training<-subset(x,x[,1]<=1000)
x.testing<-subset(x, (x[,1]>=1001 & x[,1]<=1460))
x.prediction<-subset(x,x[,1]>=1461)

#LASSO (alpha=1)
nFolds <- 1
foldid <- sample(rep(seq(nFolds), length.out = nrow(x.training)))
lasso.fit<-glmnet(x = x.training, y = y, alpha = 1, nfolds = nFolds,
                  foldid = foldid,
                  standardize = FALSE)
plot(lasso.fit, xvar = "lambda")

#selecting the best penalty lambda
crossval <-  cv.glmnet(x = x.training, y = y, alpha = 1) #create cross-validation data

plot(crossval)
penalty.lasso <- crossval$lambda.min #determine optimal penalty parameter, lambda
log(penalty.lasso) #see where it was on the graph
plot(crossval,xlim=c(-6,-4),ylim=c(0.00,0.005)) # lets zoom-in
lasso.opt.fit <-glmnet(x = x.training, y = y, alpha = 1, lambda = penalty.lasso) 
coef(lasso.opt.fit) #resultant model coefficients

# predicting the performance on the testing set
library(caret)
lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =x.testing))
mean(abs(lasso.testing-house.prices.testing$SalePrice)/house.prices.testing$SalePrice*100) #calculate and display MAPE
RMSE(lasso.testing, house.prices.testing$SalePrice)
RMSE(log(lasso.testing), log(house.prices.testing$SalePrice)) #0.1154764

predicted.prices.log.i.lasso <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =x.prediction))
predicted.prices.log.i.lasso <- data.frame("SalePrice" = predicted.prices.log.i.lasso)
names(predicted.prices.log.i.lasso)[1] <- "SalePrice" 
head(predicted.prices.log.i.lasso)

predicted.prices.log.i.lasso <- data.frame("Id" = house.prices.prediction$Id, "SalePrice" = predicted.prices.log.i.lasso$SalePrice) 
write.csv(predicted.prices.log.i.lasso, file = "C:\\Users\\hp\\Desktop\\MMA\\MMA867 - Predictive Modelling\\Clean\\submission_lasso11.csv", row.names=FALSE) # export the predicted prices into a CSV file



# Final Lasso Model----------------------------------------------------------------
set.seed(1)
y <- log(house.prices.training$SalePrice)
x <- model.matrix(Id~MSZoning + log(LotArea) + Street + LotConfig + 
                    LandSlope + Neighborhood + Condition1 + OverallQual + OverallCond + 
                    YearBuilt + YearRemodAdd + Exterior1st + MasVnrType + ExterQual + 
                    ExterCond + Foundation + BsmtExposure + BsmtUnfSF +  RoofMatl +
                    HeatingQC + CentralAir + X1stFlrSF + X2ndFlrSF + BsmtFullBath + TotalBsmtSF + 
                    FullBath + HalfBath + KitchenAbvGr + KitchenQual + Functional + 
                    Fireplaces + GarageCars + GarageArea + WoodDeckSF + EnclosedPorch + 
                    ScreenPorch + PoolArea + PoolQC + SaleCondition + GrLivArea*Neighborhood+OverallQual*Neighborhood*LotArea +
                    OverallQual*LotArea+ log(GrLivArea)*TotRmsAbvGrd +remod_dum + log(Floorratio) + yearssinceremod +  
                    YearBuilt*Neighborhood, house.prices.data)[,-1] #0.12571 on lasso Kaggle, 21% rank
x<-cbind(house.prices.data$Id,x)

# split X into testing, trainig/holdout and prediction as before
x.training<-subset(x,x[,1]<=1000)
x.testing<-subset(x, (x[,1]>=1001 & x[,1]<=1460))
x.prediction<-subset(x,x[,1]>=1461)

#LASSO (alpha=1)
nFolds <- 1
foldid <- sample(rep(seq(nFolds), length.out = nrow(x.training)))
lasso.fit<-glmnet(x = x.training, y = y, alpha = 1, nfolds = nFolds,
                  foldid = foldid,
                  standardize = FALSE)
plot(lasso.fit, xvar = "lambda")

#selecting the best penalty lambda
crossval <-  cv.glmnet(x = x.training, y = y, alpha = 1) #create cross-validation data

plot(crossval)
penalty.lasso <- crossval$lambda.min #determine optimal penalty parameter, lambda
log(penalty.lasso) #see where it was on the graph
plot(crossval,xlim=c(-6,-4),ylim=c(0.00,0.005)) # lets zoom-in
lasso.opt.fit <-glmnet(x = x.training, y = y, alpha = 1, lambda = penalty.lasso) #estimate the model with the optimal penalty
coef(lasso.opt.fit) #resultant model coefficients

# predicting the performance on the testing set
library(caret)
lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =x.testing))
mean(abs(lasso.testing-house.prices.testing$SalePrice)/house.prices.testing$SalePrice*100) #calculate and display MAPE
RMSE(lasso.testing, house.prices.testing$SalePrice)
RMSE(log(lasso.testing), log(house.prices.testing$SalePrice)) #0.1139038

predicted.prices.log.i.lasso <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =x.prediction))
predicted.prices.log.i.lasso <- data.frame("SalePrice" = predicted.prices.log.i.lasso)
names(predicted.prices.log.i.lasso)[1] <- "SalePrice" 
head(predicted.prices.log.i.lasso)

predicted.prices.log.i.lasso <- data.frame("Id" = house.prices.prediction$Id, "SalePrice" = predicted.prices.log.i.lasso$SalePrice) 
write.csv(predicted.prices.log.i.lasso, file = "C:\\Users\\hp\\Desktop\\MMA\\MMA867 - Predictive Modelling\\Clean\\submission_lassoFinal.csv", row.names=FALSE) # export the predicted prices into a CSV file

#ridge (alpha=0) - Not selected
ridge.fit<-glmnet(x = x.training, y = y, alpha = 0)
plot(ridge.fit, xvar = "lambda")

#selecting the best penalty lambda
crossval <-  cv.glmnet(x = x.training, y = y, alpha = 0)
plot(crossval)
penalty.ridge <- crossval$lambda.min 
log(penalty.ridge) 
ridge.opt.fit <-glmnet(x = x.training, y = y, alpha = 0, lambda = penalty.ridge) 
coef(ridge.opt.fit)

ridge.testing <- exp(predict(ridge.opt.fit, s = penalty.ridge, newx =x.testing))
mean(abs(ridge.testing-house.prices.testing$SalePrice)/house.prices.testing$SalePrice*100) 
RMSE(log(ridge.testing), log(house.prices.testing$SalePrice)) 

predicted.prices.log.i.ridge <- exp(predict(ridge.opt.fit, s = penalty.ridge, newx =x.prediction))
predicted.prices.log.i.ridge <- data.frame("SalePrice" = predicted.prices.log.i.ridge)
names(predicted.prices.log.i.ridge)[1] <- "SalePrice" 
head(predicted.prices.log.i.ridge)



# LASSO Submission as it provides the best results ------------------------
predicted.prices.log.i.lasso <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =x.prediction))
predicted.prices.log.i.lasso <- data.frame("SalePrice" = predicted.prices.log.i.lasso)
names(predicted.prices.log.i.lasso)[1] <- "SalePrice" 
head(predicted.prices.log.i.lasso)

predicted.prices.log.i.lasso <- data.frame("Id" = house.prices.prediction$Id, "SalePrice" = predicted.prices.log.i.lasso$SalePrice) 
write.csv(predicted.prices.log.i.lasso, file = "C:\\Users\\hp\\Desktop\\MMA\\MMA867 - Predictive Modelling\\Clean\\submission_lasso17.csv", row.names=FALSE) 


