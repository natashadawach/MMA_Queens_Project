install.packages("forecast")
library(forecast)
library(tidyverse)
library(lubridate)
library(dplyr)
library(fpp)
library(zoo)
install.packages("MLmetrics")
library(MLmetrics)



# Load Data ---------------------------------------------------------------
metdata = read.csv(file.choose())

read_cru_hemi <- function(Met) {
  # read in whole file as table
  tab <- read.table("Met.txt",fill=TRUE)
  nrows <- nrow(tab)
  # create frame
  hemi <- data.frame(
    year=tab[seq(1,nrows,2),1],
    annual=tab[seq(1,nrows,2),14],
    month=array(tab[seq(1,nrows,2),2:13]),
    cover=array(tab[seq(2,nrows,2),2:13])
  )
  # mask out months with 0 coverage
  hemi$month.1 [which(hemi$cover.1 ==0)] <- NA
  hemi$month.2 [which(hemi$cover.2 ==0)] <- NA
  hemi$month.3 [which(hemi$cover.3 ==0)] <- NA
  hemi$month.4 [which(hemi$cover.4 ==0)] <- NA
  hemi$month.5 [which(hemi$cover.5 ==0)] <- NA
  hemi$month.6 [which(hemi$cover.6 ==0)] <- NA
  hemi$month.7 [which(hemi$cover.7 ==0)] <- NA
  hemi$month.8 [which(hemi$cover.8 ==0)] <- NA
  hemi$month.9 [which(hemi$cover.9 ==0)] <- NA
  hemi$month.10[which(hemi$cover.10==0)] <- NA
  hemi$month.11[which(hemi$cover.11==0)] <- NA
  hemi$month.12[which(hemi$cover.12==0)] <- NA
  #
  return(hemi)
}

metdata = read_cru_hemi(metdata)
tail(metdata)



# Tidy the data -----------------------------------------------------------
metdata = metdata[,0:14] #remove cover columns
drop <- c("annual")
new_met = metdata[,!(names(metdata) %in% drop)]


## gathering and changing names
met_combined <- new_met %>% gather(Month, Temp, month.1:month.12)
met_combined$Month = ifelse(met_combined$Month == "month.1", 01, met_combined$Month)
met_combined$Month = ifelse(met_combined$Month == "month.2", 02, met_combined$Month)
met_combined$Month = ifelse(met_combined$Month == "month.3", 03, met_combined$Month)
met_combined$Month = ifelse(met_combined$Month == "month.4", 04, met_combined$Month)
met_combined$Month = ifelse(met_combined$Month == "month.5", 05, met_combined$Month)
met_combined$Month = ifelse(met_combined$Month == "month.6", 06, met_combined$Month)
met_combined$Month = ifelse(met_combined$Month == "month.7", 07,met_combined$Month)
met_combined$Month = ifelse(met_combined$Month == "month.8", 08, met_combined$Month)
met_combined$Month = ifelse(met_combined$Month == "month.9", 09, met_combined$Month)
met_combined$Month = ifelse(met_combined$Month == "month.10", 10, met_combined$Month)
met_combined$Month = ifelse(met_combined$Month == "month.11", 11, met_combined$Month)
met_combined$Month = ifelse(met_combined$Month == "month.12", 12, met_combined$Month)


##deleting NAs
met_combined = na.omit(met_combined)



# Changing values as per instructions -------------------------------------
met_combined$Temp = met_combined$Temp + 14



# ETS ---------------------------------------------------------------------
met = met_combined[order(met_combined$year),]
met_ts = ts(met$Temp,start=1850, end = 2021, frequency=12)

fit <- decompose(met_ts, type="multiplicative") #decompose using "classical" method, multiplicative form
plot(fit)

fit <- decompose(met_ts, type="additive") #decompose using "classical" method, additive form
plot(fit)


fit <- stl(met_ts, t.window=12, s.window="periodic") #decompose using STL (Season and trend using Loess)
plot(fit)

plot(met_ts)

#To zoom in on seasonality
met_tstest = ts(met$Temp,start=2010,end = 2014, frequency=12)
fit <- decompose(met_tstest, type="multiplicative") #decompose using "classical" method, multiplicative form
plot(fit)


# Create exponential smoothing models: additive vs multiplicative noise (first A vs M), additive vs multiplicative trend (second A vs M) and no seasonality vs automatic detection (third N vs Z) trend and no seasonlity (AAN), multiplicative (MMN)
met_AAN <- ets(met_ts, model="AAN")
met_AAZ <- ets(met_ts, model="AAZ", damped=FALSE)
met_AAA <- ets(met_ts, model="AAA", damped=FALSE)
met_MMN <- ets(met_ts, model="MMN", damped=FALSE)
met_MMZ <- ets(met_ts, model="MMZ", damped=FALSE)
met_MMM <- ets(met_ts, model="MMM", damped=FALSE)

# Create their prediction "cones" for 960 months  into the future with quintile confidence intervals
met_AAN_pred <- forecast(met_AAN, h=960, level=c(0.90))
met_AAZ_pred <- forecast(met_AAZ, h=960, level=c(0.90))
met_AAA_pred <- forecast(met_AAA, h=960, level=c(0.90))
met_MMN_pred <- forecast(met_MMN, h=960, level=c(0.90))
met_MMZ_pred <- forecast(met_MMZ, h=960, level=c(0.90))
met_MMM_pred <- forecast(met_MMM, h=960, level=c(0.90))

# Compare the prediction "cones" visually
par(mfrow=c(1,1)) # This command sets the plot window to show 1 row of 4 plots
plot(met_AAN_pred, xlab="Year", ylab="Predicted Temperatures")
plot(met_MMN_pred, xlab="Year", ylab="Predicted Temperatures")
plot(met_AAZ_pred, xlab="Year", ylab="Predicted Temperatures")
plot(met_MMZ_pred, xlab="Year", ylab="Predicted Temperatures")
plot(met_AAA_pred, xlab="Year", ylab="Predicted Temperatures")
plot(met_MMM_pred, xlab="Year", ylab="Predicted Temperatures")

# Lets look at what our models actually are -- ETS
met_AAN
met_AAZ
met_AAA
met_MMN
met_MMZ
met_MMM


# TBATS --------------------------------------------------------------------
#Create a trigonometric box-cox autoregressive trend seasonality (TBATS) model
met_tbats <- tbats(met_ts)
met_tbats_pred <-forecast(met_tbats, h=960, level=c(0.90))
par(mfrow=c(1,1))
plot(met_tbats_pred, xlab="Year", ylab="Predicted Temperatures")

par(mfrow=c(1,1)) # Lets look at the three models with seasonality on one graph on the same scale
plot(met_AAZ_pred, xlab="Year", ylab="Predicted Temperatures", xlim= c(2019, 2100))
plot(met_MMZ_pred, xlab="Year", ylab="Predicted Temperatures",xlim= c(2019, 2100))
plot(met_tbats_pred, xlab="Year", ylab="Predicted Temperatures",xlim= c(2019, 2100))

### Comparing models -- Time series Cross Validation (Rolling Horizon Holdout)
f_AAN  <- function(y, h) forecast(ets(y, model="AAN"), h = h)
errors_AAN <- tsCV(met_ts, f_AAN, h=1, window=1500)

f_MMN  <- function(y, h) forecast(ets(y, model="MMN"), h = h)
errors_MMN <- tsCV(met_ts, f_MMN, h=1, window=1500)

f_AAA  <- function(y, h) forecast(ets(y, model="AAA"), h = h)
errors_AAA <- tsCV(met_ts, f_AAA, h=1, window=1500)

f_MMM  <- function(y, h) forecast(ets(y, model="MMM"), h = h)
errors_MMM <- tsCV(met_ts, f_MMM, h=1, window=1500)


par(mfrow=c(1,1)) 
plot(errors_AAN, ylab='tsCV errors')
abline(0,0)
lines(errors_MMN, col="red")
lines(errors_AAA, col="green")
lines(errors_MMM, col="blue")
legend("left", legend=c("CV_error_AAN", "CV_error_MMN","CV_error_AAA","CV_error_MMM"), col=c("black", "red", "green", "blue"), lty=1:4)

mean(abs(errors_AAN/met_ts), na.rm=TRUE)*100 
mean(abs(errors_AAN), na.rm=TRUE) 
sqrt(mean((errors_AAN)^2, na.rm=TRUE))


mean(abs(errors_MMN/met_ts), na.rm=TRUE)*100 
mean(abs(errors_MMN), na.rm=TRUE) 
sqrt(mean((errors_MMN)^2, na.rm=TRUE))

mean(abs(errors_AAA/met_ts), na.rm=TRUE)*100 
mean(abs(errors_AAA), na.rm=TRUE) 
sqrt(mean((errors_AAA)^2, na.rm=TRUE))

mean(abs(errors_MMM/met_ts), na.rm=TRUE)*100 
mean(abs(errors_MMM), na.rm=TRUE) 
sqrt(mean((errors_MMM)^2, na.rm=TRUE))

f_TBATS  <- function(y, h) forecast(tbats(y), h = h)
errors_TBATS <- tsCV(met_ts, f_TBATS, h=1, window=1500)

plot(errors_AAA, ylab='tsCV errors', col="green")
abline(0,0)
lines(errors_MMM, col="blue")
lines(errors_TBATS, col="gray")
legend("left", legend=c("CV_error_AAA", "CV_error_MMM","CV_error_TBATS"), col=c("green", "blue", "gray"), lty=1:4)

mean(abs(errors_TBATS/met_ts), na.rm=TRUE)*100 #MAPE of 0.5033279
mean(abs(errors_TBATS), na.rm=TRUE) #MAE:
sqrt(mean((errors_TBATS)^2, na.rm=TRUE))

# ARIMA -------------------------------------------------------------------
test = met$Temp
plot(met_ts, xlab="Year",
     ylab="Temperatures")
plot(log(met_ts), xlab="Year",
     ylab="log Temperatures")
plot(diff(log(met_ts),12), xlab="Year",
     ylab="Annual change in monthly log Temperatures")

fit <- stl(diff(log(met_ts)), t.window=12, s.window="periodic", robust=TRUE)
plot(fit)

# auto-correlation function
Acf(met_ts,main="") # data "as is"
Acf(log(met_ts),main="") # log-transformed data
Acf(diff(log(met_ts),12),main="") # difference-12 log data

# partial auto-correlation function
par(mfrow=c(1,1))
Acf(diff(log(met_ts),12),main="")
Pacf(diff(log(met_ts),12),main="") 

fit <- auto.arima(met_ts, seasonal = FALSE, trace = TRUE)
fit

fit <- auto.arima((met_ts),seasonal = TRUE, trace = TRUE)
fit


f_arima  <- function(y, h) forecast(auto.arima(y, seasonal = TRUE), h = h)
errors_Arima <- tsCV(met_ts, f_arima, h=1, window=1500)

mean(abs(errors_Arima/met_ts), na.rm=TRUE)*100 #MAPE 
mean(abs(errors_Arima), na.rm=TRUE) #MAE:
sqrt(mean((errors_Arima)^2, na.rm=TRUE))

fit

accuracy(fit)

par(mfrow=c(1,1))
Acf(residuals(fit))
plot(forecast(fit,960), xlim = c(2021, 2100)) #960 stands for 960 months = 80 years

plot(test, main="Temperatures", xlab="Year")
View(test)




# Validating ARIMA with Residuals ---------------------------------------------
##### Rolling-horizon holdout: ARIMA on residuals
 

met_ts_nostart = ts(met$Temp, frequency=12)

accuracy.arima=0 # we will check average 1-day-out accuracy for 7 days
for (i in 1:12)
{ 
  nTest <- 12*i  
  nTrain <- length(met_ts_nostart)- nTest -1
  train <- window(met_ts_nostart, start=1, end=1+(nTrain)/(12*12))
  test <- window(met_ts_nostart, start=1+(nTrain+1)/(12*12), end=1+(nTrain+12)/(12*12))
  
  trainlm <- tslm(train ~ trend + season)
  trainlmf <- forecast(trainlm,h=12)
  
  residauto <- auto.arima(trainlm$residuals)
  residf <- forecast(residauto,h=12)
  
  y <- as.numeric(trainlmf$mean)
  x <- as.numeric(residf$mean)
  sp <- x+y
  
  cat("----------------------------------
      
      Data Partition",i,"
      
      Training Set includes",nTrain," time periods. Observations 1 to", nTrain, "
      Test Set includes 12 time periods. Observations", nTrain+1, "to", nTrain+12,"
      
      ")
  
  print(accuracy(sp,test))
  #  print(residauto)
  
  accuracy.arima<-rbind(accuracy.arima,accuracy(sp,test)[1,5])
  
  #print(sp$model)
}
accuracy.arima<-accuracy.arima[-1]

mean(accuracy.arima)
sd(accuracy.arima)



# ARIMA on Residuals  -------------------------------

#Q1 model
MET_msts_train <- msts(met$Temp, seasonal.periods = c(12))
WFlm_msts <- tslm(MET_msts_train ~ trend + season) # Build a linear model for trend and seasonality
summary(WFlm_msts)

residarima1 <- auto.arima(WFlm_msts$residuals) # Build ARIMA on it's residuals
residarima1
residualsArimaForecast <- forecast(residarima1, h=960, level=c(0.9)) #forecast from ARIMA
residualsF <- as.numeric(residualsArimaForecast$mean)

regressionForecast <- forecast(WFlm_msts,h=960, level=c(0.9)) #forecast from lm
regressionF <- as.numeric(regressionForecast$mean)

forecastR <- regressionF+residualsF # Total prediction

MET_arima_pred2_date <- data.frame("Date" =seq(as.Date("2021/02/1"), by = "month", length.out =960))
MET_arima_pred2_train_final <- cbind(MET_arima_pred2_date,regressionForecast)
MET_arima_pred2_final_data <- data.frame("Date" =MET_arima_pred2_train_final$Date,"forecast temp." = MET_arima_pred2_train_final$`Point Forecast`) 


write.csv(MET_arima_pred2_train_final, "C:\\Users\\hp\\Desktop\\MMA\\MMA867 - Predictive Modelling\\Assignment 2\\UKMET\\UK_met_Q1.csv")

write.csv(residualsArimaForecast, "C:\\Users\\hp\\Desktop\\MMA\\MMA867 - Predictive Modelling\\Assignment 2\\UKMET\\UK_met_Q1_1CI90.csv")

write.csv(regressionForecast, "C:\\Users\\hp\\Desktop\\MMA\\MMA867 - Predictive Modelling\\Assignment 2\\UKMET\\UK_met_CI90.csv")


#Q6 model
data_train2 = subset(met, met$year <= 2006)
data_train3 = subset(met, met$year <= 1998)


MET_msts_train <- msts(data_train2$Temp, seasonal.periods = c(12))
WFlm_msts <- tslm(MET_msts_train ~ trend + season) # Build a linear model for trend and seasonality
summary(WFlm_msts)

residarima1 <- auto.arima(WFlm_msts$residuals) # Build ARIMA on it's residuals
residarima1
residualsArimaForecast <- forecast(residarima1, h=132, level=c(0.9)) #forecast from ARIMA
residualsF <- as.numeric(residualsArimaForecast$mean)

regressionForecast <- forecast(WFlm_msts,h=132, level=c(0.9)) #forecast from lm
regressionF <- as.numeric(regressionForecast$mean)

forecastR <- regressionF+residualsF # Total prediction


write.csv(forecastR, "C:\\Users\\hp\\Desktop\\MMA\\MMA867 - Predictive Modelling\\Assignment 2\\UKMET\\UK_met_2007.csv")


#Q7 model
MET_msts_train <- msts(data_train3$Temp, seasonal.periods = c(12))
WFlm_msts <- tslm(MET_msts_train ~ trend + season) # Build a linear model for trend and seasonality
summary(WFlm_msts)

residarima1 <- auto.arima(WFlm_msts$residuals) # Build ARIMA on it's residuals
residarima1
residualsArimaForecast <- forecast(residarima1, h=252, level=c(0.9)) #forecast from ARIMA
residualsF <- as.numeric(residualsArimaForecast$mean)

regressionForecast <- forecast(WFlm_msts,h=252, level=c(0.9)) #forecast from lm
regressionF <- as.numeric(regressionForecast$mean)

forecastR <- regressionF+residualsF # Total prediction

write.csv(forecastR, "C:\\Users\\hp\\Desktop\\MMA\\MMA867 - Predictive Modelling\\Assignment 2\\UKMET\\UK_met_2007.csv")

