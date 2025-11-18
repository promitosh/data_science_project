install.packages("readxl")  # Run once
library(readxl)
# Download data from DDS # to Canada | Sub Region 2022 - 2025 O & D Estimates

df<-read_excel("D:\\Project_ASDS\\DACYYZ.xlsx") #read excel
# Pax is every 3rd column starting at col 1
pax_df<-df[ , seq(1, ncol(df), 3) ]
colnames(pax_df)
head(pax_df)
pax_t <- as.data.frame(t(pax_df)) #  transpose and converting data frame from excel file
str(pax_t)
head(pax_t)
#Delete V1 and rename V2 → as Pax
pax_t$V1 <- NULL

head(pax_t)
dim(pax_t)
str(pax_t)
pax_tt<-pax_t
dim(pax_tt)
pax_tt <- pax_tt[-44, , drop = FALSE] # Keep the data frame structure
#Check for data errors or outliers
tail(pax_tt$Pax)
# Now renaming the column will work correctly
colnames(pax_tt)[colnames(pax_tt) == "V2"]  <- "Pax"
head(pax_tt)
# to convert character data type to numeric of Pax column
pax_tt$Pax <- as.numeric(pax_tt$Pax)
str(pax_tt)
# convert into Time series object
PaxTS<- ts(pax_tt, start = c(2022, 3), frequency = 12)
PaxTS_trimmed <- window(PaxTS, start = c(2022, 9))# trimmed


plot(PaxTS_trimmed,main="Time series on monthly Pax on Dhaka to Toronto", col="blue")
install.packages("tsoutliers")
library(tsoutliers)
outlier_results <- tso(PaxTS)
outlier_results
outlier_times <- outlier_results$outliers$time
outlier_indices <- outlier_results$outliers$ind
PaxTS[6]#2428
head(pax_tc)
str(pax_tc) #data.frame':	43 obs. of  1 variable:

ttl<-sum(pax_tc)
ttl
min_row <- pax_t[which.min(pax_tc), ]
min_row
pax_t_df <- as.data.frame(pax_tc)
str(pax_t_df)
head(pax_t_df)
ggsubseriesplot(PaxTS_trimmed) + ylab("Number of Pax") + 
  ggtitle("Seasonal subseries plot: AirPassengers of BG")
# shows variance on monthly data so  seasonality exists
boxplot(PaxTS_trimmed ~ cycle(PaxTS_trimmed)) 
#moderate seasonality present, in X11 and STL decomposition may further check 
# splitting
train_ts<- window(PaxTS, end = c(2024, 12))
test_ts<- window(PaxTS, start = c(2025, 1))
#stationarity test
adf.test(train_ts,k=0) 
# Go for transformation, diff and autoarima for SARIMA
# BoxCox applied to stabilze for variance
lambda<-BoxCox.lambda(train_ts)
lambda
train_s_bc <- BoxCox(train_ts, lambda = lambda)
train_s_bc_diff <- diff(train_s_bc) # differencing
sarima_model_bc <- auto.arima(train_s_bc_diff, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
print(sarima_model_bc)
checkresiduals(sarima_model_bc)

boxplot(data_ts_diff ~ cycle(data_ts_diff))

# STL decomposition for strength calc
#convert matrix ts → univariate ts,no second dimension
PaxTS2 <- ts(as.numeric(PaxTS[,1]),
             start = c(2022, 3),
             frequency = 12)

stl_decomp_strngth <- stl(PaxTS2,s.window = "periodic")
# Assuming stl_decomp_strength is stl result object
seasonal_component <- stl_decomp_strngth$time.series[, "seasonal"]
residuals_component <- stl_decomp_strngth$time.series[, "remainder"]
var_Rt <- var(residuals_component, na.rm = TRUE)
var_St_Rt <- var(seasonal_component + residuals_component, na.rm = TRUE)
Fs <- max(0, 1 - var_Rt / var_St_Rt)
Fs

plot(decompose(PaxTS))
x11_decomp_s<- seas(PaxTS, x11 = "")
seasonal_components<-seasonal(x11_decomp_s)
seasonal_t <- t(seasonal_components)
seasonal_components_ts <- ts(seasonal_components, start = 2022, frequency = 12)
plot(seasonal_components,main="Seasonal components",col="green",lwd=2.5,ylab="Pax")

adf.test(train_ts,k=0) 
train_ts_diff<-diff(train_ts)
adf.test(train_ts_diff)#p-value = 0.01387
acf(train_ts_diff,lag=102)
pacf(train_ts_diff,lag=102)

#model based on acf and pacf analysis found p=1,i=1,q=0,so,ARIMA(110)
fit_arima<- Arima(train_ts_diff,  order = c(1,1,0)) # this is the first model on subjected
checkresiduals(fit_arima)
print(fit_arima)
#forecasted_values replaced by M1
M1<- forecast(fit_arima, h = length(test_ts))
plot(M1)
accuracy(M1,test_ts)

#automated fit
fit_arima_auto<- auto.arima(train_ts_diff)
print(fit_arima_auto)
checkresiduals(fit_arima_auto)

# Splitting
train <- window(PaxTS,frequency=12, end = c(2024, 12))
test <- window(PaxTS,frequency=12, start = c(2025, 1))

#Fit an ETS model WITHOUT any manual difference
ets_model<- ets(train) #choose the best model automatically
print(ets_model) #  ETS(ANN) model # it shows no trend,no seasonality,
checkresiduals(ets_model)#df = 14, p-value = 0.636

#ets with seasonality
M2<-ets_model_N<- ets(train, model = "ANA") # ETS(A,N,A) # with seasonality
print(M2) #
checkresiduals(M2) 
ets_fc<- forecast(M2, h = length(test))
accuracy(ets_fc,test)
# forecast with this model for next 12 months
ets_fc_model<-ets(PaxTS,model="ANA")
ets_fc_final_values <- forecast(ets_fc_model, h = 12)
plot(ets_fc_final_values)

# X11 +   ARIMA
x11_decomp<- seas(PaxTS, x11 = "")#Extract seasonally adjusted series, minus seasonal from original
sa_series<- final(x11_decomp) # sa means here seasonally adjusted
#splitting
train_length <- length(sa_series) - 12  # Last 12 months as test
train_sa <- window(sa_series, end = c(time(sa_series)[train_length]))
test_sa <- window(sa_series, start = c(time(sa_series)[train_length + 1]))

#Simple Exponential Smoothing (SES)
paxdata <- window(PaxTS, start=c(2022,3),Frequency=12)
# Estimate parameters
fc_ses<- ses(paxdata, h=12)
# Accuracy of one-step-ahead training errors
round(accuracy(fc_ses),2) # where is test method #23609.66
plot(fc_ses)
checkresiduals(fc_ses)# 
train <- window(PaxTS2,frequency=12, end = c(2024, 12))
test <- window(PaxTS2,frequency=12, start = c(2025, 1))
# STL+ Naive/ Randomwalk without drift
# Naive forecast of seasonally adjusted data
fit <- stl(train, t.window=13, s.window="periodic",
           robust=TRUE)
fit %>% seasadj() %>% naive() %>%
  autoplot() + ylab("New orders index") +
  ggtitle("Naive forecasts of seasonally adjusted data")
fit %>% forecast(method="naive") %>%
  autoplot() + ylab("New orders index")
fcast_stl_naive<- stlf(train, method='naive')
print(fcast_stl_naive)
checkresiduals(fcast_stl_naive) #nothing predictable remains
#It tests whether residuals behave like white noise,no autocorrelation left,model has captured all structure
accuracy(fcast_stl_naive,test) # 

final_model_naive<- stlf(data_ts, method='naive',h=12) #Retrain on full dataset and forecast 
plot(final_model_naive)
lines(test,col="red")

# STL and ARIMA
# splitting
train_ts<- window(PaxTS2, end = c(2024, 10))
test_ts<- window(PaxTS2, start = c(2024, 11))
stl_obj <- stl(train_ts, s.window = "periodic", robust = TRUE)   
seasadj_train <- seasadj(stl_obj)
fit_arima_sa <- auto.arima(seasadj_train, seasonal = FALSE)
print(fit_arima_sa)
checkresiduals(fit_arima_sa)
fc_arima_sa <- forecast(fit_arima_sa, h =length(test))
freq <- frequency(train_ts)
freq

seasonal_fc_vec <- rep(tail(stl_obj$time.series[, "seasonal"], freq), length.out=length(test))
seasonal_fc <- ts(seasonal_fc_vec, start = start(fc_arima_sa$mean), frequency = freq)
#  Combine ARIMA forecast and seasonal forecast
fc_final_mean <- fc_arima_sa$mean + seasonal_fc
fc_final_mean
# Calculate accuracy against test data
accuracy(fc_final_mean, test_ts)# STL + ARIMA

stl_full <- stl(PaxTS2, s.window = "periodic", robust = TRUE) #add back the seasonal component to get the final forecast
seasadj_full <- seasadj(stl_full)
fit_arima_full <- auto.arima(seasadj_full, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
# Forecast on full data
fc_arima_full <- forecast(fit_arima_full, h = length(test))
seasonal_full <- stl_full$time.series[, "seasonal"]
seasonal_fc_vec_full <- rep(tail(seasonal_full, freq), length.out = length(test))
seasonal_fc_full <- ts(seasonal_fc_vec_full, start = start(fc_arima_full$mean), frequency = freq)
# Combine ARIMA forecast and seasonal forecast for full data

final_fc_mean <- fc_arima_full$mean + seasonal_fc_full
final_fc <- fc_arima_full
final_fc$mean <- final_fc_mean
final_fc$lower <- sweep(fc_arima_full$lower, 1, seasonal_fc_full, "+")  # margin=1 for rows (time points)
final_fc$upper <- sweep(fc_arima_full$upper, 1, seasonal_fc_full, "+")
# Plot final forecast with ggplot2
autoplot(final_fc) +
  ggtitle("Final 12-Month Forecast (STL + ARIMA)") +
  ylab("Forecasted Value") + xlab("Time") +
  autolayer(test_ts, series = "Test Data", PI = FALSE, color = "red")

#thanks God
