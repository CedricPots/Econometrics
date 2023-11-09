# =================================================================================
# Case 1 Time Series Econometrics 
# Authors: Ties Bos, Cedric Pots, Barbora Rakovanova
# =================================================================================

############## Import packages #################################################
library(ggplot2)
library(forecast)
library(feasts)
library(dplyr)
library(MCS)
library(tibble)

############## Data Exploration (Full Dataset) ##################################
#   Load it into a data frame:
load("C:\\Users\\cedri\\Dropbox\\MSc Econometrics & Operations Research\\Time Series Econometrics\\Case study\\Case 1_data")    
citibike.df <- data.frame(citibike)

#   Head of the dataset
    head(citibike)

#   Summary of the dataset
    summary(citibike)

#   Looking at the possibility of a trend in the data
    plot(citibike.df$demand, type="l", ylab="Demand", xlab="Index", 
         main="Demand in the whole set")

#       Trendline:
        x <- 1:nrow(citibike.df)
        trend.lm <- lm(citibike.df$demand~x)
        summary(trend.lm)
        alpha_hat <- trend.lm$coefficients[1]
        beta.hat <- trend.lm$coefficient[2]

#       Plot with trend line:
        plot(citibike.df$demand, type="l", ylab="Demand", xlab="Index", 
             main="Demand in the whole set")
        abline(a=alpha_hat, b=beta.hat, col="red")

#     One should note that this trend might be seasonality on its own 
#     (more people might take the bike in spring compared to winter).


#   Monthly Dynamics
    boxplot(demand~month, data=citibike.df, main = "Hourly Demand per Month", 
            xlab = "Month (by index)", ylab="Hourly Demand", col="#B2DFDB")
#     Implies an upward trend in hourly demand as months go by.

#   Weekly Dynamics
    boxplot(citibike.df$demand~citibike.df$wkday, xlab="Day of the week by index", ylab="Hourly Demand", 
            main="Hourly Demand dynamics per weekday", col="#B2DFDB")
#     Weekends show a lower hourly demand than during the week.
#     --> indicates weekly seasonality in the dataset.

#   Daily Dynamics
#     General

#       First Week
        plot(1:(24*7), citibike.df$demand[1:(24*7)], type="l", xlab="Hour by index",
             ylab = "Hourly Demand", main="Hourly Demand of the First Week")

#       Boxplot
        boxplot(demand~hour, citibike.df, main = "Box Plots of Hourly demand of citibikes", 
                ylab= "Demand", xlab="Hour", col="#B2DFDB")

#       Average Hourly Demand
        avg_hourly_demand <- rep(0, 24)
        for(i in 0:23){
          hour_i <- (citibike.df$hour==i)
          hourly_demand_i <- citibike.df$demand[hour_i]
          avg_hourly_demand[i+1] <- mean(hourly_demand_i)
        }
        plot(0:23, avg_hourly_demand, type="l", lwd=2, xlab="Hour", ylab="Average Demand", 
             main= "Average Hourly Demand")
        abline(v=8, col="red", lty=2, lwd=2)
        abline(v=17, col="blue", lty=2, lwd=2)
        legend(0.5, 850, c("8:00", "17:00"), col=c("red", "blue"), lty=c(2, 2), lwd=c(2,2))

#       --> Implies seasonality as there are spikes at 8:00 and 17:00 on weekdays.


#   Only Weekdays
      # We start by defining a vector with TRUE if it is a weekday:
      weekdayindex <- c(1,2,3,4,5)
      weekdayvec <- (citibike.df$wkday %in% weekdayindex)
      
      # Now, we can make the boxplot over all weekdays and hours:
      no_weekday_hours <- sum(weekdayvec)
      matrix <- matrix(0, nrow=0, ncol=2)
      
      weekdays.df <- citibike.df[weekdayvec,]
      # Boxplots:
      boxplot(demand~hour, weekdays.df, main="Box Plots per hour for weekdays (Mo-Fri)",
              xlab="Hour", ylab="Demand", col="#B2DFDB")
      
      # Average:
      average_wkday_hr <- rep(0, 24)
      for(i in 0:23){
        average_wkday_hr[i+1] <- mean(weekdays.df$demand[weekdays.df$hour==i])
      }
      plot(0:23, average_wkday_hr, type="l", lwd=2, ylab= "Average Hourly Demand", xlab="Hour", 
           main="Average Hourly Demand on Weekdays (Mo-Fri)")
      abline(v=8, col="red", lty=2, lwd=2)
      abline(v=17, col="blue", lty=2, lwd=2)
      legend(0.5, 850, c("8:00", "17:00"), col=c("red", "blue"), lty=c(2, 2), lwd=c(2,2))

#   Only Weekends
      # We start by defining a vector with TRUE if it is in a weekend:
      weekendind <- c(6,7)
      weekendvec <- (citibike.df$wkday %in% weekendind)
      
      #  Boxplots:
      weekend_df <- citibike.df[weekendvec,]
      boxplot(demand~hour, weekend_df, col="#B2DFDB",
              main= "Box Plots per hour for weekends (Sat-Sun)")
      
      # Average:
      average_wknd_hr <- rep(0, 24)
      for(i in 0:23){
        average_wknd_hr[i+1] <- mean(weekend_df$demand[weekend_df$hour==i])
      }
      plot(0:23, average_wknd_hr, type="l", lwd=2, ylab= "Average Hourly Demand", xlab="Hour", 
           main="Average Hourly Demand on Weekend days (Sat-Sun)")
      abline(v=8, col="red", lty=2, lwd=2)
      abline(v=17, col="blue", lty=2, lwd=2)
      legend(0.5, 600, c("8:00", "17:00"), col=c("red", "blue"), lty=c(2, 2), lwd=c(2,2))

      

################### Structuring the training data ##############################
jan_apr <- c(1,2,3,4)
jan_apr_vec <- citibike.df$month %in% jan_apr

# Choosing the frequency of the timeseries object:
frequency <- 24
#   Seasonality tends to repeat itself daily.

# Full timeseries 
full_ts <- ts(citibike.df$demand, freq= frequency)

# Train timeseries:
train_ts <- ts(full_ts[jan_apr_vec], freq=frequency)

################### ARIMA Models ###############################################

# Approach 1: Manual Specification
  # We start by exploring the ACF and PACF
  acf(train_ts, main= "ACF of Demand", col="black")
  pacf(train_ts, main= "PACF of Demand", col="black")
  # --> clear signs of seasonality in the data. As the ACF is sinusoidal and 
  #     there is a significant spike the first 2 lags in the PACF, the model is  
  #     of the form ARIMA(2,d,0) (Hyndman & Athanasopoulos, 2021).
  
  # To think about the "d" parameter, we plot the first-differenced demand:
  plot(diff(train_ts), type="l", ylab = "First-differenced Demand")
  # --> Looks stationary, so we manually select the ARIMA(2,1,0) model.
  
# Approach 2: Auto-ARIMA
  # Comparison by AIC:
  # We restrict P=Q=D=0, as we do not want to use SARIMA just yet.
  ARIMA_model1 <- auto.arima(train_ts, stepwise=FALSE, approx=FALSE,
                             ic="aic", max.P=0, max.Q=0, max.D=0)
  summary(ARIMA_model1)
  # --> gives ARIMA(2,1,3).
  
  # Comparison by BIC:
  ARIMA_model2 <- auto.arima(train_ts, stepwise=FALSE, approx=FALSE,
                             ic="bic", max.P=0, max.Q=0, max.D=0)
  summary(ARIMA_model2)
  # --> gives ARIMA(2,1,3) as well.

# The two ARIMA models under consideration are the ARIMA(2,1,0) and ARIMA(2,1,3).
  
  ljung_box(arima210$residuals, lag = 48, dof = 2)
  ljung_box(arima213$residuals, lag = 48, dof = 5)

################### SARIMA Models ##############################################
# SARIMA: (p,d,q) (P,D,Q)m
# (p,d,q) are non-seasonal parameters determined as in ARIMA case
# (P,D,Q) are seasonal parameters determined using ACF and PACF, but only
# looking at lags that are multiples of the seasonl order
# m: seasonal order (in this case number of observations per day)

# Approach 1: Manual Specification
  ggtsdisplay(train_ts, plot.type="partial")
  # Series does not look stationary, ACF suggests a seasonal order of 24
  # We apply seasonal differencing:
    # Seasonal differencing is defined as a difference between a value and 
    # a value with lag that is a multiple of S (where S is span of the periodic 
    # seasonal behavior)

  # -> seasonal differencing wrt lag 24
  ggtsdisplay(x = diff(train_ts, 24), plot.type = "partial", lag = 72)
  #looks stationary
  # still seems to have some seasonality present in ACF
  
  # firstly exponential decay in ACF, then somewhat of a sinusoidal behavior
  # -> m = 0 or 1
  # PACF: p = 2
  # d = 0
  
  # ACF: exponential(ish) decay of seasonal lags 24 -> M = 0 or 1
  # PACF: P = 1
  # D = 1
  
  # -> seasonal differencing wrt lag 168
  ggtsdisplay(x = diff((train_ts), 168), plot.type = "partial", lag = 96)
  
  # ACF: exponentially(ish) decaying -> m = 0 (or 1)
  # PACF: p = 2
  # d = 0
  
  # PACF: no significant spikes at seasonal lags -> P = 0
  # ACF: M = 3 or 0
  # D = 1
  
  # models:
  # models using D wrt. 24:
  # -> (2,0,0)(1,1,0)
  # -> (2,0,1)(1,1,0)
  # -> (2,0,0)(1,1,1)
  # -> (2,0,1)(1,1,1)
  
  # models using D wrt. 168:
  # -> (2,0,0)(0,1,0)
  # -> (2,0,1)(0,1,0)
  # -> (2,0,0)(0,1,3)
  # -> (2,0,1)(0,1,3)
  
  # all models to be looked at:
  sarima200110 <- forecast::Arima(train_ts, order = c(2,0,0), seasonal = c(1,1,0))
  sarima201110 <- forecast::Arima(train_ts, order = c(2,0,1), seasonal = c(1,1,0))
  sarima200111 <- forecast::Arima(train_ts, order = c(2,0,0), seasonal = c(1,1,1))
  sarima201111 <- forecast::Arima(train_ts, order = c(2,0,1), seasonal = c(1,1,1))
  sarima200010 <- forecast::Arima(train_ts, order = c(2,0,0), seasonal = c(0,1,0))
  sarima201010 <- forecast::Arima(train_ts, order = c(2,0,1), seasonal = c(0,1,0))
  sarima200013 <- Arima(train_ts, order = c(2,0,0), seasonal = c(0,1,3))
  sarima201013 <- Arima(train_ts, order = c(2,0,1), seasonal = c(0,1,3))
  
  summary(sarima200110)
  summary(sarima201110)
  summary(sarima200111)
  summary(sarima201111)
  summary(sarima200010)
  summary(sarima201010)
  summary(sarima200013)
  summary(sarima201013)
  
  acf(sarima200110$residuals)
  acf(sarima201110$residuals)
  acf(sarima200111$residuals)
  acf(sarima201111$residuals)
  acf(sarima200010$residuals)
  acf(sarima201010$residuals)
  acf(sarima200013$residuals)
  acf(sarima201013$residuals)
  
  # still some significant spikes in the acf of all models.
  
  # rejection of ljung_box test means that there is significant correlation
  # left in the residuals that is not captured in the model
  
  # H0: null hypothesis of independence in a given time series
  ljung_box(sarima200110$residuals, lag = 48, dof = 3)
  
  ljung_box(sarima201110$residuals, lag = 48, dof = 4)
  
  ljung_box(sarima200111$residuals, lag = 48, dof = 4)
  
  ljung_box(sarima201111$residuals, lag = 48, dof = 5)
  
  ljung_box(sarima200010$residuals, lag = 48, dof = 2)
  
  ljung_box(sarima201010$residuals, lag = 48, dof = 3)
  
  ljung_box(sarima200013$residuals, lag = 48, dof = 5)
  
  ljung_box(sarima201013$residuals, lag = 48, dof = 6)
  
  # all p-values = 0 --> we reject in all cases.
  
# Approach 2: Automatic Selection
  auto_model <- forecast::auto.arima(train_ts, stepwise = FALSE, 
                                     approx = FALSE, ic = "aic")
  summary(auto_model)
  # ARIMA(1,0,0)(2,1,2)[24]
  
  acf(auto_model$residuals)
  #
  
  ljung_box(auto_model$residuals, lag = 48, dof = 5)
  # p-value of 0 --> reject ljung_box test.
  
  
  # Based on the AICs we use the SARIMA(2,0,0)(0,1,3), SARIMA(2,0,1)(0,1,3) and
  # the automatically chosen one (SARIMA(1,0,0)(2,1,2)).

################### Exponential Smoothing Models ###############################
# We consider all reasonable combinations of possible trends (No trend (N),
# Additive (A), Damped Additive (A_d), Multiplicative (M), Damped Multiplicative 
# (M_d)), seasonality (No (N), Additive (A) and Multiplicative (M) Seasonality)
# and error terms (Multiplicative (M) and Additive (A)).

# Due to the demand series containing 0s, exponential smoothing cannot use 
# multiplicative error-terms.

# As the chosen frequency is high (24), using multiplicative trends and 
# multiplicative seasonality are not recommended. Hence, we only consider the 
# exponential smoothing models with A error terms, {N, A, A_d} trends and 
# {N, A} seasonality and compare them based on AIC, in line with 
# Hyndmann & Khandakar (2008).
  
  # Matrix to store AICs:
  A.AIC.df <- data.frame(matrix(NaN, nrow=3, ncol=2), row.names=c("N", "A", "A_d"))
  colnames(A.AIC.df) <- c("N", "A")
  
  # No trend, No stationarity (ANN)
  ANN.ets <- ets(train_ts, model="ANN")
  summary(ANN.ets)
  A.AIC.df[1,1] <- ANN.ets$aic
  
  # No trend, Additive Seasonality (ANA)
  ANA.ets <- ets(train_ts, model="ANA", damped=FALSE)
  summary(ANA.ets)
  A.AIC.df[1,2] <- ANA.ets$aic
  
  # Additive trend, No seasonality (AAN)
  AAN.ets <- ets(train_ts, model="AAN", damped=FALSE)
  summary(AAN.ets)
  A.AIC.df[2,1] <- AAN.ets$aic
  
  # Additive trend, additive seasonality (AAA)
  AAA.ets <- ets(train_ts, model="AAA", damped=FALSE)
  summary(AAA.ets)
  A.AIC.df[2,2] <- AAA.ets$aic
  
  # Damped Additive trend, no seasonality (AAdN)
  AAdN.ets <- ets(train_ts, model="AAN", damped=TRUE)
  summary(AAdN.ets)
  A.AIC.df[3,1] <- AAdN.ets$aic
  
  # Damped additive trend, additive seasonality (AAdA)
  AAdA.ets <- ets(train_ts, model="AAA", damped=TRUE)
  summary(AAdA.ets)
  A.AIC.df[3,2] <- AAdA.ets$aic
  
  # To show multiplicative trends/seasonality lead to error:
  # Damped Additive Trend, Multiplicative Seasonality 
  AMA.ets <- ets(train_ts, model="AMA", damped=FALSE)

  # Choosing the optimal model can be done in multiple ways. In line with 
  # Hyndmann & Khandakar (2008), we choose the model that minimizes the AIC. This
  # corresponds to the exponential smoothing model with no trend and additive 
  # seasonality (see Table 1). However, the model with damped additive trend and 
  # additive seasonality performs only slightly worse. As we have observed a trend, 
  # especially in the last observations, it is worth considering this latter model
  # as well in the next step. In short, we consider the (A, N, A) and (A, A_d, A)
  # exponential smoothing models.

################### In-Sample Comparison #######################################
no.of.models <- 7
no.of.crit <- 2

in_samp_crit <- matrix(NA, nrow=no.of.models, ncol=no.of.crit)
colnames(in_samp_crit) <- c("MSE", "MAE")  
rownames(in_samp_crit) <- c("ARIMA(2,1,0)", "ARIMA(2,1,3)", "SARIMA(2,0,0)(0,1,3)",
                            "SARIMA(2,0,1)(0,1,3)", "SARIMA(1,0,0)(2,1,2)", "ANA",
                            "AAdA") 
  # MSE ARIMA(2,1,0)
arima210 <- Arima(train_ts, order=c(2,1,0), seasonal=c(0,0,0))
in_samp_crit[1,1] <- mean((arima210$residuals^2))

  # MSE ARIMA(2,1,3)
arima213 <- Arima(train_ts, order=c(2,1,3), seasonal=c(0,0,0))
in_samp_crit[2,1] <- mean((arima213$residuals^2))

  # MSE SARIMA(2,0,0)(0,1,3)
in_samp_crit[3,1] <- mean((sarima200013$residuals)^2)

  # MSE SARIMA(2,0,1)(0,1,3)
in_samp_crit[4,1] <- mean((sarima201013$residuals)^2)

  # MSE SARIMA(1,0,0)(2,1,2)
sarima100212 <- Arima(train_ts, order=c(1,0,0), seasonal=c(2,1,2))
in_samp_crit[5,1] <- mean((sarima100212$residuals)^2)

  # MSE ANA
in_samp_crit[6,1] <- ANA.ets$mse

  # MSE AAdA
in_samp_crit[7,1] <- AAdA.ets$mse

# --> SARIMA(1,0,0)(2,1,2) outperforms the rest on the training data

  # MAE ARIMA(2,1,0)
in_samp_crit[1,2] <- mean(abs(arima210$residuals))

  # MAE ARIMA(2,1,3)
in_samp_crit[2,2] <- mean(abs(arima213$residuals))

  # MAE SARIMA(2,0,0)(0,1,3)
in_samp_crit[3,2] <- mean(abs(sarima200013$residuals))

  # MAE SARIMA(2,0,1)(0,1,3)
in_samp_crit[4,2] <- mean(abs(sarima201013$residuals))

  # MAE SARIMA(1,0,0)(2,1,2)
in_samp_crit[5,2] <- mean(abs(sarima100212$residuals))

  # MAE ANA
in_samp_crit[6,2] <- mean(abs(ANA.ets$residuals))

  # MAE AAdA
in_samp_crit[7,2] <- mean(abs(AAdA.ets$residuals))


################### Out-of-Sample Comparison ###################################
# Test time series
test_ts <- ts(citibike.df$demand[!jan_apr_vec])

# number of days in the test data:
no_test_days <- length(test_ts)/24

    # Rolling window approach 

# We store the point estimates:
  rw_hourly_forecast_ARIMA210 <- matrix(data=NA, nrow=0, ncol=6)
  rw_hourly_forecast_ARIMA213 <- matrix(data=NA, nrow=0, ncol=6)
  rw_hourly_forecast_SARIMA200013 <- matrix(data=NA, nrow=0, ncol=6)
  rw_hourly_forecast_SARIMA201013 <- matrix(data=NA, nrow=0, ncol=6)
  rw_hourly_forecast_SARIMA100212 <- matrix(data=NA, nrow=0, ncol=6)
  rw_hourly_forecast_ANA <- matrix(data=NA, nrow=0, ncol=6)
  rw_hourly_forecast_AAdA <- matrix(data=NA, nrow=0, ncol=6)
  colnames(rw_hourly_forecast_ANA) <- colnames(rw_hourly_forecast_AAdA)  <- c("True value", "Point Estimate", "Lo 80", "Hi 80", "Lo 95", "Hi 95")
  colnames(rw_hourly_forecast_ARIMA210) <- colnames(rw_hourly_forecast_ARIMA213) <- c("True value", "Point Estimate", "Lo 80", "Hi 80", "Lo 95", "Hi 95")
  colnames(rw_hourly_forecast_SARIMA200013) <- colnames(rw_hourly_forecast_SARIMA201013) <- colnames(rw_hourly_forecast_SARIMA100212) <- c("True value", "Point Estimate", "Lo 80", "Hi 80", "Lo 95", "Hi 95")
  
  
  for(i in 0:(no_test_days-1)){
    train <- ts(full_ts[(24*i+1):(length(full_ts)-24*(no_test_days-i))], frequency = 24)
    test <- ts(full_ts[(length(full_ts)-24*(no_test_days-i)+1):length(full_ts)], frequency = 24)
    
    # ARIMA(2,1,0)
    arima210_i <- Arima(train, order=c(2,1,0), seasonal=c(0,0,0))
    forecast_arima210_i <- forecast(train, model=arima210_i, h=24) 
    rw_hourly_forecast_ARIMA210 <- rbind(rw_hourly_forecast_ARIMA210, 
                                    cbind(test[1:24], forecast_arima210_i$mean, forecast_arima210_i$lower[,1],
                                          forecast_arima210_i$upper[,1], forecast_arima210_i$lower[,2],
                                          forecast_arima210_i$upper[,2]))
    
    # ARIMA(2,1,3)
    arima213_i <- Arima(train, order=c(2,1,3), seasonal=c(0,0,0))
    forecast_arima213_i <- forecast(train, model=arima213_i, h=24) 
    rw_hourly_forecast_ARIMA213 <- rbind(rw_hourly_forecast_ARIMA213, 
                                         cbind(test[1:24], forecast_arima213_i$mean, forecast_arima213_i$lower[,1],
                                               forecast_arima213_i$upper[,1], forecast_arima213_i$lower[,2],
                                               forecast_arima213_i$upper[,2]))
    
    #SARIMA(2,0,0)(0,1,3)
    sarima200013_i <- Arima(train, order=c(2,0,0), seasonal=c(0,1,3))
    forecast_sarima200013_i <- forecast(train, model=sarima200013_i, h=24) 
    rw_hourly_forecast_SARIMA200013 <- rbind(rw_hourly_forecast_SARIMA200013, 
                                         cbind(test[1:24], forecast_sarima200013_i$mean, forecast_sarima200013_i$lower[,1],
                                               forecast_sarima200013_i$upper[,1], forecast_sarima200013_i$lower[,2],
                                               forecast_sarima200013_i$upper[,2]))
    
    # SARIMA(2,0,1)(0,1,3)
    sarima201013_i <- Arima(train, order=c(2,0,1), seasonal=c(0,1,3))
    forecast_sarima201013_i <- forecast(train, model=sarima201013_i, h=24) 
    rw_hourly_forecast_SARIMA201013 <- rbind(rw_hourly_forecast_SARIMA201013, 
                                             cbind(test[1:24], forecast_sarima201013_i$mean, forecast_sarima201013_i$lower[,1],
                                                   forecast_sarima201013_i$upper[,1], forecast_sarima201013_i$lower[,2],
                                                   forecast_sarima201013_i$upper[,2]))
    
    # SARIMA(1,0,0)(2,1,2)
    sarima100212_i <- Arima(train, order=c(1,0,0), seasonal=c(2,1,2))
    forecast_sarima100212_i <- forecast(train, model=sarima100212_i, h=24) 
    rw_hourly_forecast_SARIMA100212 <- rbind(rw_hourly_forecast_SARIMA100212, 
                                             cbind(test[1:24], forecast_sarima100212_i$mean, forecast_sarima100212_i$lower[,1],
                                                   forecast_sarima100212_i$upper[,1], forecast_sarima100212_i$lower[,2],
                                                   forecast_sarima100212_i$upper[,2]))
    
    # (A, N, A)
    ANA_i <- ets(train, model="ANA", damped=FALSE)
    forecast_ANA <- forecast(ANA_i, h=24, model="ets") 
    rw_hourly_forecast_ANA <- rbind(rw_hourly_forecast_ANA, 
                                 cbind(test[1:24], forecast_ANA$mean, forecast_ANA$lower[,1],
                                       forecast_ANA$upper[,1], forecast_ANA$lower[,2],
                                       forecast_ANA$upper[,2]))
    
    # (A, A_d, A)
    AAdA_i <- ets(train, model="AAA", damped=TRUE)
    forecast_AAdA <- forecast(AAdA_i, h=24) 
    rw_hourly_forecast_AAdA <- rbind(rw_hourly_forecast_AAdA, 
                                  cbind(test[1:24], forecast_AAdA$mean, forecast_AAdA$lower[,1],
                                        forecast_AAdA$upper[,1], forecast_AAdA$lower[,2],
                                        forecast_AAdA$upper[,2]))
  }

# Rolling Window Forecast Error Metrics:
  rw_test_errors <- matrix(NA, nrow=7, ncol=4)
  rownames(rw_test_errors) <- c("AAdA", "ANA", "ARIMA(2,1,0)", "ARIMA(2,1,3)", 
                             "SARIMA(1,0,0)(2,1,2)", "SARIMA(2,0,0)(0,1,3)",
                             "SARIMA(2,0,1)(0,1,3)")
  colnames(rw_test_errors) <- c("MSE", "RMSE", "MAE", "MRAE")
  
  # Residuals:
    rw_residuals_AAdA <- rw_hourly_forecast_AAdA[,1]-rw_hourly_forecast_AAdA[,2]
    rw_residuals_ANA <- rw_hourly_forecast_ANA[,1]-rw_hourly_forecast_ANA[,2]
    rw_residuals_ARIMA210 <- rw_hourly_forecast_ARIMA210[,1]-rw_hourly_forecast_ARIMA210[,2]
    rw_residuals_ARIMA213 <- rw_hourly_forecast_ARIMA213[,1]-rw_hourly_forecast_ARIMA213[,2]
    rw_residuals_SARIMA100212 <- rw_hourly_forecast_SARIMA100212[,1]-rw_hourly_forecast_SARIMA100212[,2]
    rw_residuals_SARIMA200013 <- rw_hourly_forecast_SARIMA200013[,1]-rw_hourly_forecast_SARIMA200013[,2]
    rw_residuals_SARIMA201013 <- rw_hourly_forecast_SARIMA201013[,1]-rw_hourly_forecast_SARIMA201013[,2]
    
  # MSE:
    rw_rest_MSE_AAdA <- mean((rw_residuals_AAdA)^2)
    rw_test_errors[1,1] <- rw_rest_MSE_AAdA
    
    rw_rest_MSE_ANA <- mean((rw_residuals_ANA)^2)
    rw_test_errors[2,1] <- rw_rest_MSE_ANA
    
    rw_rest_MSE_ARIMA210 <- mean((rw_residuals_ARIMA210)^2)
    rw_test_errors[3,1] <- rw_rest_MSE_ARIMA210
    
    rw_rest_MSE_ARIMA213 <- mean((rw_residuals_ARIMA213)^2)
    rw_test_errors[4,1] <- rw_rest_MSE_ARIMA213
    
    rw_rest_MSE_SARIMA100212 <- mean((rw_residuals_SARIMA100212)^2)
    rw_test_errors[5,1] <- rw_rest_MSE_SARIMA100212
    
    rw_rest_MSE_SARIMA200013 <- mean((rw_residuals_SARIMA200013)^2)
    rw_test_errors[6,1] <- rw_rest_MSE_SARIMA200013
    
    rw_rest_MSE_SARIMA201013 <- mean((rw_residuals_SARIMA201013)^2)
    rw_test_errors[7,1] <- rw_rest_MSE_SARIMA201013
    
  # RMSE
    rw_test_errors[,2] <- sqrt(rw_test_errors[,1])
  
  # MAE
    rw_rest_MAE_AAdA <- mean(abs(rw_residuals_AAdA))
    rw_test_errors[1,3] <- rw_rest_MAE_AAdA
    
    rw_rest_MAE_ANA <- mean(abs(rw_residuals_ANA))
    rw_test_errors[2,3] <- rw_rest_MAE_ANA
    
    rw_rest_MAE_ARIMA210 <- mean(abs(rw_residuals_ARIMA210))
    rw_test_errors[3,3] <- rw_rest_MAE_ARIMA210
    
    rw_rest_MAE_ARIMA213 <- mean(abs(rw_residuals_ARIMA213))
    rw_test_errors[4,3] <- rw_rest_MAE_ARIMA213
    
    rw_rest_MAE_SARIMA100212 <- mean(abs(rw_residuals_SARIMA100212))
    rw_test_errors[5,3] <- rw_rest_MAE_SARIMA100212
    
    rw_rest_MAE_SARIMA200013 <- mean(abs(rw_residuals_SARIMA200013))
    rw_test_errors[6,3] <- rw_rest_MAE_SARIMA200013
    
    rw_rest_MAE_SARIMA201013 <- mean(abs(rw_residuals_SARIMA201013))
    rw_test_errors[7,3] <- rw_rest_MAE_SARIMA201013
  
  # MRAE (benchmark method: SARIMA(2,0,0)(0,1,3))
    rw_test_errors[1,4] <- mean(abs(rw_residuals_AAdA/rw_residuals_SARIMA200013))
    rw_test_errors[2,4] <- mean(abs(rw_residuals_ANA/rw_residuals_SARIMA200013))
    rw_test_errors[3,4] <- mean(abs(rw_residuals_ARIMA210/rw_residuals_SARIMA200013))
    rw_test_errors[4,4] <- mean(abs(rw_residuals_ARIMA213/rw_residuals_SARIMA200013))
    rw_test_errors[5,4] <- mean(abs(rw_residuals_SARIMA100212/rw_residuals_SARIMA200013))
    rw_test_errors[6,4] <- mean(abs(rw_residuals_SARIMA200013/rw_residuals_SARIMA200013))
    rw_test_errors[7,4] <- mean(abs(rw_residuals_SARIMA201013/rw_residuals_SARIMA200013))
    
  
# Plotting Rolling Window Forecasts:
  sub_start <- nrow(citibike) -sum(1*(citibike[,2] %in% c(5)))-240
  sub_end <- nrow(citibike)
  start_may <- 2881
  
  
  # We start by creating a blank plot:
  plot(1, type = "l", col = "darkgreen", xlim=c(sub_start, sub_end), 
       ylim=c(min(rw_hourly_forecast_ANA[,5]), max(rw_hourly_forecast_ANA[,6])),
       ylab="Hourly Demand", xlab="hour (by index) in the dataset", 
       main = "Forecasted Hourly demand for May (A, N, A)")
  
  # The 95% confidence set:
  polygon(c(start_may:sub_end, rev(start_may:sub_end)), 
          c(rw_hourly_forecast_ANA[1:nrow(rw_hourly_forecast_ANA),5], 
            rev(rw_hourly_forecast_ANA[1:nrow(rw_hourly_forecast_ANA),6])),
          col = "#006666", border="#006666")
  
  # The 80% confidence set:
  polygon(c(start_may:sub_end, rev(start_may:sub_end)), 
          c(rw_hourly_forecast_ANA[1:nrow(rw_hourly_forecast_ANA),3], 
            rev(rw_hourly_forecast_ANA[1:nrow(rw_hourly_forecast_ANA),4])),
          col = "#B2DFDB", border="#B2DFDB")
  #B2DFDB
  lines(start_may:sub_end, rw_hourly_forecast_ANA[1:nrow(rw_hourly_forecast_ANA), 2],
        col="#990066", lwd=1.5)
  
  
  lines(sub_start:start_may, citibike[sub_start:start_may, 6], col="black", lwd=1)


#Expanding window approach 
ew_hourly_forecast_ARIMA210 <- matrix(data=NA, nrow=0, ncol=6)
ew_hourly_forecast_ARIMA213 <- matrix(data=NA, nrow=0, ncol=6)
ew_hourly_forecast_SARIMA200013 <- matrix(data=NA, nrow=0, ncol=6)
ew_hourly_forecast_SARIMA201013 <- matrix(data=NA, nrow=0, ncol=6)
ew_hourly_forecast_SARIMA100212 <- matrix(data=NA, nrow=0, ncol=6)
ew_hourly_forecast_ANA <- matrix(data=NA, nrow=0, ncol=6)
ew_hourly_forecast_AAdA <- matrix(data=NA, nrow=0, ncol=6)
colnames(ew_hourly_forecast_ANA) <- colnames(ew_hourly_forecast_AAdA)  <- c("True value", "Point Estimate", "Lo 80", "Hi 80", "Lo 95", "Hi 95")
colnames(ew_hourly_forecast_ARIMA210) <- colnames(ew_hourly_forecast_ARIMA213) <- c("True value", "Point Estimate", "Lo 80", "Hi 80", "Lo 95", "Hi 95")
colnames(ew_hourly_forecast_SARIMA200013) <- colnames(ew_hourly_forecast_SARIMA201013) <- colnames(ew_hourly_forecast_SARIMA100212) <- c("True value", "Point Estimate", "Lo 80", "Hi 80", "Lo 95", "Hi 95")


for(i in 0:(no_test_days-1)){
  train <- ts(full_ts[1:(length(full_ts)-24*(no_test_days-i))], frequency = 24)
  test <- ts(full_ts[(length(full_ts)-24*(no_test_days-i)+1):length(full_ts)], frequency = 24)
  
  # ARIMA(2,1,0)
  arima210_i <- Arima(train, order=c(2,1,0), seasonal=c(0,0,0))
  forecast_arima210_i <- forecast(train, model=arima210_i, h=24) 
  ew_hourly_forecast_ARIMA210 <- rbind(ew_hourly_forecast_ARIMA210, 
                                       cbind(test[1:24], forecast_arima210_i$mean, forecast_arima210_i$lower[,1],
                                             forecast_arima210_i$upper[,1], forecast_arima210_i$lower[,2],
                                             forecast_arima210_i$upper[,2]))
  
  # ARIMA(2,1,3)
  arima213_i <- Arima(train, order=c(2,1,3), seasonal=c(0,0,0))
  forecast_arima213_i <- forecast(train, model=arima213_i, h=24) 
  ew_hourly_forecast_ARIMA213 <- rbind(ew_hourly_forecast_ARIMA213, 
                                       cbind(test[1:24], forecast_arima213_i$mean, forecast_arima213_i$lower[,1],
                                             forecast_arima213_i$upper[,1], forecast_arima213_i$lower[,2],
                                             forecast_arima213_i$upper[,2]))
  
  #SARIMA(2,0,0)(0,1,3)
  sarima200013_i <- Arima(train, order=c(2,0,0), seasonal=c(0,1,3))
  forecast_sarima200013_i <- forecast(train, model=sarima200013_i, h=24) 
  ew_hourly_forecast_SARIMA200013 <- rbind(ew_hourly_forecast_SARIMA200013, 
                                           cbind(test[1:24], forecast_sarima200013_i$mean, forecast_sarima200013_i$lower[,1],
                                                 forecast_sarima200013_i$upper[,1], forecast_sarima200013_i$lower[,2],
                                                 forecast_sarima200013_i$upper[,2]))
  
  # SARIMA(2,0,1)(0,1,3)
  sarima201013_i <- Arima(train, order=c(2,0,1), seasonal=c(0,1,3))
  forecast_sarima201013_i <- forecast(train, model=sarima201013_i, h=24) 
  ew_hourly_forecast_SARIMA201013 <- rbind(ew_hourly_forecast_SARIMA201013, 
                                           cbind(test[1:24], forecast_sarima201013_i$mean, forecast_sarima201013_i$lower[,1],
                                                 forecast_sarima201013_i$upper[,1], forecast_sarima201013_i$lower[,2],
                                                 forecast_sarima201013_i$upper[,2]))
  
  # SARIMA(1,0,0)(2,1,2)
  sarima100212_i <- Arima(train, order=c(1,0,0), seasonal=c(2,1,2))
  forecast_sarima100212_i <- forecast(train, model=sarima100212_i, h=24) 
  ew_hourly_forecast_SARIMA100212 <- rbind(ew_hourly_forecast_SARIMA100212, 
                                           cbind(test[1:24], forecast_sarima100212_i$mean, forecast_sarima100212_i$lower[,1],
                                                 forecast_sarima100212_i$upper[,1], forecast_sarima100212_i$lower[,2],
                                                 forecast_sarima100212_i$upper[,2]))
  
  # (A, N, A)
  ANA_i <- ets(train, model="ANA", damped=FALSE)
  forecast_ANA <- forecast(ANA_i, h=24, model="ets") 
  ew_hourly_forecast_ANA <- rbind(ew_hourly_forecast_ANA, 
                                  cbind(test[1:24], forecast_ANA$mean, forecast_ANA$lower[,1],
                                        forecast_ANA$upper[,1], forecast_ANA$lower[,2],
                                        forecast_ANA$upper[,2]))
  
  # (A, A_d, A)
  AAdA_i <- ets(train, model="AAA", damped=TRUE)
  forecast_AAdA <- forecast(AAdA_i, h=24) 
  ew_hourly_forecast_AAdA <- rbind(ew_hourly_forecast_AAdA, 
                                   cbind(test[1:24], forecast_AAdA$mean, forecast_AAdA$lower[,1],
                                         forecast_AAdA$upper[,1], forecast_AAdA$lower[,2],
                                         forecast_AAdA$upper[,2]))
}


# Rolling Window Forecast Error Metrics:
ew_test_errors <- matrix(NA, nrow=7, ncol=4)
rownames(ew_test_errors) <- c("AAdA", "ANA", "ARIMA(2,1,0)", "ARIMA(2,1,3)", 
                              "SARIMA(1,0,0)(2,1,2)", "SARIMA(2,0,0)(0,1,3)",
                              "SARIMA(2,0,1)(0,1,3)")
colnames(ew_test_errors) <- c("MSE", "RMSE", "MAE", "MRAE")

# Residuals:
ew_residuals_AAdA <- ew_hourly_forecast_AAdA[,1]-ew_hourly_forecast_AAdA[,2]
ew_residuals_ANA <- ew_hourly_forecast_ANA[,1]-ew_hourly_forecast_ANA[,2]
ew_residuals_ARIMA210 <- ew_hourly_forecast_ARIMA210[,1]-ew_hourly_forecast_ARIMA210[,2]
ew_residuals_ARIMA213 <- ew_hourly_forecast_ARIMA213[,1]-ew_hourly_forecast_ARIMA213[,2]
ew_residuals_SARIMA100212 <- ew_hourly_forecast_SARIMA100212[,1]-ew_hourly_forecast_SARIMA100212[,2]
ew_residuals_SARIMA200013 <- ew_hourly_forecast_SARIMA200013[,1]-ew_hourly_forecast_SARIMA200013[,2]
ew_residuals_SARIMA201013 <- ew_hourly_forecast_SARIMA201013[,1]-ew_hourly_forecast_SARIMA201013[,2]

# MSE:
ew_rest_MSE_AAdA <- mean((ew_residuals_AAdA)^2)
ew_test_errors[1,1] <- ew_rest_MSE_AAdA

ew_rest_MSE_ANA <- mean((ew_residuals_ANA)^2)
ew_test_errors[2,1] <- ew_rest_MSE_ANA

ew_rest_MSE_ARIMA210 <- mean((ew_residuals_ARIMA210)^2)
ew_test_errors[3,1] <- ew_rest_MSE_ARIMA210

ew_rest_MSE_ARIMA213 <- mean((ew_residuals_ARIMA213)^2)
ew_test_errors[4,1] <- ew_rest_MSE_ARIMA213

ew_rest_MSE_SARIMA100212 <- mean((ew_residuals_SARIMA100212)^2)
ew_test_errors[5,1] <- ew_rest_MSE_SARIMA100212

ew_rest_MSE_SARIMA200013 <- mean((ew_residuals_SARIMA200013)^2)
ew_test_errors[6,1] <- ew_rest_MSE_SARIMA200013

ew_rest_MSE_SARIMA201013 <- mean((ew_residuals_SARIMA201013)^2)
ew_test_errors[7,1] <- ew_rest_MSE_SARIMA201013

# RMSE
ew_test_errors[,2] <- sqrt(ew_test_errors[,1])

# MAE
ew_rest_MAE_AAdA <- mean(abs(ew_residuals_AAdA))
ew_test_errors[1,3] <- ew_rest_MAE_AAdA

ew_rest_MAE_ANA <- mean(abs(ew_residuals_ANA))
ew_test_errors[2,3] <- ew_rest_MAE_ANA

ew_rest_MAE_ARIMA210 <- mean(abs(ew_residuals_ARIMA210))
ew_test_errors[3,3] <- ew_rest_MAE_ARIMA210

ew_rest_MAE_ARIMA213 <- mean(abs(ew_residuals_ARIMA213))
ew_test_errors[4,3] <- ew_rest_MAE_ARIMA213

ew_rest_MAE_SARIMA100212 <- mean(abs(ew_residuals_SARIMA100212))
ew_test_errors[5,3] <- ew_rest_MAE_SARIMA100212

ew_rest_MAE_SARIMA200013 <- mean(abs(ew_residuals_SARIMA200013))
ew_test_errors[6,3] <- ew_rest_MAE_SARIMA200013

ew_rest_MAE_SARIMA201013 <- mean(abs(ew_residuals_SARIMA201013))
ew_test_errors[7,3] <- ew_rest_MAE_SARIMA201013

# MRAE (benchmark method: SARIMA(2,0,0)(0,1,3))
ew_test_errors[1,4] <- mean(abs(ew_residuals_AAdA/ew_residuals_SARIMA200013))
ew_test_errors[2,4] <- mean(abs(ew_residuals_ANA/ew_residuals_SARIMA200013))
ew_test_errors[3,4] <- mean(abs(ew_residuals_ARIMA210/ew_residuals_SARIMA200013))
ew_test_errors[4,4] <- mean(abs(ew_residuals_ARIMA213/ew_residuals_SARIMA200013))
ew_test_errors[5,4] <- mean(abs(ew_residuals_SARIMA100212/ew_residuals_SARIMA200013))
ew_test_errors[6,4] <- mean(abs(ew_residuals_SARIMA200013/ew_residuals_SARIMA200013))
ew_test_errors[7,4] <- mean(abs(ew_residuals_SARIMA201013/ew_residuals_SARIMA200013))


############## Assessing Significance of Forecast Results ######################
  # Creating the Loss Matrix:
loss_matrix <- matrix(NA, nrow = 744, ncol = 14, 
                      dimnames = list(c(1:744), 
                                      c("rw_AAdA", "rw_ANA", "rw_ARIMA210", "rw_ARIMA213", "rw_SARIMA100212", "rw_SARIMA200013", "rw_SARIMA201013",
                                        "ew_AAdA", "ew_ANA", "ew_ARIMA210", "ew_ARIMA213", "ew_SARIMA100212", "ew_SARIMA200013", "ew_SARIMA201013")))

loss_matrix[,1] <- rw_residuals_AAdA
loss_matrix[,2] <- rw_residuals_ANA 
loss_matrix[,3] <- rw_residuals_ARIMA210
loss_matrix[,4] <- rw_residuals_ARIMA213
loss_matrix[,5] <- rw_residuals_SARIMA100212
loss_matrix[,6] <- rw_residuals_SARIMA200013 
loss_matrix[,7] <- rw_residuals_SARIMA201013
loss_matrix[,8] <- ew_residuals_AAdA
loss_matrix[,9] <- ew_residuals_ANA
loss_matrix[,10] <- ew_residuals_ARIMA210
loss_matrix[,11] <- ew_residuals_ARIMA213
loss_matrix[,12] <- ew_residuals_SARIMA100212
loss_matrix[,13] <- ew_residuals_SARIMA200013
loss_matrix[,14] <- ew_residuals_SARIMA201013

absolute_loss_matrix <- abs(loss_matrix)
squared_loss_matrix <- (loss_matrix)^2

# Model Confidence Sets based on the absolute and squared loss matrix:
mcs_absolute <- MCSprocedure(Loss = absolute_loss_matrix, alpha = 0.1, B = 5000, 
                             statistic = "Tmax")

mcs_squared <- MCSprocedure(Loss = squared_loss_matrix, alpha = 0.1, B = 5000, 
                            statistic = "Tmax")


  # Diebold-Mariano Test:

#Test: H0 = two methods have same forecasting accuracy, H1_greater = method 2 is more accurate than method 1

#Rolling window 
rw_dm_ARIMA213_SARIMA200013 <- dm.test(rw_residuals_ARIMA213, rw_residuals_SARIMA200013, h=24, alternative="greater")
rw_dm_ARIMA213_ETSANA <- dm.test(rw_residuals_ARIMA213, rw_residuals_ANA, h=24, alternative="greater")
rw_dm_ETSANA_SARIMA200013 <- dm.test(rw_residuals_ANA, rw_residuals_SARIMA200013, h=24, alternative="greater")

#Expanding window
ew_dm_ARIMA213_SARIMA200013 <- dm.test(ew_residuals_ARIMA213, ew_residuals_SARIMA200013, h=24, alternative="greater")
ew_dm_ARIMA213_ETSANA <- dm.test(ew_residuals_ARIMA213, ew_residuals_ANA, h=24, alternative="greater")
ew_dm_ETSANA_SARIMA200013 <- dm.test(ew_residuals_ANA, ew_residuals_SARIMA200013, h=24, alternative="greater")

print(rw_dm_ARIMA213_SARIMA200013) #SARIMA20013 is more accurate than ARIMA213
print(rw_dm_ARIMA213_ETSANA) #ANA is more accurate than ARIMA213
print(rw_dm_ETSANA_SARIMA200013) #SARIMA200013 is more accurate than ANA
print(ew_dm_ARIMA213_SARIMA200013) #SARIMA20013 is more accurate than ARIMA213
print(ew_dm_ARIMA213_ETSANA) #ANA is more accurate than ARIMA213
print(ew_dm_ETSANA_SARIMA200013) #SARIMA200013 is more accurate than ANA


dm.test(ew_residuals_SARIMA200013, rw_residuals_SARIMA200013, h=24, alternative="greater")
dm.test(ew_residuals_ARIMA213, rw_residuals_ARIMA213, h=24, alternative="greater") 
dm.test(ew_residuals_ANA, rw_residuals_ANA, h=24, alternative="greater")

#Conclusion: exponential smoothing methods are sensitive to forecasting approach. 








