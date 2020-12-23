## USE FORECAST LIBRARY.

library(forecast)

## CREATE DATA FRAME. 

# Set working directory for locating files.
setwd("C:/Apoorva/Masters - Business Analytics/Masters MSBA/4th SEMESTER/BAN 673_Time Series Analytics/Case Studies/case study #2")

# Create data frame.
Revenue.data <- read.csv("case2(1).csv")

# See the first 6 records of the file.
head(Revenue.data)

#Create time series data set in R using the ts() function.

# Function ts() takes three arguments: start, end, and freq.
# With quarterly data, frequency in a season (year) is equal to 4.
# Arguments start and end are pairs: (season number, period number).
revenue.ts <- ts(Revenue.data$Revenue, 
                 start = c(2005, 1), end = c(2020, 2), freq = 4)

# Use Acf() function to identify autocorrelation and plot autocorrelation
# for different lags (up to maximum of 12).
Acf(revenue.ts, lag.max = 12, main = "Autocorrelation for Walmart Revenue")

# Develop data partition with the validation partition of 16 periods and the rest for the 
#   training partition.

nValid <- 16 
nTrain <- length(revenue.ts) - nValid
train.ts <- window(revenue.ts, start = c(2005, 1), end = c(2005, nTrain))
valid.ts <- window(revenue.ts, start = c(2005, nTrain + 1), 
                   end = c(2005, nTrain + nValid))

# Use Acf() function to identify autocorrelation for training and validation
# data sets, and plot autocorrelation for different lags (up to maximum of 12)
Acf(train.ts, lag.max = 12, main = "Autocorrelation for Walmart Revenue Data Set")
Acf(valid.ts, lag.max = 12, main = "Autocorrelation for Walmart Revenue Data Set")

## Questions

# 1. Identify time series predictability.

  # a. Using the AR(1) model for the historical data, Provide and explain the AR(1) model summary in your report. Explain if the Walmart revenue is predictable.

    revenue.ar1 <- Arima(revenue.ts, order = c(1,0,0))
    summary(revenue.ar1)
    
   # b. Using the first differencing (lag-1) of the historical data and Acf() function Provide in the report the autocorrelation plot of the first differencing (lag-1) with the maximum of 8 lags and explain if Walmart revenue is predictable.
    
    # Create differenced ClosePrice data using (lag-1).
    diff.close.price <- diff(revenue.ts, lag = 1)
    diff.close.price
    
    # Use Acf() function to identify autocorrealtion for differenced
    # ClosePrices and plot autocorrelation for different lags 
    # (up to maximum of 12).
    Acf(diff.close.price, lag.max = 8, 
        main = "Autocorrelation for Walmart Revenue of the first differencing")


# 2. Apply the two-level forecast with regression model and AR model for residuals.

  # a. For the training data set, use the tslm() function to develop a regression model with quadratic trend and seasonality. Forecast Walmart's revenue with the forecast() function (use the associated R code from case #2). No explanation is required in your report.

    # Use tslm() function to create quadratic trend and seasonal model.
    train.trend.season <- tslm(train.ts ~ trend + I(trend^2) + season)  
    # See summary of linear trend equation and associated parameters.
    summary(train.trend.season)
    
    # Apply forecast() function to make predictions for ts with 
    # trend and seasonal model in validation set.  
    train.trend.season.pred <- forecast(train.trend.season, h = nValid, level = 0)
    train.trend.season.pred


  # b. Identify the regression model's residuals for the training period and use the Acf() function with the maximum of 8 lags to identify autocorrelation for these residuals. Provide the autocorrelation plot in your report and explain if it would be a good idea to add to your forecast an AR model for residuals.
    
    # Identify the regression model's residuals
    train.trend.season.pred$residuals
    
    # Use Acf() function to identify autocorrelation for the model residuals 
    # (training sets), and plot autocorrelation for different 
    # lags (up to maximum of 8).
    Acf(train.trend.season.pred$residuals, lag.max = 8, 
        main = "Autocorrelation for Walmart Revenue Residuals")
    
    
  # c. Develop an AR(1) model for the regression residuals, present and explain the model and its equation in your report. Use the Acf() function for the residuals of the AR(1) model (residuals of residuals), present the autocorrelation chart, and explain it in your report.

    ## USE Arima() FUNCTION TO CREATE AR(1) MODEL FOR TRAINING RESIDUALS.
    ## CREATE TWO-LEVEL MODEL WITH QUADRATIC TREND AND SEASONALITY MODEL 
    ## AND AR(1) RESIDUALS.
    ## PLOT DATA AND IDENTIFY ACCURACY MEASURES.
    
    # Use Arima() function to fit AR(1) model for training residuals. The Arima model of 
    # order = c(1,0,0) gives an AR(1) model.
    # Use summary() to identify parameters of AR(1) model. 
    res.ar1 <- Arima(train.trend.season$residuals, order = c(1,0,0))
    summary(res.ar1)
    
    # Use forecast() function to make prediction of residuals in validation set.
    res.ar1.pred <- forecast(res.ar1, h = nValid, level = 0)
    res.ar1.pred
    
    # Use Acf() function to identify autocorrelation for the training 
    # residual of residuals and plot autocorrelation for different lags 
    # (up to maximum of 8).
    Acf(res.ar1$residuals, lag.max = 8, 
        main = "Autocorrelation for Walmart Revenue Training Residuals of Residuals")
    
    
  # d. Create a two-level forecasting model (regression model with quadratic trend and seasonality + AR(1) model for residuals) for the validation period. Show in your report a table with the validation data, regression forecast for the validation data, AR() forecast for the validation data, and combined forecast for the validation period.

    # Create two-level model's forecast with quadratic trend and seasonality 
    # regression + AR(1) for residuals for validation period.
    
    valid.two.level.pred <- train.trend.season.pred$mean + res.ar1.pred$mean
    
    # Create data table with validation data, regression forecast
    # for validation period, AR(1) residuals for validation, and 
    # two level model results. 
    
    valid.df <- data.frame(valid.ts, train.trend.season.pred$mean, 
                           res.ar1.pred$mean, valid.two.level.pred)
    names(valid.df) <- c("Walmart Revenue", "Reg.Forecast", 
                         "AR(1)Forecast", "Combined.Forecast")
    valid.df
    
  # e. Develop a two-level forecast (regression model with quadratic trend and seasonality and AR(1) model for residuals) for the entire data set. Provide in your report the autocorrelation chart for the AR(1) model's residuals and explain it. Also, provide a data table with the models' forecasts for Walmart revenue in 2020-2021 (regression model, AR(1) for residuals, and two-level combined forecast).
    
    ## FIT REGRESSION MODEL WITH QUADRATIC TREND AND SEASONALITY FOR ENTIRE DATASET
    
    # Use tslm() function to create quadratic trend and seasonality model.
    trend.season <- tslm(revenue.ts ~ trend + I(trend^2) + season)
    
    # See summary of linear trend equation and associated parameters.
    summary(trend.season)
    
    # Apply forecast() function to make predictions with quadratic trend and seasonal 
    # model into the future 12 months.  
    trend.season.pred <- forecast(trend.season, h = 4, level = 0)
    trend.season.pred
    
    # Use Arima() function to fit AR(1) model for regression residuals.
    # The ARIMA model order of order = c(1,0,0) gives an AR(1) model.
    # Use forecast() function to make prediction of residuals into the future.
    residual.ar1 <- Arima(trend.season$residuals, order = c(1,0,0))
    residual.ar1.pred <- forecast(residual.ar1, h = 4, level = 0)
    
    # Use summary() to identify parameters of AR(1) model.
    summary(residual.ar1)
    
    # Use Acf() function to identify autocorrealtion for the residual of residuals 
    # and plot autocorrelation for different lags (up to maximum of 8).
    Acf(residual.ar1$residuals, lag.max = 8, 
        main = "Autocorrelation for Walmart Revenue Residuals of Residuals for Entire Data Set")
    
    # Identify forecast for the future sum of quadratic trend and seasonal model
    # and AR(1) model for residuals.
    trend.season.ar1.pred <- trend.season.pred$mean + residual.ar1.pred$mean
    trend.season.ar1.pred
    
    # Create a data table with quadratic trend and seasonal forecast for future periods,
    # AR(1) model for residuals for future periods, and combined two-level forecast for
    # future periods. 
    table.df <- data.frame(trend.season.pred$mean, 
                           residual.ar1.pred$mean, trend.season.ar1.pred)
    names(table.df) <- c("Reg.Forecast", "AR(1)Forecast","Combined.Forecast")
    table.df
    
# 3. Use ARIMA Model and Compare Various Methods.
   # a. Use Arima() function to fit ARIMA(1,1,1)(1,1,1) model for the training data set. Insert in your report the summary of this ARIMA model, present and briefly explain the ARIMA model and its equation in your report. Using this model, forecast revenue for the validation period and present it in your report.
   
    ## FIT ARIMA(1,1,1)(1,1,1) MODEL.
    
    # Use Arima() function to fit ARIMA(1,1,1)(1,1,1) model for 
    # trend and seasonality.
    # Use summary() to show ARIMA model and its parameters.
    train.arima.seas <- Arima(train.ts, order = c(1,1,1), 
                              seasonal = c(1,1,1)) 
    summary(train.arima.seas)
    
    # Apply forecast() function to make predictions for ts with 
    # ARIMA model in validation set.    
    train.arima.seas.pred <- forecast(train.arima.seas, h = nValid, level = 0)
    train.arima.seas.pred
    
   # b. Use the auto.arima() function to develop an ARIMA model using the training data set. Insert in your report the summary of this ARIMA model, present and explain the ARIMA model and its equation in your report. Use this model to forecast revenue in the validation period and present this forecast in your report.
  
    ## FIT AUTO ARIMA MODEL.
    
    # Use auto.arima() function to fit ARIMA model.
    # Use summary() to show auto ARIMA model and its parameters.
    train.auto.arima <- auto.arima(train.ts)
    summary(train.auto.arima)
    
    # Apply forecast() function to make predictions for ts with 
    # auto ARIMA model in validation set.  
    train.auto.arima.pred <- forecast(train.auto.arima, h = nValid, level = 0)
    train.auto.arima.pred
    
    # c. Apply the accuracy() function to compare performance measures of the two ARIMA models in 3a and 3b. Present the accuracy measures in your report, compare them and identify, using MAPE and RMSE, the best ARIMA model to apply.
   
    # Use accuracy() function to identify common accuracy measures 
    # for validation period forecast:
    round(accuracy(train.arima.seas.pred, valid.ts), 3)
    round(accuracy(train.auto.arima.pred, valid.ts), 3)
    
   # d. Use two ARIMA models from 3a and 3b for the entire data set. Present models' summaries in your report. Use these ARIMA models to forecast Walmart revenue in 2020-2021 and present these forecasts in your report.
  
    # Use arima() function to fit seasonal ARIMA(1,1,1)(1,1,1) model 
    # for entire data set.
    # use summary() to show auto ARIMA model and its parameters for entire data set.
    arima.seas <- Arima(revenue.ts, order = c(1,1,1), 
                        seasonal = c(1,1,1)) 
    summary(arima.seas)
    
    # Apply forecast() function to make predictions for ts with 
    # seasonal ARIMA model for the future 4 periods. 
    arima.seas.pred <- forecast(arima.seas, h = 4, level = 0)
    arima.seas.pred
    
    # Use auto.arima() function to fit ARIMA model for entire data set.
    # use summary() to show auto ARIMA model and its parameters for entire data set.
    auto.arima <- auto.arima(revenue.ts)
    summary(auto.arima)
    
    # Apply forecast() function to make predictions for ts with 
    # auto ARIMA model for the future 4 periods. 
    auto.arima.pred <- forecast(auto.arima, h = 4, level = 0)
    auto.arima.pred
    
    
   # e. Apply the accuracy() function to compare performance measures of the following forecasting models for the entire data set: (1) regression model with quadratic trend and seasonality; (2) two-level model (with AR(1) model for residuals); (3) ARIMA(1,1,1)(1,1,1) model; (4) auto ARIMA model; and (5) seasonal naïve forecast for the entire data set. Present the accuracy measures in your report, compare them, and identify, using MAPE and RMSE, the best model to use for forecasting Walmart's revenue in quarters 3 and 4 of 2020 and quarters 1 and 2 of 2021.
    
    # 1. regression model with quadratic trend and seasonality
    round(accuracy(trend.season.pred$fitted, revenue.ts), 3)
    
    # 2. two-level model (with AR(1) model for residuals)
    round(accuracy(trend.season.pred$fitted + residual.ar1.pred$fitted, revenue.ts), 3)
    
    # 3. ARIMA(1,1,1)(1,1,1) model
    round(accuracy(arima.seas.pred$fitted, revenue.ts), 3)
    
    #4. auto ARIMA model
    round(accuracy(auto.arima.pred$fitted, revenue.ts), 3)
    
    #5. Seasonal Naive
    round(accuracy(snaive(revenue.ts)$fitted, revenue.ts), 3)
    