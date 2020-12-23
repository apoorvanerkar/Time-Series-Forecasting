# CASE STUDY #1

## USE FORECAST LIBRARY.

install.packages("forecast")
library(forecast)

## CREATE DATA FRAME. 

# Set working directory for locating files.
setwd("C:/Apoorva/Masters - Business Analytics/Masters MSBA/4th SEMESTER/BAN 673_Time Series Analytics/Case Studies/case study #1")

# Create data frame.
grocery.data <- read.csv("673_case1.csv")

# See the first 6 records of the file.
head(grocery.data)


### Questions

## 1. Identify time series components and plot the data.

  # a. Create time series data set in R using the ts() function.

    grocery.ts <- ts(grocery.data$Sales, 
                      start = c(2015, 1), end = c(2019, 12), freq = 12)
    grocery.ts

    
  # b. Employ the plot() function to create a data plot with the historical data, provide it in your report, and explain what data patterns can be visualized in this plot.

    ## Use plot() to plot time series data  
    plot(grocery.ts, 
         xlab = "Time", ylab = "Sales in Millions", 
         ylim = c(100, 500), main = "Sales Forecasting", col = "blue")
    
    
  #c. Apply the Acf() function to identify possible time series components. Provide in the report the autocorrelation chart and explain the time series components existing in the historical data. 
    
    # Use Acf() function to identify autocorrelation and plot autocorrelation
    # for different lags (up to maximum of 12).
    
    autocor <- Acf(grocery.ts, lag.max = 12, main = "Autocorrelation for Grocery Sale")
    
    # Use stl() function to plot times series components of the original data. 
    # The plot includes original data, trend, seasonal, and reminder (level and noise component).

    grocery.stl <- stl(grocery.ts, s.window = "periodic")
    autoplot(grocery.stl, main = "Grocery Sales Time Series Components")
    
    
    
## 2. Use trailing MA for forecasting time series. 
    
  #a.	Use the rollmean() function to develop three trailing MAs (apply the entire data set with no partitioning) for the window width of 2, 6, and 12, respectively. Present the R code for these MAs in your report. 

    # Create trailing moving average with window (number of periods) k = 2, 6, and 12.
    # In rollmean(), use argument align = "right" to calculate a trailing MA.
    ma.trailing_2 <- rollmean(grocery.ts, k = 2, align = "right")
    ma.trailing_6 <- rollmean(grocery.ts, k = 6, align = "right")
    ma.trailing_12 <- rollmean(grocery.ts, k = 12, align = "right")
    
    
    # Combine grocery.ts and ma.trailing in one data table.
    
    ma.trail_2 <- c(rep(NA, length(grocery.ts) - length(ma.trailing_2)), ma.trailing_2)
    ma.trail_6 <- c(rep(NA, length(grocery.ts) - length(ma.trailing_6)), ma.trailing_6)
    ma.trail_12 <- c(rep(NA, length(grocery.ts) - length(ma.trailing_12)), ma.trailing_12)
    
    ma_trailing_tab <- cbind(grocery.ts, ma.trail_2, ma.trail_6, ma.trail_12)
    ma_trailing_tab

        
  #b. Use the forecast() function to create a trailing MA forecast for each window width in 12 months of 2020, and present these forecasts in your report. 
    
    ## Create trailing MA forecast for 12 periods into the future for window width 2, 6, and 12.
    
    ma.trailing_2.pred <- forecast(ma.trailing_2, h=12, level = 0)
    ma.trailing_6.pred <- forecast(ma.trailing_6, h=12, level = 0)
    ma.trailing_12.pred <- forecast(ma.trailing_12, h=12, level = 0)
  
    
  #c. Develop a seasonal naïve forecast for the entire historical data set, and apply the accuracy() function to compare accuracy of the four models: seasonal naïve forecast and trailing MAs with window width of 2, 6, and 12, respectively. Present the accuracy measures in your report, compare MAPE and RMSE of these forecasts, and identify the best forecasting model. 
    
    ## IDENTIFY FORECAST ACCURACY FOR SEASONAL NAIVAE FORECASTS.
    
    # Use accuracy() function to identify common accuracy measures.
    # Use round() function to round accuracy measures to three decimal digits.
    round(accuracy((snaive(grocery.ts)$fitted), grocery.ts), 3) 
    round(accuracy(ma.trail_2, grocery.ts), 3)
    round(accuracy(ma.trail_6, grocery.ts), 3)
    round(accuracy(ma.trail_12, grocery.ts), 3)

    
## 3. Apply the two-level forecast with regression and trailing MA for residuals. 
    
    
    #a. To de-trend and de-seasonalize the historical data for the entire data set, develop using the tslm() function a regression model with linear trend and seasonality and forecast sales in 2020 with the forecast() function. Present and briefly explain the model in your report. 
   
    # Fit a regression model with linear trend and seasonality.
    reg.trend.seas <- tslm(grocery.ts ~ trend + season)
    summary(reg.trend.seas)
    
    # Create forecast for the 12 periods into the future.
    reg.trend.seas.pred <- forecast(reg.trend.seas, h = 12, level = 0)
    reg.trend.seas.pred
    
    
    #b. Identify regression residuals, apply a trailing MA (window width of 2) for these residuals using the rollmean() function, and forecast worldwide monthly sales in 12 months of 2020 (use the forecast() function). Combine the regression and trailing MA residuals' forecast for 2020, and present in your report a table that contains regression forecast, trailing MA forecast for residuals, and total (combined) forecast in 2020. 
    
    # Identify and display residuals for time series based on the regression
    # (differences between actual and regression values in the same periods).
    reg.trend.seas.res <- reg.trend.seas$residuals
    reg.trend.seas.res
    
    # Apply trailing MA with window width of 2 for these residuals.
    ma.trailing.res_2 <- rollmean(reg.trend.seas.res, k = 2, align = "right")
    ma.trailing.res_2
    
    # Create forecast for residuals for the 12 periods into the future.
    ma.trailing.res_12.pred <- forecast(ma.trailing.res_2, h = 12, level = 0)
    ma.trailing.res_12.pred
    
    # To develop real forecast for 12 periods into the future, 
    # combine regression forecast and trailing MA forecast for residuals.
    ts.forecast.12 <- reg.trend.seas.pred$mean + ma.trailing.res_12.pred$mean
    ts.forecast.12
    
    # Create a table with regression forecast, trailing MA for residuals
    # and total forecast for 12 months into the future.
    total.reg.ma.pred <- data.frame(reg.trend.seas.pred$mean, ma.trailing.res_12.pred$mean, 
                                    ts.forecast.12)
    total.reg.ma.pred
    
    #c. Apply the accuracy() function to compare accuracy of the three forecasting models: seasonal naïve forecast (applied in question 2c), regression model with trend and seasonality, and two-level (combined) model with regression and trailing MA for residuals. Present the accuracy measures in your report, compare MAPE and RMSE of these forecasts, and identify the best forecasting model. 
    
    # Use accuracy() function to identify common accuracy measures.
    # Use round() function to round accuracy measures to three decimal digits.
    round(accuracy((snaive(grocery.ts)$fitted), grocery.ts), 3) 
    round(accuracy(reg.trend.seas.pred$fitted, grocery.ts), 3)
    round(accuracy(reg.trend.seas.pred$fitted+ma.trailing.res_12, grocery.ts), 3)
    

## 4. Use advanced exponential smoothing methods.
    
    #a. Develop data partition with the validation partition of 12 historical periods and training partition of the rest of the historical periods.
    
    # Create fixed data partitioning for data.
    # Define the numbers of months in the training and validation sets, nTrain and nValid, respectively.
    nValid <- 12
    nTrain <- length(grocery.ts) - nValid
    train.ts <- window(grocery.ts, start = c(2015, 1), end = c(2015, nTrain))
    valid.ts <- window(grocery.ts, start = c(2015, nTrain + 1), 
                       end = c(2015, nTrain + nValid))
    
    #b. For the training partition, use the ets() function to develop a Holt-Winter's model with multiplicative error, multiplicative trend, and multiplicative seasonality options, and automated selection of smoothing parameters for the training partition. Present and explain the model in your report. Use the model to forecast worldwide sales for the validation period using the forecast() function.
    
    # Create Holt-Winter's (HW) exponential smoothing for partitioned data.
    # Use ets() function with model = "AAA", i.e., multiplicative error(M), 
    # multiplicative trend (M), & multiplicative seasonality (M). 
    # Use optimal alpha, beta, & gamma to fit HW over the training period.
    hw.MMM <- ets(train.ts, model = "MMM")
    hw.MMM
    
    # Use forecast() function to make predictions using this HW model for validation period (nValid). 
    # Show predictions in tabular format.
    hw.MMM.pred <- forecast(hw.MMM, h = nValid, level = 0)
    hw.MMM.pred
    
    #c. To make a forecast for the 12 months of 2020, use the entire data set (no partitioning) to develop the Holt-Winter's model using the ets() function with the automated selection of error, trend, and seasonality options, and automated selection of smoothing parameters. Present and explain the model in your report. Use the model to forecast worldwide sales for the 12 months of 2020 using the forecast() function.
    
    HW.ZZZ <- ets(grocery.ts, model = "ZZZ")
    HW.ZZZ #model appears to be (A,N,A) with alpha 0.4469 and gamma = 0.0001
    
    # Use forecast() function to make predictions using this HW model for
    # 12 month into the future.
    length <- length(grocery.ts)
    HW.ZZZ.pred <- forecast(HW.ZZZ, h = 12 , level = 0)
    HW.ZZZ.pred
    HW.ZZZ.pred$fitted
    
    #d. Apply the accuracy() function to compare the two forecasting models: seasonal naïve forecast (applied in question 2c) and Holt-Winter's model developed in question 4c. Present the accuracy measures in your report, compare MAPE and RMSE of these forecasts, and identify the best forecasting model.
    
    # Seasonal Naive model
    round(accuracy((snaive(grocery.ts)$fitted), grocery.ts), 3) 
    # HW model
    round(accuracy(hw.ZZZ.pred$fitted, grocery.ts), 3)
    
    #e. Compare the best forecasts identified in questions 3c and 4c. Explain what your final choice of the forecasting model in this case will be.
    
    # The best model is model 3c by comparing the MAPE and RMSE values.