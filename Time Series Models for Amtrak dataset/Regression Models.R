## USE FORECAST LIBRARY.

library(forecast)

## CREATE DATA FRAME. 

# Set working directory for locating files.
setwd("C:/Apoorva/Masters - Business Analytics/Masters MSBA/4th SEMESTER/BAN 673_Time Series Analytics/Case Studies/case study #2")

# Create data frame.
Revenue.data <- read.csv("case2(1).csv")

# See the first 6 records of the file.
head(Revenue.data)



##1. Plot the data and visualize time series components.

  #a. Create time series data set in R using the ts() function.

      # Function ts() takes three arguments: start, end, and freq.
      # With quarterly data, frequency in a season (year) is equal to 4.
      # Arguments start and end are pairs: (season number, period number).
      revenue.ts <- ts(Revenue.data$Revenue, 
                         start = c(2005, 1), end = c(2020, 2), freq = 4)

  #b. Apply the plot() function to create a data plot with the historical data, provide it in your report, 
  # and explain what time series components can be visualized in this plot.
    
      ## Use plot() to plot time series data  
      
      plot(revenue.ts, 
           xlab = "Time", ylab = "Revenue in Millions", 
           ylim = c(71500, 142000), main = "Revenue Forecasting", col = "blue")
      
      
      # Use stl() function to plot times series components of the original data. 
      # The plot includes original data, trend, seasonal, and reminder (level and noise component).
      
      revenue.stl <- stl(revenue.ts, s.window = "periodic")
      autoplot(revenue.stl, main = "Walmart Revenue Time Series Components")

    
##2. Apply five regression models using data partition.
##    Consider the following 5 regression-based models:
##    i. Regression model with linear trend
##    ii. Regression mode with quadratic trend
##    iii. Regression model with seasonality
##    iv. Regression model with linear trend and seasonality
##    v. Regression model with quadratic trend and seasonality.
    
    
    #a. Develop data partition with the validation partition of 16 periods and the rest for the 
    #   training partition.
    
        nValid <- 16 
        nTrain <- length(revenue.ts) - nValid
        train.ts <- window(revenue.ts, start = c(2005, 1), end = c(2005, nTrain))
        valid.ts <- window(revenue.ts, start = c(2005, nTrain + 1), 
                           end = c(2005, nTrain + nValid))
      
    #b. Use the tslm() function for the training partition to develop each of the 5 regression 
    #   models from the above list. Apply the summary() function to identify the model structure 
    #   and parameters for each regression model, show them in your report, and also present the 
    #   respective model equation. Use each model to forecast revenues for the validation period 
    #   using the forecast() function.
        
        ## i. Regression model with linear trend
        
          # Use tslm() function (time series linear model) to create regression model with linear trend.
          train.lin <- tslm(train.ts ~ trend)
          
          # See summary of linear trend model and associated parameters.
          summary(train.lin)
          
          # Apply forecast() function to make forecast for validation period.
          train.lin.pred <- forecast(train.lin, h = nValid, level = 0)
          train.lin.pred
        
        ##ii. Regression mode with quadratic trend
        
          # Use tslm() function to create quadratic (polynomial) trend model.
          train.quad <- tslm(train.ts ~ trend + I(trend^2))
          
          # See summary of quadratic trend model and associated parameters.
          summary(train.quad)
          
          # Apply forecast() function to make predictions for ts data in
          # validation set.  
          train.quad.pred <- forecast(train.quad, h = nValid, level = 0)
          train.quad.pred
          
        ## iii. Regression model with seasonality
          
          # Use tslm() function to create seasonal model.
          train.season <- tslm(train.ts ~ season)
          
          # See summary of seasonal model and associated parameters.
          summary(train.season)
          
          # If necessary, run the following code to identify seasons
          train.season$data 
          
          # Apply forecast() function to make predictions for ts with 
          # seasonality data in validation set.  
          train.season.pred <- forecast(train.season, h = nValid, level = 0)
          train.season.pred
          
        ## iv. Regression model with linear trend and seasonality
          
          # Use tslm() function to create linear trend and seasonal model.
          train.lin.season <- tslm(train.ts ~ trend + season)
          
          # See summary of linear trend and seasonality model and associated parameters.
          summary(train.lin.season)
          
          # Apply forecast() function to make predictions for ts with 
          # trend and seasonality data in validation set.  
          train.lin.season.pred <- forecast(train.lin.season, h = nValid, level = 0)
          train.lin.season.pred
          
          
        ##  v. Regression model with quadratic trend and seasonality.
            
          # Use tslm() function to create quadratic trend and seasonal model.
          train.trend.season <- tslm(train.ts ~ trend + I(trend^2) + season)
          
          # See summary of quadratic trend and seasonality model and associated parameters.
          summary(train.trend.season)
          
          # Apply forecast() function to make predictions for ts with 
          # trend and seasonality data in validation set.  
          train.trend.season.pred <- forecast(train.trend.season, h = nValid, level = 0)
          train.trend.season.pred
    
    #c. Apply the accuracy() function to compare performance measure of the 5 forecasts you 
    #   developed in 2b. Present the accuracy measures in your report, compare them, and, using 
    #   MAPE and RMSE, identify the two most accurate regression models for forecasting. 
    
        # Use accuracy() function to identify common accuracy measures with rounded
        # values to 3 decimals.
          round(accuracy(train.lin.pred, valid.ts), 3)
          round(accuracy(train.quad.pred, valid.ts), 3)
          round(accuracy(train.season.pred, valid.ts), 3)
          round(accuracy(train.lin.season.pred, valid.ts), 3)
          round(accuracy(train.trend.season.pred, valid.ts), 3) 
          
#3. Employ the entire data set to make time series forecast.
          
  #a. Apply the two most accurate regression models identified in question to make the forecast for the 
    # last two quarters of 2020 and first two quarters of 2021. For that, use the entire data set to 
    # develop the regression model using the tslm() function. Apply the summary() function to identify 
    # the model structure and parameters, show them in your report, and also present the respective model 
    # equation. Use each model to forecast Walmart's revenue in the 4 quarters of 2020 and 2021 using the 
    # forecast() function, and present this forecast in your report. 
          
          #i.	Regression model with linear trend and seasonality
          
          # Use tslm() function to create linear trend and seasonal model.
          lin.season <- tslm(revenue.ts ~ trend + season)
          
          # See summary of linear trend and seasonality model and associated parameters.
          summary(lin.season)
          
          # Apply forecast() function to make predictions for ts with 
          # trend and seasonality data in validation set.  
          lin.season.pred <- forecast(lin.season, h = 4, level = 0)
          lin.season.pred
          
          #ii. Regression model with quadratic trend and seasonality
          
          # Use tslm() function to create quadratic trend and seasonal model.
          quad.trend.season <- tslm(revenue.ts ~ trend + I(trend^2) + season)
          
          # See summary of quadratic trend and seasonality model and associated parameters.
          summary(quad.trend.season)
          
          # Apply forecast() function to make predictions for ts with 
          # trend and seasonality data in validation set.  
          quad.trend.season.pred <- forecast(quad.trend.season, h = 4, level = 0)
          quad.trend.season.pred
          
  #b. Apply the accuracy() function to compare the performance measures of the regression models developed in 
  #  3a with those for naïve and seasonal naïve forecasts. Present the accuracy measures in your report, 
  #  compare them, and identify, using MAPE and RMSE, which forecast is most accurate to forecast Walmart's 
  #  quarterly revenue in 2020 and 2021. 
          
          round(accuracy((naive(revenue.ts))$fitted, revenue.ts), 3)
          round(accuracy((snaive(revenue.ts))$fitted, revenue.ts), 3)
          round(accuracy(lin.season.pred$fitted, revenue.ts),3)
          round(accuracy(quad.trend.season.pred$fitted, revenue.ts),3)
          