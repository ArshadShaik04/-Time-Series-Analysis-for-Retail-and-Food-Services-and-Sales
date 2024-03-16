library(forecast)
library(zoo)


## CREATE DATA FRAME. 

# Set working directory for locating files.
setwd("C:/Users/ARSHAD/Desktop/Time Series Analytics 673")

value.data <- read.csv("food_sales_new.csv")

#Creating the Time Series Data
value.ts <- ts(value.data$Value, 
                  start = c(1992, 1), end = c(2023, 12), freq = 12)
value.ts

#Plotting the Time Series Data
plot(value.ts, 
     main = "Yearly Food Sales Time Series Data (1992-2023)",
     xlab = "Year",
     ylab = "Sales Values",
     type = "l",  # 'l' for line plot
     col = "black"
)
value.stl <- stl(value.ts, s.window = "periodic")
autoplot(value.stl, main = "Time Series Components")
autocor <- Acf(value.ts, lag.max = 12, 
               main = "Autocorrelation for Food Sales")

Lag <- round(autocor$lag, 0)
ACF <- round(autocor$acf, 3)
data.frame(Lag, ACF)

#Data Partition with validation period of 5 years(60 months)
#Data Partition with Training period of 26 years(312 months)
nValid <- 60
nTrain <- length(value.ts) - nValid 
train.ts <- window(value.ts, start = c(1992, 1), end = c(1992, nTrain))
valid.ts <- window(value.ts, start = c(1992, nTrain + 1), 
                   end = c(1992, nTrain + nValid))
train.ts
valid.ts

# Apply z-test to test the null hypothesis
ar1 <- 0.9940
s.e. <- 0.0045
null_mean <- 1
alpha <- 0.05
z.stat <- (ar1-null_mean)/s.e.
z.stat
p.value <- pnorm(z.stat)
p.value
if (p.value<alpha) {
  "Reject null hypothesis"
} else {
  "Accept null hypothesis"
}

# Create first differenced Sales data using lag1.
diff.sales.ts <- diff(value.ts, lag = 1)
diff.sales.ts

# Use Acf() function to identify autocorrealtion for first differenced 
# Sales, and plot autocorrelation for different lags 
# (up to maximum of 12).
Acf(diff.sales.ts, lag.max = 12, 
    main = "Autocorrelation for Differenced Sales Data")


# Create Holt-Winter's (HW) exponential smoothing for partitioned data.
# Use ets() function with model = "ZZZ", i.e., automatic selection of
# error, trend, and seasonality options.
# Use optimal alpha, beta, & gamma to fit HW over the training period.
hw.ZZZ <- ets(train.ts, model = "ZZZ")
hw.ZZZ 

# Use forecast() function to make predictions using this HW model with 
# validation period (nValid). 
# Show predictions in tabular format.
hw.ZZZ.pred <- forecast(hw.ZZZ, h = nValid, level = 0)
hw.ZZZ.pred


# Plot HW predictions for original data, automatic selection of the 
# model and optimal smoothing parameters.
plot(hw.ZZZ.pred$mean, 
     xlab = "Time", ylab = "Yearly Sales", 
     ylim = c(6000, 26000), 
     bty = "l", xlim = c(1992, 2027), xaxt = "n",
     main = "Holt-Winter's Model with Automatic Selection of Model Options", 
     lty = 5, col = "blue", lwd = 2) 
axis(1, at = seq(1992, 2027, 1), labels = format(seq(1992, 2027, 1)))
lines(hw.ZZZ.pred$fitted, col = "blue", lwd = 2)
lines(value.ts)
legend(1992,25500, 
       legend = c("Sales values of entire data set", 
                  "Holt-Winter's Model for Training Partition",
                  "Holt-Winter's Model for Validation Partition"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2019, 2019), c(0, 26000))
lines(c(2024, 2024), c(0, 26000))
text(2001, 26000, "Training")
text(2021, 26000, "Validation")
text(2026, 26000, "Future")
arrows(1992, 25000, 2019, 25000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019, 25000, 2024, 25000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2024, 25000, 2027, 25000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


# for regression models training and validation periods with (1) Holt Winters Model, (2) Linear Trend and Seasonality


## FORECAST WITH HOLT-WINTER'S MODEL USING ENTIRE DATA SET INTO
## THE FUTURE FOR 24 PERIODS.

# Create Holt-Winter's (HW) exponential smoothing for full trade data set. 
# Use ets() function with model = "ZZZ", to identify the best HW option
# and optimal alpha, beta, & gamma to fit HW for the entire data period.
HW.ZZZ <- ets(value.ts, model = "ZZZ")
HW.ZZZ 

# Use forecast() function to make predictions using this HW model for
# 24 month into the future.
HW.ZZZ.pred <- forecast(HW.ZZZ, h = 24 , level = 0)
HW.ZZZ.pred


# Plot HW predictions for original data, automatic selection of the 
# model and optimal smoothing parameters.
plot(HW.ZZZ.pred$mean, 
     xlab = "Time", ylab = "Yearly Sales", 
     ylim = c(6000, 26000), 
     bty = "l", xlim = c(1992, 2027), xaxt = "n",
     main = "Holt-Winter's Model with Automatic Selection of Model Options", 
     lty = 5, col = "blue", lwd = 2) 
axis(1, at = seq(1992, 2027, 1), labels = format(seq(1992, 2027, 1)))
lines(HW.ZZZ.pred$fitted, col = "blue", lwd = 2)
lines(value.ts)
legend(1992,25000, 
       legend = c("Sales values of entire data set", 
                  "Holt-Winter's Model for Training and Validation Partitions",
                  "Holt-Winter's Model for Future periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2024, 2024), c(0, 26000))
text(2008, 26000, "Entire Data Set")
text(2026, 26000, "Future")
arrows(1992, 25000, 2024, 25000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2024, 25000, 2027, 25000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use accuracy() function to identify common accuracy measures
round(accuracy(hw.ZZZ.pred$mean, valid.ts), 3)
round(accuracy(HW.ZZZ.pred$fitted, value.ts), 3)


##REGRESSION MODEL WITH LINEAR TREND AND SEASONALITY FOR TRAINING AND VALIDATION

# Use tslm() function to create linear trend and seasonal model.
train.lin.season <- tslm(train.ts ~ trend + season)

# See summary of linear trend equation and associated parameters.
summary(train.lin.season)

# Apply forecast() function to make predictions for ts with 
# linear trend and seasonal model in validation set.  
train.lin.season.pred <- forecast(train.lin.season, h = nValid, level = 0)
train.lin.season.pred

# Plot ts data, linear trend and seasonality data, and predictions 
# for validation period.
plot(train.lin.season.pred$mean, 
     xlab = "Time", ylab = "Yearly Food Sales", ylim = c(6000, 26000), 
     bty = "l", xlim = c(1992, 2027), xaxt = "n",
     main = "Regression with Linear Trend and Seasonality", 
     lwd = 2, lty = 2, col = "blue") 
axis(1, at = seq(1992, 2027, 1), labels = format(seq(1992, 2027, 1)))
lines(train.lin.season.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lwd = 2, lty = 1)
lines(valid.ts, col = "black", lwd = 2, lty = 1)
legend(1992,25000, legend = c("Yearly Food Sales", "Regression for Training Data",
                              "Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2019, 2019), c(0, 26000))
lines(c(2024, 2024), c(0, 26000))
text(2001, 26000, "Training")
text(2021, 26000, "Validation")
text(2026, 26000, "Future")
arrows(1992, 25000, 2019, 25000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019, 25000, 2024, 25000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2024, 25000, 2027, 25000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Regression with linear trend and seasonality
# Use tslm() function to create regression model with linear trend 
# and seasonality.
lin.season <- tslm(value.ts ~ trend + season)

# See summary of linear trend and seasonality equation and associated parameters.
summary(lin.season)

# Apply forecast() function to make predictions for ts with 
# linear trend and seasonality data in 24 future periods.
lin.season.pred <- forecast(lin.season, h = 24, level = 0)
lin.season.pred

# Plot ts data, regression model with linear trend and seasonality data, 
# and predictions for future 24 periods.
plot(lin.season.pred$mean, 
     xlab = "Time", ylab = "Values (in 000s)", 
     ylim = c(6000, 26000), bty = "l",
     xlim = c(1992, 2027), xaxt = "n",
     main = "Regression Model with Linear Trend and Seasonality for entire data set", 
     lty = 2, lwd = 2, col = "blue") 
axis(1, at = seq(1992, 2027, 1), labels = format(seq(1992, 2027, 1)))
lines(lin.season.pred$fitted, col = "blue", lwd = 2)
lines(value.ts)
legend(1993,25000, legend = c("Food Sales Time Series", 
                              "Linear Trend and Seasonality Model for Entire Data",
                              "Linear and Seasonality Forecast for Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")



# Plot on chart vertical lines and horizontal arrows describing
# entire data set and future prediction intervals.
lines(c(2024, 2024), c(0, 25000))
text(2007, 26000, "Entire Data Set")
text(2026, 26000, "Future")
arrows(1992, 25000, 2024, 25000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2024, 25000, 2027, 25000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use accuracy() function to identify common accuracy measures
round(accuracy(train.lin.season.pred$mean, valid.ts), 3)
round(accuracy(lin.season.pred$fitted, value.ts), 3)


#Regression with linear trend and seasonality
# Use tslm() function to create regression model with linear trend 
# and seasonality.
lin.season <- tslm(value.ts ~ trend + season)

# See summary of linear trend and seasonality equation and associated parameters.
summary(lin.season)

# Apply forecast() function to make predictions for ts with 
# linear trend and seasonality data in 24 future periods.
lin.season.pred <- forecast(lin.season, h = 24, level = 0)
lin.season.pred

# Plot ts data, regression model with linear trend and seasonality data, 
# and predictions for future 24 periods.
plot(lin.season.pred$mean, 
     xlab = "Time", ylab = "Values (in 000s)", 
     ylim = c(6000, 26000), bty = "l",
     xlim = c(1992, 2027), xaxt = "n",
     main = "Regression Model with Linear Trend and Seasonality for entire data set", 
     lty = 2, lwd = 2, col = "blue") 
axis(1, at = seq(1992, 2027, 1), labels = format(seq(1992, 2027, 1)))
lines(lin.season.pred$fitted, col = "blue", lwd = 2)
lines(value.ts)
legend(1993,25000, legend = c("Food Sales Time Series", 
                              "Linear Trend and Seasonality Model for Entire Data",
                              "Linear and Seasonality Forecast for Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")



# Plot on chart vertical lines and horizontal arrows describing
# entire data set and future prediction intervals.
lines(c(2024, 2024), c(0, 25000))
text(2007, 26000, "Entire Data Set")
text(2026, 26000, "Future")
arrows(1992, 25000, 2024, 25000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2024, 25000, 2027, 25000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Two -level forecast

## FIT REGRESSION MODEL WITH LINEAR TREND AND SEASONALITY. 
## USE Acf() FUNCTION TO IDENTIFY AUTOCORRELATION FOR RESIDUALS.
## PLOT RESIDUALS.

# Use tslm() function to create linear trend and seasonal model.
train.lin.season <- tslm(train.ts ~ trend + season)

# See summary of linear trend equation and associated parameters.
summary(train.lin.season)

# Apply forecast() function to make predictions for ts with 
# linear trend and seasonal model in validation set.  
train.lin.season.pred <- forecast(train.lin.season, h = nValid, level = 0)
train.lin.season.pred

# Plot ts data, linear trend and seasonality data, and predictions 
# for validation period.
plot(train.lin.season.pred$mean, 
     xlab = "Time", ylab = "Food Sales (in 000s)", ylim = c(6000, 26000), 
     bty = "l", xlim = c(1992, 2027), xaxt = "n",
     main = "Regression with Linear Trend and Seasonality", 
     lwd = 2, lty = 2, col = "blue") 
axis(1, at = seq(1992, 2027, 1), labels = format(seq(1992, 2027, 1)))
lines(train.lin.season.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lwd = 2, lty = 1)
lines(valid.ts, col = "black", lwd = 2, lty = 1)
legend(1992,3200, legend = c("Sales Time Series", "Regression for Training Data",
                             "Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2019, 2019), c(0, 26000))
lines(c(2024, 2024), c(0, 26000))
text(2001, 26000, "Training")
text(2021, 26000, "Validation")
text(2026, 26000, "Future")
arrows(1992, 25000, 2019, 25000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019, 25000, 2024, 25000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2024, 25000, 2027, 25000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Plot residuals of the predictions with trend and seasonality.
plot(train.lin.season.pred$residuals, 
     xlab = "Time", ylab = "Residuals", 
     ylim = c(-7000, 6000), bty = "l",
     xlim = c(1992, 2027), xaxt = "n",
     main = "Regresssion Residuals for Training and Validation Data", 
     col = "brown", lwd = 2) 
axis(1, at = seq(1992, 2027, 1), labels = format(seq(1992, 2027, 1)))
lines(valid.ts - train.lin.season.pred$mean, col = "brown", lwd = 2, lty = 1)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2019, 2019), c(-7000, 6000))
lines(c(2024, 2024), c(-7000, 6000))
text(2001, 6000, "Training")
text(2021, 6000, "Validation")
text(2026, 6000, "Future")
arrows(1992, 5500, 2019, 5500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019, 5500, 2024, 5500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2024, 5500, 2027, 5500, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use Acf() function to identify autocorrelation for the model residuals 
# (training and validation sets), and plot autocorrelation for different 
# lags (up to maximum of 12).
Acf(train.lin.season.pred$residuals, lag.max = 12, 
    main = "Autocorrelation for Food Sales Residuals")
Acf(valid.ts - train.lin.season.pred$mean, lag.max = 12, 
    main = "Autocorrelation for Food Sales Residuals")


## USE Arima() FUNCTION TO CREATE AR(1) MODEL FOR TRAINING RESIDUALS.
## CREATE TWO-LEVEL MODEL WITH LINEAR TREND AND SEASONALITY MODEL 
## AND AR(1) RESIDUALS.
## PLOT DATA AND IDENTIFY ACCURACY MEASURES.

# Use Arima() function to fit AR(1) model for training residuals. The Arima model of 
# order = c(1,0,0) gives an AR(1) model.
# Use summary() to identify parameters of AR(1) model. 
res.ar1 <- Arima(train.lin.season$residuals, order = c(1,0,0))
summary(res.ar1)
res.ar1$fitted

# Use forecast() function to make prediction of residuals in validation set.
res.ar1.pred <- forecast(res.ar1, h = nValid, level = 0)
res.ar1.pred

# Develop a data frame to demonstrate the training AR model results 
# vs. original training series, training regression model, 
# and its residuals.  
train.df <- round(data.frame(train.ts, train.lin.season$fitted, 
                             train.lin.season$residuals, res.ar1$fitted, res.ar1$residuals), 3)
names(train.df) <- c("Food Sales", "Regression", "Residuals",
                     "AR.Model", "AR.Model.Residuals")
train.df

# Plot residuals of the predictions for training data before AR(1).
plot(train.lin.season.pred$residuals, 
     xlab = "Time", ylab = "Residuals", 
     ylim = c(-6000, 6000), bty = "l",
     xlim = c(1992, 2027), xaxt = "n", 
     main = "Regresssion Residuals for Training Data before AR(1)", 
     col = "brown", lwd = 3) 
axis(1, at = seq(1992, 2027, 1), labels = format(seq(1992, 2027, 1)))

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2019, 2019), c(-7000, 6000))
text(2002, 6000, "Training")
arrows(1992, 5500, 2019, 5500, code = 3, length = 0.1,
       lwd = 1, angle = 30)


# Plot residuals of the residuals for training data after AR(1).
plot(res.ar1$residuals, 
     xlab = "Time", ylab = "Residuals", 
     ylim = c(-6000, 6000), bty = "l",
     xlim = c(1992, 2027), xaxt = "n",
     main = "Residuals of Residuals for Training Data after AR(1)", 
     col = "brown", lwd = 3) 
axis(1, at = seq(1992, 2027, 1), labels = format(seq(1992, 2027, 1)))

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2019, 2019), c(-7000, 6000))
text(2002, 6000, "Training")
arrows(1992, 5500, 2019, 5500, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use Acf() function to identify autocorrelation for the training 
# residual of residuals and plot autocorrelation for different lags 
# (up to maximum of 12).
Acf(res.ar1$residuals, lag.max = 12, 
    main = "Autocorrelation for food sales Training Residuals of Residuals")

# Create two-level model's forecast with linear trend and seasonality 
# regression + AR(1) for residuals for validation period.

# Create data table with validation data, regression forecast
# for validation period, AR(1) residuals for validation, and 
# two level model results. 
valid.two.level.pred <- train.lin.season.pred$mean + res.ar1.pred$mean

valid.df <- round(data.frame(valid.ts, train.lin.season.pred$mean, 
                             res.ar1.pred$mean, valid.two.level.pred),3)
names(valid.df) <- c("Food Sales", "Reg.Forecast", 
                     "AR(1)Forecast", "Combined.Forecast")
valid.df

# plot ts data, linear trend and seasonality data, and predictions 
# for validation period.
plot(valid.two.level.pred, 
     xlab = "Time", ylab = "Sales Values (in 000s)", ylim = c(6000, 26000), 
     bty = "l", xlim = c(1992, 2027), xaxt = "n",
     main = "Two-Level Forecast: Regression with Trend
             and Seasonlity + AR(1) for Residuals", lwd = 2,
     col = "blue", lty = 2) 
axis(1, at = seq(1992, 2027, 1), labels = format(seq(1992, 2027, 1)))
lines(train.lin.season.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lwd = 2, lty = 1)
lines(valid.ts, col = "black", lwd = 2, lty = 1)
legend(1992,3200, legend = c("Ridership Time Series", "Regression for Training Data",
                             "Two Level Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2019, 2019), c(0, 26000))
lines(c(2024, 2024), c(0, 26000))
text(2001, 26000, "Training")
text(2021, 26000, "Validation")
text(2026, 26000, "Future")
arrows(1992, 25000, 2019, 25000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019, 25000, 2024, 25000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2024, 25000, 2027, 25000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use accuracy() function to identify common accuracy measures for validation period forecast:
# (1) two-level model (linear trend and seasonal model + AR(1) model for residuals),
# (2) linear trend and seasonality model only.
round(accuracy(valid.two.level.pred, valid.ts), 3)
round(accuracy(train.lin.season.pred$mean, valid.ts), 3)



# Use Acf() function to identify autocorrelation for the model residuals 
# for entire data set, and plot autocorrelation for different 
# lags (up to maximum of 12).
Acf(lin.season.pred$residuals, lag.max = 12, 
    main = "Autocorrelation of Regression Residuals for Entire Data Set")


# Use Arima() function to fit AR(1) model for regression residuals.
# The ARIMA model order of order = c(1,0,0) gives an AR(1) model.
# Use forecast() function to make prediction of residuals into the future 12 months.
residual.ar1 <- Arima(lin.season$residuals, order = c(1,0,0))
residual.ar1.pred <- forecast(residual.ar1, h = 24, level = 0)

# Use summary() to identify parameters of AR(1) model.
summary(residual.ar1)

# Use Acf() function to identify autocorrelation for the residuals of residuals 
# and plot autocorrelation for different lags (up to maximum of 12).
Acf(residual.ar1$residuals, lag.max = 12, 
    main = "Autocorrelation for Residuals of Residuals for Entire Data Set")


# Identify forecast for the future 24 periods as sum of linear trend and 
# seasonal model and AR(1) model for residuals.
lin.season.ar1.pred <- lin.season.pred$mean + residual.ar1.pred$mean
lin.season.ar1.pred


# Create a data table with linear trend and seasonal forecast 
# for 24 future periods,
# AR(1) model for residuals for 24 future periods, and combined 
# two-level forecast for 24 future periods. 
table.df <- round(data.frame(lin.season.pred$mean, 
                             residual.ar1.pred$mean, lin.season.ar1.pred),3)
names(table.df) <- c("Reg.Forecast", "AR(1)Forecast","Combined.Forecast")
table.df


# Plot historical data, predictions for historical data, and forecast 
# for 24 future periods.
plot(value.ts, 
     xlab = "Time", ylab = "Sales Values (in 000s)", 
     ylim = c(6000, 27000), xaxt = "n",
     bty = "l", xlim = c(1992, 2027), lwd = 2,
     main = "Two-Level Forecast: Regression with Trend and Seasonlity + AR(1)
     for Residuals") 
axis(1, at = seq(1992, 2027, 1), labels = format(seq(1992, 2027, 1)))
lines(lin.season$fitted + residual.ar1$fitted, col = "blue", lwd = 2)
lines(lin.season.ar1.pred, col = "blue", lty = 5, lwd = 2)
legend(1992,26000, legend = c("Food Sales Values for Training and Valiadaton Periods", 
                             "Two-Level Forecast for Training and Valiadtion Periods", 
                             "Two-Level Forecast for 24 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2024, 2024), c(0, 26000))
text(2007, 27000, "Entire Data Set")
text(2026, 27000, "Future")
arrows(1992, 26000, 2024, 26000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2024, 26000, 2027, 26000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


# Use accuracy() function to identify common accuracy measures for:
# (1) two-level model (linear trend and seasonality model 
#     + AR(1) model for residuals),
# (2) linear trend and seasonality model only, and
# (3) seasonal naive forecast. 
round(accuracy(lin.season$fitted + residual.ar1$fitted, value.ts), 3)
round(accuracy(lin.season$fitted, value.ts), 3)
round(accuracy((snaive(value.ts))$fitted, value.ts), 3)



#ARIMA MODELS
# Using Arima() function to fit AR(1) model.
values.ar1<- Arima(value.ts, order = c(1,0,0))
summary(values.ar1)



# Using Arima() function to fit ARIMA(1,1,1)(1,1,1) model for trend and seasonality.
# For Training and Validation
train.arima.seas <- Arima(train.ts, order = c(1,1,1), 
                          seasonal = c(1,1,1)) 
summary(train.arima.seas)

# Apply forecast() function to make predictions for ts with 
# ARIMA model in validation set.    
train.arima.seas.pred <- forecast(train.arima.seas, h = nValid, level = 0)
train.arima.seas.pred

# Plot ts data, ARIMA model, and predictions for validation period.
plot(train.arima.seas.pred, 
     xlab = "Time", ylab = "Sales Values (in 000s)", 
     ylim = c(6000, 26000), xaxt = "n",
     bty = "l", xlim = c(1992, 2027), 
     main = "ARIMA(1,1,1)(1,1,1) Model", lwd = 2, flty = 5) 
axis(1, at = seq(1992, 2027, 1), labels = format(seq(1992, 2027, 1)))
lines(train.arima.seas.pred$fitted, col = "blue", lwd = 2)
lines(value.ts, col = "black", lwd = 2, lty = 1)
legend(1993,26000, legend = c("Yearly Food Sales", 
                              "Seasonal ARIMA Forecast for Training Period",
                              "Seasonal ARIMA Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")
# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2019, 2019), c(0, 26000))
lines(c(2024, 2024), c(0, 26000))
text(2001, 26000, "Training")
text(2021, 26000, "Validation")
text(2026, 26000, "Future")
arrows(1992, 25000, 2019, 25000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019, 25000, 2024, 25000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2024, 25000, 2027, 25000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# FIT AUTO ARIMA MODEL FOR TRAING AND VALIDATION
# Use auto.arima() function to fit ARIMA model.
# Use summary() to show auto ARIMA model and its parameters.
train.auto.arima <- auto.arima(train.ts)
summary(train.auto.arima)


# Apply forecast() function to make predictions for ts with 
# auto ARIMA model in validation set.  
train.auto.arima.pred <- forecast(train.auto.arima, h = nValid, level = 0)
train.auto.arima.pred        

# Using Acf() function, create autocorrelation chart of auto ARIMA 
# model residuals.
Acf(train.auto.arima$residuals, lag.max = 12, 
    main = "Autocorrelations of Auto ARIMA Model Residuals")

# Plot ts data, Auto ARIMA model, and predictions for validation period.
plot(train.auto.arima.pred, 
     xlab = "Time", ylab = "Sales Values (in 000s)", 
     ylim = c(6000, 26000), xaxt = "n",
     bty = "l", xlim = c(1992, 2027), 
     main = "Auto ARIMA Model", lwd = 2, flty = 5) 
axis(1, at = seq(1992, 2027, 1), labels = format(seq(1992, 2027, 1)))
lines(train.arima.seas.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lwd = 2, lty = 1)
legend(1992,26000, legend = c("Yearly Food Sales", 
                              "Auto ARIMA Forecast for Training Period",
                              "Auto ARIMA Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")
# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2019, 2019), c(0, 26000))
lines(c(2024, 2024), c(0, 26000))
text(2001, 26000, "Training")
text(2021, 26000, "Validation")
text(2026, 26000, "Future")
arrows(1992, 25000, 2019, 25000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019, 25000, 2024, 25000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2024, 25000, 2027, 25000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#comparing of arima models
round(accuracy(train.arima.seas.pred$mean, valid.ts), 3)
round(accuracy(train.auto.arima.pred$mean, valid.ts), 3)

# Using Arima() function to fit ARIMA(1,1,1)(1,1,1) model for 
# trend and seasonality for entire data set.
arima.seas <- Arima(value.ts, order = c(1,1,1), 
                    seasonal = c(1,1,1)) 
summary(arima.seas)

# Apply forecast() function to make predictions for ts with 
# seasonal ARIMA model for the future 24 periods. 
arima.seas.pred <- forecast(arima.seas, h = 24, level = 0)
arima.seas.pred

# Use Acf() function to create autocorrelation chart of seasonal ARIMA 
# model residuals.
Acf(arima.seas$residuals, lag.max = 12, 
    main = "Autocorrelations of Seasonal ARIMA (1,1,1)(1,1,1) Model Residuals")


# Plot historical data, predictions for historical data, and seasonal 
# ARIMA forecast for 24 future period using entire data set.
plot(value.ts, 
     xlab = "Time", ylab = "Sales Values (in 000s)", 
     ylim = c(6000, 26000), xaxt = "n",
     bty = "l", xlim = c(1992, 2027), lwd = 2,
     main = "Seasonal ARIMA(1,1,1)(1,1,1) Model for Entire Data Set") 
axis(1, at = seq(1992, 2027, 1), labels = format(seq(1992, 2027, 1)))
lines(arima.seas$fitted, col = "blue", lwd = 2)
lines(arima.seas.pred$mean, col = "blue", lty = 5, lwd = 2)
legend(1993,25000, legend = c("Food Sales Time Series", 
                              "Seasonal ARIMA Forecast", 
                              "Seasonal ARIMA Forecast for 24 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")
# Plot on chart vertical lines and horizontal arrows describing
# entire data set and future prediction intervals.
lines(c(2024, 2024), c(0, 25000))
text(2007, 26000, "Entire Data Set")
text(2025.5, 26000, "Future")
arrows(1992, 25000, 2024, 25000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2024, 25000, 2027, 25000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Auto Arima
# Use auto.arima() function to fit ARIMA model for entire data set.
auto.arima <- auto.arima(value.ts)
summary(auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model for the future 24 periods. 
auto.arima.pred <- forecast(auto.arima, h = 24, level = 0)
auto.arima.pred

# Use Acf() function to create autocorrelation chart of auto ARIMA 
# model residuals.
Acf(auto.arima$residuals, lag.max = 12, 
    main = "Autocorrelations of Auto ARIMA Model Residuals")

# Plot historical data, predictions for historical data, and seasonal 
# Auto ARIMA forecast for 24 future period using entire data set.
plot(value.ts, 
     xlab = "Time", ylab = "Sales Values (in 000s)", 
     ylim = c(6000, 26000), xaxt = "n",
     bty = "l", xlim = c(1992, 2027), lwd = 2,
     main = "Auto ARIMA(3,2,4)(0,0,2)[12] Model for Entire Data Set") 
axis(1, at = seq(1992, 2027, 1), labels = format(seq(1992, 2027, 1)))
lines(auto.arima$fitted, col = "blue", lwd = 2)
lines(auto.arima.pred$mean, col = "blue", lty = 5, lwd = 2)
legend(1993,25000, legend = c("Food Sales Time Series", 
                              "Auto ARIMA Forecast", 
                              "Auto ARIMA Forecast for 24 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")
# Plot on chart vertical lines and horizontal arrows describing
# entire data set and future prediction intervals.
lines(c(2024, 2024), c(0, 25000))
text(2007, 26000, "Data Set")
text(2025.5, 26000, "Future")
arrows(1992, 25000, 2024, 25000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2024, 25000, 2027, 25000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

round(accuracy(arima.seas.pred$fitted, value.ts), 3)
round(accuracy(auto.arima.pred$fitted, value.ts), 3)
round(accuracy((snaive(value.ts))$fitted, value.ts), 3)





