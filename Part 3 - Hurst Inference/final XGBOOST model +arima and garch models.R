# Load necessary libraries
library(data.table)
library(xgboost)
library(tseries)

#XGBoosted Model 1
# Ensure both data frames have the 'date' column in Date format
economic_indicators_historical_full$date <- as.Date(economic_indicators_historical_full$date)
df_1<-economic_indicators_historical_full
# Merge the two data frames on the 'date' column
model_data_1 <- cbind(
  df_1$`CBOE Gold ETF VIX Index`,
  df_1$`Economic Growth(%)`, 
  df_1$`change in Consumer loans`,
  df_1$`Existing Home Sales Vol`,
  df_1$`All Employees, Total NonFarm Vol`,
  df_1$`US Dollar Index`,
  df_1$`10-Year Treasury Bond Yield`,
  df_1$`returns S&P 500`,
  df_1$`Returns NASDAQ`,
  df_1$`Vader Score`
)

# Convert to data.table and assign exact column names
model_data_1 <- as.data.table(model_data_1)
names(model_data_1)
setnames(model_data_1, c(
  "CBOE Gold ETF VIX Index",
  "Economic Growth(%)",
  "change in Consumer loans",
  "Existing Home Sales Vol",
  "All Employees, Total NonFarm Vol",
  "US Dollar Index",
  "10-Year Treasury Bond Yields",
  "returns S&P 500",
  "Returns NASDAQ",
  "Vader Score"
))

# Step 3: Select the necessary features for the model
# We'll use 'CBOE Gold ETF VIX', 'US Dollar Index Returns (%)', 'NASDAQ Volatility', 'US Dollar Index Volatility', 
# 'Existing Home Sales Volatility', 'Consumer Loans Volatility', 'Economic Growth(%)' along with 'vader_score'
names(df)


# Remove rows with missing values (if any)
model_data_1 <- na.omit(model_data_1)
names(model_data_1)

# Step 4: Prepare the feature matrix (X) and the target vector (y)
X1 <- as.matrix(model_data_1[, -8, with = FALSE])  # Remove the target variable (returns_S_P_500)
y1 <- model_data_1$`returns S&P 500`  # The target variable

# Step 5: Set up the XGBoost model
dtrain <- xgb.DMatrix(data = X1, label = y1)

# Step 6: Define the parameters for the XGBoost model
param <- list(
  booster = "gbtree",
  objective = "reg:squarederror",  # Regression task
  eta = 0.1,  # Learning rate
  max_depth = 6,  # Maximum tree depth
  nrounds = 100  # Number of boosting rounds
)

# Step 7: Train the XGBoost model
model_1 <- xgb.train(param, dtrain, nrounds = param$nrounds)

# Step 9: Evaluate the model using RMSE (Root Mean Square Error)
rmse1 <- sqrt(mean((predictions - y)^2))

cat("RMSE:", rmse1, "\n")


# Step 10: Evaluate the model using MAE (Mean Absolute Error)
mae1 <- mean(abs(predictions - y))
cat("MAE:", mae1, "\n")

# Step 12: Optionally, plot the predicted vs actual values
plot(y1, predictions, main = "Predicted vs Actual S&P 500 Returns", 
     xlab = "Actual Values", ylab = "Predicted Values", 
     col = "blue", pch = 19)
abline(0, 1, col = "red", lty = 2)  # Add a reference line for perfect prediction (y = x)

# Step 13: Get the feature importance from the trained model
importance_matrix_1 <- xgb.importance(model = model_1)

# Step 14: Plot the feature importance
xgb.plot.importance(importance_matrix = importance_matrix_1)

# Function to predict next 10 days of S&P 500 returns using XGBoost without looping


# Example usage: Assuming you have a trained XGBoost model 'model' and historical data 'data'
# predictions <- predict_sp500_returns(model, data)

# If you have the model and data, you can print the predicted returns



#Cointegrated ARIMA Model
library(urca)

names(model_data_1)
# Select relevant variables for cointegration test (returns S&P 500 and other economic indicators)
cointegration_data <- model_data_1[, c("CBOE Gold ETF VIX Index", "Economic Growth(%)",             
                                        "change in Consumer loans", "Existing Home Sales Vol",        
                                        "All Employees, Total NonFarm Vol", "US Dollar Index" ,               
                                        "10-Year Treasury Bond Yields","returns S&P 500",                 
                                        "Returns NASDAQ", "Vader Score")]

# Perform Johansen cointegration test
johansen_test <- ca.jo(cointegration_data, type = "trace", K = 2, spec = "longrun")
summary(johansen_test)


# Assuming 'ect' is the variable containing the cointegrated residuals (ECT)

#Returns S&P 500 and CBOE Gold ETF VIX Index:
  
 # returns.S.P.500.l2 (lagged returns of S&P 500) and CBOE.Gold.ETF.VIX.Index.l2 (lagged CBOE Gold ETF VIX Index) have a strong positive relationship with a coefficient of 1.000000000.

#These two series move together in the long term, suggesting they are cointegrated.

#Returns S&P 500 and US Dollar Index Volatility:
  
 # returns.S.P.500.l2 and US.Dollar.Index.Volatility.5.day.l2 have a weak positive relationship with a coefficient of 0.001347179 and 0.003414598, respectively, but still, their slight correlation indicates potential long-term movement together in some capacity.

#Returns S&P 500 and NASDAQ Volatility:
  
 # returns.S.P.500.l2 and NASDAQ.Vol.5.day.l2 show a negative relationship with coefficients of 0.024626439 and -0.675895280, respectively. This suggests an inverse relationship between the two, though still a long-term equilibrium relationship.

#Returns S&P 500 and 10-Year Treasury Bond Yield:
  
#  returns.S.P.500.l2 and X10.Year.Treasury.Bond.Yield.l2 have a positive relationship with coefficients of 0.337461789 and 0.162850206, respectively, suggesting that they are cointegrated.

#Returns S&P 500 and Existing Home Sales Volatility:
  
#  returns.S.P.500.l2 and Existing.Home.Sales.Vol.l2 have a negative relationship with coefficients of -0.001007666 and 0.078797803, indicating a weak correlation but still a cointegrated pair.

#Returns S&P 500 and Consumer Loans Volatility:
  
 # returns.S.P.500.l2 and Consumer.loans.Vol.l2 show a weak negative relationship with coefficients of -0.006126054 and 6.029531568, respectively, suggesting a cointegrated relationship.

#Returns S&P 500 and Economic Growth:
  
#  returns.S.P.500.l2 and Economic.Growth....l2 show a negative relationship with coefficients of -0.137810661 and 30.955134900, indicating a long-term equilibrium but inverse relationship.

#Returns S&P 500 and Vader Score:
  
 # returns.S.P.500.l2 and Vader.Score.l2 show a weak positive relationship with coefficients of 0.148697983 and -0.363320431, respectively. This suggests a mild connection, potentially significant for long-term analysis.

#Returns S&P 500 and S&P 500 VIX:
  
#  returns.S.P.500.l2 and S.P.500.VIX.l2 show a negative relationship with coefficients of -0.040599047 and 0.636605358, respectively, suggesting that these two variables could be in a long-term inverse relationship.

#Returns S&P 500 and CPI (Consumer Price Index):
  
 # returns.S.P.500.l2 and CPI..Consumer.Price.Index..l2 show a weak negative relationship with coefficients of -0.007251848 and 0.090015465, indicating some level of long-term co-movement.

#Model 2
# Load data
df <- economic_indicators_historical_full

# Convert date to Date format
df$date <- as.Date(df$date)

names(df)

# Bind selected columns into a new data frame
model_data <- cbind(
  df$`Hurst Exponent`,
  df$`CBOE Gold ETF VIX Index`,
  df$`Economic Growth(%)`, 
  df$`change in Consumer loans`,
  df$`Existing Home Sales Vol`,
  df$`All Employees, Total NonFarm Vol`,
  df$`US Dollar Index`,
  df$`10-Year Treasury Bond Yield`,
  df$`returns S&P 500`,
  df$`Returns NASDAQ`,
  df$`Vader Score`,
  df$date
)

# Convert to data.table and assign exact column names
model_data <- as.data.table(model_data)
names(model_data)
setnames(model_data, c(
  "Hurst Exponent",
  "CBOE Gold ETF VIX Index",
  "Economic Growth(%)",
  "change in Consumer loans",
  "Existing Home Sales Vol",
  "All Employees, Total NonFarm Vol",
  "US Dollar Index",
  "10-Year Treasury Bond Yields",
  "returns S&P 500",
  "Returns NASDAQ",
  "Vader Score",
  "date"
))

# Apply na.omit after naming
model_data <- na.omit(model_data)

# Prepare data for XGBoost
X <- as.matrix(model_data[, -1, with = FALSE])  # features
y <- model_data$`Hurst Exponent`                # target

dtrain <- xgb.DMatrix(data = X, label = y)

# Model parameters
param <- list(
  booster = "gbtree",
  objective = "reg:squarederror",
  eta = 0.1,
  max_depth = 6
)

# Train the model
model <- xgb.train(params = param, data = dtrain, nrounds = 100)

# Predictions
predictions <- predict(model, X)

# Evaluation
rmse <- sqrt(mean((predictions - y)^2))
mae <- mean(abs(predictions - y))
cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")

# Plot predicted vs actual
plot(y, predictions,
     main = "Predicted vs Actual Hurst Exponent",
     xlab = "Actual", ylab = "Predicted",
     col = "blue", pch = 19)
abline(0, 1, col = "red", lty = 2)

# Feature importance
importance_matrix <- xgb.importance(model = model)
xgb.plot.importance(importance_matrix)

# Forecast next 20 days using recent rows
recent_data <- tail(model_data, 20)
future_X <- as.matrix(recent_data[, -1, with = FALSE])
future_preds <- predict(model, future_X)

forecast_dates <- seq.Date(from = Sys.Date() + 1, by = "days", length.out = 20)
forecast_df <- data.table(
  Date = forecast_dates,
  Predicted_Hurst_Exponent = future_preds
)

plot(df$date, df$`Hurst Exponent`, type = "l", col = "blue", lwd = 2,
     xlab = "Date", ylab = "Hurst Exponent",
     main = "Historical Hurst Exponent (True Values)")

# --- Plot 2: Predicted Hurst Exponent (Fitted) ---
plot(df$date, predictions, type = "l", col = "darkorange", lwd = 2,
     xlab = "Date", ylab = "Predicted Hurst",
     main = "Historical Predicted Hurst Exponent (Fitted Values)")

# Forecast next 20 days using recent data
recent_data <- tail(model_data, 50)
future_X <- as.matrix(recent_data[, -c("Hurst Exponent"), with = FALSE])
future_preds <- predict(model, future_X)
forecast_dates <- seq.Date(from = Sys.Date() + 1, by = "days", length.out =50)
forecast_df <- data.table(
  Date = forecast_dates,
  Predicted_Hurst_Exponent = future_preds
)

# Plot 4: Forecasted Hurst Exponent (Next 20 Days)
plot(forecast_df$Date, forecast_df$Predicted_Hurst_Exponent,
     type = "l", col = "darkgreen", lwd = 2,
     xlab = "Forecast Date", ylab = "Predicted Hurst Exponent",
     main = "Forecasted Hurst Exponent for Next 50 Days")
abline(h = 0.5, col = "red", lty = 2)  # Reference line at H=0.5

print(forecast_df$Predicted_Hurst_Exponent)

plot(forecast_df$Date,forecast_df$Predicted_Hurst_Exponent)



