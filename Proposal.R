
# Proposal R

### 0) load libraries

library(quantmod)
library(tidyverse)
library(reshape2)
library(moments)
library(tseries)
library(xts)
library(plm)


### 1) gather data

# Swiss companies; SMI will be replaced with another more comparable index
swiss_tickers <- c("^SSMI", "UBSG.SW", "NOVN.SW", "NESN.SW", "ABBN.SW", "ROG.SW", "ZURN.SW", "SGSN.SW", "GIVN.SW")

start_date <- as.Date("2018-01-01")
end_date <- as.Date("2023-12-31")


all_stocks_data <- list()

for(ticker in swiss_tickers) {

  all_stocks_data[[ticker]] <- tryCatch({
    temp <- getSymbols(ticker, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
    Ad(temp) # adjusted closing price
  }, error = function(e) {
    cat("Error downloading data for ticker:", ticker, "\n")
    NULL # Return NULL in case of error
  })
}

# price df
swiss_price_nondf <- do.call(merge, all_stocks_data)
names(swiss_price_nondf) <- gsub(".Adjusted", "", names(swiss_price_nondf))
swiss_price <- as.data.frame(swiss_price_nondf)

# return df
swiss_returns_nondf <- diff(log(swiss_price_nondf))[-1,]
swiss_returns <- as.data.frame(swiss_returns_nondf)

# volatility df
swiss_vol <- sqrt(swiss_returns^2)


### 2) create descriptive statistics

# Function to calculate statistical summary
calculate_summary <- function(data) {
  
  if (!is.data.frame(data)) { stop("Input data is not a dataframe.") }
  
  if (!any(sapply(data, is.numeric))) { stop("Dataframe does not contain any numeric columns.") }
  
  summary_df <- data.frame(
    Mean = sapply(data, function(x) mean(x, na.rm = TRUE)),
    Median = sapply(data, function(x) median(x, na.rm = TRUE)),
    SD = sapply(data, function(x) sd(x, na.rm = TRUE)),
    Variance = sapply(data, function(x) var(x, na.rm = TRUE)),
    CV = sapply(data, function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)), # coefficient of variance
    Skewness = sapply(data, function(x) ifelse(length(unique(x[!is.na(x)]))>1, skewness(x, na.rm = TRUE), NA)),
    Kurtosis = sapply(data, function(x) ifelse(length(unique(x[!is.na(x)]))>1, kurtosis(x, na.rm = TRUE), NA)), # Normal distribution has Kurtosis of 3
    Jarque_Bera_Test = sapply(data, function(x) ifelse(length(unique(x[!is.na(x)])) > 3, jarque.bera.test(na.omit(x))$p.value, NA)),
    Q1 = sapply(data, function(x) quantile(x, 0.25, na.rm = TRUE)),
    Q3 = sapply(data, function(x) quantile(x, 0.75, na.rm = TRUE)),
    Percentile10 = sapply(data, function(x) quantile(x, 0.10, na.rm = TRUE)),
    Percentile90 = sapply(data, function(x) quantile(x, 0.90, na.rm = TRUE)),
    IQR = sapply(data, function(x) IQR(x, na.rm = TRUE)),
    Min = sapply(data, function(x) min(x, na.rm = TRUE)),
    Max = sapply(data, function(x) max(x, na.rm = TRUE)),
    Range = sapply(data, function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE)),
    N_Obs = sapply(data, function(x) length(x)),
    N_NA = sapply(data, function(x) sum(is.na(x))),
    Zeroes_Count = sapply(data, function(x) sum(x == 0, na.rm = TRUE)),
    Positive_Count = sapply(data, function(x) sum(x > 0, na.rm = TRUE)),
    Negative_Count = sapply(data, function(x) sum(x < 0, na.rm = TRUE)),
    AR1 = sapply(data, function(x) ifelse(length(na.omit(x)) > 1 && length(unique(na.omit(x))) > 1, acf(na.omit(x), lag.max = 1, plot = F)[["acf"]][2], NA))
    
  )
  return(summary_df)
}



# Calculate summaries
price_summary <- calculate_summary(swiss_price)
return_summary <- calculate_summary(swiss_returns)
vol_summary <- calculate_summary(swiss_vol)

# Print summaries
print("Statistical Summary for Price Data:")
print(price_summary)

print("Statistical Summary for Return Data:")
print(return_summary)

print("Statistical Summary for Volatitlity Data:")
print(vol_summary)


### 4) test for startionarity

test_stationarity <- function(data) {
  
  results <- data.frame(
    Column = colnames(data),
    ADF_Statistic = numeric(ncol(data)),
    ADF_P_Value = numeric(ncol(data)),
    ADF_Decision = character(ncol(data)),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(colnames(data))) {
    column_data <- na.omit(data[[i]])  # Remove NAs
    if (length(column_data) > 1) {  # Check if there's enough data after removing NAs
      # Perform Augmented Dickey-Fuller test
      adf_result <- adf.test(column_data, alternative = "stationary")
      
      # Fill in results
      results[i, c("ADF_Statistic", "ADF_P_Value", "ADF_Decision")] <- list(
        adf_result$statistic,
        adf_result$p.value,
        ifelse(adf_result$p.value < 0.05, "Stationary", "Not Stationary")  # Decision based on alpha = 0.05
      )
    } else {
      results[i, c("ADF_Statistic", "ADF_P_Value", "ADF_Decision")] <- list(NA, NA, "Insufficient Data")
    }
  }
  
  return(results)
}

test_stationarity(swiss_price)
test_stationarity(swiss_returns)
test_stationarity(swiss_vol)


### 5) create volatiltiy residuals after regressing index vol on stock vol

models_summary <- data.frame(Column = character(), Beta_0 = numeric(), Beta_1 = numeric(), R_squared = numeric(), stringsAsFactors = FALSE)
residuals_list <- list()

# Run regression
for(i in 2:ncol(swiss_vol)){
  model <- lm(swiss_vol[,i] ~ swiss_vol$SSMI, data = swiss_vol)
  
  coef <- coef(model)
  r_squared <- summary(model)$r.squared
  models_summary <- rbind(models_summary, data.frame(Column = names(swiss_vol)[i], Intercept = coef[1], Slope = coef[2], R_squared = r_squared))
  
  residuals_list[[names(swiss_vol)[i]]] <- residuals(model)
}


residuals_nondf <- do.call(cbind, residuals_list)
residuals_df <- as.data.frame(residuals_nondf)
head(residuals_df)


### 6) get information df for final regression

# This data frame consists not the final true values, but just some random values for illustration of the code
stocks_info <- data.frame(
  Stock = swiss_tickers[-1],
  ESG_Rating = c(75, 80, 65, 70, 85, 90, 60, 77), # Made-up ESG ratings
  Control_Variables = c(1.2, 0.8, 0.9, 1.1, 0.95, 1.3, 0.7, 1.05) # Made-up Control Variables
)


### 7) regress ESG & control variable on volatility residuals

## Appraoch 1: linear regression
mean_residuals <- apply(residuals_df, 2, mean)

volatility_df <- data.frame(Stock = names(mean_residuals), Volatility = mean_residuals)
regression_df <- merge(stocks_info, volatility_df, by = "Stock")

model <- lm(Volatility ~ ESG_Rating + Control_Variables, data = regression_df)

summary(model)


## Appraoch 2: panel data regression using yearly data and fixed effects
yearly_mean_residuals <- apply.yearly(residuals_df, colMeans)
yearly_mean_residuals$Year <- format(as.Date(rownames(yearly_mean_residuals)), "%Y")

yearly_mean_residuals_long <- melt(yearly_mean_residuals, id.vars = "Year", variable.name = "Stock", value.name = "Residual")
panel_data <- merge(yearly_mean_residuals_long, stocks_info, by = "Stock")
panel_data_p <- pdata.frame(panel_data, index = c("Stock", "Year"))

model_plm <- plm(Residual ~ ESG_Rating + Control_Variables, data = panel_data_p, model = "within", effect = "time")

summary(model_plm)

### 8) Further steps
# Interpret findings, create robust SE, check residuals, ...
