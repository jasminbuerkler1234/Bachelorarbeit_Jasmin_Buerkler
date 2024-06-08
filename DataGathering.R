
# Proposal R

### 0) load libraries

library(quantmod)
library(tidyverse)
library(reshape2)
library(moments)
library(tseries)
library(xts)
library(plm)
library(readxl)


### 1) gather data

# Swiss companies; SMI will be replaced with another more comparable index
company <- read_excel("ESG_Data.xlsx", sheet = "Company_Data")
company_filtered <- subset(company, Name != 0)
swiss_tickers <- sort(paste0(company_filtered$Name, "W"))
# swiss_tickers <- c("^SSMI", "UBSG.SW", "NOVN.SW", "NESN.SW", "ABBN.SW", "ROG.SW", "ZURN.SW", "SGSN.SW", "GIVN.SW")

start_date <- as.Date("2017-12-31")
end_date <- as.Date("2022-12-31")


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

spi_index <- read_excel("ESG_Data.xlsx", sheet = "SPI_Data")
names(spi_index) <- c("Date", "SPI", "RF")

# price df
swiss_price_nondf <- do.call(merge, all_stocks_data)
names(swiss_price_nondf) <- gsub(".Adjusted", "", names(swiss_price_nondf))
swiss_price <- as.data.frame(swiss_price_nondf)
names(swiss_price)
swiss_price$Date <- as.Date(rownames(swiss_price))

swiss_price_final <- left_join(swiss_price, spi_index, by = "Date")

# reorder df
swiss_price_final <- subset(swiss_price_final, select = c("Date", "SPI", "RF", setdiff(names(swiss_price_final), c("Date", "SPI", "RF"))))
head(swiss_price_final)


# remove all stocks that have NA's in our data range
na_counts <- colSums(is.na(swiss_price_final))

print(na_counts)
sum(na_counts > 0)

swiss_price_final <- swiss_price_final[,na_counts == 0]

na_counts2 <- colSums(is.na(swiss_price_final))

print(na_counts2)
sum(na_counts2 > 0)

df <- swiss_price_final


date_column <- df[, 1]
rf_column <- df[,3]


log_returns_df <- df[-1,]
log_returns_df[, -c(1,3)] <- lapply(df[, -c(1,3)], function(x) diff(log(x)))

# Add back the first column ("Date")
log_returns_df$Date <- date_column[-1]
log_returns_df$RF <- rf_column[-1]
# Print the first few rows of the log returns dataframe
head(log_returns_df)

swiss_returns <- subset(log_returns_df, select = c("Date", "SPI", "RF", setdiff(names(log_returns_df), c("Date", "SPI", "RF"))))
head(swiss_returns)

na_counts3 <- colSums(is.na(swiss_returns))

print(na_counts3)
sum(na_counts3 > 0)


dim(swiss_returns)

# volatility df
swiss_returns$Year <- format(swiss_returns$Date, "%Y")

### risk measures
# total risk
total_risk_short <- swiss_returns[, -which(names(swiss_returns) == "RF")] %>%
  group_by(Year) %>%
  summarise(across(everything(), ~ sd(.x, na.rm = TRUE))) # Standard Deviation for volatility

# Pivot to longer format
total_risk <- total_risk_short[, !names(total_risk_short) %in% c("Date", "SPI")] %>% 
  pivot_longer(cols = -Year, names_to = "Stock", values_to = "Total_Risk")



swiss_returns_CAPM <- swiss_returns[, -which(names(swiss_returns) == "Date")]

swiss_returns_CAPM <- swiss_returns_CAPM %>%
  select(Year, RF, everything())

names(swiss_returns_CAPM)[3] <- "Index"

swiss_returns_CAPM$RF <- (1 + swiss_returns_CAPM$RF / 100)^(1/365) - 1

# Initialize lists to store annual beta values and residuals for each stock
annual_betas <- list()
annual_residuals <- list()

# Loop through each stock column starting from the third column
for (i in 4:ncol(swiss_returns_CAPM)) {
  stock_name <- colnames(swiss_returns_CAPM)[i]
  
  # Create a temporary dataframe for each stock with necessary columns
  stock_data <- swiss_returns_CAPM %>%
    mutate(market_premium = Index - RF) %>% 
    select(Year, stock = stock_name, market_premium)
  
  # Group by year and run CAPM regression, extracting beta and storing residuals
  annual_data <- stock_data %>%
    group_by(Year) %>%
    do({
      model <- lm(stock ~ market_premium, data = .)
      residuals <- resid(model)
      beta <- coef(model)[2]
      data.frame(Year = unique(.$Year), Beta = beta, Idiosyncratic = sd(residuals))
    }) %>%
    ungroup()
  
  # Store the beta and residuals data in the lists with stock name as the key
  annual_betas[[stock_name]] <- annual_data %>% select(Year, Beta)
  annual_residuals[[stock_name]] <- annual_data %>% select(Year, Idiosyncratic)
}

# Combine all betas into a single dataframe
betas_df <- bind_rows(annual_betas, .id = "Company")
# Combine all residuals into a single dataframe
residuals_df <- bind_rows(annual_residuals, .id = "Company")

# Print results
print(betas_df)
print(residuals_df)

total_risk_final <- total_risk
names(total_risk_final) <- c("Year", "Company", "Total_Risk")


DependentVar <- total_risk_final %>%
  full_join(betas_df, by = c("Company", "Year")) %>%
  full_join(residuals_df, by = c("Company", "Year"))

DependentVar$Year <- as.integer(DependentVar$Year)
DependentVar <- DependentVar %>%
  mutate(Company = gsub("\\.SW", ".S", Company))

dim(DependentVar)
summary(DependentVar)
head(DependentVar)
# View(DependentVar)



ggplot(DependentVar, aes(x = as.factor(Year), y = Total_Risk)) +
  geom_boxplot() +
  labs(
    title = "Total Risk by Year",
    x = "Year",
    y = "Total Risk"
  ) +
  theme_minimal()


ggplot(DependentVar, aes(x = as.factor(Year), y = Beta)) +
  geom_boxplot() +
  labs(
    title = "Systematic Risk by Year",
    x = "Year",
    y = "Beta"
  ) +
  theme_minimal()


ggplot(DependentVar, aes(x = as.factor(Year), y = Idiosyncratic)) +
  geom_boxplot() +
  labs(
    title = "Idiosyncratic Risk by Year",
    x = "Year",
    y = "Idiosyncratic Risk"
  ) +
  theme_minimal()


# Calculate median scores by year
median_scores_risk <- DependentVar %>%
  group_by(Year) %>%
  summarise(
    median_total = median(Total_Risk, na.rm = TRUE)*100,
    median_beta = median(Beta, na.rm = TRUE),
    median_idiosyncratic = median(Idiosyncratic, na.rm = TRUE)*100
  ) %>%
  pivot_longer(cols = starts_with("median"), names_to = "risk_type", values_to = "median_value")

# Create the line plot
ggplot(median_scores_risk, aes(x = Year, y = median_value, color = risk_type)) +
  geom_line() +
  labs(
    title = "Median Risk Over the Years",
    x = "Year",
    y = "Median Score",
    color = "Risk Type"
  ) +
  theme_minimal()


