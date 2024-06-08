
# Control Variables

library(quantmod)
library(tidyverse)
library(reshape2)
library(moments)
library(tseries)
library(xts)
library(plm)
library(readxl)
library(zoo)

CV <- read_excel("ESG_Data.xlsx", sheet = "Control_Data")
names(CV)
summary(CV)
head(CV)
CV$Year <- rep(2022:2018, nrow(CV)/5)
# View(CV)

CV$"KNRS.S" <- NA

# Pivot the dataframe
CV_pivoted <- pivot_longer(CV,
                            cols = -c("Year", "RIC"),
                            names_to = "Company",
                            values_to = "Value")

CV_pivoted_wider <- pivot_wider(CV_pivoted,
                                 names_from = RIC,
                                 values_from = Value)


print(CV_pivoted_wider)

CV_data <- read_excel("Data_CV.xlsx", sheet = "Sheet1")
names(CV_data)
summary(CV_data)
head(CV_data)

names(CV_data)[1:2] <- c("Year", "Company")

CV_data_cleaner <- CV_data[,-c(3:15)]

head(CV_data_cleaner)

CV_data_mutated <- CV_data_cleaner %>% 
  mutate(ROA = `Net Income After Taxes` / `Total Assets, Reported`,
         Leverage = `Total Debt` / `Total Assets, Reported`,
         DivYield = `Dividend yield`,
         TotalAssets = `Total Assets, Reported`,
         CurrentRatio = `Current Ratio`) %>% 
  filter(Year >= 2016 & Year <= 2022) %>% 
  select(Year, Company, ROA, Leverage, DivYield, TotalAssets, CurrentRatio)


CV_data_mutated_ROAsd <- CV_data_cleaner %>% 
  mutate(ROA = `Net Income After Taxes` / `Total Assets, Reported`,
         Leverage = `Total Debt` / `Total Assets, Reported`,
         DivYield = `Dividend yield`,
         TotalAssets = `Total Assets, Reported`,
         CurrentRatio = `Current Ratio`) %>% 
  group_by(Company) %>% 
  arrange(Year) %>% 
  mutate(ROA_sd_4yr = rollapply(ROA, width = 4, FUN = sd, align = "right", fill = NA, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(Year >= 2018 & Year <= 2022) %>% 
  select(Year, Company, ROA, Leverage, DivYield, TotalAssets, CurrentRatio, ROA_sd_4yr)



# CV_final <- CV_pivoted_wider
CV_final <- CV_data_mutated
CV_final <- CV_data_mutated_ROAsd

head(CV_final)


