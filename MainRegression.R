
# Regression main
library(lfe)
library(tidyverse)
library(car)
library(reshape2)
library(car)
library(stargazer)
library(officer)
library(lmtest)
library(sandwich)
library(clubSandwich)
library(plm)
#felm (dep ~ indepe + CV | Fixed Effects | Instrumental Variables | Clustered SE, data)
# eg. result <- felm(GDP_growth ~ Investment + Pop_growth | Year + Industry | 0 | Country, data = df)
# robust_se <- coeftest(result, vcov = vcovHC(result, type = 'HC0'))
# bp_test <- bptest(result)
# print(bp_test)

head(felm_df)

##### Total Risk ##### 
lm_totalRisk_s <- felm(Beta ~ `ESG.Score` | 0 | 0 | 0, felm_df)
summary(lm_totalRisk_s)

lm_totalRisk_s_fe <- felm(Beta ~ `ESG.Score`
                          | Year | 0 | 0, felm_df)
summary(lm_totalRisk_s_fe)

lm_totalRisk_m <- felm(Beta ~ `ESG.Score` + ROA + Leverage + DivYield + I(log(TotalAssets)) + CurrentRatio
                       | 0 | 0 | 0, felm_df)
summary(lm_totalRisk_m)

lm_totalRisk_m_fe <- felm(Beta ~ `ESG.Score` + ROA + Leverage + DivYield + I(log(TotalAssets)) + CurrentRatio
                          | Year | 0 | 0, felm_df)
summary(lm_totalRisk_m_fe)



# Extract coefficients and robust standard errors
robust_se_totalRisk_s <- coeftest(lm_totalRisk_s, vcov = vcovHC(lm_totalRisk_s, type = 'HC1'))
robust_se_totalRisk_s_fe <- coeftest(lm_totalRisk_s_fe, vcov = vcovHC(lm_totalRisk_s_fe, type = 'HC1'))
robust_se_totalRisk_m <- coeftest(lm_totalRisk_m, vcov = vcovHC(lm_totalRisk_m, type = 'HC1'))
robust_se_totalRisk_m_fe <- coeftest(lm_totalRisk_m_fe, vcov = vcovHC(lm_totalRisk_m_fe, type = 'HC1'))

# Create a stargazer table with robust standard errors
stargazer(lm_totalRisk_s, lm_totalRisk_s_fe, lm_totalRisk_m, lm_totalRisk_m_fe,
          type = "text",
          title = "Regression Results with Robust Standard Errors",
          dep.var.labels = "Beta",
          covariate.labels = c("ESG Score", "ROA", "Leverage", "Dividend Yield", "ln(Total Assets)", "Current Ratio"),
          column.labels = c("Model 1", "Model 2", "Model 3", "Model 4"),
          model.numbers = FALSE,
          se = list(robust_se_totalRisk_s[, "Std. Error"],
                    robust_se_totalRisk_s_fe[, "Std. Error"],
                    robust_se_totalRisk_m[, "Std. Error"],
                    robust_se_totalRisk_m_fe[, "Std. Error"]),
          add.lines = list(c("Fixed Effects", "No", "Year", "No", "Year")),
          keep.stat = c("n", "rsq", "adj.rsq"), out = paste0("output/robust_Beta.txt"),
          digits = 3)


##### Idiosyncratic ##### 
lm_Idiosyncratic_s <- felm(Idiosyncratic ~ `ESG.Score` | 0 | 0 | 0, felm_df)
summary(lm_Idiosyncratic_s)

lm_Idiosyncratic_s_fe <- felm(Idiosyncratic ~ `ESG.Score`
                              | Year | 0 | 0, felm_df)
summary(lm_Idiosyncratic_s_fe)

lm_Idiosyncratic_m <- felm(Idiosyncratic ~ `ESG.Score` + ROA + Leverage + DivYield + I(log(TotalAssets)) + CurrentRatio
                           | 0 | 0 | 0, felm_df)
summary(lm_Idiosyncratic_m)

lm_Idiosyncratic_m_fe <- felm(Idiosyncratic ~ `ESG.Score` + ROA + Leverage + DivYield + I(log(TotalAssets)) + CurrentRatio
                              | Year | 0 | 0, felm_df)
summary(lm_Idiosyncratic_m_fe)



# Extract coefficients and robust standard errors
robust_se_Idio_s <- coeftest(lm_Idiosyncratic_s, vcov = vcovHC(lm_Idiosyncratic_s, type = 'HC1'))
robust_se_Idio_s_fe <- coeftest(lm_Idiosyncratic_s_fe, vcov = vcovHC(lm_Idiosyncratic_s_fe, type = 'HC1'))
robust_se_Idio_m <- coeftest(lm_Idiosyncratic_m, vcov = vcovHC(lm_Idiosyncratic_m, type = 'HC1'))
robust_se_Idio_m_fe <- coeftest(lm_Idiosyncratic_m_fe, vcov = vcovHC(lm_Idiosyncratic_m_fe, type = 'HC1'))

# Create a stargazer table with robust standard errors
stargazer(robust_se_Idio_s, robust_se_Idio_s_fe, robust_se_Idio_m, robust_se_Idio_m_fe,
          type = "text",
          title = "Regression Results with Robust Standard Errors",
          dep.var.labels = "Idiosyncratic",
          covariate.labels = c("ESG Score", "ROA", "Leverage", "Dividend Yield", "ln(Total Assets)", "Current Ratio"),
          column.labels = c("Model 1", "Model 2", "Model 3", "Model 4"),
          model.numbers = FALSE,
          se = list(robust_se_Idiosyncratic_s[, "Std. Error"],
                    robust_se_Idiosyncratic_s_fe[, "Std. Error"],
                    robust_se_Idiosyncratic_m[, "Std. Error"],
                    robust_se_Idiosyncratic_m_fe[, "Std. Error"]),
          add.lines = list(c("Fixed Effects", "No", "Year", "No", "Year")),
          keep.stat = c("n", "rsq", "adj.rsq"),
          digits = 3)
