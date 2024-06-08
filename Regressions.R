
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
library(xtable)
#felm (dep ~ indepe + CV | Fixed Effects | Instrumental Variables | Clustered SE, data)
# eg. result <- felm(GDP_growth ~ Investment + Pop_growth | Year + Industry | 0 | Country, data = df)
# robust_se <- coeftest(result, vcov = vcovHC(result, type = 'HC0'))
# bp_test <- bptest(result)
# print(bp_test)



# Regressions

Regression_DF_complete <- left_join(DependentVar, ESG_final, by = c("Year", "Company")) %>%
  left_join(CV_final, by = c("Year", "Company"))


summary(Regression_DF_complete)
dim(Regression_DF_complete)
names(Regression_DF_complete)
# View(Regression_DF_complete)



na_counts_lm <- colSums(is.na(Regression_DF_complete))

print(na_counts_lm)
sum(na_counts_lm > 0)
dim(Regression_DF_complete)

Regression_DF_complete_final <- Regression_DF_complete[!is.na(Regression_DF_complete$`ESG Score`),]

na_counts_lm_final <- colSums(is.na(Regression_DF_complete_final))

print(na_counts_lm_final)
sum(na_counts_lm_final > 0)
dim(Regression_DF_complete_final)

felm_df <- Regression_DF_complete_final
dim(felm_df)
summary(felm_df)

colnames(felm_df) <- make.names(colnames(felm_df))
names(felm_df)
summary(felm_df)

# Select the columns for the first set
set1 <- c("ROA", "Leverage", "DivYield", "TotalAssets", "CurrentRatio")

# Select the columns for the second set
set2 <- c("ESG.Score", "Resource.Use.Score", "Emissions.Score", "Environmental.Innovation.Score", 
          "Workforce.Score", "Human.Rights.Score", "Community.Score", 
          "Product.Responsibility.Score", "Management.Score", 
          "Shareholders.Score", "CSR.Strategy.Score", "ESG.Combined.Score", 
          "ESG.Controversies.Score", "EnvironmentalPillar", "SocialPillar", "GovernancePillar", "ROA", "Leverage", "DivYield", "TotalAssets", "CurrentRatio")

set <- c("ESG.Score", "EnvironmentalPillar", "SocialPillar", "GovernancePillar", 
          "ROA", "Leverage", "DivYield", "TotalAssets", "CurrentRatio")

# Combine both sets for subsetting the dataframe
all_columns <- c(set1, set2)

# Subset the dataframe
subset_df <- felm_df[, all_columns]

# Calculate the correlation matrix
correlation_matrix <- cor(subset_df, use = "complete.obs")

# Print the correlation matrix
print(correlation_matrix)

# Extract the correlation between set1 and set2
cor_set1_set2 <- correlation_matrix[set1, set2]

# Print the subsetted correlation matrix
print(cor_set1_set2)


# Convert to data frame for stargazer
cor_df <- as.data.frame(cor_set1_set2)


# Print the correlation matrix using xtable
xtable_output <- xtable(cor_df, caption = "Correlation Matrix")
print(xtable_output, type = "html", include.rownames = TRUE)



# View(cor_set1_set2)

vif_model <- lm(`ESG.Score` ~ ROA + Leverage + DivYield + TotalAssets + CurrentRatio, data = felm_df)
vif(vif_model) # very low VIF


#####################
##### Total Risk
lm_totalRisk_s <- felm(Total_Risk ~ `ESG.Score` | 0 | 0 | 0, felm_df)
summary(lm_totalRisk_s)

lm_totalRisk_s_fe <- felm(Total_Risk ~ `ESG.Score`
                          | Year | 0 | 0, felm_df)
summary(lm_totalRisk_s_fe)

lm_totalRisk_m <- felm(Total_Risk ~ `ESG.Score` + ROA + Leverage + DivYield + I(log(TotalAssets)) + CurrentRatio
                           | 0 | 0 | 0, felm_df)
summary(lm_totalRisk_m)

lm_totalRisk_m_fe <- felm(Total_Risk ~ `ESG.Score` + ROA + Leverage + DivYield + I(log(TotalAssets)) + CurrentRatio
                       | Company + Year | 0 | 0, felm_df)
summary(lm_totalRisk_m_fe)

robust_se <- coeftest(lm_totalRisk_m_fe, vcov = vcovHC(lm_totalRisk_m_fe, type = 'HC1'))
bp_test <- bptest(lm_totalRisk_m_fe)
print(bp_test)


stargazer(lm_totalRisk_s, lm_totalRisk_s_fe, lm_totalRisk_m, robust_se,
          type = "text",
          title = "Regression Results",
          dep.var.labels = "Total Risk",
          covariate.labels = c("ESG Score", "ROA", "Leverage", "Dividend Yield", "Total Assets", "Current Ratio"),
          column.labels = c("Model 1", "Model 2", "Model 3", "Model 4"),
          model.numbers = FALSE,
          add.lines = list(c("Fixed Effects", "No", "Time", "No", "Time")),
          keep.stat = c("n", "rsq", "adj.rsq"),
          digits = 3)
#####################




generate_stargazer_output <- function(data, indep_var, dep_var) {

  # Define control variables
  control_vars <- "ROA + Leverage + DivYield + I(log(TotalAssets)) + CurrentRatio"
  
  # Construct the formulas for the models
  formula_s <- as.formula(paste(as.symbol(dep_var), " ~ ", as.symbol(indep_var), "| 0 | 0 | 0"))
  formula_s_fe <- as.formula(paste(as.symbol(dep_var), " ~  ", as.symbol(indep_var), "| Year | 0 | 0"))
  formula_m <- as.formula(paste(as.symbol(dep_var), " ~", as.symbol(indep_var), "+", control_vars, "| 0 | 0 | 0"))
  formula_m_fe <- as.formula(paste(as.symbol(dep_var), " ~ ", as.symbol(indep_var), "+", control_vars, "| Year | 0 | 0"))
  
  # Fit the models
  # dep_var <- as.character(dep_var)
  # indep_var <- as.character(indep_var)
  # formula_s <- as.formula(paste(dep_var, "~", indep_var, "| 0 | 0 | 0"))
  # lm_Risk_s <- felm(formula_s, data)
  # robust_se <- coef_test(lm_Risk_s, vcov = "HC1")
  # robust_se_s <- coeftest(lm_Risk_s, vcov = vcovHC(lm_Risk_s, type = 'HC1'))
  

  # plm_model <- plm(Total_Risk ~ ESG.Score + ROA + Leverage + DivYield + I(log(TotalAssets)) + CurrentRatio, 
  #                  data = pdata, model = "within")
  
  # # Compute robust standard errors
  # robust_se <- coeftest(plm_model, vcov = vcovHC(plm_model, type = 'HC1'))
  
  lm_Risk_s <- felm(formula_s, data)
  summary(lm_Risk_s)
  # lm_Risk_s_fe <- felm(formula_s_fe, data)
  
  lm_Risk_s_fe <- felm(formula_s_fe, data)
  summary(lm_Risk_s_fe)
  # robust_se_s_fe <- coeftest(lm_Risk_s_fe, vcov = vcovHC(lm_Risk_s_fe, type = 'HC1'))
  
  lm_Risk_m <- felm(formula_m, data)
  summary(lm_Risk_m)
  # robust_se_m <- coeftest(lm_Risk_m, vcov = vcovHC(lm_Risk_m, type = 'HC1'))
  
  lm_Risk_m_fe <- felm(formula_m_fe, data)
  summary(lm_Risk_m_fe)
  # robust_se_m_fe <- coeftest(lm_Risk_m_fe, vcov = vcovHC(lm_Risk_m_fe, type = 'HC1'))
  
  dep_var_label <- switch(dep_var,
                          "Total_Risk" = "Total Risk",
                          "Beta" = "Beta",
                          "Idiosyncratic" = "Idiosyncratic Risk",
                          dep_var)
  clean_indep_var <- function(expr) {
    expr <- gsub("\\^2", "SQ", expr)
    expr <- gsub("I\\(", "", expr)
    expr <- gsub("\\)", "", expr)
    return(expr)
  }
  
  indep_var_lab_for_label <- clean_indep_var(indep_var)
  indep_var_lab <- trimws(strsplit(indep_var_lab_for_label, "\\+")[[1]])


  # base_dir <- ifelse(switch_dir > 0, "output_short", "output")

  # Create stargazer output
  stargazer(lm_Risk_s, lm_Risk_s_fe, lm_Risk_m, lm_Risk_m_fe,
            type = "text",
            title = "Regression Results",
            dep.var.labels = dep_var_label,
            covariate.labels = c(indep_var_lab, "ROA", "Leverage", "Dividend Yield", "ln(Total Assets)", "Current Ratio"),
            column.labels = c("Model 1", "Model 2", "Model 3", "Model 4"),
            model.numbers = FALSE,
            add.lines = list(c("Fixed Effects", "No", "Time", "No", "Time")),
            keep.stat = c("n", "rsq", "adj.rsq"),
            digits = 3, out = paste0("output/",dep_var, "_", indep_var, ".txt"))
  
  output <- stargazer(lm_Risk_s, lm_Risk_s_fe, lm_Risk_m, lm_Risk_m_fe,
            type = "text",
            title = "Regression Results",
            dep.var.labels = dep_var_label,
            covariate.labels = c(indep_var_lab, "ROA", "Leverage", "Dividend Yield", "ln(Total Assets)", "Current Ratio"),
            column.labels = c("Model 1", "Model 2", "Model 3", "Model 4"),
            model.numbers = FALSE,
            add.lines = list(c("Fixed Effects", "No", "Time", "No", "Time")),
            keep.stat = c("n", "rsq", "adj.rsq"),
            digits = 3)
  
  return(output)
}


#####################

names(felm_df)
dep_vars <- names(felm_df)[3:5]
indep_vars <- names(felm_df)[6:21]

# Initialize the nested list to store results
results <- list()
results <- list()


for (dep_var in dep_vars) {
  dep_results <- list()
  
  for (indep_var in indep_vars) {
    # Generate stargazer output for each combination
    stargazer_output <- generate_stargazer_output(felm_df, indep_var, dep_var)
    dep_results[[indep_var]] <- stargazer_output
  }
  
  results[[dep_var]] <- dep_results
}

print(results$Total_Risk$`ESG.Score`)

# results_temp <- results

txt_files <- list.files(path = "output", pattern = "\\.txt$", full.names = TRUE)
all_content <- ""


for (file in txt_files) {
  file_content <- read_file(file)
  all_content <- paste(all_content, file_content, sep = "\n")
}


write_file(all_content, file.path("AllOutputs.txt"))

#####################

names(felm_df)
dep_vars <- names(felm_df)[3:5]
# dep_vars <- c("Beta")
indep_vars_short <- c(
                      # "ESG.Score"
                      # "ESG.Combined.Score", "ESG.Controversies.Score",
                      "EnvironmentalPillar",
                      # ("Resource.Use.Score + Emissions.Score + Environmental.Innovation.Score"), 
                      "SocialPillar",
                      # ("Workforce.Score + Human.Rights.Score + Community.Score"),
                      "GovernancePillar"
                      # ("Product.Responsibility.Score + Management.Score + Shareholders.Score + CSR.Strategy.Score")
                      )


# Initialize the nested list to store results
results <- list()
results <- list()


for (dep_var in dep_vars) {
  dep_results <- list()
  
  for (indep_var in indep_vars_short) {
    # Generate stargazer output for each combination
    stargazer_output <- generate_stargazer_output(felm_df, indep_var, dep_var)
    dep_results[[indep_var]] <- stargazer_output
  }
  
  results[[dep_var]] <- dep_results
}

txt_files <- list.files(path = "output", pattern = "\\.txt$", full.names = TRUE)
all_content <- ""


for (file in txt_files) {
  file_content <- read_file(file)
  all_content <- paste(all_content, file_content, sep = "\n")
}


write_file(all_content, file.path("AllOutputs_short.txt"))


#####################

names(felm_df)
dep_vars <- names(felm_df)[3:5]
indep_vars_short <- c("ESG.Score + I(ESG.Score^2)",
                      "EnvironmentalPillar + I(EnvironmentalPillar^2)",
                      "SocialPillar + I(SocialPillar^2)",
                      "GovernancePillar + I(GovernancePillar^2)")


# Initialize the nested list to store results
results <- list()
results <- list()


for (dep_var in dep_vars) {
  dep_results <- list()
  
  for (indep_var in indep_vars_short) {
    # Generate stargazer output for each combination
    stargazer_output <- generate_stargazer_output(felm_df, indep_var, dep_var)
    dep_results[[indep_var]] <- stargazer_output
  }
  
  results[[dep_var]] <- dep_results
}

txt_files <- list.files(path = "output", pattern = "\\.txt$", full.names = TRUE)
all_content <- ""


for (file in txt_files) {
  file_content <- read_file(file)
  all_content <- paste(all_content, file_content, sep = "\n")
}


write_file(all_content, file.path("AllOutputsSquared.txt"))
