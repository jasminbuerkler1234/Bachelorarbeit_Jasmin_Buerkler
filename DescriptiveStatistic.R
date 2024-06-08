# Descriptive statistic

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



calculate_summary(total_risk)


calculate_summary(idiosyncratic_risk)



