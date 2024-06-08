
# Independent Variables
library(tidyverse)

ESG <- read_excel("ESG_Data.xlsx", sheet = "ESG_Data")
names(ESG)
summary(ESG)
stary <- 2018
endy <- 2022
ESG$Year <- rep(2022:2018, nrow(ESG)/5)

View(ESG)

ESG2_raw <- read_excel("ESG_Data.xlsx", sheet = "ESG_Data2")
ESG2 <- ESG2_raw[, c(1:211)]
ESG2$Year <- rep(2022:2016, nrow(ESG2)/7)
View(ESG2)

ESG_choosen <- ESG2

# Pivot the dataframe
ESG_pivoted <- pivot_longer(ESG_choosen,
                            cols = -c("Year", "RIC"),
                            names_to = "Company",
                            values_to = "Value")

ESG_pivoted_wider <- pivot_wider(ESG_pivoted,
                          names_from = RIC,
                          values_from = Value)


print(ESG_pivoted_wider)

ESG_final <- ESG_pivoted_wider

ESG_final$EnvironmentalPillar <- 0.35 * ESG_final$`Emissions Score` +
  0.35 * ESG_final$`Resource Use Score` +
  0.29 * ESG_final$`Environmental Innovation Score`

ESG_final$SocialPillar <- 0.28 * ESG_final$`Community Score` +
  0.17 * ESG_final$`Human Rights Score` +
  0.13 * ESG_final$`Product Responsibility Score` +
  0.43 * ESG_final$`Workforce Score`

ESG_final$GovernancePillar <- 0.20 * ESG_final$`Shareholders Score` +
  0.13 * ESG_final$`CSR Strategy Score` +
  0.67 * ESG_final$`Management Score`


# View(ESG_final)


head(ESG_final)


ggplot(ESG_final, aes(x = as.factor(Year), y = `ESG Score`)) +
  geom_boxplot() +
  labs(
    title = "ESG Score by Year",
    x = "Year",
    y = "ESG Score"
  ) +
  theme_minimal()


ggplot(ESG_final, aes(x = as.factor(Year), y = EnvironmentalPillar)) +
  geom_boxplot() +
  labs(
    title = "Environmental Pillar by Year",
    x = "Year",
    y = "Environmental Pillar"
  ) +
  theme_minimal()

ggplot(ESG_final, aes(x = as.factor(Year), y = SocialPillar)) +
  geom_boxplot() +
  labs(
    title = "Social Pillar by Year",
    x = "Year",
    y = "Social Pillar"
  ) +
  theme_minimal()

ggplot(ESG_final, aes(x = as.factor(Year), y = GovernancePillar)) +
  geom_boxplot() +
  labs(
    title = "Governance Pillar by Year",
    x = "Year",
    y = "Governance Pillar"
  ) +
  theme_minimal()


# Calculate median scores by year
median_scores_ESG <- ESG_final %>%
  group_by(Year) %>%
  summarise(
    median_ESG = median(`ESG Score`, na.rm = TRUE),
    median_environmental = median(EnvironmentalPillar, na.rm = TRUE),
    median_social = median(SocialPillar, na.rm = TRUE),
    median_governance = median(GovernancePillar, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = starts_with("median"), names_to = "score_type", values_to = "median_value")

# Create the line plot
ggplot(median_scores_ESG, aes(x = Year, y = median_value, color = score_type)) +
  geom_line() +
  labs(
    title = "Median ESG Scores Over the Years",
    x = "Year",
    y = "Median Score",
    color = "Score Type"
  ) +
  theme_minimal()

