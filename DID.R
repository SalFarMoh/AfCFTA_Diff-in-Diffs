
#===============================================================================================
#Econometrics Project AfCFTA: Codes
#===============================================================================================

setwd("/Users/apple/Desktop/ECONOMETRICS/DID")
getwd()

install.packages("tidyr")
library(tidyr)

#Packages
install.packages("dplyr")
library(dplyr)

install.packages("ggplot2")
library(ggplot2)

#Read Excel Data
install.packages("readxl")
library(readxl)
Excel <- "/Users/apple/Desktop/ECONOMETRICS/DID/DID.xlsx"
Did_data<- read_excel(Excel)
head(Did_data)


#===============================================================================================
#Descriptive Statistics for Annual GDP and Export Growths of all countries 
#===============================================================================================

install.packages("psych")
library(psych)

# Detailed descriptive statistics for export in Did_data
Did_data$GDP_growth <- as.numeric(Did_data$GDP_growth)
Did_data$annual_growth_exports <- as.numeric(Did_data$annual_growth_exports)

describe(Did_data$annual_growth_exports)
describe(Did_data$GDP_growth)


#===============================================================================================
#Regress Annual GDP Growth on Annual exports Growth for check significance
#===============================================================================================

model <- lm(GDP_growth ~ annual_growth_exports, data = Did_data)

summary(model)

plot(Did_data$annual_growth_exports, Did_data$GDP_growth,
     xlab = "Annual Growth of Exports", ylab = "GDP Growth",
     main = "Scatter Plot with Regression Line")
abline(model, col = "red")


#===============================================================================================
#Descriptive Statistics for Countries that received the treatment 
#===============================================================================================
GDP_values <- Did_data$GDP_growth[Did_data$country_name %in% c("Ghana", "Kenya", "Rwanda", "Niger", "Chad", "Eswatini",
                                                               "Guinea", "Cote_dIvoire", "Mali", "Namibia", "South_Africa",
                                                               "Congo_Dem", "Djibouti", "Mauritania", "Uganda", "Senegal",
                                                               "Togo", "Egypt", "Ethiopia", "Gambia", "Sierra_Leone",
                                                               "Zimbabwe", "Burkina_Faso", "Equatorial_Guinea", "Gabon",
                                                               "Mauritius")]

mean_GDP <- mean(GDP_values, na.rm = TRUE)
std_dev_GDP <- sd(GDP_values, na.rm = TRUE)

print(mean_GDP)
print(std_dev_GDP)

library(ggplot2)

# Overall mean GDP growth for all countries
overall_mean_GDP <- mean(Did_data$GDP_growth, na.rm = TRUE)

# Data frame for plotting
plot_data <- data.frame(
  Country = c("Selected Countries", "Overall"),
  Mean_GDP = c(mean_GDP, overall_mean_GDP)
)

# Bar plot
ggplot(plot_data, aes(x = Country, y = Mean_GDP, fill = Country)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Mean GDP Growth: Selected Countries vs. Overall",
       y = "Mean GDP Growth") +
  theme_minimal()


#T-test to check for significance of difference
t_test_result <- t.test(GDP_values, mu = overall_mean_GDP)

# Print the test result
print(t_test_result)

#===============================================================================================
#Descriptive Statistics for Countries that received the treatment- Annual GDP Growth
#===============================================================================================
library(ggplot2)

# Select export values for specific countries
export_values <- Did_data$annual_growth_exports[Did_data$country_name %in% c("Ghana", "Kenya", "Rwanda", "Niger", "Chad", "Eswatini",
                                                                             "Guinea", "Cote_dIvoire", "Mali", "Namibia", "South_Africa",
                                                                             "Congo_Dem", "Djibouti", "Mauritania", "Uganda", "Senegal",
                                                                             "Togo", "Egypt", "Ethiopia", "Gambia", "Sierra_Leone",
                                                                             "Zimbabwe", "Burkina_Faso", "Equatorial_Guinea", "Gabon",
                                                                             "Mauritius")]

overall_mean_exports <- mean(Did_data$annual_growth_exports, na.rm = TRUE)

mean_exports <- mean(export_values, na.rm = TRUE)

# Data frame for plotting
plot_data_exports <- data.frame(
  Category = c("Selected Countries", "Overall"),
  Mean_Exports = c(mean_exports, overall_mean_exports)
)

# Bar plot
ggplot(plot_data_exports, aes(x = Category, y = Mean_Exports, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Mean Exports: Selected Countries vs. Overall",
       y = "Mean Exports") +
  theme_minimal()


#===============================================================================================
#Descriptive Statistics for Countries that did not receive the treatment- Annual GDP & Export Growth 
#===============================================================================================
GDP_late <- Did_data$GDP_growth[Did_data$country_name %in% c("Algeria", "Angola", "Benin", "Botswana", "Burundi", "Carbo_Verde",
                                                             "Cameroon", "CAR", "Comoros", "Congo_dem", "Guinea_Bissau",
                                                             "Lesotho", "Liberia", "Libya", "Malawi",
                                                             "Mali", "Mauritania", "Morocco", "Mozambique", "Namibia",
                                                             "Nigeria", "South_Africa", "South_Sudan", "Sudan","Tanzania",
                                                             "Tunisia","Zambia")]

# Calculate the mean GDP growth for each country
mean_GDP_late <- mean(GDP_late, na.rm = TRUE)
std_dev_GDP_late <- sd(GDP_late, na.rm = TRUE)

# Print or use the mean_GDP as needed
print(mean_GDP_late)
print(std_dev_GDP_late)


Export_late <- Did_data$annual_growth_exports[Did_data$country_name %in% c("Algeria", "Angola", "Benin", "Botswana", "Burundi", "Carbo_Verde",
                                                                           "Cameroon", "CAR", "Comoros", "Congo_dem", "Guinea_Bissau",
                                                                           "Lesotho", "Liberia", "Libya", "Malawi",
                                                                           "Mali", "Mauritania", "Morocco", "Mozambique", "Namibia",
                                                                           "Nigeria", "South_Africa", "South_Sudan", "Sudan","Tanzania",
                                                                           "Tunisia","Zambia")]

# Mean GDP growth for each country
mean_export_late <- mean(Export_late, na.rm = TRUE)
std_dev_export_late <- sd(Export_late, na.rm = TRUE)

# Print mean_GDP 
print(mean_export_late)
print(std_dev_export_late)


plot_data_GDP_late <- data.frame(
  Category = c("Late Countries", "Overall"),
  Mean_GDP = c(mean_GDP_late, overall_mean_GDP)
)

# Bar plot
ggplot(plot_data_GDP_late, aes(x = Category, y = Mean_GDP, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Mean GDP Growth: Late Countries vs. Overall",
       y = "Mean GDP Growth") +
  theme_minimal()


# Overall mean exports for all countries
overall_mean_exports <- mean(Did_data$annual_growth_exports, na.rm = TRUE)

# Mean exports for "late" countries
mean_export_late <- mean(Export_late, na.rm = TRUE)

# Data frame for plotting
plot_data_export_late <- data.frame(
  Category = c("Late Countries", "Overall"),
  Mean_Exports = c(mean_export_late, overall_mean_exports)
)

# Bar plot
ggplot(plot_data_export_late, aes(x = Category, y = Mean_Exports, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Mean Exports: Late Countries vs. Overall",
       y = "Mean Exports") +
  theme_minimal()




#===============================================================================================
#DiD Analysis 
#===============================================================================================
Did_data$Treatment <- ifelse(Did_data$country_name %in% c("Ghana", "Kenya", "Rwanda", "Niger", "Chad", "Eswatini",
                                                          "Guinea", "Cote_dIvoire", "Mali", "Namibia", "South_Africa",
                                                          "Congo_Dem", "Djibouti", "Mauritania", "Uganda", "Senegal",
                                                          "Togo", "Egypt", "Ethiopia", "Gambia", "Sierra_Leone",
                                                          "Zimbabwe", "Burkina_Faso", "Equatorial_Guinea", "Gabon",
                                                          "Mauritius"), 1, 0)

Did_data$Post <- ifelse(Did_data$year > 2020, 1, 0)

# first few rows of the updated dataset
head(Did_data)

#Interaction
Did_data$Treatment_Post <- Did_data$Treatment * Did_data$Post


# Fit the Difference-in-Differences model
did_model <- lm(GDP_growth ~ Treatment + Post + Treatment_Post, data = Did_data)

# Display the model summary
summary(did_model)



# Heteroscedasticity test
library(lmtest)
bptest(did_model)



# Fit the Difference-in-Differences model with export growth
did_model <- lm(GDP_growth ~ Treatment + Post + Treatment_Post + annual_growth_exports, data = Did_data)

# Display the model summary
summary(did_model)

#""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
#""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

did_data_summary <- aggregate(GDP_growth ~ Post + Treatment, data = Did_data, FUN = mean)

ggplot(Did_data, aes(x = Post, y = GDP_growth, group = Treatment, color = as.factor(Treatment))) +
  geom_boxplot() +
  labs(title = "Before-and-After Plots",
       x = "Time Period",
       y = "GDP Growth")

#===============================================================================================
#
#===============================================================================================















