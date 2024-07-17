# Calling necessary Libraries. #####
library(readr)
library(readxl)
library(tidyr)
library(dplyr)
library(purrr)
library(ggplot2)
library(skimr)
library(lubridate)
library(caret)
library(reshape2) # Correlation Heatmap
library(e1071) # Build SVM Model
library(rpart) # Build RT Model
library(randomForest) # Build RF Model
install.packages("gbm")
library(gbm) # Build Gradient Boosting Model

# Setting the working directory.
setwd("C:/Users/USER/OneDrive - Bournemouth University/MSc Research/dataset")
getwd()

# This is where I read the datasets for all the 10 years used for this analysis.

demanddata_2013 <- read.csv("demanddata_2013.csv")
demanddata_2014 <- read.csv("demanddata_2014.csv")
demanddata_2015 <- read.csv("demanddata_2015.csv")
demanddata_2016 <- read.csv("demanddata_2016.csv")
demanddata_2017 <- read.csv("demanddata_2017.csv")
demanddata_2018 <- read.csv("demanddata_2018.csv")
demanddata_2019 <- read.csv("demanddata_2019.csv")
demanddata_2020 <- read.csv("demanddata_2020.csv")
demanddata_2021 <- read.csv("demanddata_2021.csv")
demanddata_2022 <- read.csv("demanddata_2022.csv")

# Data Preparation - Cleaning and Transformation####

# I combined all datasets into a list
all_datasets <- list(
  demanddata_2013,
  demanddata_2014,
  demanddata_2015,
  demanddata_2016,
  demanddata_2017,
  demanddata_2018,
  demanddata_2019,
  demanddata_2020,
  demanddata_2021,
  demanddata_2022
)

# This Check for missing values in each dataset
missing_values <- all_datasets %>% 
  map(~sum(is.na(.)))

# This print the number of missing values for each dataset
print(missing_values)

#This step convert the datasets to proper date format.
demanddata_2013$SETTLEMENT_DATE <- parse_date_time(demanddata_2013$SETTLEMENT_DATE, orders = c("ymd", "dmy", "mdy"))
demanddata_2013$SETTLEMENT_DATE <- as.Date(demanddata_2013$SETTLEMENT_DATE)

demanddata_2014$SETTLEMENT_DATE <- parse_date_time(demanddata_2014$SETTLEMENT_DATE, orders = c("ymd", "dmy", "mdy"))
demanddata_2014$SETTLEMENT_DATE <- as.Date(demanddata_2014$SETTLEMENT_DATE)

demanddata_2015$SETTLEMENT_DATE <- parse_date_time(demanddata_2015$SETTLEMENT_DATE, orders = c("ymd", "dmy", "mdy"))
demanddata_2015$SETTLEMENT_DATE <- as.Date(demanddata_2015$SETTLEMENT_DATE)

demanddata_2016$SETTLEMENT_DATE <- parse_date_time(demanddata_2016$SETTLEMENT_DATE, orders = c("ymd", "dmy", "mdy"))
demanddata_2016$SETTLEMENT_DATE <- as.Date(demanddata_2016$SETTLEMENT_DATE)

demanddata_2017$SETTLEMENT_DATE <- parse_date_time(demanddata_2017$SETTLEMENT_DATE, orders = c("ymd", "dmy", "mdy"))
demanddata_2017$SETTLEMENT_DATE <- as.Date(demanddata_2017$SETTLEMENT_DATE)

demanddata_2018$SETTLEMENT_DATE <- parse_date_time(demanddata_2018$SETTLEMENT_DATE, orders = c("ymd", "dmy", "mdy"))
demanddata_2018$SETTLEMENT_DATE <- as.Date(demanddata_2018$SETTLEMENT_DATE)

demanddata_2019$SETTLEMENT_DATE <- parse_date_time(demanddata_2019$SETTLEMENT_DATE, orders = c("ymd", "dmy", "mdy"))
demanddata_2019$SETTLEMENT_DATE <- as.Date(demanddata_2019$SETTLEMENT_DATE)

demanddata_2020$SETTLEMENT_DATE <- parse_date_time(demanddata_2020$SETTLEMENT_DATE, orders = c("ymd", "dmy", "mdy"))
demanddata_2020$SETTLEMENT_DATE <- as.Date(demanddata_2020$SETTLEMENT_DATE)

demanddata_2021$SETTLEMENT_DATE <- parse_date_time(demanddata_2021$SETTLEMENT_DATE, orders = c("ymd", "dmy", "mdy"))
demanddata_2021$SETTLEMENT_DATE <- as.Date(demanddata_2021$SETTLEMENT_DATE)

demanddata_2022$SETTLEMENT_DATE <- parse_date_time(demanddata_2022$SETTLEMENT_DATE, orders = c("ymd", "dmy", "mdy"))
demanddata_2022$SETTLEMENT_DATE <- as.Date(demanddata_2022$SETTLEMENT_DATE)

# Data Merging ####

# This is where I combined all datasets into a single dataframe using bind_rows()
merged_data <- bind_rows(
  demanddata_2013,
  demanddata_2014,
  demanddata_2015,
  demanddata_2016,
  demanddata_2017,
  demanddata_2018,
  demanddata_2019,
  demanddata_2020,
  demanddata_2021,
  demanddata_2022
)
skim(merged_data)

#This steps replace the NA values in from year 2013 to 2018 because there was no record for NSL_FLOW and ELECLINK_FLOW for those years.
preprocessed_data <- merged_data %>%
  replace_na(list(NSL_FLOW = 0, ELECLINK_FLOW = 0))

# This step was used to check for missing values (NA) in the final dataset called preprossed_data.
preprocessed_data %>%
  map(~sum(is.na(.)))

skim(preprocessed_data)

# Dealing with Outliers ########################
#This step is to check for Outliers in the pre-processed data. 
# Set the figure margins - this allows for better control over the layout and spacing of the figures in a plot, and helps to improve the overall appearance and readability of the figures in the plot.
par(mar = c(2, 2, 2, 2))  

# Create boxplots for all variables
par(mfrow = c(4, 5))

#This step provided is a loop that iterates over each column in the preprocessed_data dataset. For each column, a boxplot is generated using the boxplot() function. The main parameter is used to set the title of each boxplot, which is dynamically created by concatenating the string "Boxplot of" with the column name (col).
for (col in names(preprocessed_data)) {
  boxplot(preprocessed_data[[col]], main = paste("Boxplot of", col))
}

#To remove outlier from ENGLAND_WALES_DEMAND and NON_BM_STOR variables #
#I calculated the IQR for ENGLAND_WALES_DEMAND
demand <- preprocessed_data$ENGLAND_WALES_DEMAND
Q1_demand <- quantile(demand, 0.25)
Q3_demand <- quantile(demand, 0.75)
IQR_demand <- Q3_demand - Q1_demand

# I calculated the IQR for NON_BM_STOR
stor <- preprocessed_data$NON_BM_STOR
Q1_stor <- quantile(stor, 0.25)
Q3_stor <- quantile(stor, 0.75)
IQR_stor <- Q3_stor - Q1_stor

# Define lower and upper bounds for ENGLAND_WALES_DEMAND
#The choice of 1.5 as the multiplier is a common convention, but it can be adjusted depending on the specific requirements.
lower_bound_demand <- Q1_demand - 1.5 * IQR_demand
upper_bound_demand <- Q3_demand + 1.5 * IQR_demand

# Define lower and upper bounds for NON_BM_STOR
lower_bound_stor <- Q1_stor - 1.5 * IQR_stor
upper_bound_stor <- Q3_stor + 1.5 * IQR_stor

# Identify indices of outliers for ENGLAND_WALES_DEMAND
outlier_indices_demand <- which(demand < lower_bound_demand | demand > upper_bound_demand)

# Identify indices of outliers for NON_BM_STOR
outlier_indices_stor <- which(stor < lower_bound_stor | stor > upper_bound_stor)

# Combine outlier indices for both variables
outlier_indices <- union(outlier_indices_demand, outlier_indices_stor)

# Remove outliers from the preprocessed_data dataset
preprocessed_data_clean <- preprocessed_data[-outlier_indices, ]

#This step confirm if the outliers was removed successfully.
# Set the figure margins
par(mar = c(2, 2, 2, 2))

# Create boxplots for all variables
par(mfrow = c(4, 5))

for (col in names(preprocessed_data_clean)) {
  boxplot(preprocessed_data_clean[[col]], main = paste("Boxplot of", col))
}

file_path <- "C:/Users/USER/OneDrive - Bournemouth University/MSc Research/preprocessed_data_clean.csv"
write.csv(preprocessed_data_clean, file = file_path, row.names = FALSE)

# Correlation Analysis #################################################################################
# This is important to know the most important variables, these variable were later used to build an optimised model.
selected_columns <- c("ENGLAND_WALES_DEMAND", "EMBEDDED_WIND_GENERATION", "ND", "IFA2_FLOW", "SETTLEMENT_DATE", "TSD", "IFA_FLOW", "NEMO_FLOW", "EAST_WEST_FLOW", "MOYLE_FLOW", "BRITNED_FLOW", "EMBEDDED_SOLAR_GENERATION", "PUMP_STORAGE_PUMPING", "SETTLEMENT_PERIOD", "EMBEDDED_WIND_CAPACITY", "EMBEDDED_SOLAR_CAPACITY", "ELECLINK_FLOW", "NSL_FLOW")

# Convert selected columns to numeric
preprocessed_data_clean[selected_columns] <- lapply(preprocessed_data_clean[selected_columns], as.numeric)

# Calculate the correlation matrix
correlation_matrix <- cor(preprocessed_data_clean[selected_columns])

print(correlation_matrix)

# Plot correlation heatmap
#This step is using the melt() function to transform a correlation matrix into a melted format.Melting a matrix means reshaping it from a wide format to a long format, typically for easier analysis or visualization
melted_correlation <- melt(correlation_matrix)
#plotting the heatmap
ggplot(melted_correlation, aes(Var2, Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Correlation Heatmap", x = "Variables", y = "Variables") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Data Spliting ####
# This step use a set.seed() function is used to set the seed value for generating random numbers. it is important to be able to obtain the same set of random numbers each time the code is run.
set.seed(123)
#This create a new column called "cid" in the preprocessed_data_clean data frame
preprocessed_data_clean <- preprocessed_data_clean %>%
  mutate(cid = row_number())

# This is where I split the data into 70% training and 30% testing
train_data <- preprocessed_data_clean %>% sample_frac(0.70)
test_data  <- anti_join(preprocessed_data_clean, train_data, by = "cid")

#### ML Model Development ####
# 1. Linear Regression Algorithm #################################################################

# Fit the linear regression model
lm_model <- lm(ENGLAND_WALES_DEMAND ~ ., data = train_data)

# Make predictions on the test data
predictions_lm <- predict(lm_model, newdata = test_data)

# To plot the actual demand against the predicted demand for Linear Regression
# I created a data frame with actual and predicted values.
results <- data.frame(Actual = test_data$ENGLAND_WALES_DEMAND, Predicted = predictions_lm)

# I Ploted actual vs predicted values
plot(results$Actual, results$Predicted, pch = 16, col = "blue",
     xlab = "Actual Values", ylab = "Predicted Values",
     main = "Predicted vs Actual Values by Linear Regression")

# I added a diagonal reference line
abline(0, 1, col = "red")

#Evaluate the model:
#I compared the predicted values with the actual values from the test data to evaluate the performance of the linear regression model. I use various evaluation metrics such as Mean Absolute Error (MAE), Root Mean Squared Error (RMSE), or Mean Squared Error (MSE) to assess the model's accuracy.
# Calculate evaluation metrics
MAE_lr <- mean(abs(predictions_lm - test_data$ENGLAND_WALES_DEMAND))
#MAE = 183.05
MSE_lr <- mean((predictions_lm - test_data$ENGLAND_WALES_DEMAND)^2)
#MSE = 56,572.46
RMSE_lr <- sqrt(mean((predictions_lm - test_data$ENGLAND_WALES_DEMAND)^2))
#RMSE = 237.85

#2. Support Vector Regression ##################################################################
# This step is used to scale the NON_BM_STOR variables, to ensure that all variables are on a similar scale.
train_data <- subset(train_data, select = -c(NON_BM_STOR))

# This is where I define and train the SVR model
#The running time for training this model is approximately 9mins and 8secs.
model_SVR <- svm(ENGLAND_WALES_DEMAND ~ ., data = train_data, type = "eps-regression",
             kernel = "radial", cost = 1, epsilon = 0.1)

# This step is used to make predictions on the test dataset
predictions_SVR <- predict(model_SVR, newdata = test_data)

# This step is used to Evaluate the model performance
actual_values <- test_data$ENGLAND_WALES_DEMAND
MSE_SVR <- mean((predictions_SVR - actual_values)^2)  
# MSE = 79,517.76
MAE_SVR <- mean(abs(predictions_SVR - actual_values)) 
# MAE = 222.90
RMSE_SVR <- sqrt(MSE_SVR) 
# RMSE = 281.98

# This Step visualize the predictions and actual values
plot(predictions_SVR, type = 'l', col = 'blue', xlab = "Actual Values", ylim = c(0, max(actual_values, predictions_SVR)), ylab = "Predicted Values", main = "Actual vs Predicted Energy Demand by Support Vector Regression")
lines(actual_values, col = 'red')
legend("topright", legend = c("Predictions", "Actual"), col = c("blue", "red"), lty = 1)

#3. Regression Tree #############################################################################

# This Step Define the formula and create the regression tree model
formula <- ENGLAND_WALES_DEMAND ~ .
#This is where I build the regression tree model.
model_RT <- rpart(formula, data = train_data, method = "anova")

# This step make predictions on the test set
predictions_RT <- predict(model_RT, newdata = test_data)

# Step 5: Evaluate the model performance
MSE_RT <- mean((predictions_RT - test_data$ENGLAND_WALES_DEMAND)^2)  
# Mean Squared Error = 113,707.55
MAE_RT <- mean(abs(predictions_RT - test_data$ENGLAND_WALES_DEMAND))  
# Mean Absolute Error = 275.55
RMSE_RT <- sqrt(MSE_RT)  
# Root Mean Squared Error = 337.205

# This Step Plot actual vs. predicted values to show if the model fit the dataset well enough.
plot_data <- data.frame(Actual = test_data$ENGLAND_WALES_DEMAND, Predicted = predictions_RT)
ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(x = "Actual Values", y = "Predicted Values", title = "Actual vs. Predicted Energy Demand by Regression Tree") +
  theme_minimal() +
  theme(panel.grid = element_blank(), panel.border = element_rect(color = "black", fill = NA, size = 1))


#4. Random Forest ##################################################################
# This Step Train the Random Forest model and setting the hyperparameters as needed.
model_RF <- randomForest(ENGLAND_WALES_DEMAND ~ ., data = train_data, ntree = 100, importance = TRUE)

# This Step Make predictions on the test set.
predictions_RF <- predict(model_RF, newdata = test_data)

# Step 5: Evaluate the model performance
actual_values <- test_data$ENGLAND_WALES_DEMAND
MSE_RF <- mean((actual_values - predictions_RF)^2) # Mean Squared Error = 43,166.68
MAE_RF <- mean(abs(actual_values - predictions_RF)) # Mean Absolute Error = 156.34
RMSE_RF <- sqrt(MSE_RF)  # Root Mean Squared Error = 207.76

# Step 6: Optionally, extract variable importance measures
var_importance <- importance(model_RF)

# Convert the variable importance to a data frame
var_importance_df <- as.data.frame(var_importance)

# Display the variable importance table
print(var_importance_df)


# Step 7: Optionally, visualize the variable importance measures
var_importance_plot <- varImpPlot(model_RF, type = 1)

# Step 8: Optionally, visualize the actual vs predicted values
plot(actual_values, type = "l", col = "red", ylim = c(0, max(actual_values, predictions_RF)), xlab = "Actual Values", ylab = "Predicted Values", main = "Actual vs. Predicted Energy Demand by Random Forest")
lines(predictions_RF, col = "blue")
legend("topright", legend = c("Actual", "Predicted"), col = c("red", "blue"), lty = 1)


#5. Gradient Boosting ##################################################################
# Load the required libraries

# This Step convert the the non-numeric data to numeric, and define the gradient boosting model
train_data$SETTLEMENT_DATE <- as.numeric(train_data$SETTLEMENT_DATE) # convert to numeric.

model_gb <- gbm(ENGLAND_WALES_DEMAND ~ ., data = train_data, n.trees = 100, interaction.depth = 3, shrinkage = 0.1)

# This step Make predictions on the test set
predictions_gb <- predict(model_gb, newdata = test_data, n.trees = 100, type = "response")

#This is where I Evaluate the model performance
actual_values <- test_data$ENGLAND_WALES_DEMAND
MSE_gb <- mean((predictions_gb - actual_values)^2)  
# Mean Squared Error = 71667.71
MAE_gb <- mean(abs(predictions_gb - actual_values))  
# Mean Absolute Error = 208.35
RMSE_gb <- sqrt(MSE_gb)  
# Root Mean Squared Error = 267.71

# This step visualize the actual vs predicted values
plot(actual_values, type = "l", col = "red", ylim = c(0, max(actual_values, predictions_gb)),
     xlab = "Actual Values", ylab = "Predicted Values", main = "Actual vs Predicted Energy Demand by Gradient Boosting")
lines(predictions_gb, col = "blue")
legend("topright", legend = c("Actual", "Predicted"), col = c("red", "blue"), lty = 1)

### Development of an Optimised Model #################################################################
# In this phase, the most significant variables selected through the feature selection and correlation analysis were utilized. By incorporating these variables into the model building process, we were able to compare the performance of the previous unoptimized model with the optimized model. This approach allowed us to evaluate the impact of feature selection on the model's performance and assess the effectiveness of the optimization process. It was observed that the purported optimized model exhibited poor performance. This indicates that certain important variables, which contributed to the previous model’s performance, were excluded in the optimized model
# Random Forest Optimized Model ####################################################

# This Step Train the Random Forest model and setting the hyperparameters as needed.
model_RF_OPT <- randomForest(ENGLAND_WALES_DEMAND ~ EMBEDDED_WIND_GENERATION + EAST_WEST_FLOW + MOYLE_FLOW + ND + TSD + IFA2_FLOW + SETTLEMENT_DATE + NEMO_FLOW, data = train_data, ntree = 100, importance = TRUE)

# This Step Make predictions on the test set.
predictions_RF_OPT <- predict(model_RF_OPT, newdata = test_data)

# Step 5: Evaluate the model performance
actual_values <- test_data$ENGLAND_WALES_DEMAND
MSE_RF_OPT <- mean((actual_values - predictions_RF_OPT)^2) # Mean Squared Error
MAE_RF_OPT <- mean(abs(actual_values - predictions_RF_OPT)) # Mean Absolute Error
RMSE_RF_OPT <- sqrt(MSE_RF_OPT)  # Root Mean Squared Error

# RMSE = 240.62; MAE = 184.48; MSE = 57,902.63
#
# Step 8: Optionally, visualize the actual vs predicted values
plot(actual_values, type = "l", col = "red", ylim = c(0, max(actual_values, predictions_RF_OPT)), xlab = "Observation", ylab = "Energy Demand", main = "Actual vs. Predicted Energy Demand by Random Forest")
lines(predictions_RF_OPT, col = "blue")
legend("topright", legend = c("Actual", "Predicted"), col = c("red", "blue"), lty = 1)
