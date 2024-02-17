#Submitted by BR Sricharan-23902

#Indian Weather Dataset - Single and Multiple linear regression

# Importing the Dataset
weather_data = read.csv("IndianWeatherRepository.csv")

# Load necessary libraries
#install.packages(c("caTools", "ggplot2"))
library(caTools)
library(ggplot2)

# Split the dataset into training and testing sets
set.seed(123)  # Set a seed for reproducibility
split = sample.split(weather_data$temperature_celsius, SplitRatio = 0.7)  # Split the dataset into 70% training and 30% testing
training_set = subset(weather_data, split == TRUE)  # Create the training set based on the split
test_set = subset(weather_data, split == FALSE)  # Create the testing set based on the split

# Fit the Simple Linear Regression Model using Training Set
regressor = lm(temperature_celsius ~ wind_mph, data = training_set)  # Fit a simple linear regression model with 'wind_mph' as predictor
print(regressor)  # Print the regression model summary

# Predicting the Test Set Results
y_pred = predict(regressor, newdata = test_set)  # Predict the target variable for the test set

# Visualizing the Training Set Results
ggplot(training_set, aes(x = wind_mph, y = temperature_celsius)) +
  geom_point(color = "red") +
  geom_line(aes(x = wind_mph, y = predict(regressor, newdata = training_set)), color = "blue") +
  ggtitle("Temperature Vs Wind Speed (Training Set Results)") +
  xlab("Wind Speed (mph)") +
  ylab("Temperature (Celsius)")

# Fitting the Multiple Linear Regression Model using Training Set
regressor = lm(temperature_celsius ~ wind_mph + humidity + pressure_mb, data = training_set)  # Fit a multiple linear regression model with multiple predictors
print(regressor)  # Print the regression model summary

# Predicting the Test Set Results
y_pred = predict(regressor, newdata = test_set)  # Predict the target variable for the test set

# Comparing Predictions with Actual Values
comparison_df = data.frame(Actual = test_set$temperature_celsius, Predicted = y_pred)  # Create a data frame with actual and predicted values
print(comparison_df)  # Print the comparison data frame

# Visualizing the Predictions
plot(comparison_df$Actual, col = "red", main = "Actual vs Predicted Temperature",
     xlab = "Observation", ylab = "Temperature (Celsius)")
points(comparison_df$Predicted, col = "blue")  # Plot actual and predicted values
legend("topright", legend = c("Actual", "Predicted"), col = c("red", "blue"), pch = 1)  # Add legend
