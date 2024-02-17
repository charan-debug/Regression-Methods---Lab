#Submitted by BR Sricharan-23902

#Walmart Dataset - Multiple linear regression
#WEEKLY SALES AND CPI AS DEPENDENT VARIABLES

# Importing the Dataset
walmart_dataset = read.csv("walmart.csv")

# Splitting the Dataset into Training and Testing set
#install.packages("caTools")  # Install the 'caTools' package 
library(caTools)  # Load the 'caTools' library for data splitting

set.seed(123)  # Set a seed for reproducibility
split = sample.split(walmart_dataset$Weekly_Sales, SplitRatio = 0.8)  # Split the dataset into 80% training and 20% testing
print(split)  # Print the split to see the outcome

training_set = subset(walmart_dataset, split == TRUE)  # Create the training set based on the split
test_set = subset(walmart_dataset, split == FALSE)  # Create the testing set based on the split

#WEEKLY SALES AS DEPENDENT VARIABLE

# Fitting the Multiple Linear Regression Model using Training Set
regressor = lm(formula = Weekly_Sales ~ ., data = training_set)  # Fit a multiple linear regression model with all variables as predictors
print(regressor)  # Print the regression model summary

# Predicting the Test Set Results
y_pred = predict(regressor, newdata = test_set)  # Predict the target variable for the test set
print(y_pred)  # Print the predicted values
print(test_set$Weekly_Sales)  # Print the actual values from the test set

#CPI AS DEPENDENT VARIABLE

# Fitting the Multiple Linear Regression Model using Training Set
regressor = lm(formula = CPI ~ ., data = training_set)  # Fit a multiple linear regression model with all variables as predictors
print(regressor)  # Print the regression model summary

# Predicting the Test Set Results
y_pred = predict(regressor, newdata = test_set)  # Predict the target variable for the test set
print(y_pred)  # Print the predicted values
print(test_set$CPI)  # Print the actual values from the test set

# Importing the required library
library(ggplot2)  # Load the 'ggplot2' library for data visualization

# Creating a data frame with actual and predicted CPI values
plot_data <- data.frame(Actual_CPI = test_set$CPI, Predicted_CPI = y_pred)

# Creating the scatter plot
ggplot(plot_data, aes(x = Actual_CPI, y = Predicted_CPI)) +
  geom_point(color = "blue", size = 3, alpha = 0.7) +  # Scatter plot with blue points
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", size = 1.5) +  # Diagonal dashed line for reference
  ggtitle("Actual vs Predicted CPI") +  # Set plot title
  xlab("Actual CPI") +  # Set x-axis label
  ylab("Predicted CPI") +  # Set y-axis label
  theme_minimal() +  # Set minimal theme
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +  # Remove grid lines
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14)) +  # Set text size for axis
  theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold", color = "darkblue"))  # Set text size and styling for plot title
