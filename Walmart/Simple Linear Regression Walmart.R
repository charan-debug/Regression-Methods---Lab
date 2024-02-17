#Submitted by BR Sricharan-23902
#Walmart Dataset- Single linear regression

# Importing the Dataset
walmart_dataset = read.csv("Walmart.csv")

# Splitting the Dataset into Training and Testing set
library(caTools)  # Load the 'caTools' library for data splitting

set.seed(123)  # Set a seed for reproducibility
split = sample.split(walmart_dataset$Weekly_Sales, SplitRatio = 0.7)  # Splitting the dataset into 70% training and 30% testing
print(split)  # Printing the split 

training_set = subset(walmart_dataset, split == TRUE)  # Create the training set based on the split
test_set = subset(walmart_dataset, split == FALSE)  # Create the testing set based on the split

# Fitting the Simple Linear Regression Model using Training Set
regressor = lm(formula = Weekly_Sales ~ Temperature, data = training_set)  # Fit a linear regression model on the training set using 'Temperature'
print(regressor)  # Print the regression model 

# Predicting the Test Set Results
y_pred = predict(regressor, newdata = test_set)  # Predict the target variable for the test set
print(y_pred)  # Print the predicted values

# Visualizing the Training Set Results
library(ggplot2)  # Load the 'ggplot2' library for data visualization

ggplot() + 
  geom_point(aes(x = training_set$Temperature, y = training_set$Weekly_Sales), colour = "red") +  # Scatter plot of training set
  geom_line(aes(x = training_set$Temperature, y = predict(regressor, newdata = training_set)), colour = "blue") +  # Regression line
  ggtitle("Weekly Sales Vs Temperature (Training Set Results)") +  # Set plot title
  xlab("Temperature") +  # Set x-axis label
  ylab("Weekly Sales")  # Set y-axis label

# Visualizing the Testing Set Results
ggplot() + 
  geom_point(aes(x = test_set$Temperature, y = test_set$Weekly_Sales), colour = "red") +  # Scatter plot of testing set
  geom_line(aes(x = test_set$Temperature, y = y_pred), colour = "blue") +  # Regression line based on predictions
  ggtitle("Weekly Sales Vs Temperature (Testing Set Results)") +  # Set plot title
  xlab("Temperature") +  # Set x-axis label
  ylab("Weekly Sales")  # Set y-axis label

# CPI VS WEEKLY SALES

# Fitting the Simple Linear Regression Model using Training Set
regressor = lm(formula = Weekly_Sales ~ CPI, data = training_set)  # Fit a linear regression model on the training set using 'CPI'
print(regressor)  # Print the regression model summary

# Predicting the Test Set Results
y_pred = predict(regressor, newdata = test_set)  # Predict the target variable for the test set
print(y_pred)  # Print the predicted values

# Visualizing the Training Set Results
ggplot() + 
  geom_point(aes(x = training_set$CPI, y = training_set$Weekly_Sales), colour = "red") +  # Scatter plot of training set
  geom_line(aes(x = training_set$CPI, y = predict(regressor, newdata = training_set)), colour = "blue") +  # Regression line
  ggtitle("Weekly Sales Vs CPI (Training Set Results)") +  # Set plot title
  xlab("CPI") +  # Set x-axis label
  ylab("Weekly Sales")  # Set y-axis label

# Visualizing the Testing Set Results
ggplot() + 
  geom_point(aes(x = test_set$CPI, y = test_set$Weekly_Sales), colour = "red") +  # Scatter plot of testing set
  geom_line(aes(x = test_set$CPI, y = y_pred), colour = "blue") +  # Regression line based on predictions
  ggtitle("Weekly Sales Vs CPI (Testing Set Results)") +  # Set plot title
  xlab("CPI") +  # Set x-axis label
  ylab("Weekly Sales")  # Set y-axis label
