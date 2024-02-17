# Importing the Dataset
eucalypt_data <- read.csv("eucalypt_hardwoods.csv")

# Splitting the Dataset into Training and Testing set
install.packages("caTools")
library(caTools)
set.seed(123)
split <- sample.split(eucalypt_data$hardness, SplitRatio = 2/3)
print(split)

training_set <- subset(eucalypt_data, split == TRUE)
test_set <- subset(eucalypt_data, split == FALSE)

# Fitting the Simple Linear Regression Model using Training Set
regressor <- lm(formula = hardness ~ density, data = training_set)
print(regressor)

# Predicting the Test Set Results
y_pred <- predict(regressor, newdata = test_set)

print(y_pred)

# Visualizing the Training Set Results
#install.packages("ggplot2")
library(ggplot2)

ggplot(training_set, aes(x = density, y = hardness)) +
  geom_point(color = "red") +
  geom_line(aes(y = predict(regressor, newdata = training_set)), color = "blue") +
  ggtitle("Hardness Vs Density (Training Set Results)") +
  xlab("Density") +
  ylab("Hardness")

# Visualizing the Testing Set Results
ggplot(test_set, aes(x = density, y = hardness)) +
  geom_point(color = "red") +
  geom_line(aes(y = y_pred), color = "blue") +
  ggtitle("Hardness Vs Density (Testing Set Results)") +
  xlab("Density") +
  ylab("Hardness")
