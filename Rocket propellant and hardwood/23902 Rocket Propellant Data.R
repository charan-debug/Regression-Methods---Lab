# Importing the Dataset
rocket_data <- read.csv("The Rocket propellant Data.csv")
# Splitting the Dataset into Training and Testing set
library(caTools)
set.seed(123)
indices <- sample(1:nrow(rocket_data), nrow(rocket_data)*0.67)

training_set <- rocket_data[indices, ]
test_set <- rocket_data[-indices, ]

# Fitting the Simple Linear Regression Model using Training Set
regressor <- lm(formula = Shear.strength.y. ~ Age.of.propellant.x., data = training_set)

# Predicting the Test Set Results
y_pred <- predict(regressor, newdata = test_set)
print(y_pred)
# Visualizing the Training Set Results
library(ggplot2)

ggplot(training_set, aes(x = Age.of.propellant.x., y = Shear.strength.y.)) +
  geom_point(color = "red") +
  geom_line(aes(y = predict(regressor, newdata = training_set)), color = "blue") +
  ggtitle("Shear Strength Vs Age of Propellant (Training Set Results)") +
  xlab("Age of Propellant") +
  ylab("Shear Strength")

# Visualizing the Testing Set Results
ggplot(test_set, aes(x = Age.of.propellant.x., y = Shear.strength.y.)) +
  geom_point(color = "red") +
  geom_line(aes(y = y_pred), color = "blue") +
  ggtitle("Shear Strength Vs Age of Propellant (Testing Set Results)") +
  xlab("Age of Propellant") +
  ylab("Shear Strength")