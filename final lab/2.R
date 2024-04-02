dataset = read.csv("GRE_Total.csv")
#mlr
model <- lm(GGPA ~ GRE_Total + UGPA , data=dataset)

summary(model)
#p-value
p_values <- summary(model)$coefficients[,4]
p_values

predicted <- predict(model)

# Plot actual versus predicted GGPA
plot(dataset$GGPA, predicted, main = "Actual vs Predicted GGPA",
     xlab = "Actual GGPA", ylab = "Predicted GGPA", pch = 19)
abline(0, 1, col = "blue")