# QUESTION 1

# QUESTION 2

data$lggain <- log(data$gain)
log_model <- lm(lggain ~ density, data = data) # fit a regression model
summary(log_model)

# graph: scatterplot
plot(data$density, data$lggain, main = 'Log Gamma Ray Intensity as a Function of the Density of Polyethylene Blocks', xlab = 'Density of Polyethylene Blocks', ylab = 'Log of Gamma Ray Intensity')
abline(log_model, col = 'red') # plot line of best fit

res <- resid(log_model)
plot(fitted(log_model), res, xlab='Predicted Gamma Ray Intensity', ylab = 'Residuals', main = 'Residual vs. Fitted Plot of Log Model')
abline(0,0)

qqnorm(res)
qqline(res)

set.seed(123)
shapiro_result <- shapiro.test(data$lggain)
print(shapiro_result)

# QUESTION 3

# QUESTION 4

# QUESTION 5

# QUESTION 6