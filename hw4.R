data <- read.table("gauge.txt",  header = TRUE)
library(pander)

# QUESTION 1
model <- lm(gain ~ density, data = data) # fit a regression model
pander(summary(model))

# graph: scatterplot
plot(data$density, data$gain, main = 'Gamma Ray Intensity as a Function of the Density of Polyethylene Blocks', xlab = 'Density of Polyethylene Blocks', ylab = 'Gamma Ray Intensity')
abline(model, col = 'red') # plot line of best fit

res <- resid(model)
plot(fitted(model), res, xlab='Predicted Gamma Ray Intensity', ylab = 'Residuals', main = 'Residual vs. Fitted Plot of Model')
abline(0,0)

qqnorm(res)
qqline(res)

plot(density(res))

set.seed(123)
shapiro_result <- shapiro.test(data$gain)
pander(shapiro_result)

# QUESTION 2

data$lggain <- log(data$gain)
log_model <- lm(lggain ~ density, data = data) # fit a regression model
pander(summary(log_model))

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
# Define true relationship parameters (arbitrarily chosen)
A <- 150
beta <- -0.05
n <- 90

# GENERATING SYNTHETIC DATA
set.seed(123)
# DENSITY - use the density given from the dataset
true_densities <- data$density

# Simulate measurement errors
# Set the mean to 0 to simulate unbiased random errors
density_noise <- rnorm(n, mean = 0, sd = 0.05)
recorded_densities <- true_densities + density_noise # observed x with noise

# GAINS - based on the equation g = Ae^(beta *d)
true_gains <- A * exp(beta * true_densities) # theoretical values, no noise

# Fit the exponential model to the noisy data
noise_fit <- lm(log(true_gains) ~ recorded_densities)
fitted_gains <- exp(coef(noise_fit)[1]) * exp(coef(noise_fit)[2] * recorded_densities) # predicted y with noise

library(ggplot2)
noise_data <- data.frame(Density = recorded_densities, True_Gains = true_gains, Fitted_Gains = fitted_gains)
ggplot(noise_data, aes(x = Density)) +
  geom_point(aes(y = True_Gains, color = "True Gains"), size = 2) +
  geom_line(aes(y = Fitted_Gains, color = "Fitted Gains"), size = 1) +
  labs(
    title = 'Gamma Ray Intensity as a Function of Density',
    y = 'Observed Gamma Ray Intensity (Gain)'
  )

res_noise <- resid(noise_fit)
plot(fitted(noise_fit), res_noise, xlab='Predicted Gamma Ray Intensity', ylab = 'Residuals', main = 'Residual vs. Fitted Plot of Log Model with Noise')
abline(0,0)

qqnorm(res_noise)
qqline(res_noise)

set.seed(123)
shapiro_result_noise <- shapiro.test(res_noise)
pander(shapiro_result_noise)

# QUESTION 4
library(pander)
pander(summary(data[c('gain', 'density')]))

model <- lm(gain ~ density, data = data) # fit a regression model

# Generate predictions with confidence intervals
predictions <- predict(model, newdata = data.frame(density = data$density), 
                       interval = "confidence", level = 0.95)

# Plot the scatterplot
plot(data$density, data$gain, 
     main = 'Gamma Ray Intensity as a Function of the Density of Polyethylene Blocks', 
     xlab = 'Density of Polyethylene Blocks', 
     ylab = 'Gamma Ray Intensity')

# Add the regression line
abline(model, col = 'red')

# Add confidence intervals as shaded bands
# Sort values for proper plotting
sorted_indices <- order(data$density)
lines(data$density[sorted_indices], predictions[sorted_indices, "lwr"], col = "green", lty = 2) # Lower CI
lines(data$density[sorted_indices], predictions[sorted_indices, "upr"], col = "green", lty = 2) # Upper CI

# Alternatively, use polygon to fill the area
polygon(c(data$density[sorted_indices], rev(data$density[sorted_indices])),
        c(predictions[sorted_indices, "lwr"], rev(predictions[sorted_indices, "upr"])),
        col = rgb(0, 1, 0, 0.2), border = NA) # Semi-transparent green

res <- resid(model)
plot(fitted(model), res, xlab='Predicted Gamma Ray Intensity', ylab = 'Residuals', main = 'Residual vs. Fitted Plot of Model')
abline(0,0)

# QUESTION 5
# Use an inverted model, where we predict density from gain
inverted_model <- lm(density ~ gain, data = data)

# Generate predictions with prediction intervals
predictions <- predict(inverted_model, newdata = data.frame(gain = data$gain), interval = 'prediction')

# Plot the scatterplot
plot(data$gain, data$density,
     main = 'Gamma Ray Intensity as a Function of the Density of Polyethylene Blocks',
     xlab = 'Gamma Ray Intensity',
     ylab = 'Density of Polyethylene Blocks')

# Add regression line
abline(inverted_model, col = 'red')

# Add prediction intervals as shaded bands
# Sort values for proper plotting
sorted_indices <- order(data$gain)
lines(data$gain[sorted_indices], predictions[sorted_indices, "lwr"], col = "green", lty = 2) # Lower PI
lines(data$gain[sorted_indices], predictions[sorted_indices, "upr"], col = "green", lty = 2) # Upper PI

# Alternatively, use polygon to fill the area
polygon(c(data$gain[sorted_indices], rev(data$gain[sorted_indices])),
        c(predictions[sorted_indices, "lwr"], rev(predictions[sorted_indices, "upr"])),
        col = rgb(0, 1, 0, 0.2), border = NA) # Semi-transparent green

# Make prediction intervals
predictions <- predict(inverted_model, newdata = data.frame(gain = data$gain), interval = "prediction")
# Specific prediction intervals for gamma ray intensities 38.6 and 426.7
specific_preds <- predict(inverted_model, newdata = data.frame(gain = c(38.6, 426.7)), interval = "prediction")
low_point_estimate <- specific_preds[1] # Point estimate for gamma ray intensity 38.6
high_point_estimate <- specific_preds[2] # Point estimate for gamma ray intensity 426.7

# Plot the scatterplot
plot(data$gain, data$density,
     main = 'Gamma Ray Intensity as a Function of the Density of Polyethylene Blocks',
     xlab = 'Gamma Ray Intensity',
     ylab = 'Density of Polyethylene Blocks')

abline(inverted_model, col = 'red') # Add regression line
abline(v = c(38.6, 426.7), col = "blue", lwd = 2, lty = 2) # Add vertical lines to show prediction

# Add prediction intervals as shaded bands
# Sort values for proper plotting
sorted_indices <- order(data$gain)
lines(data$gain[sorted_indices], predictions[sorted_indices, "lwr"], col = "green", lty = 2) # Lower PI
lines(data$gain[sorted_indices], predictions[sorted_indices, "upr"], col = "green", lty = 2) # Upper PI

# Alternatively, use polygon to fill the area
polygon(c(data$gain[sorted_indices], rev(data$gain[sorted_indices])),
        c(predictions[sorted_indices, "lwr"], rev(predictions[sorted_indices, "upr"])),
        col = rgb(0, 1, 0, 0.2), border = NA) # Semi-transparent green

# QUESTION 6
dataset <- read.table("gauge.txt",  header = TRUE)
no_0.508 <- dataset[dataset$density != 0.508, ]

inverted_model <- lm(density ~ gain, data = no_0.508) 
predictions <- predict(inverted_model, newdata = data.frame(gain = no_0.508$gain), interval = 'confidence', level = 0.95)

# Plot the scatterplot
plot(no_0.508$gain, no_0.508$density,
     main = 'Inverse Model without Density = 0.508',
     xlab = 'Gamma Ray Intensity',
     ylab = 'Density of Polyethylene Blocks')

# Add regression line
abline(inverted_model, col = 'red')
abline(v = c(38.6), col = "blue", lwd = 2, lty = 2) # Add horizontal line

# Add confidence intervals as shaded bands
# Sort values for proper plotting
sorted_indices <- order(no_0.508$gain)
lines(no_0.508$gain[sorted_indices], predictions[sorted_indices, "lwr"], col = "green", lty = 2) # Lower CI
lines(no_0.508$gain[sorted_indices], predictions[sorted_indices, "upr"], col = "green", lty = 2) # Upper CI

# Alternatively, use polygon to fill the area
polygon(c(no_0.508$gain[sorted_indices], rev(no_0.508$gain[sorted_indices])),
        c(predictions[sorted_indices, "lwr"], rev(predictions[sorted_indices, "upr"])),
        col = rgb(0, 1, 0, 0.2), border = NA) # Semi-transparent green
pander(predict(inverted_model, newdata = data.frame(gain = 38.6), interval = 'confidence', level = 0.95))

dataset <- read.table("gauge.txt",  header = TRUE)
no_0.001 <- dataset[dataset$density != 0.001, ]

inverted_model <- lm(density ~ gain, data = no_0.001) 
predictions <- predict(inverted_model, newdata = data.frame(gain = no_0.001$gain), interval = 'confidence', level = 0.95)

# Plot the scatterplot
plot(no_0.001$gain, no_0.001$density,
     main = 'Inverse Model without Density = 0.001',
     xlab = 'Gamma Ray Intensity',
     ylab = 'Density of Polyethylene Blocks')

# Add regression line
abline(inverted_model, col = 'red')
abline(v = c(38.6), col = "blue", lwd = 2, lty = 2) # Add horizontal line

# Add confidence intervals as shaded bands
# Sort values for proper plotting
sorted_indices <- order(no_0.001$gain)
lines(no_0.001$gain[sorted_indices], predictions[sorted_indices, "lwr"], col = "green", lty = 2) # Lower CI
lines(no_0.001$gain[sorted_indices], predictions[sorted_indices, "upr"], col = "green", lty = 2) # Upper CI

# Alternatively, use polygon to fill the area
polygon(c(no_0.001$gain[sorted_indices], rev(no_0.001$gain[sorted_indices])),
        c(predictions[sorted_indices, "lwr"], rev(predictions[sorted_indices, "upr"])),
        col = rgb(0, 1, 0, 0.2), border = NA) # Semi-transparent green

pander(predict(inverted_model, newdata = data.frame(gain = 38.6), interval = 'confidence', level = 0.95))

# ADVANCED ANALYSIS
nls_fx <- function(density, A, beta){
  A * exp(beta * density) # from the write-up: g = A * e^(beta * d)
}
nls_model <- nls(gain ~ nls_fx(density, A, beta), data = data, start = list(A = 400, beta = -1))

predictions <- predict(nls_model, newdata = data.frame(density = data$density))

# Plot the scatterplot
plot(data$density, data$gain,
     main = 'Gamma Ray Intensity as a Function of the Density of Polyethylene Blocks',
     xlab = 'Density of Polyethylene Blocks',
     ylab = 'Gamma Ray Intensity')

lines(data$density, predictions, col='red')

pander(summary(nls_model))
res <- residuals(nls_model)
rss <- sum(res ** 2) # residual sum of squares
tss <- sum((data$gain - mean(data$gain)) ** 2)
r_squared <- 1 - (rss / tss) # 0.998
mse <- mean(res ** 2) # 35.345
