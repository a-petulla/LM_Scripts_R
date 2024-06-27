library("tidyverse")
library("dplyr")
library("ggplot2")

df <- read.csv("regrex1.csv")

# Create scatter plot
ggplot(data = df, aes(x = x, y = y)) +
  geom_point(color = "blue") +  # Scatter plot with blue points
  labs(
    title = "Scatter Plot of x vs y",  # Title of the plot
    x = "X Axis",                     # Label for x-axis
    y = "Y Axis"                      # Label for y-axis
  )


# Fit linear regression model
lm_model <- lm(y ~ x, data = df)

# Extract R-squared value from model summary
rsquared <- summary(lm_model)$r.squared

# Create scatter plot
plot <- ggplot(data = df, aes(x = x, y = y)) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add linear regression line
  labs(
    title = "Scatter Plot of x vs y",  # Title of the plot
    x = "X Axis",                     # Label for x-axis
    y = "Y Axis"                      # Label for y-axis
  )

# Annotate plot with R-squared value
plot + annotate("text", x = max(df$x) - 0.5, y = max(df$y) - 0.5,
             label = paste("R-squared =", round(rsquared, 3)),
             color = "black", size = 4, hjust = 1)


# Create scatter plot
plot <- ggplot(data = df, aes(x = x, y = y)) +
  geom_point(color = "blue") +  # Scatter plot with blue points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add linear regression line
  labs(
    title = "Scatter Plot of x vs y",  # Title of the plot
    x = "X Axis",                     # Label for x-axis
    y = "Y Axis"                      # Label for y-axis
  )

# Annotate plot with R-squared value
plot + annotate("text", x = max(df$x) - 0.5, y = max(df$y) - 0.5,
             label = paste("R-squared =", round(rsquared, 3)),
             color = "black", size = 4, hjust = 1)



