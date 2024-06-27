library("tidyverse")
library("dplyr")
library("ggplot2")

# Check if command-line arguments were provided
args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  stop("Usage: Rscript script.R <parameter>")
}

# First argument is the parameter passed from the terminal
filename <- args[1]

df <- read.csv(filename)

# Create scatter plot
orig_plot <- ggplot(data = df, aes(x = x, y = y)) +
  geom_point(color = "blue") +  # Scatter plot with blue points
  labs(
    title = "Scatter Plot of x vs y",  # Title of the plot
    x = "X Axis",                     # Label for x-axis
    y = "Y Axis"                      # Label for y-axis
  )


# Save plot to a PNG file
png("r_orig.png", width = 800, height = 600)  # Specify dimensions if needed
print(orig_plot)  # Print the plot object to the file
dev.off()    # Close the PNG device


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
combined_plot <- plot + annotate("text", x = max(df$x) - 0.5, y = max(df$y) - 0.5,
             label = paste("R-squared =", round(rsquared, 3)),
             color = "black", size = 4, hjust = 1)


# Save plot to a PNG file
png("r_lm.png", width = 800, height = 600)  # Specify dimensions if needed
print(combined_plot)  # Print the plot object to the file
dev.off()    # Close the PNG device
