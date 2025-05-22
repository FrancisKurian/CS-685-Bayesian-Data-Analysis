library(ggplot2)

# Define the marginal density function
f_y <- function(y, sigma) {
  0.5 * (1/(sqrt(2*pi*sigma^2)) * exp(-(y-1)^2/(2*sigma^2)) +
           1/(sqrt(2*pi*sigma^2)) * exp(-(y-2)^2/(2*sigma^2)))
}

# Create a data frame with y values and densities for each sigma
y <- seq(-2, 5, by = 0.01)
df <- data.frame(
  y = rep(y, 3),
  density = c(sapply(y, f_y, sigma = 1),
              sapply(y, f_y, sigma = 2),
              sapply(y, f_y, sigma = 3)),
  sigma = factor(rep(c(1, 2, 3), each = length(y)))
)

# Create the plot
ggplot(df, aes(x = y, y = density, color = sigma)) +
  geom_line() +
  labs(title = "Marginal Probability Density for y",
       x = "y",
       y = "Density",
       color = "σ") +
  theme_minimal()
