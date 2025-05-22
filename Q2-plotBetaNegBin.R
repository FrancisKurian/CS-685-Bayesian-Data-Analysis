plot_beta(3,2)
plot_beta_binomial(alpha = 3, beta = 2,y=5,n=12)
summarize_beta_binomial(alpha = 3, beta = 2,y=5,n=12)

require(ggplot2)

# Define the likelihood function for the negative binomial
neg_binom_likelihood <- function(p, r, k) {
  return(dnbinom(k, size = r, prob = p))
}

plot_neg_bin = function(alpha_post,beta_post){

  # Define parameters
  r <- 5  # number of successes
  k <- 7  # number of failures
  
  # Prior parameters
  alpha_prior <- 3
  beta_prior <- 2
  
  # Create a sequence of p values between 0 and 1
  p_values <- seq(0, 1, length.out = 1000)
  p_values <- p_values[2:1000]
  
  # Calculate the likelihood for each p value
  likelihood_values <- sapply(p_values, function(z) neg_binom_likelihood(r = r, k = k, p=z))
  
  # Scale the likelihood function
  scaled_likelihood <- likelihood_values / max(likelihood_values)
  
  # Calculate the prior and posterior Beta densities
  prior_values <- dbeta(p_values, alpha_prior, beta_prior)
  posterior_values <- dbeta(p_values, alpha_post, beta_post)
  
  # Scale the prior and posterior for plotting (just to align visually with the likelihood)
  scaled_prior <- prior_values / max(prior_values)
  scaled_posterior <- posterior_values / max(posterior_values)
  
  # Create a dataframe for plotting
  plot_data <- data.frame(
    p = p_values,
    Scaled_Likelihood = scaled_likelihood,
    Scaled_Prior = scaled_prior,
    Scaled_Posterior = scaled_posterior
  )
  
  # Plot all three distributions: Likelihood, Prior, and Posterior
  ggplot(plot_data, aes(x = p)) +
    geom_line(aes(y = Scaled_Likelihood, color = "Scaled Likelihood"), size = 1) +
    geom_line(aes(y = Scaled_Prior, color = "Scaled Prior"), size = 1) +
    geom_line(aes(y = Scaled_Posterior, color = "Scaled Posterior"), size = 1) +
    scale_color_manual(values = c("blue", "red", "green")) +
    labs(title = "Scaled Likelihood, Prior, and Posterior Distributions",
         x = "Probability of Success (p)",
         y = "Scaled Density",
         color = "Distributions") +
    theme_minimal()
}

plot_neg_bin(8,9)


