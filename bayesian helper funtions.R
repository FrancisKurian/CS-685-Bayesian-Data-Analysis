# Beta Binomial

#when Mode is given with a range
mode=0.4
a=17
b = (a * (1 - mode) + 2 * mode - 1) / mode
cat("a:", a, "b:", b, "\n")
plot_beta(a,b,mode=T)+geom_vline(xintercept = c(0.2,0.6),col='red')


#when mean is given with a range
mean=0.45
a=45
b = (a * (1 - mean)) / mean
cat("a:", a, "b:", b, "\n")
plot_beta(a,b,mean=T)+geom_vline(xintercept = c(0.25,0.65),col='red')

#given variance and mean, find a nd b

calculate_a_b <- function(variance, mean) {
  
  # Helper function to calculate b in terms of a
  b_in_terms_of_a <- function(a, mean) {
    return((a * (1 - mean)) / mean)
  }
  
  # Variance function to find a based on variance and mean
  variance_in_terms_of_a <- function(a) {
    # Calculate b in terms of a and mean
    b <- (a * (1 - mean)) / mean
    
    # Calculate the variance using the formula
    calculated_variance = (a * b) / ((a + b)^2 * (a + b + 1))
    
    # Return the difference between calculated variance and the target variance
    return(calculated_variance - variance)
  }
  
  # Use uniroot to solve for a, given the variance and mean
  a_solution <- uniroot(variance_in_terms_of_a, c(0.0001, 1000))$root
  
  # Calculate b using the solved value of a
  b_solution <- (a_solution * (1 - mean)) / mean
  
  return(list(a = a_solution, b = b_solution))
}

# Example usage
variance_value <- 0.02  # Given variance
mean_value <- 0.9       # Given mean

# Solve for a and b
results <- calculate_a_b(variance_value, mean_value)

# Print the results
cat("For variance =", variance_value, "and mean =", mean_value, ", a =", results$a, "and b =", results$b, "\n")

#Poisson-Gamma
mean=5
s=15
r=3
plot_gamma(shape=s,rate=r,mean = T)+geom_vline(xintercept = c(2,7),col='red')



# Let A represent the rate of text messages received per hour, and assume a Gamma distribution to model λ, the rate parameter. We are given:
# Mean number of text messages: 5 messages/hour.
# Standard deviation: 0.25 messages/hour.
# Part (a) Tuning the Gamma Prior:
#   
# We need to tune a Gamma distribution λ ~ Gamma(s,r), where s is the shape parameter and r is the rate parameter. Recall:
#   
#   Mean of Gamma: 
#   μ = s/r
# 
# Variance of Gamma: 
#   Var = s/r^2
# 
# Given:
#   
# μ = 5
# σ = 0.25, so 
# Var = 0.25^2 = 0.0625
# 
# We can solve for s and r:
#   
#   1. From the mean equation: 
#   s/r = 5
# 
# 2. From the variance equation: 
#   s/r^2 = 0.0625
# 
# We solve these two equations:
#   
#   From the mean, 
# r = s/5.
# 
# Substituting this into the variance equation:
#   s/((s/5)^2) = 0.0625
# 
# Simplifying:
#   
#   25s = 0.0625s^2
# 
# Solving for s:
#   
#   s = 25/0.0625 = 400.
# 
# Then:
# 
#   r = 400/5 = 80.
# 
# Thus, the Gamma prior model is 
# λ ~ Gamma(400, 80).

#posterior model =gamma(s+sumY, r+n) 



  