library(bayesrules)
library(ggplot2)

#Playing Around With Beta Functions

plot_beta(7,3)
plot_beta(7.12,2.92)

plot_beta(2,5)
plot_beta(2.12,2.92)

plot_beta(1,5)
plot_beta(1.12,4.92)
plot_beta(0.92,4.84)
plot_beta(0.16,4.84)

plot_beta(5,1)
plot_beta(4.77,1.12)
plot_beta(4.77,0.84)
plot_beta(5.12,1.01)

plot_beta(0.5,0.5)
plot_beta(0.15,0.15)
plot_beta(0.97,0.97)

plot_beta(0.75,0.25)
plot_beta(0.9,0.1)
plot_beta(0.25,0.75)
plot_beta(0.85,0.45)
plot_beta(0.22,0.75)

plot_beta(0,0)

### Selecting the Beta(9,11) prior
c=5
plot_beta(c*9,c*11,mean=T,mode=T)

### Plot Binomial likelihood
plot_binomial_likelihood(80, 100, mle = F)


###Functions that leverage beta-binomial conjugacy
plot_beta_binomial(alpha = 45, beta = 55, y = 30, n = 50)
summarize_beta_binomial(alpha = 45, beta = 55, y = 30, n = 50)



plot_beta(1,1)
plot_beta(0.1,0.1)





# Credit Card Debt Example

## Researchers studied whether doctors completing residency take on high credit card debt.

# Let pi= population proportion of all medical residents with over $3000 in credit card debt

#  Prior belief:  20% do??

########### Analysis with beta(2,8) prior:

# Plot the Beta(2, 8) prior
plot_beta(2,8,mean=T,mode=T)

# Sample of 115 residents revealed 22 had credit card debt over $3000.

# Comparing the prior and the posterior in the credit card debt example:

plot_beta_binomial(alpha = 8, beta = 2, y = 22, n = 115)

# prior and the posterior summary statistics in the credit card debt example:

summarize_beta_binomial(alpha = 2, beta = 8, y = 22, n = 115)


########### Analysis with beta(1,1) (i.e., Uniform) prior:


# Sample of 115 residents revealed 22 had credit card debt over $3000.

# Comparing the prior and the posterior in the credit card debt example:

plot_beta_binomial(alpha = 1, beta = 1, y = 22, n = 115)

# prior and the posterior summary statistics in the credit card debt example:

summarize_beta_binomial(alpha = 1, beta = 1, y = 22, n = 115)


