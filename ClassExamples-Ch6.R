library(bayesrules)
library(rstan)
library(bayesplot)
library(dplyr)
library(ggplot2)

#Simulation Step 1: Discretize domain
grid_data = data.frame(pi_grid = seq(0,1,length=101))
grid_data$pi_grid

#Simulation Step 2: Calc likelihood & prior
grid_data = grid_data %>% 
  mutate(prior = dbeta(pi_grid,2,2),
         likelihood = dbinom(9,10,pi_grid))

#Simulation Step 3: Estimate posterior
grid_data = grid_data %>% 
  mutate(unnormalized = likelihood*prior,
         posterior = unnormalized/sum(unnormalized))

#Plot the posterior
ggplot(grid_data, aes(x=pi_grid, y=posterior))+
  geom_point()+
  geom_segment(aes(xend=pi_grid,y=0,yend=posterior))

#Simulation Step 4: Sample from the posterior
set.seed(381)
post_sample = sample_n(grid_data, size = 10000, 
                       weight = posterior, replace=T)

#Plot values sampled from the posterior
ggplot(post_sample, aes(x=pi_grid))+
  geom_histogram(aes(y = after_stat(density)), binwidth=0.05, col='black')

#Contrast sampled values w/ Beta(11,3)
ggplot(post_sample, aes(x=pi_grid))+
  geom_histogram(aes(y = after_stat(density)),binwidth=0.05, col='black')+
  stat_function(fun=dbeta, args = list(11,3),col='blue',size=0.75)+
  xlim(0,1)
  

################################################
###########Beta-Binomial in Stan##############
#Step 1: Define the data and model
bb_model = "
  data{
    int<lower=0,upper=10> Y;
  }
  parameters{
    real<lower=0, upper=1> pi;
  }
  model{
    Y~binomial(10,pi);
    pi~beta(2,2);
  }
"
bb_model

#Step 2: Simulate the posterior
library(rstan)
bb_sim = stan(model_code=bb_model, data = list(Y=9),
              chains = 4, iter = 5000*2, seed=381)

s =summary(bb_sim)
as.array(bb_sim)

#Step 3a: Plot the traces
mcmc_trace(bb_sim, pars='pi')

#Step 3b: Plot histogram
mcmc_hist(bb_sim, pars='pi')

#Step 3c: Plot histogram
mcmc_dens(bb_sim, pars='pi')+
  stat_function(fun=dbeta, args = list(11,3),col='magenta',size=0.75)

#Step 4: Effective Sample Size
neff_ratio(bb_sim, pars='pi')

#Step 5: Autocorrelation
mcmc_acf(bb_sim, pars='pi')

#Step 6: r_hat
rhat(bb_sim, pars='pi')

