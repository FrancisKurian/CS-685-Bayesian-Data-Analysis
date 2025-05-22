library(rstan)
library(bayesrules)
library(rstanarm)
library(bayesplot)
library(broom.mixed)
library(dplyr)
library(ggplot2)
library(tidybayes)

###############################################
#####Capital Bike Share Program via rstan######
###############################################
# STEP 1: DEFINE the model
stan_bike_model <- "
  data {
    int<lower = 0> n;
    vector[n] Y;
    vector[n] X;
  }
  parameters {
    real beta0;
    real beta1;
    real<lower = 0> sigma;
  }
  model {
    Y ~ normal(beta0 + beta1 * X, sigma);
    beta0 ~ normal(-2000, 1000);
    beta1 ~ normal(100, 40);
    sigma ~ exponential(0.0008);
  }
"

stan_bike_sim <- 
  stan(model_code = stan_bike_model, 
       data = list(n = nrow(bikes), Y = bikes$rides, X = bikes$temp_feel), 
       chains = 4, iter = 5000*2, seed = 84735)

###############################################
###Capital Bike Share Program via rstanarm#####
###############################################
bike_model <- stan_glm(rides ~ temp_feel, data = bikes,
                       family = gaussian,
                       prior_intercept = normal(5000, 1000),
                       prior = normal(100, 40), 
                       prior_aux = exponential(0.0008),
                       chains = 4, iter = 5000*2, seed = 84735)

###############################################
#################Diagnostics###################
###############################################
neff_ratio(bike_model)

rhat(bike_model)

mcmc_trace(bike_model)

mcmc_dens_overlay(bike_model)


###############################################
###################Results#####################
###############################################
tidy(bike_model)

tidy(bike_model, effects  = c('fixed','aux'))

tidy(bike_model, effects  = c('fixed','aux'),
     conf.int = T, conf.level = 0.8)


###############################################
########Interpreting the Posterior#############
###############################################
# 50 simulated model lines
bikes %>%
  add_linpred_draws(bike_model, ndraws = 50) %>%
  ggplot(aes(x = temp_feel, y = rides)) +
  geom_line(aes(y = .linpred, group = .draw), alpha = 0.15) + 
  geom_point(data = bikes, size = 0.05)+
  ylim(c(0,10000))


# Simulate four sets of data
bikes %>%
  add_predicted_draws(bike_model, ndraws = 4) %>%
  ggplot(aes(x = temp_feel, y = rides)) +
  geom_point(aes(y = .prediction, group = .draw), size = 0.2) + 
  facet_wrap(~ .draw)+
  ylim(c(0,12000))



###############################################
#Posterior Predictive Model for a Single Parameter Set 
###############################################
bike_model_df <- as.data.frame(bike_model)
head(bike_model_df)

#First parameter set
first_set <- head(bike_model_df, 1)
first_set

#Calculate mu
mu <- first_set$`(Intercept)` + first_set$temp_feel * 75
mu

#Sample data from this distribution with sigma from first set
set.seed(381)
y_new <- rnorm(1, mean = mu, sd = first_set$sigma)
y_new


###############################################
#Posterior Predictive Model w/ RStan 
###############################################
predict75 = 
  posterior_predict(bike_model, newdata = data.frame(temp_feel = 75))
head(predict75)

# Construct a 95% posterior credible interval
posterior_interval(predict75, prob = 0.95)


# Plot the approximate predictive model
mcmc_dens(predict75) + 
  xlab("predicted ridership on a 75 degree day")


###############################################
#Variability in Average vs Daily Predictor
###############################################
#Predict mu
bike_model_df %>%
  mutate(mu = `(Intercept)` + temp_feel * 75) %>%
  ggplot(aes(x = mu)) + 
  geom_density() + 
  ylim(c(0,0.007))

#Predict y_mu
predict75.df = data.frame(predict75)

predict75.df %>% 
  ggplot(aes(x = X1)) + 
  geom_density()+ 
  ylim(c(0,0.007))


###############################################
#############Autotuning via rStan##############
###############################################

bike_model_default <- stan_glm(
  rides ~ temp_feel, data = bikes, 
  family = gaussian,
  prior_intercept = normal(5000, 2.5, autoscale = TRUE),
  prior = normal(0, 2.5, autoscale = TRUE), 
  prior_aux = exponential(1, autoscale = TRUE),
  chains = 4, iter = 5000*2, seed = 84735)

prior_summary(bike_model_default)


# Examine understanding with default priors
bike_default_priors <- update(bike_model_default, prior_PD = TRUE)

# 200 prior model lines
bikes %>%
  add_linpred_draws(bike_default_priors, ndraws = 200) %>%
  ggplot(aes(x = temp_feel, y = rides)) +
  geom_line(aes(y = .linpred, group = .draw), alpha = 0.15)

# 4 prior simulated datasets
set.seed(381)
bikes %>%
  add_predicted_draws(bike_default_priors, ndraws = 4) %>%
  ggplot(aes(x = temp_feel, y = rides)) +
  geom_point(aes(y = .prediction, group = .draw)) + 
  facet_wrap(~ .draw)

