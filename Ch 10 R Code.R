library(rstan)
library(bayesrules)
library(rstanarm)
library(bayesplot)
library(broom.mixed)
library(dplyr)
library(ggplot2)
library(tidybayes)


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
##########Posterior Predictive Check########### 
###############################################
pp_check(bike_model, nreps = 100) + 
  xlab("rides")

###############################################
#####Posterior Predictive Summarires########### 
###############################################

#Data on 10/22/2012
bikes %>% 
  filter(date == "2012-10-22") %>% 
  select(temp_feel, rides)

obs = bikes %>% 
  filter(date == "2012-10-22") %>% 
  select(temp_feel, rides) %>%
  pull(rides)

bike_model_df <- as.data.frame(bike_model)

# Simulate the posterior predictive model for this day
set.seed(84735)
predict_75 = bike_model_df %>% 
  mutate(mu = `(Intercept)` + temp_feel*75,
         y_new = rnorm(20000, mean = mu, sd = sigma))

# Plot the posterior predictive model
ggplot(predict_75, aes(x = y_new)) + 
  geom_density()

ggplot(predict_75, aes(x = y_new)) + 
  geom_density()+
  geom_vline(xintercept = 6228, col = 'magenta')

#Absolute Prediction Error
predict_75_mu = mean(predict_75$y_new)
mae = abs(obs-predict_75_mu)
mae

#Absolute Prediction Error
mae_sd = mae/sd(predict_75$y_new)
mae_sd


#Posterior Prediction Interval
predict_75 %>% 
  summarize(lower_50 = quantile(y_new, 0.25),
            upper_50 = quantile(y_new, 0.75),
            lower_95 = quantile(y_new, 0.025),
            upper_95 = quantile(y_new, 0.975))

obs

#Prediction stats over all observation
prediction_summary(bike_model,bikes)

#Prediction viz over all observations
set.seed(84735)
predictions <- posterior_predict(bike_model, newdata = bikes)

ppc_intervals(bikes$rides, yrep = predictions, x = bikes$temp_feel, 
              prob = 0.5, prob_outer = 0.95)

#Prediction stats over all observation with cross-validation
prediction_summary_cv(model = bike_model,bikes,k=10)

#ELPD
model_elpd <- loo(bike_model)
model_elpd$estimates
