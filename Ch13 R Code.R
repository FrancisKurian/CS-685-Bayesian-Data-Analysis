library(ggplot2)
library(TeachingDemos)
library(bayesrules)
library(rstan)
library(bayesplot)
library(broom.mixed)
library(dplyr)
library(rstanarm)
library(tidybayes)
library(janitor)
library(pROC)

data(weather_perth)
weather <- weather_perth %>% 
  select(day_of_year, raintomorrow, humidity9am, humidity3pm, raintoday)

# Run a prior simulation
rain_model_prior <- stan_glm(raintomorrow ~ humidity9am,
                             data = weather, family = binomial,
                             prior_intercept = normal(-1.4, 0.7),
                             prior = normal(0.07, 0.035),
                             chains = 4, iter = 5000*2, seed = 84735,
                             prior_PD = TRUE)

# Plot 100 prior models with humidity
weather %>% 
  add_linpred_draws(rain_model_prior, ndraws = 100, transform = TRUE) %>% 
  ggplot(aes(x = humidity9am, y = raintomorrow)) +
  geom_line(aes(y = .linpred, group = .draw), size = 0.1)


# Plot the observed proportion of rain in 100 prior datasets
weather %>% 
  add_predicted_draws(rain_model_prior, ndraws = 100) %>% 
  group_by(.draw) %>% 
  summarize(proportion_rain = mean(.prediction == 1)) %>% 
  ggplot(aes(x = proportion_rain)) +
  geom_histogram(color = "white")


#Now add data 
rain_model_1 <- update(rain_model_prior, prior_PD = FALSE)

#Diagnostics
mcmc_trace(rain_model_1)
mcmc_dens_overlay(rain_model_1)
mcmc_acf(rain_model_1)
  

weather %>%
  add_linpred_draws(rain_model_1, ndraws = 100, transform = T) %>%
  ggplot(aes(x = humidity9am, y = raintomorrow)) +
  geom_line(aes(y = .linpred, group = .draw), alpha = 0.15) + 
  labs(y = "probability of rain")


posterior_interval(rain_model_1, prob = 0.80)
exp(posterior_interval(rain_model_1, prob = 0.80))

rain_model_1_df = data.frame(rain_model_1)
head(rain_model_1_df)

set.seed(84735)
binary_prediction <- posterior_predict(
  rain_model_1, newdata = data.frame(humidity9am = 99))

table(binary_prediction)

# Posterior predictions of binary outcome - from scratch
set.seed(84735)
rain_model_1_df <- as.data.frame(rain_model_1) %>% 
  mutate(log_odds = `(Intercept)` + humidity9am*99,
         odds = exp(log_odds),
         prob = odds / (1 + odds),
         Y = rbinom(20000, size = 1, prob = prob))


# Check it out
head(rain_model_1_df, 2)

pp  = pp_check(rain_model_1,nreps = 100)


proportion_rain <- function(x){mean(x == 1)}
pp_check(rain_model_1, nreps = 100,
         plotfun = "stat", stat = "proportion_rain") + 
  xlab("probability of rain")

# Posterior predictive models for each day in dataset
set.seed(84735)
rain_pred_1 <- posterior_predict(rain_model_1, newdata = weather)
dim(rain_pred_1)

weather_classifications = weather %>% 
  mutate(rain_prob = colMeans(rain_pred_1),
         rain_class_1 = as.numeric(rain_prob >= 0.5)) %>% 
  select(humidity9am, rain_prob, rain_class_1, raintomorrow)


head(weather_classifications, 3)

weather_classifications %>% 
  tabyl(raintomorrow, rain_class_1) %>% 
  adorn_totals(c("row", "col"))

set.seed(84735)
classification_summary(model = rain_model_1, data = weather, cutoff = 0.5)

set.seed(84735)
classification_summary(model = rain_model_1, data = weather, cutoff = 0.7)

set.seed(84735)
classification_summary(model = rain_model_1, data = weather, cutoff = 0.3)


# true_labels should be a binary vector of actual outcomes (0 or 1)
roc_curve <- roc(response = weather_classifications$raintomorrow, predictor = weather_classifications$rain_prob)
plot(roc_curve, xlim = c(1, 0))
auc(roc_curve)  # Calculate and display AUC

#auc: 0.716  fair/acceptable

best_threshold = coords(roc_curve, "best", ret = "threshold", best.method = "youden")

best_threshold
