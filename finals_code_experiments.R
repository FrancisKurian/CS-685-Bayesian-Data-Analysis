library(bayesrules)
library(bayesplot)
library(rstan)
library(rstanarm)
library(ggplot2)
library(broom.mixed)
library(dplyr)
library(pROC)
library(car)      # For residual plots and variance checks
library(e1071)    # For skewness calculation

x1 = c(6.7, 5.1, 7.4, 6.5, 7.8, 5.8, 5.7, 3.7, 6, 3.7, 6.3, 6.7, 5.8, 5.8, 7.7,
       7.4, 6, 3.7, 7.3, 5.6, 5.2, 3.4, 6.7, 5.8, 6.3, 5.8, 5.2, 11.2, 5.2, 5.8,
       3.2, 8.7, 5, 5.8, 5.4, 5.3, 2.6, 4.3, 4.8, 5.4, 5.2, 3.6, 8.8, 6.5, 3.4,
       6.5, 4.5, 4.8, 5.1, 3.9, 6.6, 6.4, 6.4, 8.8)

x2 = c(62, 59, 57, 73, 65, 38, 46, 68, 67, 76, 84, 51, 96, 83, 62, 74, 85, 51,
       68, 57, 52, 83, 26, 67, 59, 61, 52, 76, 54, 76, 64, 45, 59, 72, 58, 51,
       74, 8, 61, 52, 49, 28, 86, 56, 77, 40, 73, 86, 67, 82, 77, 85, 59, 78)

x3 = c(81, 66, 83, 41, 115, 72, 63, 81, 93, 94, 83, 43, 114, 88, 67, 68, 28, 41,
       74, 87, 76, 53, 68, 86, 100, 73, 86, 90, 56, 59, 65, 23, 73, 93, 70, 99,
       86, 119, 76, 88, 72, 99, 88, 77, 93, 84, 106, 101, 77, 103, 46, 40, 85,
       72)

x4 = c(2.59, 1.7, 2.16, 2.01, 4.3, 1.42, 1.91, 2.57, 2.5, 2.4, 4.13, 1.86, 3.95,
       3.95, 3.4, 2.4, 2.98, 1.55, 3.56, 3.02, 2.85, 1.12, 2.1, 3.4, 2.95, 3.5,
       2.45, 5.59, 2.71, 2.58, 0.74, 2.52, 3.5, 3.3, 2.64, 2.6, 2.05, 2.85,
       2.45, 1.81, 1.84, 1.3, 6.4, 2.85, 1.48, 3, 3.05, 4.1, 2.86, 4.55, 1.95,
       1.21, 2.33, 3.2)

y = c(200, 101, 204, 101, 509, 80, 80, 127, 202, 203, 329, 65, 830, 330, 168, 
      217, 87, 34, 215, 172, 109, 136, 70, 220, 276, 144, 181, 574, 72, 178, 71,
      58, 116, 295, 115, 184, 118, 120, 151, 148, 95, 75, 483, 153, 191, 123,
      311, 398, 158, 310, 124, 125, 198, 313)

df = data.frame(x1=x1,x2=x2,x3=x3,x4=x4,y=y)


cat("Skewness of df$y =", skewness(df$y), "| Skewness of log(df$y) =", skewness(log(df$y)), "\n")

sd(log(df$y))

centered_intercept=mean(log(df$y))

model_survival <- stan_glm(
  formula = log(y) ~ x1 + x2 + x3 + x4,
  data = df,
  family = gaussian(),
  prior_intercept = normal(centered_intercept, 5),
  prior = normal(0, 10,autoscale = TRUE),
  prior_aux = exponential(0.6,autoscale = TRUE),
  chains = 4,
  iter = 5000*2,
  warmup = 5000,
  seed = 12345
)

prior_summary(model_survival)

neff_ratio(model_survival)
rhat(model_survival)
mcmc_trace(model_survival)
mcmc_dens_overlay(model_survival)

tidy(model_survival, effects  = c('fixed','aux'),
     conf.int = T, conf.level = 0.95)

# 1d

new_data <- tibble::tibble(  x1 = 6.0,    x2 = 65,    x3 = 72,     x4 = 1.50  )

# incorporate mean and variance from the simulation for the point estimate

posterior_predictions <- posterior_predict(
  model_survival,      
  newdata = new_data
)

# Compute point estimate and credible interval
point_estimate <- mean(posterior_predictions)
credible_interval <- quantile(posterior_predictions, probs = c(0.025, 0.975))

# Conver log Y into original scale using exponents
tibble::tibble(
  `Point Estimate (days)` = exp(point_estimate),
  `Lower 95% CI (days)` = exp(credible_interval[1]),
  `Upper 95% CI (days)` = exp(credible_interval[2])
)

#1F

library(rstanarm)
library(loo)

# Define the outcome variable and predictors
outcome <- "log(y)"  # Change as per your dataset
predictors <- c("x1", "x2", "x3", "x4")

# Generate all possible combinations of predictors
model_formulas <- unlist(lapply(1:length(predictors), function(k) {
  combn(predictors, k, FUN = function(x) paste(outcome, "~", paste(x, collapse = " + ")))
}))

# Fit models for each formula and store them
models <- lapply(model_formulas, function(formula) {
  stan_glm(
    formula = as.formula(formula),
    data = df,
    family = gaussian(),
    prior_intercept = normal(centered_intercept, 5),
    prior = normal(0, 10, autoscale = TRUE),
    prior_aux = exponential(0.6, autoscale = TRUE),
    chains = 4,
    iter = 5000*2,
    seed = 123456
  )
})


# Compute LOO for all models
loo_results <- lapply(models, loo)
loo_compare(loo_results)


# Extract the best model
best_model_index <- which.max(sapply(loo_results, function(x) x$estimates["elpd_loo", "Estimate"]))
best_model <- models[[best_model_index]]
# Print summary of the best model
summary(best_model)


# Loop through each model and compute ELPD
for (i in seq_along(models)) {
  model_elpd <- loo(models[[i]])  # Compute LOO for the model
  elpd_df <- rbind(elpd_df, data.frame(
    Model_Index = i,
    ELPD = model_elpd$estimates["elpd_loo", "Estimate"],
    SE = model_elpd$estimates["elpd_loo", "SE"]
  ))
}
# Print the results
print(elpd_df)





# Perform LOO comparison
loo_comparison <- loo_compare(loo_results)

# Convert the loo_compare results to a data frame
loo_df <- as.data.frame(loo_comparison)

# Add model indices to the data frame
loo_df$Model_Index <- seq_len(nrow(loo_df))

# Rearrange the columns to show Model_Index first
loo_df <- loo_df[, c("Model_Index", names(loo_df))]

# Print the data frame
print(loo_df)

loo(models[[1]])


#2 Fake news regression

# Create a new data frame selecting the desired fields
fake_news_df <- data.frame(
  type = fake_news$type,
  title_has_excl = fake_news$title_has_excl,
  title_words = fake_news$title_words,
  negative = fake_news$negative,
  title_caps_percent = fake_news$title_caps_percent,
  text_caps_percent = fake_news$text_caps_percent
)

# View the new data frame
print(fake_news_df)

# Ensure the dependent variable is a factor
fake_news$type <- as.factor(fake_news$type)

# Logistic regression model
logistic_model <- glm(
  type ~ title_has_excl + title_words + negative + title_caps_percent + text_caps_percent,
  data = fake_news,
  family = binomial(link = "logit")
)

# Summary of the model
summary(logistic_model)



model_fake_news <- stan_glm(
  formula = type ~ title_has_excl + title_words + negative + title_caps_percent + text_caps_percent,
  data = fake_news,
  family = binomial(link = "logit"), 
  prior_intercept = normal(0, 10),     
  prior = normal(0, 2.5, autoscale = TRUE),
  chains = 4,                        
  iter = 5000*2,                     
  warmup = 5000,                     
  seed = 12345                      
)

prior_summary(model_fake_news)
neff_ratio(model_fake_news)
rhat(model_fake_news)
mcmc_trace(model_fake_news)
mcmc_dens_overlay(model_fake_news)

tidy(model_fake_news, effects  = c('fixed','aux'),
     conf.int = T, conf.level = 0.90)


set.seed(84735)
classification_summary(model = model_fake_news, data = fake_news_df, cutoff = 0.5)

classification_summary_cv(model=model_fake_news,data=fake_news_df,cutoff = 0.5,k=10)


set.seed(84735)
classification_summary(model = model_fake_news, data = fake_news_df, cutoff = 0.8)

classification_summary(model = model_fake_news, data = fake_news_df, cutoff = 0.71)



library(pROC)
library(bayestestR)

# Assuming your model is already fitted
# model_fake_news <- stan_glm(...)

# Generate predictions
predictions <- posterior_predict(model_fake_news, newdata = fake_news_df)
pred_mean <- colMeans(predictions)

# Create ROC object
roc_obj <- roc(fake_news_df$type, pred_mean)
pROC::auc(roc_obj)  

# Plot ROC curve
plot(roc_obj, main = "ROC Curve for Bayesian Fake News Model", 
     col = "blue", lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "gray")
coords(roc_obj, "best", best.method = "youden")

auc_value <- auc(roc_obj)
print(auc_value)



# true_labels should be a binary vector of actual outcomes (0 or 1)
roc_curve <- roc(response = fake_news_df$type, predictor = colMeans(predictions))
plot(roc_curve, xlim = c(1, 0))
auc(roc_curve)  # Calculate and display AUC

#auc: 0.716  fair/acceptable

best_threshold = coords(roc_curve, "best", ret = "threshold", best.method = "youden")

best_threshold



#3

library(rstan)

# Data provided in the question
age_veh <- c(2.07, 4.28, 2.43, 4.24, 1.52, 7.48, 1.55, 4.95, 12.22, 0.68, 
             9.58, 4.19, 9.86, 1.95, 6.49, 9.03, 10.62, 5.37, 2.43, 4.80, 
             10.95, 0.54, 5.34, 8.37, 11.09, 9.16, 1.07, 9.86, 6.11, 7.79, 
             0.90, 8.05, 8.18, 9.05, 10.26, 10.33, 8.75, 9.60, 6.50, 6.53, 
             12.14, 4.18, 3.93, 5.39, 7.03, 3.42, 7.63, 8.56, 1.57, 8.74)

claim_amt <- c(32.158, 17.333, 24.626, 34.212, 27.237, 22.989, 29.699, 
               24.645, 16.252, 40.845, 23.610, 20.053, 18.934, 26.811, 
               20.895, 18.507, 13.311, 18.895, 32.832, 24.098, 18.407, 
               50.508, 18.726, 17.957, 25.952, 18.037, 32.998, 19.124, 
               17.817, 21.987, 36.295, 22.137, 13.967, 26.902, 16.806, 
               18.110, 27.802, 17.959, 24.661, 21.244, 17.785, 23.572, 
               28.522, 18.877, 21.419, 22.244, 20.068, 21.061, 29.754, 
               20.512)

# Combine data for Stan
stan_data <- list(
  N = length(age_veh),
  x = age_veh,
  y = claim_amt
)

#regular regression:
data <- data.frame(age_veh = age_veh, claim_amt = claim_amt)
model <- lm(claim_amt ~ age_veh, data = data)

# Display summary of the regression
summary(model)


# Load required packages
library(rstan)
library(bayesplot)

# Provided data
age_veh <- c(2.07,4.28,2.43,4.24,1.52,7.48,1.55,4.95,12.22,0.68,
             9.58,4.19,9.86,1.95,6.49,9.03,10.62,5.37,2.43,4.80,
             10.95,0.54,5.34,8.37,11.09,9.16,1.07,9.86,6.11,7.79,
             0.90,8.05,8.18,9.05,10.26,10.33,8.75,9.60,6.50,6.53,
             12.14,4.18,3.93,5.39,7.03,3.42,7.63,8.56,1.57,8.74)

claim_amt <- c(32.158,17.333,24.626,34.212,27.237,22.989,29.699,
               24.645,16.252,40.845,23.610,20.053,18.934,26.811,
               20.895,18.507,13.311,18.895,32.832,24.098,18.407,
               50.508,18.726,17.957,25.952,18.037,32.998,19.124,
               17.817,21.987,36.295,22.137,13.967,26.902,16.806,
               18.110,27.802,17.959,24.661,21.244,17.785,23.572,
               28.522,18.877,21.419,22.244,20.068,21.061,29.754,
               20.512)

# Data list for Stan
claim_data <- list(
  N = length(age_veh),
  x = age_veh,
  y = claim_amt
)


claim_model <- "
data {
  int<lower=0> N;
  vector[N] x;
  vector[N] y;
}
parameters {
  real beta_0;
  real beta_1;
}
model {
  vector[N] shape_param;
  beta_0 ~ normal(0, 100);
  beta_1 ~ normal(0, 100);

  for (n in 1:N) {
    shape_param[n] = exp(beta_0 + beta_1 / sqrt(x[n]));
  }

  y ~ gamma(shape_param, 1);
}
"

# Fit the model using rstan
claim_sim <- stan(model_code = claim_model, data = claim_data,
            chains = 4, iter = 4000, warmup = 2000, seed = 12345)


# Visualize the posterior distributions and diagnostics
mcmc_trace(claim_sim, pars = c("beta_0","beta_1"))
mcmc_hist(claim_sim, pars = c("beta_0","beta_1"))
mcmc_dens(claim_sim, pars = c("beta_0","beta_1"))

#3b
tidy(claim_sim , effects  = c('fixed','aux'),
     conf.int = T, conf.level = 0.90)

#3c


parameters <- rstan::extract(claim_sim)
set.seed(12345)
X_values <- seq(0.5, 15, by = 0.5)

# E[Y|X] = s = exp(beta_0 + beta_1 / sqrt(X))
expected_values <- sapply(X_values, function(xi) {
  exp(parameters$beta_0 + parameters$beta_1 / sqrt(xi))
})

summary_stats <- t(apply(expected_values, 2, function(vals) {
  c(mean = mean(vals),
    lower = quantile(vals, 0.025),
    upper = quantile(vals, 0.975))
}))

colnames(summary_stats) <- c("mean", "lower", "upper")
plot_data <- data.frame(X = X_values,
                        mean = summary_stats[, "mean"],
                        lower = summary_stats[, "lower"],
                        upper = summary_stats[, "upper"])

ggplot(plot_data, aes(x = X, y = mean)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              alpha = 0.2, fill = "blue") +
  labs(title = "Estimated Expected Claim Amount vs. Vehicle Age",
       x = "Age of Vehicle (Years)",
       y = "Expected Claim Amount (Thousands of Dollars)") +
  theme_minimal()

#3d

beta_0 <- parameters$beta_0
beta_1 <- parameters$beta_1

new_x <- 10.5
new_expected_values <- exp(beta_0 + beta_1 / sqrt(new_x))
mean_estimate <- mean(new_expected_values)
ci <- quantile(new_expected_values, c(0.025, 0.975))

mean_estimate
ci


