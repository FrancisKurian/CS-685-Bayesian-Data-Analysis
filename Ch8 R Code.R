library(ggplot2)
library(TeachingDemos)
library(bayesrules)
library(rstan)
library(bayesplot)
library(broom.mixed)
library(dplyr)


####Credible Intervals for a Beta Distribution
#95% CI
qbeta(c(0.025,0.975),18,92)
qbeta(c(0.025,0.975),4,16)

#80% CI
qbeta(c(0.1,0.9),18,92)
qbeta(c(0.1,0.9),4,16)

a=2
b=8

####Credible intervals via hpd
hpd80 = hpd(qbeta, shape1 = a, shape2 = b, conf=0.8)

plot_beta(a,b)+
  xlim(0,0.7)+
  stat_function(fun = dbeta,
                args = list(shape1 = a, shape2=b), 
                geom = "area", 
                fill = "steelblue", n = 1001,
                xlim = hpd80)

equal = qbeta(c(0.1,0.9),a,b)

plot_beta(a,b)+
  xlim(0,0.7)+
  stat_function(fun = dbeta,
                args = list(shape1 = a, shape2=b), 
                geom = "area", 
                fill = "steelblue", n = 1001,
                xlim = equal)



plot_beta(18,92)+
  xlim(0,0.4)+
  stat_function(fun = dbeta,
                args = list(shape1 = 18, shape2=92), 
                geom = "area", 
                fill = "steelblue", n = 1001,
                xlim = c(0.17,0.23))

pbeta(0.23,18,92) - pbeta(0.17,18,92)

############################################
#########CREATE A POSTERIOR MODEL###########
##Simulating a posterior from MOMA example
# STEP 1: DEFINE the model
art_model <- "
  data {
    int<lower = 0, upper = 100> Y;
  }
  parameters {
    real<lower = 0, upper = 1> pi;
  }
  model {
    Y ~ binomial(100, pi);
    pi ~ beta(4, 6);
  }
"

# STEP 2: SIMULATE the posterior
art_sim <- stan(model_code = art_model, data = list(Y = 14), 
                chains = 4, iter = 5000*2, seed = 84735)

############################################
#############MODEL DIAGNOSTICS##############
## Parallel trace plots & density plots
mcmc_trace(art_sim, pars = "pi", size = 0.5) + 
  xlab("iteration")
mcmc_dens_overlay(art_sim, pars = "pi")

# Autocorrelation plot
mcmc_acf(art_sim, pars = "pi")


############################################
##################ESTIMATION################
#Get values for credible interval
tidy(art_sim, conf.int = TRUE, conf.level = 0.95)

#Plot credible interval
mcmc_areas(art_sim, pars = "pi", prob = 0.95)

#Create data frame with 20K values in case you 
#want more flexibility
art_chains_df <- as.data.frame(art_sim, pars = "lp__", include = FALSE)
dim(art_chains_df)

#We can do whatever we want with our 20k values now
art_chains_df %>% 
  summarize(post_mean = mean(pi), 
            post_median = median(pi),
            post_mode = sample_mode(pi),
            lower_95 = quantile(pi, 0.025),
            upper_95 = quantile(pi, 0.975))

############################################
############HYPOTHESIS TESTING###############
#Probability of pi < 0.2
art_chains_df %>% 
  mutate(exceeds = pi < 0.20) %>% 
  count(exceeds) %>%
  mutate(percentage = n / sum(n)) 


############################################
##############PREDICTION###################
# Set the seed
set.seed(1)

# Predict a value of Y' for each pi value in the chain
art_chains_df = art_chains_df %>% 
  mutate(y_predict = rbinom(length(pi), size = 20, prob = pi))

head(art_chains_df)

#Show outcome of predictions
ggplot(art_chains_df, aes(x = y_predict)) + 
  stat_count()

#Expected Value
mean(art_chains_df$y_predict)

#Prob predicted value is > 5
art_chains_df %>% 
  mutate(exceeds = y_predict > 5) %>% 
  count(exceeds) %>%
  mutate(percentage = n / sum(n))
