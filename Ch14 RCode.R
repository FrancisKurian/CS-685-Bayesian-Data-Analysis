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
library(e1071)

data("penguins_bayes")
penguins = penguins_bayes

naive_model_1 <- naiveBayes(species ~ bill_length_mm, data = penguins)
naive_model_2 <- naiveBayes(species ~ bill_length_mm + flipper_length_mm, 
                            data = penguins)

naive_classification_summary(model = naive_model_1, 
                             data = penguins, y = "species")


naive_classification_summary(model = naive_model_2,
                             data = penguins, y = "species")


naive_classification_summary_cv(model = naive_model_2, 
                                data = penguins, y = "species",k=10)
