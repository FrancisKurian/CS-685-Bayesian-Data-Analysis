library(bayesrules)

#Playing with Gamma Distributions
plot_gamma(1,1, mean=T) + xlim(c(0,50))
plot_gamma(2,1, mean=T) + xlim(c(0,50))
plot_gamma(5,1, mean=T) + xlim(c(0,50))
plot_gamma(10,1, mean=T)+ xlim(c(0,50))
plot_gamma(30,1, mean=T)+ xlim(c(0,50))


plot_gamma(5,1, mean=T) + xlim(c(0,15))
plot_gamma(5,2, mean=T) + xlim(c(0,15))
plot_gamma(5,3, mean=T) + xlim(c(0,15))

plot_gamma(5,1, mean=T) + xlim(c(0,15))
plot_gamma(10,2, mean=T) + xlim(c(0,15))
plot_gamma(15,3, mean=T) + xlim(c(0,15))
plot_gamma(25,5, mean=T) + xlim(c(0,15))

#Bayes Rules Function for Gamma-Poisson Conjugate
summarize_gamma_poisson(10,2,sum_y = 11, n=4)
plot_gamma_poisson(10,2,sum_y = 11, n=4)

#Bayes Rules Function for Normal-Normal Conjugate
summarize_normal_normal(6.5,0.4,sigma = 0.5, y_bar = 5.735, n=25)
plot_normal_normal(6.5,0.4,sigma = 0.5, y_bar = 5.735, n=25)

