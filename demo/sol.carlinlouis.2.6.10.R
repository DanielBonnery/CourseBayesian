data(dugong,package="dataMisc")
library(ggplot2)
#1. Print
graph1<-ggplot(data=dugong,aes(x=log(x),y=Y))+geom_point()
graph1
#2. Run linear model
model1 <- lm(dugong$Y~log(dugong$x))
model1.summary<-summary(model1)
model1.summary
signif(model1$coefficients,3); signif(summary(model1)$sigma^2,3)
#3. Confidence interval
conf.int<-confint(model1)["log(dugong$x)",]
conf.int
#4.Bayesian computing
#4.0 Part common to stan and jags
observed.data<-c(list(n=dim(dugong)[1]),dugong)
#4.1.Bayesian computations with jags
library(R2jags)
jags.model.text<-"model{
for(i in 1:n) {
logage[i] <-log(x[i])
Y[i] ~ dnorm(mu[i],tau)
mu[i] <-beta0+beta1*logage[i]
}
beta0 ~ dnorm(0,1e-8)
beta1 ~ dnorm(0,1e-8)

tau ~ dgamma(0.1,0.1)
sigma<-1/sqrt(tau)
}"

jags.dugong <- 
  jags(model.file=textConnection(jags.model.text),
       data=observed.data,
       inits=list(list(beta0=0,
                       beta1=1,
                       tau=1),
                  list(beta0=.5,beta1=.5,
                       tau=.5)),
       n.chains=2,
       parameters.to.save=c('beta0','beta1','sigma'),
       n.burnin = 1000, n.iter=20000,DIC=TRUE)  

jags.dugong$BUGSoutput$summary["beta1",
                               c("2.5%","97.5%")]
jags.dugong$BUGSoutput[c("DIC","pD")]
beta1.1<-jags.dugong$BUGSoutput$sims.list$beta1

graph2<-ggplot(data.frame(beta1.1=beta1.1),aes(x=beta1.1))+geom_density()
graph2
#4. Bayesian computations with stan
#This code is adapted from a solution by Soumojit Das
  
#We will first use classical approach and then move on to using Bayesian methods.
  
stan.model.text<-"data{
    int<lower = 0> n;
    real x[n];
    real Y[n];
  }
  
  transformed data{
    real logage[n]; 
    logage = log(x);
  }
  
  parameters{
    real beta0;
    real beta1;
    real<lower = 0> sigma;
  }
  
  model{
    real mu[n];
    for (i in 1:n) {
      mu[i] = beta0 + beta1*(logage[i] - mean(logage));
    }
    Y ~ normal(mu, sigma);
    beta0 ~ uniform(-100, 100);
    beta1 ~ uniform(-100, 100);
    sigma ~ uniform(0.01, 100);
  }
  
  // This following block is required for loo to work
  
  generated quantities {
    vector[n] log_lik;
    real mu[n];
    for (i in 1:n) {
      mu[i] = beta0 + beta1*(logage[i] - mean(logage));
    }
    for (i in 1:n) {
      log_lik[i] = normal_lpdf(Y[i] | mu[i], sigma);
    }
  }"

#  We use R to draw samples and inference for the given problem
    
  library(rstan)
  stanfit = sampling(
    stan_model(model_name="stan_model",model_code=stan.model.text), 
    data =   observed.data, 
    chains = 3, 
    cores =3,
    iter = 3000, warmup = 1000)
  
  ex = extract(stanfit)
  graph3<-ggplot(data.frame(ex$beta1), aes(x = ex$beta1)) + labs(x = expression(beta)) +
    geom_density(fill = "blue", alpha = 0.5) + 
    theme_bw()
  graph3
  graph4<-ggplot(data.frame(sqrt(ex$sigma)), aes(x = sqrt(ex$sigma))) + labs(x = expression(sigma)) +
    geom_density(fill = "darkolivegreen", color="brown", alpha = 0.44) + 
    theme_bw()
  graph4
  
  summary(stanfit)$summary
  
  
  ## Leave-one-out Cross-validation using "loo" package
  
  library("loo")
  
  # Extract pointwise log-likelihood and compute LOO
  log_lik = extract_log_lik(stanfit, merge_chains = FALSE)
  
  # as of loo v2.0.0 we can optionally provide relative effective sample sizes
  # when calling loo, which allows for better estimates of the PSIS effective
  # sample sizes and Monte Carlo error
  r_eff = relative_eff(exp(log_lik)) 
  
  loo.res = loo(log_lik, r_eff = r_eff, cores = 3)
  print(loo.res)

  