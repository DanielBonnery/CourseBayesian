data(rats,package="dataMisc")

With Jags
library(R2jags)
T=5; N=30
rats.data  <- list(y=structure(rats$y,.Dim=c(N,T)), 
                   x=unique(rats$x), 
                   T=T, N=N,x.bar=mean(rats$x))

rats.params <- c("tau.c", "sigma", "alpha.0","tau.alpha", "tau.beta", "beta.0", "alpha0")

model.text<-"model {
for (i in 1:N) {
for (j in 1:T) {
mu[i,j] <- alpha[i] + beta[i]*(x[j] - x.bar);
y[i,j]   ~ dnorm(mu[i,j],tau.c)
}
alpha[i] ~ dnorm(alpha.0,tau.alpha);
beta[i]  ~ dnorm(beta.0,tau.beta);
}
alpha.0   ~ dnorm(0,1.0E-4);
beta.0    ~ dnorm(0,1.0E-4);
tau.c     ~ dgamma(1.0E-3,1.0E-3);
tau.alpha ~ dgamma(1.0E-3,1.0E-3);
tau.beta  ~ dgamma(1.0E-3,1.0E-3);
sigma    <- 1.0/sqrt(tau.c);
alpha0   <- alpha.0 - beta.0*x.bar;
}"

rats.inits <- function(){
  list("alpha"=rep(250.0,N), 
       "beta"=rep(6.0,N), 
       "alpha.0"=0, "beta.0"=2, "tau.c"=1, "tau.alpha"=1, "tau.beta"=1)
}
jags.fit <- jags(data=rats.data,
                 inits=rats.inits,
                 parameters.to.save=rats.params, 
                 n.chains=3, n.iter=10000, n.burnin=1000, model.file=textConnection(model.text))
  print(jags.fit)
  class(jags.fit);typeof(jags.fit)

  
  names(jags.fit$BUGSoutput)
  dim(jags.fit$BUGSoutput$sims.array)
dimnames(jags.fit$BUGSoutput$sims.array)
mean(jags.fit$BUGSoutput$sims.array[,,"beta.0"])
mean(jags.fit$BUGSoutput$sims.list$beta.0)
jags.fit$BUGSoutput$mean$beta.0
  plot(jags.fit)
  posterior.mean.beta.0<-mean(jags.fit$BUGSoutput$sims.list$beta.0)
ggplot(data.frame(x=jags.fit$BUGSoutput$sims.list$beta.0), aes(x = x)) + labs(x = expression("beta_0")) +
  geom_density(alpha = 0.5) +theme_bw()+geom_vline(xintercept=posterior.mean.beta.0)


## With stan
  library(rstan)  

stan.model<-"
data {
int<lower=0> N;
int<lower=0> T;
real x[T];
real y[N,T];
real xbar;
}
parameters {
real alpha[N];
real beta[N];

real mu_alpha;
real mu_beta;          // beta.c in original bugs model

real<lower=0> sigmasq_y;
real<lower=0> sigmasq_alpha;
real<lower=0> sigmasq_beta;
}
transformed parameters {
real<lower=0> sigma_y;       // sigma in original bugs model
real<lower=0> sigma_alpha;
real<lower=0> sigma_beta;

sigma_y <- sqrt(sigmasq_y);
sigma_alpha <- sqrt(sigmasq_alpha);
sigma_beta <- sqrt(sigmasq_beta);
}
model {
mu_alpha ~ normal(0, 100);
mu_beta ~ normal(0, 100);
sigmasq_y ~ inv_gamma(0.001, 0.001);
sigmasq_alpha ~ inv_gamma(0.001, 0.001);
sigmasq_beta ~ inv_gamma(0.001, 0.001);
alpha ~ normal(mu_alpha, sigma_alpha); // vectorized
beta ~ normal(mu_beta, sigma_beta);  // vectorized
for (n in 1:N)
for (t in 1:T) 
y[n,t] ~ normal(alpha[n] + beta[n] * (x[t] - xbar), sigma_y);

}
generated quantities {
real alpha0;
alpha0 <- mu_alpha - xbar * mu_beta;
}"
rats.data <- list(N = N, T = T, 
                 y=structure(rats$y,.Dim=c(N,T)), 
                   x=unique(rats$x),xbar=mean(rats$x))
stan.fit <- stan(model.text=stan.model, data = rats.data, verbose = FALSE,
                 warmup=1000,iter = 11000, n_chains = 3)
system.time(stan.fit2 <- stan(fit=stan.fit, data = rats.data, verbose = FALSE,
                  warmup=1000,iter = 11000, n_chains = 3))
print(stan.fit2)
plot(stan.fit2)

