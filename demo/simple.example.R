# Run with Jags
data=list("y"=10)
parameters.to.save="theta"
model.text="model {
y ~ dnorm(theta,1);
theta ~ dnorm(0,.0001)
}"
#Initial values
  init.values<-function(){list("theta"=1)}
  library(R2jags)
jags.fit <- jags(model.file=textConnection(model.text),
                 data=data,
                 inits=init.values,
                 parameters.to.save=parameters.to.save, 
                 n.chains=1,
                 n.iter=10000)$BUGSoutput$sims.list$theta
library(ggplot2);
ggplot(data.frame(jags.fit), aes(x = jags.fit)) + labs(x = expression(theta)) +
  geom_density(alpha = 0.5) +
  theme_bw()
#Run with stan 
##model specification
model.text="data {real y;}
parameters {real theta;}
model {
y ~ normal(theta, 1);
theta ~ normal(0, sqrt(10^4));}"
#Observed dat
observed.data<-list("y" = 10)
#Run stan
library(rstan)
stanmodel <- stan_model(model_code=model.text, model_name = "stanmodel.4.1")
stanfit <- sampling(stanmodel, data = observed.data, chains = 3, cores = 3,
                    iter = 3000, warmup = 1000)
print(stanfit)
results<- extract(stanfit)
  library(ggplot2);
ggplot(data.frame(results$theta), aes(x = results$theta)) + labs(x = expression(theta)) +
geom_density(alpha = 0.5) +
theme_bw()
