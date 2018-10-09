library(R2jags)
#question 1
fit<-jags(
  data=list(x=Orange$age[Orange$Tree==1],
            y=Orange$circumference[Orange$Tree==1],
            N=7),
  inits=list(list("beta"=0,
                  "tau"=1)),
  n.chains=1,
  parameters.to.save=c("sigma", "beta"),
  n.iter=10000,
  n.burnin=1000,
  model.file=textConnection(
    "model {
    for (i in 1:N) {
    mu[i] <- beta*x[i];
    y[i]   ~ dnorm(mu[i],tau)
    }
    beta    ~ dnorm(0,1.0E-4);
    tau     ~ dgamma(1.0E-4,1.0E-4);
    sigma <- 1/tau}"))

  #question 2
  fit2<-jags(
    data=list(x=c(Orange$age[Orange$Tree==1],1700),
              y=c(Orange$circumference[Orange$Tree==1],NA),
              N=8),
    inits=list(list(beta=0,
                    tau=1,
                    y=c(rep(NA,7),0))),
    n.chains=1,
    parameters.to.save=c("sigma", "beta","y[8]"),
    n.iter=10000,
    n.burnin=1000,
    model.file=textConnection(
      "model {
     for (i in 1:N) {
     mu[i] <- beta*x[i];
     y[i]   ~ dnorm(mu[i],tau)
     }
     beta    ~ dnorm(0,1.0E-4);
     tau     ~ dgamma(1.0E-4,1.0E-4);
     sigma <- 1/tau}"))

sol3<-sum(fit2$BUGSoutput$sims.list$y<Orange$circumference[7])/length(fit2$BUGSoutput$sims.list$y)

  