library(ggplot2)
library(R2jags)
aspirin <- dataDownloadCarlinLouis::ddCL_aspirin()
aspirin[,'Z'] <- aspirin[,'X'] - aspirin[,'Y'] 
# Question 1
graph1<-ggplot(aspirin,aes(X,Z))+geom_point()+geom_smooth(method='lm',formula=y~x,se=FALSE)
graph1

# Question 2

model.text<-"model{
      for(i in 1:n) {
      Z[i] ~ dnorm(mu[i] , tau)
      mu[i] <- beta0 + beta1*(X[i] - mean(X[]))}
      beta0 ~ dnorm(0.0,1.0E-6) 
      beta1 ~ dnorm(0.0,1.0E-6)
      tau <- 1/(sigma*sigma) # Gelman prior on sigma
      sigma ~ dunif(0.01, 100)
      }"

aspirin_b <- 
  jags(model.file=textConnection(model.text),
       data=list(n=dim(aspirin)[1],
                 X=aspirin[,'X'],
                 Z=aspirin[,'Z']),
       inits=list(list(beta0=0,
                       beta1=1,
                       sigma=1),
                  list(beta0=.5,beta1=.5,
                       sigma=.5)),
       n.chains=2,
       parameters.to.save=c('beta0','beta1','mu'),
       n.burnin = 1000, n.iter=20000,DIC=TRUE)  

  ci<-aspirin_b$BUGSoutput$summary["beta1",
                                   c("2.5%","97.5%")];
  ci

  aspirin_b$BUGSoutput[c("DIC","pD")]
  beta1<-aspirin_b$BUGSoutput$sims.list$beta1

  question2<-0>ci[2]|0<ci[1]
  #Question 3
  graph2<-ggplot(data.frame(x=beta1),aes(x))+geom_density()+geom_vline(xintercept=ci) 
  graph2      

  #Question 4
  
    aspirin[13,] <- c(13,100.0,NA,NA) # New patient
  attach(aspirin)

    model.text2<-"model{
          for(i in 1:n) {
          Z[i] ~ dnorm(mu[i] , tau)
          mu[i] <- beta0 + beta1*(X[i] - mean(X[]))    
          }
          
          beta0 ~ dnorm(0.0,1.0E-6) 
          beta1 ~ dnorm(0.0,1.0E-6)
          
          tau <- 1/(sigma*sigma) # Gelman prior on sigma
          sigma ~ dunif(0.01, 100)
          }"
  aspirin_c <- 
    jags(model.file=textConnection(model.text2),
         data=list(n=dim(aspirin)[1],
                   X=aspirin[,'X'],
                   Z=aspirin[,'Z']),
         inits=list(list(beta0=0,
                         beta1=1,
                         sigma=1,
                         Z=c(rep(NA,12),0)),
                    list(beta0=.5,beta1=.5,
                         sigma=.5,Z=c(rep(NA,12),.5))),
         n.chains=2,
         parameters.to.save=c('Z[13]'),
         n.burnin = 1000, n.iter=20000,DIC=TRUE)  
  
  ci_c<-aspirin_c$BUGSoutput$summary["Z[13]",
                                     c("2.5%","97.5%")];ci_c
  aspirin_c$BUGSoutput[c("DIC","pD")]
  
  
  Z13c<-aspirin_c$BUGSoutput$sims.list[["Z"]]
  est_c<-mean(Z13c);est_c
  pest_c<-mean(Z13c>0);pest_c

  graph3<-ggplot(data.frame(x=Z13c),aes(x))+geom_density()+geom_vline(xintercept=est_c) 
  graph3
  
  #question
  
  
  model.text3<-"model{
          for(i in 1:n) {
          Z[i] ~ dnorm(mu[i] , tau)
          mu[i] <- beta0 + beta1*(X[i] - mean(X[]))    
          + beta2*(X[i] - mean(X[]))*(X[i] - mean(X[]))
          }
          
          beta0 ~ dnorm(0,1e-8) 
          beta1 ~ dnorm(0,1e-8)
          beta2 ~ dnorm(0,1e-8)
          
          tau <- 1/(sigma*sigma) # Gelman prior on sigma
          sigma ~ dunif(0.01, 100)
          }"
  aspirin_e <- 
    jags(model.file=textConnection(model.text3),
         data=list(n=dim(aspirin)[1],
                   X=aspirin[,'X'],
                   Z=aspirin[,'Z']),
         inits=list(
           list(beta0=0,beta1=1,beta2=1,
                sigma=1,Z=c(rep(NA,12),0)),
           list(beta0=.5,beta1=.5,beta2=.5,
                sigma=.5,Z=c(rep(NA,12),.5))),
         n.chains=2,
         parameters.to.save=
           c('beta0','beta1','beta2','mu',
             'Z[13]'),
         n.burnin = 1000, n.iter=20000,DIC=TRUE)  
    ci_e<-aspirin_e$BUGSoutput$summary["Z[13]",
                                       c("2.5%","97.5%")];ci_e
  aspirin_c$BUGSoutput[c("DIC","pD")]
  
  Z13e<-aspirin_e$BUGSoutput$sims.list[["Z"]]
  est_e<-mean(Z13e);est_e
  pest_e<-mean(Z13e>0);pest_e
  aspirin_e$BUGSoutput$mean$beta2
  ci_beta2_e<-
    aspirin_e$BUGSoutput$summary["beta2",
                                 c("2.5%","97.5%")]
  aspirin_e$BUGSoutput[c("DIC","pD")]

  graph4<-ggplot(data.frame(x=Z13e),aes(x))+geom_density()+geom_vline(xintercept=ci_e) 
  graph4
  #question
  xx<-seq(min(X),max(X),length.out=100)
  means<-aspirin_e$BUGSoutput$mean
  
  graph5<-ggplot(data.frame(X,Z=c(Z[-13],est_e),Predicted=rep(c("Observed","Predicted"),c(12,1))),aes(X,Z,color=Predicted))+
    geom_point()+
    stat_function(fun=function(xx){means$beta0+means$beta1*(xx-mean(X))+means$beta2*(xx-mean(X))^2}, geom="line", aes(color="Posterior mean of Z|X")) +
    scale_colour_manual("", values=c("black","red","blue"))
  
  graph5