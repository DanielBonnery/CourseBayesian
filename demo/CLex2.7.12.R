#Code adapted from Carlin Louis Solution manual

require(MASS)
require(ggplo2)
require(reshape2)
model.text<-"
model {
for (i in 1:389) {
Y ~ dnorm(mu[i], tau)
mu[i] <- beta[1] + beta[2]*X[i,1] +
beta[3]*X[i,2] + beta[4]*X[i,3]
}
for (i in 1:4) {
beta[i] ~ dflat()
}
tau ~ dgamma(3,b)
b <- 1/(1/(2*(1/(0.73*0.73))))
sigma <- 1/sqrt(tau)
}"

## Inits (2 choices):
list(beta = c(0,0,0,0), tau = 1.0)
list(beta = c(0.5,0.5,0.5,0.5), tau = 4.0)
list(beta = c(0.5,0.5,0.5,0.5), tau = 4.0)

land.data <- dataDownloadCarlinLouis::ddCL_land()
lm1<-lm(Y~X1+X2+X3,data=land.data)
slm1<-summary(lm1)
s.sq<-slm1$sigma^2
dfs<-slm1$df
X<-model.matrix(lm1)
tXX.inv<-solve(t(X)%*%X)
beta.hat<-lm1$coefficients
niter=5000
Y<-land.data$Y
land.samples <- plyr::raply(.n = niter,(function(){
  rsigma.sq <- 1/rgamma(1, dfs[2]/2, dfs[2]*s.sq/2)
  rsigma.sq.of.beta <- (tXX.inv) * rsigma.sq
  rbeta <- MASS::mvrnorm(1, beta.hat, rsigma.sq.of.beta)
  c(rbeta,sqrt(rsigma.sq))}),.progress="text")
colnames(land.samples)<-c(paste0("beta",1:4),"sigma")

# Function to compile summary statistics
sumstats <- function(vector){
  stats <- cbind(mean=mean(vector),
                 sd=sd(vector),
                 t(quantile(vector,c(.025,.5,.975))))
  names(stats) <- c('mean','sd','2.5%','50%','97.5%')
  stats
}
Summary.land.samples<-apply(land.samples,2,sumstats)

Trep<- plyr::rdply(.n=niter,(function(){
                     rsigma.sq <- 1/rgamma(1, dfs[2]/2, dfs[2]*s.sq/2)
                     rsigma.sq.of.beta <- (tXX.inv) * rsigma.sq
                     rbeta <- mvrnorm(1, beta.hat, rsigma.sq.of.beta)
                     y.rep <- mvrnorm(1, X%*%rbeta, rsigma.sq*diag(nrow(land.data)))
                     T.rep <- t(y.rep - X%*%rbeta) %*%(y.rep - X%*%rbeta)
                     T.orig <- t(Y - X%*%rbeta)%*%(Y - X%*%rbeta)
                     c(Trep=T.rep,Torig=T.orig,diff=T.rep-T.orig)
})(),.progress="text")

p.val <- mean(Trep[,3]>0)

graph<-ggplot(data=Trep,aes(x=Torig,y=Trep))+geom_point()+geom_rug()+geom_abline(slope=1,intercept = 0,color="red")

