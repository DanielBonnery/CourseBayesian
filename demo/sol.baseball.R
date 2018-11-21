require("R2jags")
library(ggplot2)
data(baseball,package="dataBaseball")
tmpfile=tempfile()
modelfile<-function(i){
  write(file=tmpfile,
        paste0("model{
               for (i in 1:N){
               nb[i] ~ dbin(p[i],n)
               p[i] <- ilogit(alpha+",
               paste(c("x[i]*beta","z[i]*gamma")[i],collapse="+"),")}
               alpha ~ dnorm(0,1e-8)
               ",
               paste(c("beta ~ dnorm(0,1e-8)","gamma ~ dnorm(0,1e-8)")[i],collapse="\n"),
               "}"))
  return(tmpfile)}
n<-45;N=18
models<-list(1,2,c(1,2))
jags.fit<-lapply(models,function(i){
  jags(
    model.file=modelfile(i),
    data=c(list(N=N,n=n,nb=round(n*baseball$hat.P)),
           list(x=baseball$x1,z=baseball$x2)[i]),
    inits=list(c(list(alpha=-.4),
                 list(beta=0,gamma=0)[i])),
    n.chains=1,
    parameters.to.save=c("p","alpha",c("beta","gamma")[i]),
    n.burnin = 10000, n.iter=30000,DIC=TRUE)
})
names(jags.fit)<-paste0("model ",1:3)
DIC<-sapply(jags.fit,function(l){l$BUGSoutput$DIC})
i<-(1:3)[DIC==min(DIC)]
s<-signif(jags.fit[[i]]$BUGSoutput$summary[c("beta","gamma")[models[[i]]],c("mean","sd","2.5%","97.5%")],4)
p.b1<-signif(jags.fit[[i]]$BUGSoutput$summary[paste0("p[",1:N,"]"),c("mean")],4)
p.b1
modelfile2<-function(i){
  write(file=tmpfile,
        paste0("model{
               for (i in 1:N){
               nb[i] ~ dbin(p[i],n)
               p[i] <- ilogit(alpha+",
               paste(c("x[i]*beta","z[i]*gamma")[i],collapse="+"),"+eps[i])
               eps[i] ~ dnorm(0,tau)}
               alpha ~ dnorm(0,1e-8)
               tau ~ dgamma(1e-3,1e-3)
               sigma2<- 1/tau
               ",
               paste(c("beta ~ dnorm(0,1e-8)","gamma ~ dnorm(0,1e-8)")[i],collapse="\n"),
               "}"))
  return(tmpfile)}
n<-45;N=18
models<-list(1,2,c(1,2))
jags.fit2<-lapply(models,function(i){
  jags(
    model.file=modelfile2(i),
    data=c(list(N=N,n=n,nb=round(n*baseball$hat.P)),
           list(x=baseball$x1,z=baseball$x2)[i]),
    inits=list(c(list(alpha=-.4,tau=1),
                 list(beta=0,gamma=0)[i])),
    n.chains=1,
    parameters.to.save=c("p","sigma2","alpha",c("beta","gamma")[i]),
    n.burnin = 10000, n.iter=30000,DIC=TRUE)
})
names(jags.fit2)<-paste0("model ",1:3)
DIC2<-sapply(jags.fit2,function(l){l$BUGSoutput$DIC})
i<-(1:3)[DIC2==min(DIC2)]
samplejags<-jags.fit2[[i]]$BUGSoutput$sims.list$sigma2
graph1<-ggplot(data=data.frame(x=samplejags[,1]),aes(x=x))+geom_histogram()+geom_density()+xlab(expression(sigma^2))

p.b2<-signif(jags.fit2[[i]]$BUGSoutput$summary[paste0("p[",1:N,"]"),c("mean")],4)

L=list(baseball$P,p.b1,p.b2,baseball$hat.P)
plot(c(1,N),range(unlist(L)),type="n")
lapply(1:4,function(l){points(1:N,L[[l]],col=l,pch=20)})
ex4solgraph2.texte<-"par(oma=c(2,0,0,0),mar=c(0,3,0,0),mgp=c(4,1,0))
                      
                      layout(matrix(1:2, 2, 1, byrow = TRUE),  heights=c(.1,.9))
                      plot.new()
                      legend(0,1,'$p_i$',col=1,bty='n',pch=20); 
                      legend(0.2,1,'$\\\\tilde{p}_i$',bty='n',col=2,pch=20)
                      legend(0.4,1,'$\\\\breve{p}_i$', bty='n',col=3,pch=20)
                      legend(0.6,1,'$\\\\hat{p}_i$', bty='n',col=4,pch=20)       
                      plot(c(1,N),range(unlist(L)),type='n',ylab='',xlab='$i$');lapply(1:4,function(l){points(1:N,L[[l]],col=l,pch=20)})
                      "
                      
MSEe<-sapply(L[2:4],function(l){signif(mean((l-L[[1]])^2),3)})
MSEe
samplejags<-jags.fit2[[i]]$BUGSoutput$sims.list$sigma2
require(lattice)
histogram( ~ samplejags ,
           xlab = "sigma2", 
           type = "density",nint=40,
           panel = function(x, ...) {
             panel.histogram(x, ...,col="gray")
             panel.densityplot(x,plot.points=FALSE)})

