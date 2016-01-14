setwd("C:/Users/gisadmin/Dropbox/学会発表/2016_生態学会/bayes_data")
source("R2WBwrapper.R")
load("C:/Users/gisadmin/Dropbox/学会発表/2016_生態学会/bayes_data/d5.Rdata")

setwd("F:/test/testmodel")

########################################model1
m1<-"model{
  for(i in 1:N_site){
    y[i]~dbern(psi)
  }
  psi~dunif(0,1)
}"
write(m1, file="m1.txt")
clear.data.param() # for initialization

set.data("N_site", N_site)
set.data("y", as.integer(data5$sp1>0))
set.param("psi", 0.5)
post.bugs_m1 <- call.bugs( 
  model.file = "m1.txt",
  n.iter = 2000, n.burnin = 1000, n.thin = 1,
  debug=T)

#######################################model2
m2<-"model{
  for(i in 1:N_site){
    y[i]~dbin(mu[i],K[i])
    mu[i]<-p*z[i]
    z[i]~dbern(psi)
  }
p~dunif(0,1)
psi~dunif(0,1)
}"
write(m2, file="m2.txt")

clear.data.param() # for initialization
set.data("N_site", N_site)
set.data("y", as.vector(data5$sp1))
set.data("K", as.vector(data5$trial.ref))
set.param("psi", 0.5)
set.param("p", 0.5)
set.param("z", 0)
post.bugs_m2 <- call.bugs( 
  model.file = "m2.txt",
  n.iter = 5000, n.burnin = 2000, n.thin = 1,
  debug=T)


#######################################model3
m3<-"model{
  for(i in 1:N_site){
    y[i]~dbin(mu[i],K[i])
    mu[i]<-p*z[i]
    z[i]~dbern(psi[i])
    logit(psi[i])<-b0+b1*v1[i]
  }
  p~dunif(0,1)
  b0~dnorm(0,0.1)I(-5, 5)
  b1~dnorm(0,0.1)I(-5, 5)
}"
write(m3, file="m3.txt")

clear.data.param() # for initialization
set.data("N_site", N_site)
set.data("y", as.vector(data5$sp1))
set.data("K", as.vector(data5$trial.ref))
set.data("v1", as.vector(data5$maxtemp_annual))
set.param("p", 0.5)
set.param("b0", 0)
set.param("b1", 0)
#set.param("psi", 0.5)
#set.param("z", 1)
post.bugs_m3 <- call.bugs( 
  model.file = "m3.txt",
  n.iter = 5000, n.burnin = 2000, n.thin = 1,
  debug=T)

#######################################model4
m4<-"model{
  for(i in 1:N_site){
    y[i]~dbin(mu[i],K[i])
    mu[i]<-p[i]*z[i]
    z[i]~dbern(psi[i])
    logit(psi[i])<-b0+b1*v1[i]
    p[i]~dunif(0,1)
  }
  b0~dunif(0,1)
  b1~dunif(0,1)
}"
write(m4, file="m4.txt")

clear.data.param() # for initialization
set.data("N_site", N_site)
set.data("y", as.vector(data5$sp1))
set.data("K", as.vector(data5$trial.ref))
set.data("v1", as.vector(data5$maxtemp_annual))
set.param("p",rep(0.25,N_site))
set.param("b0",0.25)
set.param("b1",0.25)
#set.param("psi",rep(0.2,N_site))
#set.param("z",rep(1,N_site))
post.bugs_m4 <- call.bugs( 
  model.file = "m4.txt",
  n.iter = 5000, n.burnin = 2000, n.thin = 1,
  debug=T)

#######################################model5
#ramdam

m5<-"model{
    for(i in 1:N_site){
    y[i]~dbin(mu[i],K[i])
    mu[i]<-p[i]*z[i]
    z[i]~dbern(psi[i])
    logit(psi[i])<-b0+b1*v1[i]
    p[i]~dunif(0,1)
  }
  b0~dunif(0,1)
  b1~dunif(0,1)
}"
write(m5, file="m5.txt")

clear.data.param() # for initialization
set.data("N_site", N_site)
set.data("y", as.vector(data5$sp1))
set.data("K", as.vector(data5$trial.ref))
set.data("v1", as.vector(data5$maxtemp_annual))
set.param("p",rep(0.25,N_site))
set.param("b0",0.25)
set.param("b1",0.25)
#set.param("psi",rep(0.2,N_site))
#set.param("z",rep(1,N_site))
post.bugs_m5 <- call.bugs( 
  model.file = "m5.txt",
  n.iter = 5000, n.burnin = 2000, n.thin = 1,
  debug=T)




