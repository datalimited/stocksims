##--------------------------------
## generate AR(1) lognormal errors
## on Bev-Holt SR model

## number of observations
n<-40

## deterministic recruitment part
## ssb
s<-seq(0,30,length=40)
## Bev-Holt pars
a<-3
b<-5
## mean recruitment
r.det<-a*s/(b+s)

## innovation standard deviation
sd.innov<-0.2
## strength of the AR(1) process - see Pyper and Peterman
rho<-0.8

## generate the residuals
e<-arima.sim(model=list(ar=rho),rand.gen = function(n) rnorm(n, sd=sd.innov), n=n)

## generate stochasitic AR(1) recruitment
r.sto<-r.det*exp(e)

plot(s,r.sto, type="o", col="grey", pch=19)
lines(s,r.det)
##--------------------------------
