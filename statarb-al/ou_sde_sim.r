pollard.sde.sim<-function(dt,n=1000,mu=function(x,t){0},sigma=function(x,t){1},x0=1,type="Euler",
                          plot=T,innov=c()){
  dw<-innov
  t<-seq(0,by=dt,length=n)
  x<-1:n
  x[1]<-x0
  h<-10^-6
  if (is.null(innov))
    dw <- sqrt(dt)*rnorm(n)
    ## dw<-rnorm(n,sd=sqrt(t/n))
  if (type == "Euler"){
    for (i in 2:n)
      x[i]<-(x[i-1]+mu(x[i-1],t[i])*dt+sigma(x[i-1],t[i])*dw[i])
  }
  if (type != "Euler"){
    for (i in 2:n){
      x[i]<-(x[i-1]+mu(x[i-1],t[i])*dt+sigma(x[i-1],t[i])*dw[i]
             +0.5*sigma(x[i-1],t[i])
             *(sigma(x[i-1],t[i])-sigma(x[i-1]+h,t[i]))/h*(dw[i]^2-dt))
    }}
  if (plot==T)
    plot(cbind(t,x),type="l")
  return(ts(x)) }


ou.sim.exact<-function(dt,n=1000,ou.params,x0=ou.params["m"]){
  k <- ou.params["k"]; m <- ou.params["m"]; s <- ou.params["sigma"]
  x<-1:n
  x[1]<-x0
  b <- exp(-k*dt)
  sigma <- s*sqrt((1-b^2)/(2*k))
  dw <- rnorm(n)
  for (i in 2:n)
    x[i]<-(m*(1-b) + b*x[i-1] + sigma*dw[i])
  return(ts(x)) }

ou.sim.as.ar <- function(dt,n=1000,ou.params){
  k <- ou.params["k"]; m <- ou.params["m"]; s <- ou.params["sigma"]
  varz <- s^2*(1-b^2)/(2*k)
  b <- exp(-k*dt)
  arima.sim(list(order=c(1,0,0), ar=b), n=n, rand.gen = function(n,...) rnorm(n,mean=a,sd=sqrt(varz))) }

ou.sim.euler<-function(dt,n=1000,ou.params,x0=ou.params["m"]){
  k <- ou.params["k"]; m <- ou.params["m"]; s <- ou.params["sigma"]
  x<-1:n
  x[1]<-x0
  b <- exp(-k*dt)
  sigma <- s*sqrt((1-b^2)/(2*k))
  dw <- rnorm(n)
  for (i in 2:n)
    x[i]<-(x[i-1]+k*(m-x[i-1])*dt+s*sqrt(dt)*dw[i])
  return(ts(x)) }

## convert from X[t]=a+b*X[t-1]+z[t-1] with z~N(0,varz) to OU simulation parameters
ar2ou.params <- function(a,b,varz,dt) c("k"=-log(b)/dt,"m"=a/(1-b),"sigma"=sqrt(varz*2*(-log(b)/dt)/(1-b^2)))
ar.list2ou.params <- function(ar.params,dt){
  a <- unname(ar.params["a"]); b <- unname(ar.params["b"]); varz <- unname(ar.params["varz"])
  c("k"=-log(b)/dt,"m"=a/(1-b),"sigma"=sqrt(varz*2*(-log(b)/dt)/(1-b^2))) }

ou2ar.params <- function(ou.params,dt){
    k <- unname(ou.params["k"]); m <- unname(ou.params["m"]); s <- unname(ou.params["sigma"])
    c("a"=m*(1-b),"b"=exp(-k*dt),"varz"=s^2*(1-b^2)/2/k)  }

## to quantify the rate of mean reversion:
transitions.k.dep <- function(k,varz,m=0.5,n=2520,sd.thr.factors=c(1,1)){
  dt <- 1/252
  b <- exp(-k*dt)
  a <- m*(1-b)
  ou.params.mc <- c("k"=-log(b)/dt,"m"=a/(1-b),"sigma"=sqrt(varz*2*(-log(b)/dt)/(1-b^2)))
  ou <- ou.sim.exact(dt=dt,ou.params.mc,n=n)
  empirical.mean <- mean(ou)
  empirical.var <- var(ou)
  thr.low <- empirical.mean-sd.thr.factors[1]*sqrt(empirical.var)
  thr.hi <- empirical.mean+sd.thr.factors[2]*sqrt(empirical.var)
  num.transitions <- function(path,low,hi){
    tr <- (-1)*as.numeric(path<=low)+as.numeric(path>=hi)
    floor(length(rle(tr[tr!=0])$values)/2)
  }
  num.transitions(ou,thr.low,thr.hi)/(n/252)
}
