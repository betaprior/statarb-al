## simulate a large artificial price series "inspired" by JPM/XLF pair trading
setwd("/home/leo/projects/finance/research/statarb-al/")
require("fBasics")
source(search.path("my_density_plot.r")) ## my.densityPlot from fBasics with x/y limits
source(search.path("zoom.r"))
require("timeSeries")
require("fGarch")
require("sde")
source("ou_sde_sim.r")  ## for SDE simulation
source("f_trading_sim.r") ## for the trading simulation

load("xlf.prices.RObj")
xlf.pr <- rev(as.numeric(xlf.prices$XLF))[1:2000]
xlf.lret <- diff(log(xlf.pr))
xlf.lret <- removeNA(xlf.lret)

## diagnostics: the fat tails are obvious
# qqnorm(xlf.lret)
# qqline(xlf.lret)
# Box.test(xlf.lret,lag=10,type='Ljung')

## 	Box-Ljung test

## data:  xlf.lret 
## X-squared = 17.9807, df = 10, p-value = 0.05529

xlf.arima.fit <- arima(xlf.lret,order=c(2,0,0))
Box.test(xlf.arima.fit$residuals,10,type='Ljung')
## AR(2) B-L: X-squared = 14.8332, df = 10, p-value = 0.1383

## {{{ garch fit functions:

fGarch.obj.to.model <- function(fgarch.obj){
  params <- fgarch.obj@fit$params$params
  ar.idxs <- grep("^ar",names(params))
  ma.idxs <- grep("^ma",names(params))
  alpha.idxs <- grep("^alpha",names(params))
  beta.idxs <- grep("^beta",names(params))
  model <- list()
  if(!zero.length(ar.idxs)) model$ar <- params[ar.idxs]
  if(!zero.length(ma.idxs)) model$ma <- params[ma.idxs]
  model$omega <- params["omega"]
  model$alpha <- params[alpha.idxs]
  model$beta <- params[beta.idxs]
  model$mu <- params["mu"]
  if(!is.na(params["skew"])) model$skew=params["skew"]
  if(!is.na(params["shape"])) model$shape=params["shape"]
  return(model)
}

build.sim.spec <- function(fgarch.fit){
  garchSpec(model=fGarch.obj.to.model(fgarch.fit)
            , cond.dist=fgarch.fit@fit$params$cond.dist
            , presample=cbind(residuals(fgarch.fit,standardize=T),fgarch.fit@h.t,fgarch.fit@data)) }
  
plot.sim.results <- function(ser.list,xlim.d=c(-0.2,0.2),ylim.d=NULL,xlim.q=NULL,ylim.q=c(-0.3,0.3)){
  stopifnot(class(ser.list)=="list")
  par.save()
  par(mfcol=c(length(ser.list),2))
  lapply(ser.list,function(x)  my.densityPlot(x,xlim=xlim.d,ylim=ylim.d))
  lapply(ser.list,function(x){ qqnorm(x,xlim=xlim.q,ylim=ylim.q); qqline(x) })
  par.restore()
}

## }}}

## fit an AR(1)/GARCH(1,1) model to this series 
cond.dist <- "std"
gf1=garchFit(formula=~arma(0,0)+garch(1,1),xlf.lret,trace=F)
gf1.t=garchFit(formula=~arma(0,0)+garch(1,1),xlf.lret,trace=F,cond.dist=cond.dist)
gf2=garchFit(formula=~arma(2,0)+garch(1,1),xlf.lret,trace=F)
gf2.t=garchFit(formula=~arma(2,0)+garch(1,1),xlf.lret,trace=F,cond.dist=cond.dist)
## fitted parameters:
## > gf1.t@fit$params$params
##           mu        omega       alpha1       gamma1        beta1        delta 
## 3.952131e-04 1.002515e-06 8.046919e-02 1.000000e-01 9.175640e-01 2.000000e+00 
##         skew        shape 
## 1.000000e+00 7.289308e+00 


## testing this with garchOxFit:
#gf1.ox <-garchOxFit(formula.mean=~arma(0,0),formula.var=~garch(1,1),series=xlf.lret)
#gf1.ox.t <-garchOxFit(formula.mean=~arma(0,0),formula.var=~garch(1,1),series=xlf.lret,cond.dist="t")
## for the t-fit:
##  Maximum Likelihood Estimation (Std.Errors based on Second derivatives)
##                   Coefficient  Std.Error  t-value  t-prob
## Cst(M)               0.000395 0.00020328    1.944  0.0520
## Cst(V)               0.010035  0.0043347    2.315  0.0207
## ARCH(Alpha1)         0.080417   0.012998    6.187  0.0000
## GARCH(Beta1)         0.917573   0.012489    73.47  0.0000
## Student(DF)          7.293178     1.1525    6.328  0.0000

## No. Observations :      1959  No. Parameters  :         5
## Mean (Y)         :   0.00015  Variance (Y)    :   0.00019
## Skewness (Y)     :   0.12138  Kurtosis (Y)    :   6.54805
## Log Likelihood   :  6003.978  Alpha[1]+Beta[1]:   0.99799

## Warning : To avoid numerical problems, the estimated parameter
## Cst(V), and its std.Error have been multiplied by 10^4.


## simulate the series based on fits
N <- length(xlf.lret)
gf0.sim <- arima.sim(list(order=c(2,0,0)
                , ar=xlf.arima.fit$coef[c("ar1","ar2")])
                , n=N,sd=sqrt(xlf.arima.fit$sigma2))
gf0.t.sim <- arima.sim(list(order=c(2,0,0)
                , ar=xlf.arima.fit$coef[c("ar1","ar2")])
                , n=N, rand.gen = function(n,...) rnorm(n,mean=a,sd=sqrt(xlf.arima.fit$sigma2)))
gf1.sim <- garchSim(spec=build.sim.spec(gf1),n=N)
gf1.t.sim <- garchSim(spec=build.sim.spec(gf1.t),n=N)
gf2.sim <- garchSim(spec=build.sim.spec(gf2),n=N)
gf2.t.sim <- garchSim(spec=build.sim.spec(gf2.t),n=N)

## plot.sim.results(list(as.timeSeries(xlf.lret)
##                       , as.timeSeries(gf0.sim)
##                       , as.timeSeries(gf0.t.sim)))
## x11()
plot.sim.results(list(as.timeSeries(xlf.lret)
                      , as.timeSeries(gf1.sim)
                      , as.timeSeries(gf1.t.sim)
                      , as.timeSeries(gf2.sim)
                      , as.timeSeries(gf2.t.sim)))
## typical output saved in XLF series fits.png

sim.moments <- function(model,nsim=50,N=1000,t=FALSE){
  sim.function.arima <- function(...){
    as.matrix(arima.sim(list(order=c(2,0,0)
                             , ar=model$coef[c("ar1","ar2")]), n=N,...)) }
  if(class(model)=="Arima"){
    if(t)
      sim.function <- function() sim.function.arima(sd=sqrt(model$sigma2))
    else
      sim.function <- function() sim.function.arima(rand.gen = function(n,...) rnorm(n,mean=a,sd=sqrt(model$sigma2)))
  } else {
    sim.function <- function() garchSim(spec=build.sim.spec(model),n=N)
  }
  sim.moments.mtx <- replicate(nsim,apply(sim.function(),2,function(x){ c("mean"=mean(x),"var"=var(x),"kurtosis"=kurtosis(x)) }))
  rbind(  apply(sim.moments.mtx,1,mean)
        , apply(sim.moments.mtx,1,var)) }

apply(xlf.lret,2,function(x){ c("mean"=mean(x),"var"=var(x),"kurtosis"=kurtosis(x)) })
## mean     0.0001538354
## var      0.0001906673
## kurtosis 3.5413673573
sim.moments(xlf.arima.fit,N=length(xlf.lret),t=F)
sim.moments(xlf.arima.fit,N=length(xlf.lret),t=T)
## mean, var kurtosis: average of 50 simulations / variance of 50 simulations:
##              [,1]         [,2]        [,3]
## [1,] 4.752595e-02 1.905359e-04 -0.02188857
## [2,] 1.397460e-07 4.531894e-11  0.01263717
## >              [,1]         [,2]       [,3]
## [1,] 2.963036e-05 1.906039e-04 0.01948538
## [2,] 9.722425e-08 3.528374e-11 0.01583488
sim.moments(gf1,N=length(xlf.lret))
sim.moments(gf1.t,N=length(xlf.lret))
sim.moments(gf2,N=length(xlf.lret))
sim.moments(gf2.t,N=length(xlf.lret))
##              [,1]         [,2]     [,3]
## [1,] 3.575709e-04 2.899914e-04 2.643517
## [2,] 1.953980e-07 3.498118e-08 6.208736
## > 
##              [,1]         [,2]      [,3]
## [1,] 4.161631e-04 2.830024e-04  7.119734
## [2,] 1.739914e-07 4.047955e-08 25.213541
## >              [,1]         [,2]     [,3]
## [1,] 3.193022e-04 2.526313e-04 2.663691
## [2,] 1.115082e-07 2.056872e-08 7.074312
## >              [,1]         [,2]     [,3]
## [1,] 4.732606e-04 3.559424e-04  7.73392
## [2,] 1.264819e-07 1.419837e-07 29.93503

## Conclusions of these simulations:
## while the BL test suggests that there is serial correlation
## in the series, parsimonious ARMA models don't seem to
## capture that correlation (can't get large BL p-value for the residuals)
## Results of fitting ARMA(0,0)+GARCH(1,1) are mostly the same as fitting
## ARMA(2,0)+GARCH(1,1)
## both models result in fat tails (excess kurtosis about 2.6), underestimating their heaviness
## (excess kurtosis of original series ~ 3.5)
## Models seem to have a huge variance in excess kurtosis generated by the series
## GARCH models drawing innovations from t-distribution display very large
## excess kurtosis (~7.7)
## The decision on whether to use normal or t deviates should perhaps be made based on minimizing AIC:
## gf1@fit$ics
##       AIC       BIC       SIC      HQIC 
## -6.091167 -6.079773 -6.091175 -6.086979 
## > gf1.t@fit$ics
##       AIC       BIC       SIC      HQIC 
## -6.124537 -6.110295 -6.124550 -6.119302 
## Is this enough to go with the weird large kurtosis?  It's unclear tome.

Box.test(gf1@residuals,10,type='Ljung')
Box.test(gf1.t@residuals,10,type='Ljung')
Box.test(gf2@residuals,10,type='Ljung')
Box.test(gf2.t@residuals,10,type='Ljung')
## data:  gf1@residuals 
## X-squared = 17.9807, df = 10, p-value = 0.05529
## data:  gf1.t@residuals 
## X-squared = 17.9807, df = 10, p-value = 0.05529
## data:  gf2@residuals 
## X-squared = 16.4848, df = 10, p-value = 0.08657
## data:  gf2.t@residuals 
## X-squared = 19.0844, df = 10, p-value = 0.0392
## looks like there's residual correlation.

#plot.sim(garchSpec(garchFit(formula=~arma(1,1)+garch(1,1),xlf.lret,trace=F)))

## fitting returns series opens up a whole another can of worms, so right now just
## go with the ARMA(1,1) / GARCH(1,1) model (gf3)

N <- 1000
etf.sim <- garchSim(spec=build.sim.spec(gf1),n=N)  ##using straight up GARCH(1,1) w/ normal innov.

dt <- 1/252
const.beta <- 1.25
const.a <- 0.0009
const.b <- 0.82
const.varz <- 8e-5
const.k <- -log(const.b)/dt
const.m <- const.a/(1-const.b)
const.seq <- sqrt(const.varz/(1-const.b^2))
## AR fitting/simulation functions:
fit.ar1.series <- function(x) ar(x, aic=F, order.max=1, method="yw")
ar.params.from.fit <- function(x){ ## x is ar fit
  stopifnot(class(x)=="ar" || class(x)=="Arima")
  if(class(x)=="ar"){
    return(lapply(list(m=x$x.mean
                       , a=x$x.mean*(1-x$ar)
                       , b=x$ar
                       , varz=x$var.pred),unname))
  }else{
    return(lapply(list(m=x$coef["intercept"]
                       , a=x$coef["intercept"]*(1-x$coef["ar1"])
                       , b=x$coef["ar1"]
                       , varz=x$sigma2),unname))
  } }
sim.ar1.series <- function(ar,mean,innov.var,n=1000){ ## mean here is series mean=a/(1-b)
  arima.sim(list(order=c(1,0,0), ar=ar), n=n, rand.gen = function(n,...) rnorm(n,mean=mean*(1-ar),sd=sqrt(innov.var)))
}
## this assumes ret[i+1]=(S[i+1]-S[i])/S[i]
ret.to.prices <- function(ret,p0){
  x <- rep(0,length(ret))
  x[1] <- p0
  for(i in seq(along=ret)[c(-1)])
    x[i] <- x[i-1]*(ret[i]+1)
  x
}
## can simulate this either using the OU process or AR1 series simulation
arima(sim.ar1.series(const.b,const.a/(1-const.b),const.varz),order=c(1,0,0))
arima(ou.sim.exact(dt=dt,ar2ou.params(const.a,const.b,const.varz,dt),n=N),order=c(1,0,0))

stk.ret.beta <- etf.sim*const.beta
stk.ret.ar1 <- sim.ar1.series(const.b,const.a/(1-const.b),const.varz,length(etf.sim))
stk.ret.ar1.diff <- c(stk.ret.ar1[1],diff(stk.ret.ar1))
stk.ret.tot <- stk.ret.beta+stk.ret.ar1.diff
stk.s.score <- (stk.ret.ar1-const.m)/const.seq
cat("exp. no transitions: ",paste(mean.var(replicate(10,transitions.k.dep(const.k,const.varz,const.m)))*length(stk.s.score)/252,collapse="-var-"),"\n")
cat("actual no. transitions: ",num.transitions(stk.s.score,-1,1),"\n")
names(stk.ret.tot) <- "STK"
names(etf.sim) <- "ETF"

arima(stk.ret.ar1, order=c(1,0,0))

c(const.b,const.a/(1-const.b),const.varz,length(etf.sim))
s.price.init <- 25.385
e.price.init <- 22.825
## generate the price series
stk.prices.beta <- ret.to.prices(stk.ret.beta,s.price.init)
stk.prices <- ret.to.prices(stk.ret.tot,s.price.init)
etf.prices <- ret.to.prices(etf.sim,e.price.init)
sim.prices.df <- data.frame(ETF=etf.prices,STK=stk.prices,row.names=row.names(etf.sim))

## plot the price series
plot(xlf.pr,type='l')
x11()
plot(etf.prices,type='l')
## problem: mean reversion makes stock returns series look odd
x11()
plot(stk.prices.beta,type='l',ylim=c(15,80))
lines(stk.prices,col=2)

lines(etf.prices,type='l')


get.ticker.classifier.df <- function(t,c){ data.frame(TIC=t,SEC_ETF=c,row.names=t,stringsAsFactors=FALSE) }

tc.df <- get.ticker.classifier.df(c("STK"),c("ETF"))
num.days <- N
thresholds=c(sbo=1.25,sso=1.25,sbc=0.75,ssc=0.5,kmin=8.4)
draw.thresholds <- function(){
  abline(h=-thresholds["sbo"],lty=2)
  abline(h=thresholds["sso"],lty=2)
  abline(h=thresholds["sbc"],lty=2)
  abline(h=-thresholds["ssc"],lty=2)
}
draw.signal.lines <- function(act.mtx){
  lines(-as.numeric(act.mtx$bto)*abs(thresholds["sbo"]),col=2)
  lines(as.numeric(act.mtx$sto)*abs(thresholds["sso"]),col=3)
  lines(as.numeric(act.mtx$close.short)*abs(thresholds["sbc"]),col=4)
  lines(-as.numeric(act.mtx$close.long)*abs(thresholds["ssc"]),col=5)
}
draw.actions.lines <- function(a){ abline(v=which(as.numeric(a)==1),lty=3) }
get.sim.signals <- function(stk.series,etf.series,tkr.classifier,num.days){
  stock.etf.signals(data.frame(stk.series), data.frame(etf.series), tkr.classifier, num.days=num.days,compact.output=TRUE) }

est.win <- 60
sim.sig.1 <- get.signals(stk.ret.tot,etf.sim,tc.df,N-est.win+1)
sim.sig.mtx.1 <- get.signals.mtx(sim.sig.1)
sim.sig.actions.1 <- get.signals.actions(sim.sig.mtx.1)

## plot the signals:
plot(sim.sig.mtx.1$s,type='l')
draw.thresholds()
draw.signal.lines(sim.sig.actions.1)

## run the trading simulation on the generated data
sim.trades <- run.trading.simulation(  sim.sig.1, sim.prices.df
                                     , c("STK"), c("STK","ETF"), debug=TRUE, stop.on.wrn=T
                                     , tc.df)

draw.actions.lines(sim.trades$log$actions) 

extent <- list(x=c(100,600),y=c(-2,2))
plot.func <- function(lim){
  plot(sim.sig.mtx.1$s, type='l', xlim=lim$x, ylim=lim$y)
  draw.thresholds()
  draw.signal.lines(sim.sig.actions.1)
}
zoom(plot.func,extent)
