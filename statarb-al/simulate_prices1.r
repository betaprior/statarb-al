## simulate a large artificial price series "inspired" by JPM/XLF pair trading
require("timeSeries")
require("fGarch")
require("sde")

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


## fit an AR(1)/GARCH(1,1) model to this series

gf1=garchFit(formula=~arma(0,0)+garch(1,1),xlf.lret,trace=F)
gf2=garchFit(formula=~arma(1,0)+garch(1,1),xlf.lret,trace=F)
gf3=garchFit(formula=~arma(1,1)+garch(1,1),xlf.lret,trace=F)
gf.ar=ar(xlf.lret,aic=F,order.max=1,method="yw")

## simulate the series based on fits
N <- 1000
gf1.sim <- garchSim(spec=garchSpec(gf1),n=N)
gf2.sim <- garchSim(spec=garchSpec(gf2),n=N)
gf3.sim <- garchSim(spec=garchSpec(gf3),n=N)

plot.sim <- function(spec){
  sim <- garchSim(spec=spec)
  qqnorm(sim)
  qqline(sim)
}

#plot.sim(garchSpec(garchFit(formula=~arma(1,1)+garch(1,1),xlf.lret,trace=F)))

## fitting returns series opens up a whole another can of worms, so right now just
## go with the ARMA(1,1) / GARCH(1,1) model (gf3)

N <- 1000
etf.sim <- garchSim(spec=garchSpec(gf3),n=N)

const.beta <- 1.25
const.a <- 0.0009
const.b <- 0.82
const.varz <- 8e-5
const.k <- 50
ar.sim <- function(a,b,varz,N){
  zeta <- rnorm(N,sd=sqrt(varz))
  filter(a+zeta,as.numeric(b),method="recursive")
}
fit.ar1.series <- function(x) ar(x, aic=F, order.max=1, method="yw")
ar.params.from.fit <- function(x){ ## x is ar fit
  list(m=x$x.mean
       , a=x$x.mean*(1-x$ar)
       , b=x$ar
       , varz=x$var.pred) }

ar.sim.simple <- function(ar1,varz,N){
  zeta <- rnorm(N,sd=sqrt(varz))
  filter(zeta,as.numeric(ar1),method="recursive")
}

## this assumes ret[i+1]=(S[i+1]-S[i])/S[i]
ret.to.prices <- function(ret,p0){
  x <- rep(0,length(ret))
  x[1] <- p0
  for(i in seq(along=ret)[c(-1)])
    x[i] <- x[i-1]*(ret[i]+1)
  x
}



stk.ret.beta <- etf.sim*const.beta
stk.ret.ar1 <- ar.sim(const.a,const.b,const.varz,length(etf.sim))
stk.ret.ar1.diff <- c(stk.ret.ar1[1],diff(stk.ret.ar1))
stk.ret.tot <- stk.ret.beta+stk.ret.ar1.diff

names(stk.ret.tot) <- "STK"
names(etf.sim) <- "ETF"


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
plot(stk.prices.beta,type='l',ylim=c(22,28))
lines(stk.prices,col=2)
 

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
get.sim.signals.mtx <- function(sig.list){
  sig.mtx <- prealloc.mtx(  length(sig.list$sig.dates)
                          , length(sig.list$sig.dates[[1]])
                          , rownames=rev(names(sig.list$sig.dates)))
  for(i in rev(seq(along=sig.list$sig.dates))){
    sig.mtx[i,] <- sig.list$sig.dates[[i]]
  }
  colnames(sig.mtx) <- c("action","s","k","m","mbar","a","b","varz","beta")
  data.frame(sig.mtx)
}
get.sim.signals.actions <- function(sig.mtx){
  sim.actions <- lapply(sig.mtx[,"action"],decode.signals)
  sim.actions <- as.data.frame(do.call("rbind",sim.actions))
  row.names(sim.actions) <- row.names(sig.mtx)
  return(sim.actions)
}
  
sim.sig.1 <- get.sim.signals(stk.ret.tot,etf.sim,tc.df,N-59)
sim.sig.mtx.1 <- get.sim.signals.mtx(sim.sig.1)
sim.sig.actions.1 <- get.sim.signals.actions(sim.sig.mtx.1)

## plot the signals:
plot(sim.sig.mtx.1$s,type='l')
draw.thresholds()
draw.signal.lines(sim.sig.actions.1)

## run the trading simulation on the generated data
sim.trades <- run.trading.simulation(  sim.sig.1, sim.prices.df
                                     , c("STK"), c("STK","ETF"), debug=TRUE
                                     , tc.df)
draw.actions.lines(sim.trades$log$actions) 
