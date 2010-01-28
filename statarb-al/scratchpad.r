## Plotting the s-factor and the signal/transcation lines
##  setwd("e:/projects/finance/research/statarb-al")
##  setwd("~/projects/finance/research/statarb-al")
## install.packages(c("fGarch","timeSeries","xts","quantmod","sde"))
library("sde")

plot(as.numeric(s),type='l')
thresholds=c(sbo=1.25,sso=1.25,sbc=0.75,ssc=0.5,kmin=8.4)
abline(h=-thresholds["sbo"],lty=2)
abline(h=thresholds["sso"],lty=2)
abline(h=thresholds["sbc"],lty=2)
abline(h=-thresholds["ssc"],lty=2)
lines(-as.numeric(s.bto)*abs(thresholds["sbo"]),col=2)
lines(as.numeric(s.sto)*abs(thresholds["sso"]),col=3)
lines(as.numeric(s.close.short)*abs(thresholds["sbc"]),col=4)
lines(-as.numeric(s.close.long)*abs(thresholds["ssc"]),col=5)
abline(v=which(as.numeric(s.action)==1),lty=3)


## Debugging the JPM trading simulation
## First, need to ascertain that returns series and prices series are consistent

ret.s <- get.stock.returns("spx_ret_mtx",M=252,offset=offset.2005,na.pct.cutoff=0.01,file=TRUE) 
ret.e <- get.etf.returns("etf_ret_mtx",M=252,offset=offset.2005,file=TRUE) 

ret.one.s <- ret.s[,"JPM",drop=F]
ret.one.e <- ret.e[,"XLF",drop=F]
rm("ret.s"); rm("ret.e")

## this assumes ret[i+1]=(S[i+1]-S[i])/S[i]
ret.to.prices <- function(ret,p0){
  x <- rep(0,length(ret))
  x[1] <- p0
  for(i in seq(along=ret)[c(-1)])
    x[i] <- x[i-1]*(ret[i]+1)
  x
}

portf <- c(103, -180)
## debugging jpm/xlf signals: what is actually going on?
jx.dbg1 <- read.csv('jpm.xlf.dbg1')
jx.dbg1[,1] <- 1/jx.dbg1[,1]
names(jx.dbg1) <- c("beta","jpm.act","xlf.act")
attach(jx.dbg1)
jx.dbg1 <- cbind(jx.dbg1,c(NA,(diff(jpm.act)/jpm.act[c(-length(jpm.act))])))
jx.dbg1 <- cbind(jx.dbg1,c(NA,(diff(xlf.act)/xlf.act[c(-length(xlf.act))])))
names(jx.dbg1)[c(4,5)] <- c("jpm.ret","xlf.ret")
jpm.pred <- ret.to.prices(beta*xlf.ret,jpm.act[1])
jpm.pred.const.b <- ret.to.prices(beta[1]*xlf.ret,jpm.act[1])
jx.dbg1 <- cbind(jx.dbg1,jpm.pred)
jx.dbg1 <- cbind(jx.dbg1,jpm.pred.const.b)
val.act <- jpm.act*portf[1]+xlf.act*portf[2]
val.pred <- jpm.pred*portf[1]+xlf.act*portf[2]
val.pred.const.b <- jpm.pred.const.b*portf[1]+xlf.act*portf[2]
jx.dbg1 <- cbind(jx.dbg1,val.act)
jx.dbg1 <- cbind(jx.dbg1,val.pred)
jx.dbg1 <- cbind(jx.dbg1,val.pred.const.b)
detach(jx.dbg1)



## Investigate the exact behavior of the time series



ret.s <- get.stock.returns("spx_ret_mtx",M=num.days,offset=offset.2005,na.pct.cutoff=0.01,file=TRUE) 
ret.e <- get.etf.returns("etf_ret_mtx",M=num.days,offset=offset.2005,file=TRUE) 

ret.one.s <- ret.s[,"JPM",drop=F]
ret.one.e <- ret.e[,"XLF",drop=F]
rm("ret.s"); rm("ret.e")

ret.one.s <- reverse.rows(ret.one.s)
ret.one.e <- reverse.rows(ret.one.e)

ret.subset <- merge(ret.one.e,ret.one.s,by="row.names")
series.data <- cbind(ret.subset,as.numeric(s.betas["JPM",]))

ret.subset <- ret.subset[names(ret.subset) %w/o% "Row.names"][c(-1),]

names(series.data)[1] <- c("dates")
names(series.data)[4] <- c("beta.60")
series.data <- cbind(series.data,as.numeric(s["JPM",]))
names(series.data)[5] <- "s"

series.data <- cbind(series.data,as.numeric(s.action["JPM",]))
names(series.data)[6] <- "s.act"

series.data <- cbind(series.data,as.numeric(s.a["JPM",]))
names(series.data)[7] <- "a"
series.data <- cbind(series.data,as.numeric(s.b["JPM",]))
names(series.data)[8] <- "b"
series.data <- cbind(series.data,as.numeric(s.varz["JPM",]))
names(series.data)[9] <- "varz"
series.data <- cbind(series.data,as.numeric(s.k["JPM",]))
names(series.data)[10] <- "k"


## varz appears to be negative in half the cases.  not a good thing.
## call the fitting functions on this series to investigate
start.row <- 1; win <- 60
manual.fit <- get.ou.series.etf( reverse.rows(series.data[start.row:(start.row+win-1),"JPM",drop=F])
                                ,reverse.rows(series.data[start.row:(start.row+win-1),"XLF",drop=F]),tc.subset)

## > mean(series.data$a)
## [1] 0.0009187012
## > mean(series.data$b)
## [1] 0.8248221
## > mean(series.data$varz)
## [1] 7.886057e-05
## > mean(series.data$beta.60)
## [1] 1.246905
## mean(series.data$k)
## [1] 50.14241

const.beta <- 1.25
const.a <- 0.0009
const.b <- 0.82
const.varz <- 8e-5
const.k <- 50
ou.params <- c("k"=const.k,"m"=const.a/(1-const.b),"sigma"=sqrt(const.varz*2*const.k/(1-const.b^2)))
ou.theta <- c(ou.params["k"]*ou.params["m"],ou.params["k"],ou.params["sigma"])
##  head(current.univ.price)[c("JPM","XLF")]
##             JPM    XLF
## 20030102 25.385 22.825
## 20030103 25.925 22.815
s.price.init <- 25.385
e.price.init <- 22.825

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

## testing AR simulation
## does it look like an AR1?
sim.test1 <- ar.sim.simple(-0.7,1,1000)
## acf(sim.test1); pacf(sim.test1)

## does the simulation/fit work?..
fit.ar1.series(ar.sim.simple(0.75,0.4,10000))  ##should recover the coeffs
ar.params.from.fit(fit.ar1.series(ar.sim(0.5,0.75,0.4,10000))) ##likewise should recover coeffs


## now run the simulation on our numbers
unlist(ar.params.from.fit(fit.ar1.series(ar.sim(const.a,const.b,const.varz,10000)))) ##likewise should recover coeffs
c(const.a/(1-const.b),const.a,const.b,const.varz)

## simulate the Vasicek process using the SDE package and ascertain the
## results match those from AR(1) model simulation
## R:sde notation:            dX_t = (theta[1]-theta[2]*Xt)*dt + theta[3]*dWt.
## 
sim.ret.beta <- series.data$XLF*const.beta
sim.ret.ou <- rsOU(n=nrow(series.data),theta=ou.theta)
sim.ret <- sim.ret.beta+sim.ret.ou
unlist(ar.params.from.fit(fit.ar1.series(sim.ret.ou)))
## it doesn't match

sim.ret.ar1 <- ar.sim(const.a,const.b,const.varz,nrow(series.data))
sim.ret.ar1.diff <- c(sim.ret.ar1[1],diff(sim.ret.ar1))
sim.ret.tot <- sim.ret.beta+sim.ret.ar1.diff
series.data <- cbind(series.data,as.numeric(sim.ret.tot))
names(series.data)[11] <- "jpm.sim.ret"

jpm.pr.beta.sim <- ret.to.prices(sim.ret.beta,s.price.init)
jpm.pr.sim <- ret.to.prices(sim.ret.tot,s.price.init)
xlf.pr <- ret.to.prices(series.data$XLF,e.price.init)

sim.ar.series <- ar.sim(const.a,const.b,const.varz,200)
sim.ar.fit <- fit.ar1.series(sim.ar.series)
ar.params.from.fit(sim.ar.fit)


sig.jpm.synthetic.list <- stock.etf.signals(  data.frame(JPM=series.data$jpm.sim.ret,row.names=series.data$dates)
                                       , data.frame(XLF=series.data$XLF,row.names=series.data$dates)
                                       , tc.subset["JPM",,drop=F]
                                       , num.days=num.days-59,compact.output=TRUE)

sig.jpm.synthetic <-      
  prealloc.mtx(length(sig.jpm.synthetic.list$sig.dates)
               ,length(sig.jpm.synthetic.list$sig.dates[[1]])
               ,rownames=rev(names(sig.jpm.synthetic.list$sig.dates)))

for(i in rev(seq(along=sig.jpm.synthetic.list$sig.dates))){
  sig.jpm.synthetic[i,] <- sig.jpm.synthetic.list$sig.dates[[i]]
}
colnames(sig.jpm.synthetic) <-
  c("action","s","k","m","mbar","a","b","varz","beta")
sig.jpm.synthetic <- data.frame(sig.jpm.synthetic)

sig.jpm.synthetic.action <- lapply(sig.jpm.synthetic[,"action"],decode.signals)
sig.jpm.synthetic.action <- data.frame(t(data.frame(sig.jpm.synthetic.action)))
rownames(sig.jpm.synthetic.action) <- rownames(sig.jpm.synthetic)


plot(as.numeric(sig.jpm.synthetic$s),type='l')
thresholds=c(sbo=1.25,sso=1.25,sbc=0.75,ssc=0.5,kmin=8.4)
abline(h=-thresholds["sbo"],lty=2)
abline(h=thresholds["sso"],lty=2)
abline(h=thresholds["sbc"],lty=2)
abline(h=-thresholds["ssc"],lty=2)
with(sig.jpm.synthetic.action,{
     lines(-as.numeric(bto)*abs(thresholds["sbo"]),col=2)
     lines(as.numeric(sto)*abs(thresholds["sso"]),col=3)
     lines(as.numeric(close.short)*abs(thresholds["sbc"]),col=4)
     lines(-as.numeric(close.long)*abs(thresholds["ssc"]),col=5)
    ## abline(v=which(as.numeric(s.action)==1),lty=3)
     })


price.subset <- current.univ.price[names(current.univ.price) %in% c("JPM","XLF")]
ret.from.prices <- apply(price.subset,2,function(x){ c(NA,diff(x))/x })[c(-1),]
lret.from.prices <- apply(price.subset,2,function(x){ diff(log(x)) })

jpm.compare <- cbind(ret.subset[,"JPM"],ret.from.prices[,"JPM"],lret.from.prices[,"JPM"])
xlf.compare <- cbind(ret.subset[,"XLF"],ret.from.prices[,"XLF"],lret.from.prices[,"XLF"])

## plot(1:251,jpm.compare[,1],type='l')
## lines(1:251,jpm.compare[,2],col=2)
## lines(1:251,jpm.compare[,3],col=3)
 
## plot(1:251,xlf.compare[,1],type='l')
## lines(1:251,xlf.compare[,2],col=2)
## lines(1:251,xlf.compare[,3],col=3)

## well, it looks like the price series are all correct
