## Plotting the s-factor and the signal/transcation lines

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

if(!testObject(ret.s)) { ret.s <- get.stock.returns("spx_ret_mtx",M=252,offset=offset.2005,na.pct.cutoff=0.01,file=TRUE) }
if(!testObject(ret.e)) { ret.e <- get.etf.returns("etf_ret_mtx",M=252,offset=offset.2005,file=TRUE) }

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

