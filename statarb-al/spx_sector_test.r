## Examine what happens when you regress on SPY as opposed to the sector ETF

## start by isolating a sector and making sure you can a fixed-factor regression
## matches the old results, where the sector ETFs are picked automatically

## trading wrapper function
run.trading.1 <- function(this.sig.mtx,is.pca=FALSE,q.alloc.mtx,num.factors,instr.p.all=NULL,instr.q.all=NULL,debug=FALSE,debug.name="",silent=FALSE,pos.allocation="beta.neutral",na.interpolate=FALSE,num.iterations=-1,init.cash=1e5){
  if(is.null(instr.p.all)){ instr.p.all <- dimnames(this.sig.mtx)[[3]] }
  if(is.null(instr.q.all)){
    if(is.pca){ instr.q.all <- c(instr.p.all,names(ret.e)) }else{ instr.q.all <- names(ret.e) }
  }
  load("univ1.mid.price.nodups.RObj")  
  price.df.f <- univ1.master.price
  if(!has.zero.length(instr.p.all %w/o% names(price.df.f))){ cat("Some price info missing\n") }
  instr.p.all <- intersect(instr.p.all,names(price.df.f))
  price.df.f <- price.df.f[,c(instr.p.all,instr.q.all)]
  run.trading.simulation.cpp(  this.sig.mtx, price.df.f
                             , instr.p.all, c(instr.p.all,instr.q.all), q.alloc.mtx
                             , num.factors=num.factors
                             , PCA=is.pca, na.interpolate=na.interpolate
                             , debug=debug, debug.name=debug.name
                             , silent=silent, num.iterations=num.iterations
                             , init.cash=init.cash, pos.allocation=pos.allocation) }
load.prices <- function(names,instr){
    load("univ1.mid.price.nodups.RObj")  
    price.df.f <- univ1.master.price
    if(!has.zero.length(instr %w/o% names(price.df.f))){ cat("Some price info missing\n") }
    instr <- intersect(instr,names(price.df.f))
    price.df.f[,instr,drop=F]
  }

## Take the SPX1 stock set as a basis:
ret.mtx.file <- "spx_ret_mtx"
source("tr_test_generic_batch.r")
sig.num.days <- N-est.win+1

tc.xlf <- subset(tc.subset,SEC_ETF=="XLF")
tc.xlf <- subset(tc.xlf,TIC %in% intersect(tc.xlf$TIC,names(ret.s)))

## generate signals based on tc.xlf:
sig.xlf <- stock.etf.signals(ret.s,ret.e,tc.xlf,num.days=sig.num.days,subtract.average=TRUE)


source("f_signals_gen.r")
## now feed only XLF as ret.e and compare
ret.e.xlf <- ret.e[,c("XLF"),drop=F]
sig.xlf2 <- stock.etf.signals(ret.s,ret.e.xlf,tc.xlf,num.days=sig.num.days,subtract.average=TRUE,select.factors=FALSE)
all.equal(sig.xlf2,sig.xlf)
## [1] TRUE

## generate signals based on SPY:
ret.e.spy <- ret.e[,c("SPY"),drop=F]
sig.spy <- stock.etf.signals(ret.s,ret.e.spy,tc.xlf,num.days=sig.num.days,subtract.average=TRUE,select.factors=FALSE)
tc.spy <- tc.xlf; tc.spy$SEC_ETF = rep("SPY",nrow(tc.spy))

## generate signals based on SPY && XLF:
tc.spy.xlf <- cbind(tc.spy, tc.xlf$SEC_ETF)
sig.spy.xlf <- stock.etf.signals(ret.s,ret.e,tc.spy.xlf,num.days=sig.num.days
                                 ,subtract.average=TRUE,select.factors=TRUE,factor.names=c("beta1","beta2"))

## save(sig.xlf,sig.xlf2,sig.spy,sig.spy.xlf,file="spx_sec_test_signals.RObj")
## load("spx_sec_test_signals.RObj")

## run trading simulation of financials vs XLY:
source("f_trading_sim_cpp_mtx.r")
sim.tr.fin <- run.trading.1(sig.xlf, is.pca=FALSE, q.alloc.mtx=tc.xlf, num.factors=1,na.interpolate=T)
save(sim.tr.fin,file="tmp.RObj")

## run trading simulation of financials vs spy:
dyn.load("TradingLoopCPP/TradingLoop1.so")
sim.tr.fin.spy <- run.trading.1(sig.spy, is.pca=FALSE, q.alloc.mtx=tc.spy, num.factors=1,na.interpolate=T)
save(sim.tr.fin.spy,file="tmp2.RObj")

sim.tr.fin.spy.xlf <- run.trading.1(sig.spy.xlf, is.pca=FALSE, q.alloc.mtx=tc.spy.xlf, num.factors=2,na.interpolate=T)
last(sim.tr.fin.spy.xlf$equity)
save(sim.tr.fin.spy.xlf,file="tmp3.RObj")
## results saved as fin_reg_spx_xly_both.png (both black, xlf green, spy red)


### SPY + XLF produces very volatile results.  Run simulations on individual instruments in order to gain insight into
### what's driving the volatility

instr.p.all.d <- instr.p.all %w/o% "DFS"
eq.mtx.dbg <- matrix(0,nrow=length(instr.p.all.d),ncol=dim(sig.xlf)[1])
for(i in seq(along=instr.p.all.d)){
  instr.p.all.dbg <- instr.p.all.d[i]
  cat(instr.p.all.dbg)
  sim.tr.pca.dbg <- run.trading.1(sig.spy.xlf, is.pca=FALSE, q.alloc.mtx=tc.spy.xlf, num.factors=2,na.interpolate=T,instr.p.all=instr.p.all.dbg)
  cat("\n max: ",max(sim.tr.pca.dbg$equity))
  eq.mtx.dbg[i,] <- sim.tr.pca.dbg$equity
}

eq.mtx.max <- apply(eq.mtx.dbg,1,max)
eq.mtx.min <- apply(eq.mtx.dbg,1,min)

this.instr <- "HCBK"
this.instr.dbg <- run.trading.1(sig.spy.xlf, is.pca=FALSE, q.alloc.mtx=tc.spy.xlf, num.factors=2,na.interpolate=T
                                ,instr.p.all=this.instr,debug=TRUE,debug.name=this.instr,num.iterations=200)
## .... well never you mind, it seems that the abnormal volatility comes from trading only one instrument
## in a model fit for two: to illustrate, try
tmp <- run.trading.1(sig.spy.xlf, is.pca=FALSE, q.alloc.mtx=tc.spy.xlf, num.factors=1,na.interpolate=T)
last(tmp$equity); save(tmp,file="tmp1.RObj")

### Now compare SPY / SPY + XLF to 1 / 2 factor eigenportfolio fits
eigenvector.cache <- "eigenmatrix_spx.RObj"
corr.est.win <- 252; fit.est.win <- 60
sig.pca.num.days <- N-corr.est.win-fit.est.win+1
load(eigenvector.cache)

sig.ev.1 <- stock.pca.signals(ret.s,tc.subset,num.days=sig.pca.num.days,num.eigs=1, eigenstuff.file=eigenvector.cache, save.eigenm.fn=NULL)
sig.ev.2 <- stock.pca.signals(ret.s,tc.subset,num.days=sig.pca.num.days,num.eigs=2, eigenstuff.file=eigenvector.cache, save.eigenm.fn=NULL)
sig.ev.3 <- stock.pca.signals(ret.s,tc.subset,num.days=sig.pca.num.days,num.eigs=3, eigenstuff.file=eigenvector.cache, save.eigenm.fn=NULL)
## save(sig.ev.1,sig.ev.2,sig.ev.3,file="spx_sec_test_signals_ev.RObj")
## load("spx_sec_test_signals_ev.RObj")
sig.eig.list <- list(sig.ev.1,sig.ev.2,sig.ev.3)
for(i in 4:15){
  sig.n.evecs <- stock.pca.signals(ret.s,tc.subset,num.days=sig.pca.num.days,num.eigs=i, eigenstuff.file=eigenvector.cache, save.eigenm.fn=NULL)
  sig.eig.list[[i]] <- sig.n.evecs
  save(sig.n.evecs,file=paste("spx_sig_pca_ev",i,".RObj",sep=""))
}
save(sig.eig.list,file="spx_signals_for_varying_eigvec_num.RObj")

qalloc.chunk <- length(attributes(eig.mtx)$subdivision.idxs$eigvecs)/attributes(eig.mtx)$num.eigs
stopifnot(qalloc.chunk == length(attributes(eig.mtx)$tickers))
eigenreturns <- eig.mtx[ ,attributes(eig.mtx)$subdivision.idxs$returns]

num.factors <- 1
q.alloc.mtx1 <- (eig.mtx[ ,attributes(eig.mtx)$subdivision.idxs$eigvecs])[, 1:(qalloc.chunk*num.factors)]
colnames(q.alloc.mtx1) <- rep(attributes(eig.mtx)$tickers,num.factors)

sim.tr.fin.1ev <- run.trading.1(sig.ev.1, is.pca=TRUE, q.alloc.mtx=q.alloc.mtx1,num.factors=num.factors,na.interpolate=T,num.iterations=NULL)
save(sim.tr.fin.1ev,file="tmp1ev.RObj")
## try with with lots of init cash
sim.tr.fin.1ev.1e8 <- run.trading.1(sig.ev.1, is.pca=TRUE, q.alloc.mtx=q.alloc.mtx1,num.factors=num.factors,na.interpolate=T,num.iterations=NULL,init.cash=1e8)
save(sim.tr.fin.1ev.1e8,file="tmp1ev1.RObj")


num.factors <- 2
q.alloc.mtx2 <- (eig.mtx[ ,attributes(eig.mtx)$subdivision.idxs$eigvecs])[ ,1:(qalloc.chunk*num.factors)]
colnames(q.alloc.mtx2) <- rep(attributes(eig.mtx)$tickers,num.factors)
sim.tr.fin.2ev <- run.trading.1(sig.ev.2, is.pca=TRUE, q.alloc.mtx=q.alloc.mtx2,num.factors=num.factors,na.interpolate=T,num.iterations=NULL)
save(sim.tr.fin.2ev,file="tmp2ev.RObj")
sim.tr.fin.2ev.1e8 <- run.trading.1(sig.ev.2, is.pca=TRUE, q.alloc.mtx=q.alloc.mtx2,num.factors=num.factors,na.interpolate=T,num.iterations=NULL,init.cash=1e8)
save(sim.tr.fin.2ev.1e8,file="tmp2ev1.RObj")

num.factors <- 3
q.alloc.mtx3 <- (eig.mtx[ ,attributes(eig.mtx)$subdivision.idxs$eigvecs])[ ,1:(qalloc.chunk*num.factors)]
colnames(q.alloc.mtx3) <- rep(attributes(eig.mtx)$tickers,num.factors)
sim.tr.fin.3ev.1e8 <- run.trading.1(sig.ev.3, is.pca=TRUE, q.alloc.mtx=q.alloc.mtx3,num.factors=num.factors,na.interpolate=T,num.iterations=NULL,init.cash=1e8)
save(sim.tr.fin.3ev.1e8,file="tmp3ev1.RObj")
sim.tr.fin.3ev <- run.trading.1(sig.ev.3, is.pca=TRUE, q.alloc.mtx=q.alloc.mtx3,num.factors=num.factors,na.interpolate=T,num.iterations=NULL)
save(sim.tr.fin.3ev,file="tmp3ev.RObj")

sim.tr.spx.ev.1e8=list(sim.tr.fin.1ev.1e8,sim.tr.fin.2ev.1e8,sim.tr.fin.3ev.1e8)
for(num.factors in 4:15){
  q.alloc.mtx.n <- (eig.mtx[ ,attributes(eig.mtx)$subdivision.idxs$eigvecs])[ ,1:(qalloc.chunk*num.factors)]
  colnames(q.alloc.mtx.n) <- rep(attributes(eig.mtx)$tickers,num.factors)
  sim.tr.spx.n.ev <- run.trading.1(sig.eig.list[[num.factors]], is.pca=TRUE, q.alloc.mtx=q.alloc.mtx.n,num.factors=num.factors,na.interpolate=T,num.iterations=NULL,init.cash=1e8)
  sim.tr.spx.ev.1e8[[num.factors]] <- sim.tr.spx.n.ev
}  
save(sim.tr.spx.ev.1e8,file="spx_tr_sim_for_varying_eigvec_num.RObj")

num.factors <- 2; stk <- "JPM"; n.iter <- 50; init.cash <- 1e8
dyn.load("TradingLoopCPP/TradingLoopPCA5.so")
sim.tr.2ev.wtf <- run.trading.1(sig.ev.2, is.pca=TRUE, q.alloc.mtx=q.alloc.mtx2, debug=TRUE,debug.name=stk
                                , instr.p.all=stk
                                , instr.q.all=c(dimnames(sig.ev.2)[[3]],names(ret.e))
                                , num.factors=num.factors,na.interpolate=T,num.iterations=n.iter,init.cash=init.cash)



## so both these plots are an epic fail.  To understand why, ascertain that the first eigenportfolio should
## behave very similar to SPY (and, likewise, the signals on it should mirror the signals on SPY for any given instrument)
inv.amounts <- rowSums(q.alloc.mtx1)
eig.evolution <-cumsum(eigenreturns[,1])
eig.evolution.pct <- cumsum(eigenreturns[,1]/inv.amounts)
spy.ret <- reverse.rows(ret.e)[names(eig.evolution),"SPY"]
eig.ret <- eigenreturns[,1]/inv.amounts
save(spy.ret,eig.ret,file="tmp_eig1.RObj")
## these eigenreturns are 90% similar, so expect the trading to produce similar results
lm(formula = spy.ret ~ eig.ret)$coefficients
##   (Intercept)       eig.ret 
## -0.0002547444  0.8928950667

## generate fake price series data from eigen1 returns signal
spy.pr <- load.prices(names(eig.ret),"SPY")
spy.ret.pr <- ret.to.prices(spy.ret,spy.pr[first(names(eig.ret)),])
ev1.ret.pr <- ret.to.prices(eig.ret,0.5*spy.pr[first(names(eig.ret)),])
## note that exact price levels don't really matter
save(spy.ret,eig.ret,spy.ret.pr,ev1.ret.pr,file="tmp_eig1.RObj")

## generate signals
ret.e.ev1 <- reverse.rows(as.data.frame(eig.ret)); names(ret.e.ev1) <- "E01"
N <- nrow(ret.e.ev1)
sig.fake.ev1 <- stock.etf.signals(ret.s[1:nrow(ret.e.ev1),],ret.e.ev1
                                  , tc.spy,num.days=sig.num.days,subtract.average=TRUE,select.factors=FALSE)
sig.fake.ev1 <- sig.fake.ev1[dimnames(sig.ev.1)[[1]],,]
sig.spy.1 <- sig.spy[dimnames(sig.ev.1)[[1]],,]
save(sig.fake.ev1,sig.ev.1,sig.spy.1,file="tmp_eig2.RObj")
## plot(sig.ev.1[,,"JPM"][,"s"],type='l'); lines(sig.fake.ev1[,,"JPM"][,"s"],col=2)
## savePlot("spx_eig_fake_vs_actual_ev1_sig.png")
## the signals track each other rather closely

## trade them
num.factors <- 1; stk <- "JPM"
tr.sim.jpm.spy1 <- run.trading.1(sig.spy.1, is.pca=FALSE, q.alloc.mtx=tc.spy, num.factors=num.factors,na.interpolate=T
                                , instr.p.all=stk,debug=FALSE,debug.name=stk,num.iterations=NULL)
tr.sim.jpm.fake.e1 <- run.trading.1(sig.fake.ev1, is.pca=FALSE, q.alloc.mtx=tc.spy, num.factors=num.factors
                                    , na.interpolate=T,instr.p.all=stk,debug=FALSE,debug.name=stk,num.iterations=NULL)
save(tr.sim.jpm.fake.e1,tr.sim.jpm.spy1,spy.ret,eig.ret,spy.ret.pr,ev1.ret.pr,file="tmp_eig1.RObj")
## plot(tr.sim.jpm.spy1$equity,type='l')
## lines(tr.sim.jpm.fake.e1$equity,col=2)
## get very similar results 

plot(eig.evolution.pct-first(eig.evolution.pct),type='l')
lines(-first(cumsum(spy.ret))+cumsum(spy.ret),col=2)
## savePlot("spy_vs_ev1.png")

## The eigenportfolio, up to a proportionality factor, seems to consistently trend with the SPY
## Thus, I expect the signals for trading e.g. JPM/SPY and JPM/EIGENP-1 to be very similar.
## Let us check this:
stk <- "JPM"
stk.ev1.sig.mtx <- sig.ev.1[,,stk]
stk.spy.sig.mtx <- sig.spy[,,stk]
stopifnot(nrow(stk.spy.sig.mtx) >= nrow(stk.ev1.sig.mtx))
stk.spy.sig.mtx <- stk.spy.sig.mtx[rownames(stk.ev1.sig.mtx),]

get.long.short.signals <- function(sig.mtx){
  act.mtx <- get.signals.actions(sig.mtx)
  list(  long.signal=position.signal(act.mtx$bto,act.mtx$close.long)
       , short.signal=position.signal(act.mtx$sto,act.mtx$close.short) )}

signals.spy <- get.long.short.signals(stk.spy.sig.mtx)
signals.ev1 <- get.long.short.signals(stk.ev1.sig.mtx)

par.save()
par(mfrow=c(2,1))
plot(stk.spy.sig.mtx[,"s"],type='l')
draw.thresholds()
lines(-thresholds["sbo"]*as.numeric(signals.spy$long.signal),col=2)
lines( thresholds["sso"]*as.numeric(signals.spy$short.signal),col=3)
plot(stk.ev1.sig.mtx[,"s"],type='l')
draw.thresholds()
lines(-thresholds["sbo"]*as.numeric(signals.ev1$long.signal),col=2)
lines( thresholds["sso"]*as.numeric(signals.ev1$short.signal),col=3)
par.restore()
## savePlot("jpm_spy_ev1_signals.png")

## same, but with signals only
par.save()
par(mfrow=c(2,1))
plot(-thresholds["sbo"]*as.numeric(signals.spy$long.signal),type='l',col=2,ylim=c(-2,2))
lines( thresholds["sso"]*as.numeric(signals.spy$short.signal),col=3)
draw.thresholds()
plot(-thresholds["sbo"]*as.numeric(signals.ev1$long.signal),type='l',col=4,ylim=c(-2,2))
lines( thresholds["sso"]*as.numeric(signals.ev1$short.signal),col=5)
draw.thresholds()
par.restore()
## savePlot("jpm_spy_ev1_signals_only.png")

## all right, so given similar signals, how does PNL look for the two trading simulations?
source("f_trading_sim_cpp_mtx.r")
dyn.load("TradingLoopCPP/TradingLoop1.so")
num.factors <- 1

## align dates for comparison
dates.ev1 <- dimnames(sig.ev.1)[[1]]
sig.spy.al <- sig.spy[dates.ev1,,]
q.alloc.mtx1 <- (eig.mtx[dates.ev1, attributes(eig.mtx)$subdivision.idxs$eigvecs])[, 1:(qalloc.chunk*num.factors)]
colnames(q.alloc.mtx1) <- rep(attributes(eig.mtx)$tickers,num.factors)
## trade with init.cash = 1e8 (otherwise get huge rounding errors)
n.iter <- NULL; init.cash <- 1e8
dyn.load("TradingLoopCPP/TradingLoop1.so")
tr.sim.jpm.spy <- run.trading.1(sig.spy.al, is.pca=FALSE, q.alloc.mtx=tc.spy, num.factors=num.factors,na.interpolate=T
                                , instr.p.all=stk,debug=FALSE,debug.name=stk,num.iterations=n.iter,init.cash=init.cash)
tr.sim.jpm.1ev <- run.trading.1(sig.ev.1, is.pca=TRUE, q.alloc.mtx=q.alloc.mtx1
                                , num.factors=num.factors, instr.p.all=stk
                                , instr.q.all=c(dimnames(sig.ev.1)[[3]],names(ret.e))
                                , debug=FALSE,debug.name=stk,na.interpolate=T,num.iterations=n.iter,init.cash=init.cash)

save(tr.sim.jpm.spy,tr.sim.jpm.1ev,file="tmp_eig2.RObj")

plot(tr.sim.jpm.spy$equity,type='l')
lines(tr.sim.jpm.1ev$equity,col=2)
## fail
## run w/ debugging info
tr.sim.jpm.spy.dbg <- run.trading.1(sig.spy.al, is.pca=FALSE, q.alloc.mtx=tc.spy
                                    , num.factors=num.factors,na.interpolate=T
                                    , instr.p.all=stk,debug=TRUE,debug.name=stk,num.iterations=400)
tr.sim.jpm.1ev.dbg <- run.trading.1(sig.ev.1, is.pca=TRUE, q.alloc.mtx=q.alloc.mtx1
                                , num.factors=num.factors, instr.p.all=stk
                                , instr.q.all=c(dimnames(sig.ev.1)[[3]],names(ret.e))
                                , debug=TRUE,debug.name=stk,na.interpolate=T,num.iterations=400)

## trade "fake ev1" vs "actual ev1", with debug info
## trade them
num.factors <- 1; stk <- "JPM"; n.iter <- 100; init.cash <- 1e8
dyn.load("TradingLoopCPP/TradingLoop1.so")
tr.sim.jpm.fake.e1.dbg1 <- run.trading.1(sig.fake.ev1, is.pca=FALSE, q.alloc.mtx=tc.spy, num.factors=num.factors
                                         , na.interpolate=T,instr.p.all=stk,debug=TRUE,debug.name=stk
                                         , num.iterations=n.iter, init.cash=init.cash)
dyn.load("TradingLoopCPP/TradingLoopPCA4.so"); n.iter <- 50
tr.sim.jpm.1ev.dbg1 <- run.trading.1(sig.ev.1, is.pca=TRUE, q.alloc.mtx=q.alloc.mtx1
                                     , num.factors=num.factors, instr.p.all=stk
                                     , instr.q.all=c(dimnames(sig.ev.1)[[3]],names(ret.e))
                                     , debug=TRUE,debug.name=stk,na.interpolate=T
                                     , num.iterations=n.iter,init.cash=init.cash)


## save.image("screwed_by_comint_again.RData")
## load("screwed_by_comint_again.RData")

## source("tr_test_generic_batch.r")
## source("f_signals_gen.r")
## source("f_trading_sim_cpp_mtx.r")            
