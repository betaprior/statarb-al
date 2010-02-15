ret.mtx.file <- "spx_ret_mtx"
source("tr_test_generic_batch.r")


corr.est.win <- 252
fit.est.win <- 60
num.eigs <- 15  
source("functions.r")
## Rprof("pcastuff.Rprof")
mtx1 <- stock.pca.signals(ret.s,tc.subset,num.days=N-corr.est.win-fit.est.win+1,num.eigs=num.eigs, save.eigenm.fn="eigenmatrix_spx.RObj",flipsign=TRUE)
## save(mtx1,file="sig.pca.spx.flipsign.RObj")
## load("sig.pca.spx.flipsign.RObj")
mtx1NI <- stock.pca.signals(ret.s,tc.subset,num.days=N-corr.est.win-fit.est.win+1,num.eigs=num.eigs, save.eigenm.fn="eigenmatrix_spx.RObj",flipsign=FALSE)
## Rprof(NULL)


load("pca_one_eigen_record.RObj") ## loads q.factors, q.sdevs, q.evals
q.eigenvecs <- q.factors*q.sdevs
plot(rev(sort(q.eigenvecs[,1])),type='l'); abline(h=0)
corr.etfs <- tc.spx$SEC_ETF[rev(sorted.vector.indices(q.eigenvecs[,1]))]
require("ggplot2")
sorted.ev <- data.frame(ev=rev(sort(q.eigenvecs[,1])),ind=corr.etfs)
p <- ggplot(sorted.ev)
p+aes(x=ind,y=ev)


qplot(1:nrow(sorted.ev),ev,data=sorted.ev,color=ind,xlab="Sorted eigenvector index",ylab="Eigenvector component")

plot(rev(sort(q.eigenvecs[,2])),type='l'); abline(h=0)

plot(rev(sort(q.eigenvecs[,3])),type='l'); abline(h=0)

#Rprof("pcastuff.Rprof")
registerDoMC(4)
getDoParWorkers()
stock.pca.signals(ret.s,tc.subset,num.days=N-corr.est.win-fit.est.win+1,num.eigs=num.eigs,eigenstuff.file="")
#Rprof(NULL)
num.stocks <- dim(mtx1)[3]
q.factors <-
  matrix(mtx1[1,1:(num.eigs*num.stocks),1],nrow=num.stocks,ncol=num.eigs,byrow=F)
q.sdevs <- mtx1[1,(num.eigs*num.stocks+1):(num.eigs*(num.stocks+1)),1]
q.evals <- mtx1[1,(num.eigs*(num.stocks+1)+1):(num.eigs*(num.stocks+2)),1]


###--------------start here:
ret.mtx.file <- "spx_ret_mtx"
source("tr_test_generic_batch.r")

corr.est.win <- 252
fit.est.win <- 60
num.eigs <- 15  
source("functions.r")


mtx1NI <- stock.pca.signals(ret.s,tc.subset,num.days=N-corr.est.win-fit.est.win+1,num.eigs=num.eigs, save.eigenm.fn="eigenmatrix_spx.RObj")
## save(mtx1NI,file="sig.pca.spx.2.RObj")
load("sig.pca.spx.2.RObj") ## loads mtx1NI
sig.mtx.pca <- mtx1NI

##load("sig.mtx.pca.RObj")

## load("pca_spx_eig_mtx_spx.RObj")  ##loads eig.mtx
load("eigenmatrix_spx.RObj")  ##loads eig.mtx
q.alloc.mtx <- eig.mtx[ ,attributes(eig.mtx)$subdivision.idxs$eigvecs]
colnames(q.alloc.mtx) <- rep(attributes(eig.mtx)$tickers,num.eigs)

instr.p.all <- dimnames(sig.mtx.pca)[[3]]

### --- load clenaned-up price data file (dup col names removed)
load("univ1.mid.price.nodups.RObj")   # loads univ1.master.price
price.df.f <- univ1.master.price
## should now have no duplicates and all instr.p.all have price info
if(!has.zero.length(instr.p.all %w/o% names(price.df.f))){ cat "Some price info missing\n" }
instr.p.all <- intersect(instr.p.all,names(price.df.f))
instr.q.all <- names(ret.e)
price.df.f <- price.df.f[,c(instr.p.all,instr.q.all)]

source("f_trading_sim_cpp_mtx.r")
sim.trades.f.all.pca <- run.trading.simulation.cpp(  sig.mtx.pca, price.df.f
                                                   , instr.p.all, c(instr.p.all,instr.q.all), q.alloc.mtx
                                                   , num.factors=num.eigs
                                                   , PCA=TRUE
                                                   , debug=FALSE, debug.name="JPM"
                                                   , silent=FALSE
                                                   , pos.allocation="beta.neutral")


## --------------- 

source("f_trading_sim_cpp_mtx.r")
st.pca.dbg.inv <- run.trading.simulation.cpp(  mtx1, price.df.f
                                             , instr.p.all, c(instr.p.all,instr.q.all), q.alloc.mtx
                                             , num.factors=num.eigs
                                             , PCA=TRUE
                                             , debug=FALSE, debug.name="JPM"
                                             , silent=FALSE
                                             , pos.allocation="beta.neutral")

### debugging the PCA trading simulation
which(dimnames(sig.mtx.pca)[[3]]=="JPM")
## [1] 199

source("time_series_functions.r")
this.instr <- "JPM"
sig.mtx.dbg <- as.data.frame(sig.mtx.pca[,,this.instr])
sig.actions.dbg <- get.signals.actions(sig.mtx.dbg)
dates.dbg <- dimnames(sig.mtx.pca)[[1]]
prices.dbg <- price.df.f[dates.dbg,c(this.instr,"XLF")]
ret.dbg <- cbind(ret.s,ret.e)[dates.dbg,c(this.instr,"XLF")]

long.signal <- position.signal(sig.actions.dbg$bto,sig.actions.dbg$close.long)
short.signal <- position.signal(sig.actions.dbg$sto,sig.actions.dbg$close.short)


par.save()
## par(mfrow=c(2,1))
plot(sig.mtx.dbg$s,type='l')
draw.thresholds()
## draw.signal.lines(sig.actions.dbg)
draw.thresholds()
lines(-thresholds["sbo"]*as.numeric(long.signal),col=2)
lines( thresholds["sso"]*as.numeric(short.signal),col=3)
## plot(prices.dbg[[this.instr]],type='l',ylim=c(20,90))
## lines(prices.dbg$XLF,col=2)
par.restore()


source("f_trading_sim_cpp_mtx.r")
instr.p.all.dbg <- c("DTE")
sim.tr.pca.dbg <- run.trading.simulation.cpp(  sig.mtx.pca, price.df.f
                                             , instr.p.all.dbg, c(instr.p.all,instr.q.all), q.alloc.mtx
                                             , num.factors=num.eigs
                                             , PCA=TRUE
                                             , debug=FALSE, debug.name="JPM"
                                             , silent=FALSE
                                             , pos.allocation="beta.neutral")


eq.mtx.dbg <- matrix(0,nrow=length(instr.p.all),ncol=200)
for(i in seq(along=instr.p.all)){
  instr.p.all.dbg <- instr.p.all[i]
  sim.tr.pca.dbg <- run.trading.simulation.cpp(  sig.mtx.pca, price.df.f
                                               , instr.p.all.dbg, c(instr.p.all,instr.q.all), q.alloc.mtx
                                             , num.factors=num.eigs
                                             , PCA=TRUE
                                             , debug=FALSE, debug.name="JPM"
                                             , silent=FALSE
                                             , pos.allocation="beta.neutral")
  eq.mtx.dbg[i,] <- sim.tr.pca.dbg$equity[1:200]
}

eq.mtx.max <- apply(eq.mtx.dbg,1,max)
eq.mtx.min <- apply(eq.mtx.dbg,1,min)
