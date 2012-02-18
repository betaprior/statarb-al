## test pair trading on financial stocks vs xlf
source("tr_test_spx1_batch.r")

##signals$sig.dates list will be reverse-chronological and might contain mor dates 
##than what we want for a trading simulation.  The dates for backtesting are determined
##by signals$sig.dates (prices data frame is subset accordingly)

load("sig.spx2NI.RObj")
sig.mtx.f <- get.signals.mtx(sig.f)
##sig.mtx usage: > head(sig.mtx.f[,,"JPM"])
##sig.actions.f <- get.signals.actions(sig.mtx.f[,,"JPM"])
##sig.mtx.dbg <- as.data.frame(sig.mtx.f[,,this.instr])
## (w/o casting to data frame can't subset by column names)

instr.p.all <- intersect(sig.f$tickers, tc.spx$TIC) ## make sure all tickers are classified
instr.q.all <- names(ret.e)
load("univ1.mid.price.RObj") # loads univ1.master.price
## current issue: duplcate names in the price dataframe
dups <- names(univ1.master.price)[duplicated(names(univ1.master.price))]
## find duplicated names in price file:
which(names(univ1.master.price)=="HHH")
## [1]  201 1621
## > head(univ1.master.price[,c(201,1621)])
##             HHH HHH.1
## 20081231 31.900    NA
univ1.master.price <- univ1.master.price[,-c( which(names(univ1.master.price)=="HHH")[2]
                                             , which(names(univ1.master.price)=="JCI")[2]
                                             , which(names(univ1.master.price)=="PXD")[2])]
price.df.f <- univ1.master.price[,names(univ1.master.price) %w/o% (dups %w/o%
                                                                   c("HHH","JCI","PXD"))]
if(length(-which(instr.p.all %in% (instr.p.all %w/o% names(price.df.f))))>0){
  instr.p.all <- instr.p.all[which(instr.p.all %in% (instr.p.all %w/o%
                                                    names(price.df.f)))]
}
## gets rid of JCI, PXD


##Now we try to trade all of them in the same simulation
sim.trades.f.all <- run.trading.simulation(  sig.f, price.df.f
                                           , instr.p.all, c(instr.p.all,instr.q.all), tc.spx
                                           , debug=FALSE, silent=FALSE
                                           , pos.allocation="beta.neutral")

sim.trades.f.all.cpp <- run.trading.simulation.cpp(  sig.f, price.df.f
                                                   , instr.p.all, c(instr.p.all,instr.q.all), tc.spx
                                                   , debug=FALSE, silent=FALSE
                                                   , pos.allocation="beta.neutral")


## testing the new trading simulation interface:
source("f_trading_sim_cpp_mtx.r")
sim.trades.f.all.cpp <- run.trading.simulation.cpp(  sig.mtx.f, price.df.f
                                                   , instr.p.all, c(instr.p.all,instr.q.all), tc.spx
                                                   , num.factors=1, PCA=FALSE
                                                   , debug=FALSE, debug.name="JPM"
                                                   , silent=FALSE
                                                   , pos.allocation="beta.neutral")

instr <- "FDX"
sim.trades.f <- run.trading.simulation(  sig.f, price.df.f
                                       , instr, c(instr,tc.spx[instr,]$SEC_ETF), tc.spx
                                       , debug=FALSE, silent=FALSE)
##pgr.equity <- sim.trades.f$equity


if(testObject(trading.f.list)) rm(trading.f.list)
trading.f.list <- list()
for(i in seq(along=instr.p.all)){
  instr <- instr.p.all[i]
  trading.f.list[[instr]] <- run.trading.simulation(  sig.f.bt, price.df.f
                                                    , instr, c(instr,"XLF"), tc.spx
                                                    , debug=FALSE, silent=T)
  cat(instr,":",last(trading.f.list[[instr]]$equity),"\n")
}

fin.pnl <- numeric(0)
for(i in seq(along=instr.p.all))
  fin.pnl[i] <- last(trading.f.list[[i]]$equity)

## fin.portfolio.equity1 <- as.timeSeries(data.frame(dates.dbg,sim.trades.f.all$equity))
dates.dbg <- rownames(sig.mtx.f.bt)


## running this on all financials loses 16% in 600 days!
## ACAS : 98101.96 
## AFL : 108767.7 
## AIG : 107003.8 
## ALL : 100346.2 
## AOC : 94916.84 
## AXP : 98161.39 
## BAC : 97241.95 
## BEN : 102173.7 
## BK : 93119.79 
## C : 100213.7 
## CB : 96651.05 
## CINF : 98959.34 
## CMA : 101172.3 
## COF : 99483.34 
## DFS : 99356.69 
## FII : 98335.45 
## GS : 94857.8 
## HCBK : 101570.8 
## HIG : 101519 
## JPM : 98966.99 
## LM : 106065.3 
## LNC : 91956.99 
## LUK : 101797.9 
## MBI : 96803.55 
## MER : 97863.91 
## MMC : 102809.9 
## NTRS : 98631.32 
## PGR : 88882.02 
## SLM : 100105.5 
## STT : 111677.7 
## TMK : 98949.95 
## TROW : 99844.54 
## UNM : 94329.85 
## USB : 101388.1 
## WB : 104909.9 
## WFC : 99540.78 
## XL : 97140.68 
  
this.instr <- "PGR"
sig.mtx.dbg <- as.data.frame(sig.mtx.f.bt[,,this.instr])
sig.actions.dbg <- get.signals.actions(sig.mtx.dbg)
dates.dbg <- rownames(sig.mtx.dbg)
prices.dbg <- price.df.f[dates.dbg,c(this.instr,"XLF")]
ret.dbg <- cbind(ret.s,ret.e)[dates.dbg,c(this.instr,"XLF")]

par.save()
par(mfrow=c(2,1))
plot(sig.mtx.dbg$s,type='l')
draw.thresholds()
draw.signal.lines(sig.actions.dbg)
lines((trading.f.list[[this.instr]]$equity/100000)^2,col=6)
plot(prices.dbg[[this.instr]],type='l',ylim=c(20,90))
lines(prices.dbg$XLF,col=2)
par.restore()

plot(sig.mtx.dbg$beta,type='l')

p0.s <- prices.dbg[1,this.instr]
p0.e <- prices.dbg[1,"XLF"]
prices.from.ret <- data.frame( ret.to.prices(ret.dbg[[this.instr]],p0.s),ret.to.prices(ret.dbg[["XLF"]],p0.e),row.names=row.names(ret.dbg))
names(prices.from.ret) <- c(this.instr,"XLF")

plot(prices.dbg[[this.instr]],type='l',ylim=c(20,90))
lines(prices.from.ret[[this.instr]],col=2)
 
plot(prices.dbg[["XLF"]],type='l',ylim=c(20,30))
lines(prices.from.ret[["XLF"]],col=2)
## looks like cumulative errors arise in XLF, but how does that affect the trading?
instr <- this.instr
trading.dbg <- run.trading.simulation(  sig.f.bt, prices.from.ret
                                                  , instr, c(instr,"XLF"), tc.spx
                                                  , debug=FALSE, silent=T)
cat(instr,":",last(trading.dbg$equity),"\n")
## > PGR : 89310.97 # better, but only slightly

## now test (1,-beta) investment strategy with a holding period of n days:
## try this on synthetic data (variables set up in synthetic_trading.r)
## first, try on a priori market neutral returns:
ret.dbg <- data.frame(stk.ret.beta,etf.sim)
names(ret.dbg) <- c(this.instr,"XLF") ## but this is fake data
ret.dbg <- ret.dbg[rownames(sim.sig.mtx.1),]
mn.test <- cbind(ret.dbg,rep(const.beta,nrow(ret.dbg))); names(mn.test)[3] <- "beta"
mn.ret.1 <- rep(0,nrow(mn.test)); mn.ret.1 <- NA
for(i in 2:nrow(mn.test))
  mn.ret.1[i] <- sum(c(1,-mn.test[i,"beta"])*(mn.test[i,c(this.instr,"XLF")]))
## 0, as expected
ret.dbg <- data.frame(stk.ret.tot,etf.sim)
names(ret.dbg) <- c(this.instr,"XLF") ## but this is fake data
ret.dbg <- ret.dbg[rownames(sim.sig.mtx.1),]
mn.test <- cbind(ret.dbg,sim.sig.mtx.1$beta); names(mn.test)[3] <- "beta"
mn.ret.1 <- rep(0,nrow(mn.test)); mn.ret.1 <- NA
for(i in 2:nrow(mn.test))
  mn.ret.1[i] <- sum(c(1,-mn.test[i,"beta"])*(mn.test[i,c(this.instr,"XLF")]))
##  mean.var(mn.ret.1,na.rm=T) are close to zero, as expected
##         mean          var 
## 1.048131e-05 8.091061e-05 

## now do this on real data
this.instr <- "PGR"
sig.mtx.dbg <- as.data.frame(sig.mtx.f.bt[,,this.instr])
sig.actions.dbg <- get.signals.actions(sig.mtx.dbg)
dates.dbg <- rownames(sig.mtx.dbg)
prices.dbg <- price.df.f[dates.dbg,c(this.instr,"XLF")]
ret.dbg <- cbind(ret.s,ret.e)[dates.dbg,c(this.instr,"XLF")]
mn.test <- cbind(ret.dbg,sig.mtx.dbg$beta); names(mn.test)[3] <- "beta"


## mean.var(mn.ret.1,na.rm=T)  ## looks OK-ish
##         mean          var 
## 0.0004061637 0.0001221197

#sapply(1:20,function(x) mean(mn.returns(mn.test,this.instr,"XLF",holding.period=x),na.rm=T))
#mean.mn.ret.vs.holding.period <- .Last.value
#save(mean.mn.ret.vs.holding.period,file="SavedValues.RObj")
## .Last.value
##  [1] 0.0004061637 0.0007305319 0.0010728035 0.0014334767 0.0017592087
##  [6] 0.0021088965 0.0024279900 0.0027932853 0.0031852878 0.0036246463
## [11] 0.0040976294 0.0045896189 0.0050781089 0.0055941486 0.0061112211
## [16] 0.0067151329 0.0072953824 0.0078607890 0.0084405935 0.0090983408
## get monotonic increase -- slippage gets worse with time 

mn.ret.5 <- mn.returns(mn.test,this.instr,holding.period=5)
plot(mn.ret.5,type='l')
lines(-.05*as.numeric(sig.actions.dbg$bto),col=2)
lines(-.05*as.numeric(sig.actions.dbg$close.long),col=3)
lines(-0.07*as.numeric(position.signal(sig.actions.dbg$bto,sig.actions.dbg$close.long)),col=6)
lines(0.07*as.numeric(position.signal(sig.actions.dbg$sto,sig.actions.dbg$close.short)),col=8)

long.signal <- position.signal(sig.actions.dbg$bto,sig.actions.dbg$close.long)
short.signal <- position.signal(sig.actions.dbg$sto,sig.actions.dbg$close.short)

a.times.long <- action.times(sig.actions.dbg$bto,sig.actions.dbg$close.long)
## [1,]  82   43
## [2,] 232   69
## [3,] 320   25
## [4,] 376   33
## [5,] 461   27
mn.returns.periods(mn.test, this.instr, "XLF", a.times.long)
## [1] -0.0758454503 -0.0778199908  0.0604014638 -0.0334155644 -0.0004703328
a.times.short <- action.times(sig.actions.dbg$sto,sig.actions.dbg$close.short)
## [1,]  16    3
## [2,]  29    8
## [3,] 141   41
## [4,] 291   20
## [5,] 331    2
## [6,] 402    7
## [7,] 498   19
## [8,] 541   29
# mn.returns.periods(mn.test, this.instr, "XLF", a.times.short)
## [1] -0.0004606238 -0.0054781997  0.1452396587 -0.0365526399  0.0059381733
## [6]  0.0397362843 -0.0711789878  0.0374084250
## these have to be taken with a (-) sign, since this is shorting -- and index [3]
## is where the strategy gets hosed.

##finally, plot what's going on with this instrument
mn.ret.20 <- mn.returns(mn.test,this.instr,holding.period=20)
long.signal <- position.signal(sig.actions.dbg$bto,sig.actions.dbg$close.long)
short.signal <- position.signal(sig.actions.dbg$sto,sig.actions.dbg$close.short)
sig.returns <- get.signal.returns(mn.test,this.instr,"XLF",sig.actions.dbg)

plot(mn.ret.20,type='l')
lines(-.05*as.numeric(long.signal),col=2)
lines( .05*as.numeric(short.signal),col=3)
points(sig.returns$idx,sig.returns$ret)


## plots for print-outs:
par.save()
par(mfrow=c(3,1))
plot(mn.ret.20,type='l')
lines(-.05*as.numeric(long.signal),col=2)
lines( .05*as.numeric(short.signal),col=3)
points(sig.returns$idx,sig.returns$ret)
plot(sig.mtx.dbg$s,type='l')
draw.thresholds()
lines(-thresholds["sbo"]*as.numeric(long.signal),col=2)
lines( thresholds["sso"]*as.numeric(short.signal),col=3)
plot(sim.trades.f$equity,type='l')
tmp <- rbind(action.times(sig.actions.dbg$bto,sig.actions.dbg$close.long)
             ,action.times(sig.actions.dbg$sto,sig.actions.dbg$close.short))
abline(v=(tmp[,1]-tmp[,2]),lty=2)
abline(v=sig.returns$idx,lty=1)
par.restore()

x11()
par(mfrow=c(2,1))
tmp <- rbind(action.times(sig.actions.dbg$bto,sig.actions.dbg$close.long)
             ,action.times(sig.actions.dbg$sto,sig.actions.dbg$close.short))
plot(prices.dbg[[this.instr]],type='l')
abline(v=(tmp[,1]-tmp[,2]),lty=2)
abline(v=sig.returns$idx,lty=1)
plot(prices.dbg[["XLF"]],type='l')
abline(v=(tmp[,1]-tmp[,2]),lty=2)
abline(v=sig.returns$idx,lty=1)
par.restore()



mn.ret.10 <- mn.returns(mn.test,this.instr,holding.period=10)
mn.ret.30 <- mn.returns(mn.test,this.instr,holding.period=30)

## par(mfrow=c(4,1))
## plot(mn.ret.1,type='l')
## plot(mn.ret.5,type='l')
## plot(mn.ret.10,type='l')
## plot(mn.ret.30,type='l')
## par.restore()


## or, using ggplot:
plst <- list()
plst[[1]] <- qplot(1:length(mn.ret.1),y=mn.ret.1, geom="line", xlab="")
plst[[2]] <- qplot(1:length(mn.ret.5),y=mn.ret.5, geom="line", xlab="")
plst[[3]] <- qplot(1:length(mn.ret.10),y=mn.ret.10, geom="line", xlab="")
plst[[4]] <- qplot(1:length(mn.ret.30),y=mn.ret.30, geom="line", xlab="")
grid.newpage()
pushViewport(viewport(layout=grid.layout(4,1)))
vplayout<-function(x,y)viewport(layout.pos.row=x,layout.pos.col=y)
sapply(1:4,function(x) print(plst[[x]],vp=vplayout(x,1)))


## plot MN stuff for simulated data
ret.dbg.sim <- data.frame(stk.ret.tot,etf.sim)
names(ret.dbg.sim) <- c(this.instr,"XLF") ## but this is fake data
ret.dbg.sim <- ret.dbg.sim[rownames(sim.sig.mtx.1),]
mn.test.sim <- cbind(ret.dbg.sim,sim.sig.mtx.1$beta); names(mn.test.sim)[3] <- "beta"
mn.ret.sim.1 <- mn.returns(mn.test.sim,this.instr,holding.period=1)
mn.ret.sim.5 <- mn.returns(mn.test.sim,this.instr,holding.period=15)
mn.ret.sim.10 <- mn.returns(mn.test.sim,this.instr,holding.period=10)
mn.ret.sim.30 <- mn.returns(mn.test.sim,this.instr,holding.period=30)


## par(mfrow=c(4,1))
## plot(mn.ret.sim.1,type='l')
## plot(mn.ret.sim.5,type='l')
## plot(mn.ret.sim.10,type='l')
## plot(mn.ret.sim.30,type='l')
## par.restore()

x11()

plst <- list()
plst[[1]] <- qplot(1:length(mn.ret.sim.1),y=mn.ret.sim.1, geom="line", xlab="")
plst[[2]] <- qplot(1:length(mn.ret.sim.5),y=mn.ret.sim.5, geom="line", xlab="")
plst[[3]] <- qplot(1:length(mn.ret.sim.10),y=mn.ret.sim.10, geom="line", xlab="")
plst[[4]] <- qplot(1:length(mn.ret.sim.30),y=mn.ret.sim.30, geom="line", xlab="")
grid.newpage()
pushViewport(viewport(layout=grid.layout(4,1)))
vplayout<-function(x,y)viewport(layout.pos.row=x,layout.pos.col=y)
sapply(1:4,function(x) print(plst[[x]],vp=vplayout(x,1)))
