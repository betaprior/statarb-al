## this is based on trading_backtest_dbg3.r
## ans possibly synthetic_trading.r
require("ggplot2")
require("timeSeries")
require("xts")
source("tr_test_financials_1_batch.r")


##signals$sig.dates list will be reverse-chronological and might contain mor dates 
##than what we want for a trading simulation.  The dates for backtesting are determined
##by signals$sig.dates (prices data frame is subset accordingly)

load("sig.financials2.RObj")
sig.mtx.f <- get.signals.mtx(sig.f)
##sig.mtx usage: > head(sig.mtx.f[,,"JPM"])
##sig.actions.f <- get.signals.actions(sig.mtx.f[,,"JPM"])
##sig.mtx.dbg <- as.data.frame(sig.mtx.f[,,this.instr])
## (w/o casting to data frame can't subset by column names)



this.instr <- "JPM"
num.days.bt <- 502
sig.f.bt <- list(sig.dates=sig.f$sig.dates[1:num.days.bt],tickers=sig.f$tickers)
sig.mtx.f.bt <- get.signals.mtx(sig.f.bt)
instr <- this.instr
sim.trades.f <- run.trading.simulation(  sig.f.bt, price.df.f
                                       , instr, c(instr,"XLF"), tc.xlf
                                       , debug=FALSE, silent=FALSE)

jpm.s.signal <- as.timeSeries(sig.mtx.dbg[,"s",drop=F])
plot.xts(as.xts(-jpm.s.signal))


sig.mtx.dbg <- as.data.frame(sig.mtx.f.bt[,,this.instr])
sig.actions.dbg <- get.signals.actions(sig.mtx.dbg)
dates.dbg <- rownames(sig.mtx.dbg)
prices.dbg <- price.df.f[dates.dbg,c(this.instr,"XLF")]
ret.dbg <- cbind(ret.s,ret.e)[dates.dbg,c(this.instr,"XLF")]
mn.test <- cbind(ret.dbg,sig.mtx.dbg$beta); names(mn.test)[3] <- "beta"


mn.ret.20 <- mn.returns(mn.test,this.instr,holding.period=20)
long.signal <- position.signal(sig.actions.dbg$bto,sig.actions.dbg$close.long)
short.signal <- position.signal(sig.actions.dbg$sto,sig.actions.dbg$close.short)
sig.returns <- get.signal.returns(mn.test,this.instr,"XLF",sig.actions.dbg)


mn.ret.1 <- mn.returns(mn.test,this.instr,holding.period=1)
mn.ret.5 <- mn.returns(mn.test,this.instr,holding.period=5)
mn.ret.10 <- mn.returns(mn.test,this.instr,holding.period=10)
mn.ret.30 <- mn.returns(mn.test,this.instr,holding.period=30)


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



x11()
plst <- list()
plst[[1]] <- qplot(1:length(mn.ret.1),y=mn.ret.1, geom="line", xlab="")
plst[[2]] <- qplot(1:length(mn.ret.5),y=mn.ret.5, geom="line", xlab="")
plst[[3]] <- qplot(1:length(mn.ret.10),y=mn.ret.10, geom="line", xlab="")
plst[[4]] <- qplot(1:length(mn.ret.30),y=mn.ret.30, geom="line", xlab="")
grid.newpage()
pushViewport(viewport(layout=grid.layout(4,1)))
vplayout<-function(x,y)viewport(layout.pos.row=x,layout.pos.col=y)
sapply(1:4,function(x) print(plst[[x]],vp=vplayout(x,1)))

