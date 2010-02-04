source("tr_test_financials_1_batch.r")
require("timeSeries")
require("xts")

load("sig.financials2.RObj")
sig.mtx.f <- get.signals.mtx(sig.f)

instr.p.all <- intersect(sig.f$tickers, tc.xlf$TIC) ## make sure all tickers are classified
instr.q.all <- names(ret.e)
load("univ1.mid.price.RObj") # loads univ1.master.price
## current issue: duplcate names in the price dataframe
dups <- names(univ1.master.price)[duplicated(names(univ1.master.price))]
## find duplicated names in price file:
# c(instr.p.all,instr.q.all)[which(c(instr.p.all,instr.q.all) %in% dups)]
## don't deal with this for the time being
price.df.f <- univ1.master.price[,c(instr.p.all,"XLF")]


this.instr <- "JPM"
sig.mtx.dbg <- as.data.frame(sig.mtx.f[,,this.instr])
dates.dbg <- rownames(sig.mtx.dbg)
dates.dbg <- dates.dbg[as.numeric(dates.dbg) >= 20060101 &
                       as.numeric(dates.dbg) < 20080101]
sig.actions.dbg <- get.signals.actions(sig.mtx.dbg)
prices.dbg <- price.df.f[dates.dbg,c(this.instr,"XLF")]

## returns data frame from source(...batch...) might not have the correct
## dates
this.offset <- offset[["2008"]]
ret.s <- get.stock.returns("spx_ret_mtx",M=5*252,offset=this.offset,na.pct.cutoff=0.0,file=TRUE)
ret.e <- get.etf.returns("etf_ret_mtx",M=5*252,offset=this.offset,file=TRUE)
stopifnot(all(row.names(ret.e)==row.names(ret.s)))
ret.dbg <- cbind(ret.s,ret.e)[dates.dbg,c(this.instr,"XLF")]

sig.mtx.dbg <- sig.mtx.dbg[dates.dbg,]
s.jpm.inv.ts <- as.timeSeries(-sig.mtx.dbg[,"s",drop=F])

 plot.xts(as.xts(s.jpm.inv.ts))

window(s.jpm.inv.ts,"2007-06-27","2007-07-03")
## 2007-06-27 0.8886287
## 2007-06-28 1.0346759
## 2007-06-29 1.2050198
## 2007-07-02 0.8626147
## 2007-07-03 0.6329363

## Now try to reproduce the signal value at the point 20070629 manually to see
## if there still is a sign change
source("time_series_functions.r")
source("ou_sde_sim.r")

which(dates.dbg=="20070629")
##> 375
dates.offset <- 375
dates.window <- dates.dbg[(dates.offset-60+1):(dates.offset)]
## depending on whether you include today in the signal value the window might
## have to be shifted back by a day
ret.window <- ret.dbg[dates.window,]
ret.fit <- lm(JPM ~ XLF, data=ret.window)
ret.fit.beta <- ret.fit$coef["XLF"]
## how does that compare to what we have recorded?
ret.fit.beta
tail(sig.mtx.dbg[dates.window,"beta",drop=F])
## matches perfectly
resid.int <- cumsum(ret.fit$residuals) ##integral of residuals, according to
                                        #model an AR(1) process
ret.fit <- arima(resid.int,order=c(1,0,0))
ret.fit <- fit.ar1.series(resid.int,method="yw")
fit.ar.params <- unlist(ar.params.from.fit(ret.fit))
fit.m <- fit.ar.params["a"]/(1-fit.ar.params["b"])
fit.sigeq <- sqrt(fit.ar.params["varz"]/(1-fit.ar.params["b"]^2))
## this is the s-signal:
-unname(fit.m/fit.sigeq)
