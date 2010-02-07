## test pair trading on financial stocks vs xlf
setwd("/home/leo/projects/finance/research/statarb-al/")
source("functions.r")
source("f_trading_sim.r") ## for the trading simulation
source("tr_signals_processing_fns.r")  ## for (at least) the following:
       ## position.signal, interval.lengths, action.times, get.signal.returns
       ## ret.to.prices, mn.returns (actual returns of b-neutral portf.), mn.returns.periods


tickers.classified <-
  sort.data.frame(get.classified.tickers("ticker_to_sec_etf.csv"), by=~TIC)
row.names(tickers.classified) <- tickers.classified$TIC

tc.xlf <- subset(tickers.classified,SEC_ETF=="XLF")


dates.vector <- get.dates.vec("spx_ret_mtx")
dates.vector.etf <- get.dates.vec("etf_ret_mtx")
stopifnot(all(dates.vector==dates.vector.etf))
dates.vector <- rev(dates.vector)


## nb: dates.vector is reverse-sorted wrt the original file 
offset <- list()
offset[["2009"]] <- which(as.logical(match(dates.vector,20090102)))
offset[["2008"]] <- which(as.logical(match(dates.vector,20080102)))
offset[["2007"]] <- which(as.logical(match(dates.vector,20070103)))
offset[["2006"]] <- which(as.logical(match(dates.vector,20060103)))
offset[["2005"]] <- which(as.logical(match(dates.vector,20050103)))
offset[["2004"]] <- which(as.logical(match(dates.vector,20040102)))
offset[["2003"]] <- which(as.logical(match(dates.vector,20030102)))
this.offset <- offset[["2005"]]
num.days <- 252*4
ret.s <- get.stock.returns("spx_ret_mtx",M=5*252,offset=this.offset,na.pct.cutoff=0.0,file=TRUE)
ret.e <- get.etf.returns("etf_ret_mtx",M=5*252,offset=this.offset,file=TRUE)
stopifnot(all(row.names(ret.e)==row.names(ret.s)))

## limit the ticker DB to the entries that we have in the price matrix
tc.xlf <- subset(tc.xlf,TIC %in% intersect(tc.xlf$TIC,names(ret.s)))

ret.s.fin <- ret.s[,tc.xlf$TIC]
N <- nrow(ret.s.fin)
est.win <- 60
##sig.f <- stock.etf.signals(ret.s.fin,ret.e,tc.xlf,num.days=N-est.win+1,compact.output=T)
## save(sig.f,file="sig.financials1.RObj")

##signals$sig.dates list will be reverse-chronological and might contain mor dates 
##than what we want for a trading simulation.  The dates for backtesting are determined
##by signals$sig.dates (prices data frame is subset accordingly)

