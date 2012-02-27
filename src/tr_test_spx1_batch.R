## test pair trading on financial stocks vs xlf
setwd("/home/leo/projects/finance/research/statarb-al/src/")
source("functions.R")
source("f_trading_sim.R") ## for the trading simulation
source("f_trading_sim_cpp.R") ## for the trading simulation
source("tr_signals_processing_fns.R")  ## for (at least) the following:
       ## position.signal, interval.lengths, action.times, get.signal.returns
       ## ret.to.prices, mn.returns (actual returns of b-neutral portf.), mn.returns.periods


tickers.classified <-
  sort.data.frame(get.classified.tickers("ticker_to_sec_etf.csv"), by=~TIC)
row.names(tickers.classified) <- tickers.classified$TIC



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
offset.arg <- as.character(getCmdArgs("-offsetYear"))
if(is.na(offset.arg)){ offset.arg <- "2009" }else{ cat("using",offset.arg,"offset\n") }
this.offset <- offset[[offset.arg]]
yrs.bk <- as.numeric(getCmdArgs("-yearsBack"))
if(is.na(yrs.bk)){ yrs.bk <- as.numeric(offset.arg)-2002 ## want this to be 7 for 2009
                   }else{ cat("going",yrs.bk,"years back\n") }
num.days <- 252*yrs.bk+30
ret.s <- get.stock.returns("spx_ret_mtx",M=(yrs.bk+1)*252,offset=this.offset,na.pct.cutoff=0.0,file=TRUE)
ret.e <- get.etf.returns("etf_ret_mtx",M=(yrs.bk+1)*252,offset=this.offset,file=TRUE)
stopifnot(all(row.names(ret.e)==row.names(ret.s)))

## limit the ticker DB to the entries that we have in the price matrix
tc.spx <- subset(tickers.classified,TIC %in% names(ret.s))

#### signal generation -----------------------
## for batch mode:
save.sig.file <- as.logical(getCmdArgs("-saveSigFile"))
if(is.na(save.sig.file)) save.sig.file <- FALSE
save.sig.filename <- as.character(getCmdArgs("-filename"))
if(is.na(save.sig.filename)) save.sig.filename <- "sig.file.RObj"

N <- nrow(ret.s)
est.win <- 60
if(save.sig.file){
  sig.f <- stock.etf.signals(ret.s,ret.e,tc.spx,num.days=N-est.win+1,compact.output=T,flipsign=F)
  save(sig.f,file=save.sig.filename)
}else{
  cat("note: not generating signals by default\n")
}
##signals$sig.dates list will be reverse-chronological and might contain mor dates 
##than what we want for a trading simulation.  The dates for backtesting are determined
##by signals$sig.dates (prices data frame is subset accordingly)

