source("functions.r")

tickers.classified <-
  sort.data.frame(get.classified.tickers("ticker_to_sec_etf.csv"), by=~TIC)
row.names(tickers.classified) <- tickers.classified$TIC

dates.vector <- get.dates.vec("spx_ret_mtx")
dates.vector.etf <- get.dates.vec("etf_ret_mtx")
stopifnot(all(dates.vector==dates.vector.etf))
dates.vector <- rev(dates.vector)

offset.2009 <- 124 #jan 09, jan 08, etc
offset.2008 <- 377
offset.2007 <- 628
offset.2006 <- 879
offset.2005 <- 1131
offset.2004 <- 1383
offset.2003 <- 1635

num.days <- 50
offset.2004 <- offset.2004+50

ret.s <- get.stock.returns("spx_ret_mtx",M=4*252,offset=offset.2004,na.pct.cutoff=0.0,file=TRUE)
ret.e <- get.etf.returns("etf_ret_mtx",M=4*252,offset=offset.2004,file=TRUE)
stopifnot(all(row.names(ret.e)==row.names(ret.s)))

sig.list.03.04 <- stock.etf.signals(ret.s,ret.e,tickers.classified,num.days=num.days,compact.output=TRUE)

save(sig.list.03.04,file="sig.0304.RObj")

