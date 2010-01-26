source("functions.r")

tickers.classified <-
  sort.data.frame(get.classified.tickers("ticker_to_sec_etf.csv"), by=~TIC)
row.names(tickers.classified) <- tickers.classified$TIC

tc.subset <- tickers.classified[c("JPM","AA","MS","AAPL","MSFT","INTC"),]

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

num.days <- (offset.2004-offset.2005)+252

ret.s <- get.stock.returns("spx_ret_mtx",M=5*252,offset=offset.2005,na.pct.cutoff=0.0,file=TRUE)
ret.e <- get.etf.returns("etf_ret_mtx",M=5*252,offset=offset.2005,file=TRUE)
stopifnot(all(row.names(ret.e)==row.names(ret.s)))


sig.list.dbg1 <- stock.etf.signals(ret.s,ret.e,tc.subset,num.days=num.days,compact.output=TRUE)

save(sig.list.dbg1,file="sig.dbg1.RObj")



