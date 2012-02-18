source("functions.r")
default.par <- par(no.readonly = TRUE)

tickers.classified <-
  sort.data.frame(get.classified.tickers("ticker_to_sec_etf.csv"), by=~TIC)
row.names(tickers.classified) <- tickers.classified$TIC

dates.vector <- get.dates.vec("spx_ret_mtx")
dates.vector.etf <- get.dates.vec("etf_ret_mtx")
stopifnot(all(dates.vector==dates.vector.etf))
dates.vector <- rev(dates.vector)

## get offset:
which(as.logical(match(dates.vector,20070501)))
## [1] 547
offset <- 547

ret.s <- get.stock.returns("spx_ret_mtx",M=252,offset=offset,na.pct.cutoff=0.01,file=TRUE)
ret.e <- get.etf.returns("etf_ret_mtx",M=252,offset=offset,file=TRUE)

ret.s.60 <- ret.s[1:60,]
ret.e.60 <- ret.e[1:60,]
stopifnot(all(row.names(ret.e.60)==row.names(ret.s.60)))

fit1 <- fit.stock(ret.s.60[,1,drop=F],ret.e.60)
as.numeric(fit1)

test.df <- ret.s.60[,1:2]


print.ar1.estimates <- function(r.fit){
  cat('order', r.fit$order, ', m =',r.fit$x.mean,', a[1] =',r.fit$ar,', sigma =',sqrt(r.fit$var.pred),'\n')
}


test.r <- as.data.frame(get.ou.series(test.df,ret.e.60))
test.ar1 <- fit.ar1(test.r)

## check to see that we get roughly the same results using various estimation methods:
invisible(lapply(fit.ar1(test.r,method="mle"),print.ar1.estimates))
invisible(lapply(fit.ar1(test.r,method="yw"),print.ar1.estimates))
invisible(lapply(fit.ar1(test.r,method="ols"),print.ar1.estimates))

get.s.score(fit.ar1(as.data.frame(get.ou.series(ret.s.60[,1:10],ret.e.60))))

## after failing to find sense in current numbers, try to replicate fig 7
## use updated ETF data; take Jan 06 to Dec 07 and get s-score of JPM vs XLF

which(as.logical(match(dates.vector,20071231)))
## [1] 378
offset <- 377


which(as.logical(match(dates.vector,20070601)))
## [1] 525
##offset <- 514

ret.s <- get.stock.returns("spx_ret_mtx",M=3*252,offset=offset,na.pct.cutoff=0.01,file=TRUE)
ret.e <- get.etf.returns("etf_ret_mtx",M=3*252,offset=offset,file=TRUE)
stopifnot(all(row.names(ret.e)==row.names(ret.s)))

ret.s.jpm <- ret.s["JPM"]
ret.e.jpm <- ret.e[tickers.classified["JPM",]$SEC_ETF]
ret.e.xlf <- ret.e["XLF"]

ret.s.test1 <- ret.s[c("JPM","XOM")]
ret.e.test1 <- ret.e


## generate signals
## first test in on jpm/xlf
dates.range <- row.names(ret.s.jpm)[1:20]
win <- 60
sig.list <- vector('list',length(dates.range))
for(i in seq(along=dates.range)){
  sig.list[[i]] <-
    get.signals(fit.ar1(  get.ou.series(ret.s.jpm[i:(i+win),,drop=F],ret.e.xlf[i:(i+win),,drop=F])
                        , method="yw"),subtract.average=F)}
names(sig.list) <- dates.range
  
## now test on test1 set
dates.range <- row.names(ret.s.test1)[1:20]
win <- 60
sig.list.test1 <- vector('list',length(dates.range))
for(i in seq(along=dates.range)){
  sig.list.test1[[i]] <-
    get.signals(fit.ar1(  get.ou.series(ret.s.test1[i:(i+win),,drop=F],ret.e.test1[i:(i+win),,drop=F])
                        , method="yw"),subtract.average=F)}
names(sig.list.test1) <- dates.range
  

sig.list.test2 <- stock.etf.signals(ret.s,ret.e,tickers.classified[c("JPM","XOM"),],num.days=20)

sig.list.test3 <- stock.etf.signals(ret.s,ret.e,tickers.classified,num.days=25)

sig.list.test4 <- stock.etf.signals(ret.s,ret.e,tickers.classified,num.days=25,compact.output=TRUE)


save.image()

save(sig.list.test3,file="sig.25d.RObj")
save(sig.list.test4,file="sig.25d.mtx.RObj")

test.names <- c("JPM","XOM")
test.list <- vector('list',length(test.names))
test.entry1 <- c(1,2.2,3.4)
test.entry2 <- c(2,1.1,4.4)
names(test.entry1) <- c("A","B","C")
names(test.entry2) <- c("A","B","C")
test.list[[1]] <- unname(test.entry1)
test.list[[2]] <- unname(test.entry2)
names(test.list) <- test.names

test.list.df <- data.frame(test.list)
test.list.mtx <- matrix(unlist(test.list),ncol=3,byrow=TRUE)

offset.2009 <- 124
offset.2008 <- 377
offset.2007 <- 628
offset.2006 <- 879
offset.2005 <- 1131
offset.2004 <- 1383
offset.2003 <- 1635


ret.s.big <- get.stock.returns("univ1_ret_mtx",M=9*252,offset=offset,na.pct.cutoff=0.01,file=TRUE)
ret.s.big.spx <- get.stock.returns("spx_ret_mtx",M=9*252,offset=offset.2009,na.pct.cutoff=0,file=TRUE)

test.ask <- get.stock.returns("univ1_ask_mtx",M=9*252,offset=offset.2009,file=TRUE)
test.bid <- get.stock.returns("univ1_bid_mtx",M=9*252,offset=offset.2009,file=TRUE)

##-------------------------------------------------------------
## Eigenportfolio stuff
