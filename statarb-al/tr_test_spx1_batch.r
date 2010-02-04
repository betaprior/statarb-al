## test pair trading on financial stocks vs xlf
setwd("/home/leo/projects/finance/research/statarb-al/")
source("functions.r")
source("f_trading_sim.r") ## for the trading simulation

## ==== functions ===
## process trading signals:

## function: position.signal
position.signal <- function(sig.open,sig.close){
  stopifnot(length(sig.open)==length(sig.close) && length(sig.open)>=2)
  pos <- logical(length(sig.open))
  pos[1] <- sig.open[1]
  for(i in 2:length(pos))
    pos[i] <- ((sig.open[i] || pos[i-1]) && !sig.close[i])
  pos
}
interval.lengths <- function(position.signal){
  rle.lengths <- rle(position.signal)$lengths
  change.idxs <- cumsum(rle.lengths)
  ints <- integer(length(position.signal))
  ints[change.idxs] <- rle.lengths
  ints
}
## gives indices and durations of the last entry before trailing edge of the signal
## state choices: "on", "off", "all"
action.times <- function(sig.open,sig.close,state="on"){
  pos <- position.signal(sig.open,sig.close)
  ints <- interval.lengths(pos)
  a.times <- as.matrix(cbind(1:length(pos),ints,pos))
  a.times <- a.times[a.times[,2]!=0,]
  if(a.times[nrow(a.times),1]==length(sig.open))
    a.times <- a.times[-nrow(a.times),]
  if(state=="all"){
    a.times
  }else if(state=="on"){
    a.times[a.times[,3]==1,1:2]
  }else{
    a.times[a.times[,3]==0,1:2]
  }
}

get.signal.returns <- function(s.e.beta,this.instr,pair.instr="XLF",sig.actions){
  long.signal <- position.signal(sig.actions$bto,sig.actions$close.long)
  short.signal <- position.signal(sig.actions$sto,sig.actions$close.short)
  a.times.long <- action.times(sig.actions$bto,sig.actions$close.long)
  a.times.short <- action.times(sig.actions$sto,sig.actions$close.short)
  long.ret <- mn.returns.periods(s.e.beta, this.instr, pair.instr, a.times.long)
  short.ret <- -mn.returns.periods(s.e.beta, this.instr, pair.instr, a.times.short)
  data.frame(  idx=c(a.times.long[,1,drop=T],a.times.short[,1,drop=T])
             , ret=c(long.ret,short.ret))  }

## test whether returns accurately reproduce prices
ret.to.prices <- function(ret,p0){
  x <- rep(0,length(ret))
  x[1] <- p0
  for(i in seq(along=ret)[c(-1)])
    x[i] <- x[i-1]*(ret[i]+1)
  x
}


## Compute actual returns of a market-neutral portfolio:
## assumptions: estimates/trades are all done at EOD;
## in this version today can be included in both estimate generation and trading
## (think of it as trading right at the close)
## In this function, get beta from the signals, and compute the returns on
## (1,-beta) portfolio (holding.period) days later
## if one.shot is false, use it for all range of dates in mn.test data frame
## one.shot/one.idx used to do computation once for a day spec'd by one.idx
## mn.test is a data frame that contains {instr, pair, beta} columns
## _in that order(!)_.  This is enforced by looking at df names.
mn.returns <- function(mn.test,this.instr,pair.instr="XLF",holding.period=1,one.shot=FALSE,one.idx=NA){
  stopifnot(all(names(mn.test)[1:2]==c(this.instr,pair.instr)))
  if(!one.shot){
    mn.ret.1 <- rep(0,nrow(mn.test)); mn.ret.1[1:holding.period] <- NA
    dates.seq <- holding.period:nrow(mn.test)
  }else{
    stopifnot(one.idx >= holding.period);  mn.ret.1 <- 0
    dates.seq <- one.idx
  }
  for(i in dates.seq){
    period.returns <-
      apply(1+mn.test[i:(i-holding.period+1),c(this.instr,pair.instr)],2,function(x) exp(sum(log(x))))-c(1,1) ## exp(sum(log(x))) is poor man's product(x)
    this.ret <- sum(c(1,-mn.test[i-holding.period,"beta"])*period.returns)
    if(!one.shot){ mn.ret.1[i] <- this.ret } else{ mn.ret.1 <- this.ret }
  }
  mn.ret.1
}
## test: > mn.returns(mn.test,this.instr,holding.period=5,one.shot=TRUE,one.idx=5)

## Compute actual returns of a market-neutral portfolio on certain dates looking back a set number of days
## periods list is a matrix with rows giving the (date_index, lookback_period) to compute returns
## mn.test contains {instr, pair, beta} columns, in that order, w/ appropriate names
mn.returns.periods <- function(mn.test,this.instr,pair.instr="XLF",periods.list){
  apply(periods.list,1,function(x) mn.returns(mn.test,this.instr,pair.instr,holding.period=x[2],one.shot=TRUE,one.idx=x[1])) }
## test:   test.periods <- rbind(c(5,5),c(6,5),c(10,3))
## mn.returns.periods(mn.test,this.instr,"XLF",test.periods)

## === ~end functions, begin data loading


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
  sig.f <- stock.etf.signals(ret.s,ret.e,tc.spx,num.days=N-est.win+1,compact.output=T)
  save(sig.f,file=save.sig.filename)
}else{
  cat("note: not generating signals by default\n")
}
##signals$sig.dates list will be reverse-chronological and might contain mor dates 
##than what we want for a trading simulation.  The dates for backtesting are determined
##by signals$sig.dates (prices data frame is subset accordingly)

