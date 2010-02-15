## Trading simulation function
## input: signals object, prices df
###source("TradingLoopCPP/TradingLoopCPP.r")

dyn.load("TradingLoopCPP/TradingLoop.so")
dyn.load("TradingLoopCPP/TradingLoopPCA.so")

## 20100213:
## instead of passing a list of {P,Q} instruments,
## now pass the Q-allocations matrix (NumDays x m*N)
## 
## in light of the new signal generation routines, the signals.struct is
## now assumed to be a (dates x sig_params x tickers) matrix
## (i.e. what we had before after a conversion function)
## ticker list is assumed to be contained in the dimension names
## matrix is assumed to be sorted chronologically along the dates dimension
## positions matrix is a matrix of instruments / pairs
## rows ("P") indicate instruments for which signals are generated
## cols ("Q") indicate intruments acting as pairs (beta factors)
## NB: q should always also include the underlying p
## enforce pq.factor.list row names and first column identical
##
## Inputs:
## - signals.struct
##    array of dims (NumDays x (signals params) x NumTickers)
##    dimnames (1,3) mandatory to indicate dates and tickers
## - prices
##    data frame or matrix; (dates x instruments); dimnames mandatory
## - instr.p
##    trading instruments for which Q's are regressors/factors
## - instr.q
##    "pair" instruments -- regressors/factors for "p" instruments.
##    these may include "p" instruments (if they are part of the factor
##    portfolio).  If they are not included, they will be appended to the
##    positions ledger automatically.
## - q.allocations.matrix
##    NB: this behaves differently on whether the PCA flag is set
##    If PCA is TRUE, q.allocations.matrix is a matrix of dims (NumDays x m*N)
##    where m is the number of factors and N is the number of q-pair
##    instruments.  Colnames attribute must contain names of instruments, and
##    will be checked for being a subsets of (instr.p U intr.q).
##    If PCA is FALSE, q.allocations.matrix is used in the old sense of
##    pq.factor list -- a data frame with fields TIC and SEC_ETF, wich rownames
##    identical to the TIC column
## - PCA
##    flag indicating PCA or single-factor approach
## - num.factors
## --- other stuff.........
run.trading.simulation.cpp <- function(  signals.struct, prices, instr.p, instr.q
                                       , q.allocations.matrix, num.factors
                                       , PCA=NULL
                                       , debug=FALSE, silent=FALSE
                                       , debug.name=instr.p[1]
                                       , init.cash=100000
                                       , pos.allocation="beta.neutral"){

  stopifnot(class(signals.struct)=="array")
  tickers <- dimnames(signals.struct)[[3]]
  dates <- dimnames(signals.struct)[[1]]
  instr.pq <- union(instr.p,instr.q)
  stopifnot(all(instr.p %in% tickers))
  tickers.instrp.idx <- match(instr.p,tickers)
  ## instr.p, tickers.instrp.idx form an aligned key/value set
  ## i.e. 2nd el. of t.i.i array is the index of 2nd el. of instr.p array in
  ## tickers array
  stopifnot(all(dates %in% row.names(prices))) ##prices dates range
                                        # must include all signals+more
  stopifnot(all(instr.pq %in% colnames(prices)))
  stopifnot(!any(duplicated(colnames(prices))))

  if (!PCA) {
    pq.factor.list <- q.allocations.matrix
    stopifnot(all(row.names(pq.factor.list)==pq.factor.list$TIC))
    stopifnot(all(instr.p %in% pq.factor.list$TIC))
    pq.factor.list <- pq.factor.list[instr.p, ,drop=F] ##make sure it's aligned with instr.p
    stopifnot(!any(is.na(pq.factor.list$SEC_ETF))) ##sanity check against NAs
    q.allocations.matrix <- pq.factor.list
    prices.qalloc.idx <- NULL
    qalloc.names <- NULL
  } else {
    qalloc.names <-
       colnames(q.allocations.matrix)[1:(ncol(q.allocations.matrix)/num.factors)]
    stopifnot(all(qalloc.names %in% colnames(prices)))
  }
  instr.pqa <- union(instr.pq,qalloc.names)
  prices <- prices[, instr.pqa] ## align instr.q (and also positions) and prices columns
  prices.instrpq.idx <- match(instr.pq,colnames(prices))
   ## instr.q, price.instrpq.idx form an aligned key/value set
  if(PCA){
    prices.qalloc.idx <- match(qalloc.names,colnames(prices))
    stopifnot(all(qalloc.names %in% colnames(prices)))
  }

  
  prices <- prices[dates,] ## align the data frames
  stopifnot(all(row.names(prices)==dates))
  positions <- matrix(0,length(instr.p),length(instr.pqa))
  colnames(positions) <- instr.pqa;  row.names(positions) <- instr.p
  positions.p <- rep(0,length(instr.p))

  ## get the number of signal array entries
  sig.arr.len <- dim(signals.struct)[2]
  num.stks <- length(tickers)
  sig.mtx.2d <- matrix(signals.struct,nrow=length(dates),ncol=num.stks*sig.arr.len,byrow=F)
  sig.actions <- matrix(as.integer(sig.mtx.2d[,seq(1,num.stks*sig.arr.len,by=sig.arr.len)]),nrow=length(dates),ncol=num.stks,byrow=F)
  ## sig.actions[1,1] <- NA
  ## sig.mtx.2d[1,2] <- NA
  stopifnot(all(colnames(positions)==colnames(prices))) ##since same corr. map used
  if(!PCA){ cpp.function <- "backtest_loop" } else { cpp.function <- "backtest_loop_pca" }
  ## browser()
  prices[is.na(prices) || (prices < 0)] <- 0
  .Call(cpp.function
        , instr.p, tickers.instrp.idx, as.logical(PCA)
        , instr.pq, prices.instrpq.idx, dates, num.factors
        , as.matrix(q.allocations.matrix), prices.qalloc.idx
        , as.matrix(prices), as.matrix(positions), positions.p
        , as.matrix(sig.mtx.2d), as.matrix(sig.actions)
        , list(debug=debug, debug.name=debug.name, silent=silent
               , pos.allocation=pos.allocation, init.cash=init.cash))
  
}
