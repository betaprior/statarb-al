## Trading simulation function
## input: signals object, prices df
###source("TradingLoopCPP/TradingLoopCPP.r")

dyn.load("TradingLoopCPP/TradingLoop.so")

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
run.trading.simulation.cpp <- function(  signals.struct, prices
                                       , instr.p, instr.q, pq.factor.list
                                       , debug=FALSE, silent=FALSE
                                       , debug.name=instr.p[1]
                                       , init.cash=100000
                                       , pos.allocation="beta.neutral"){

  stopifnot(class(signals.struct)=="array")
  tickers <- dimnames(signals.struct)[[3]]
  dates <- dimnames(signals.struct)[[1]]

  stopifnot(all(instr.p %in% tickers))
  tickers.instrp.idx <- match(instr.p,tickers)
   ## instr.p, tickers.instrp.idx form an aligned key/value set
  stopifnot(all(dates %in% row.names(prices))) ##prices dates range
                                        # must include all signals+more
  stopifnot(all(instr.p %in% instr.q))
  stopifnot(all(instr.q %in% names(prices)))
  stopifnot(!any(duplicated(names(prices))))
  prices <- prices[, instr.q] ## align instr.q (and also positions) and prices columns
  prices.instrpq.idx <- match(instr.q,names(prices))
   ## instr.q, price.instrpq.idx form an aligned key/value set

  stopifnot(all(row.names(pq.factor.list)==pq.factor.list$TIC))
  stopifnot(all(instr.p %in% pq.factor.list$TIC))
  pq.factor.list <- pq.factor.list[instr.p, ,drop=F] ##make sure it's aligned with instr.p
  stopifnot(!any(is.na(pq.factor.list$SEC_ETF))) ##sanity check against NAs
  
  prices <- prices[dates,] ## align the data frames
  stopifnot(all(row.names(prices)==dates))
  positions <-as.data.frame(matrix(0,length(instr.p),length(instr.q)))
  names(positions) <- instr.q;  row.names(positions) <- instr.p


  ## get the number of signal array entries
  sig.arr.len <- dim(signals.struct)[2]
  num.stks <- length(tickers)
  sig.mtx.2d <- matrix(signals.struct,nrow=length(dates),ncol=num.stks*sig.arr.len,byrow=F)
  sig.actions <- matrix(as.integer(sig.mtx.2d[,seq(1,num.stks*sig.arr.len,by=sig.arr.len)]),nrow=length(dates),ncol=num.stks,byrow=F)
  ## sig.actions[1,1] <- NA
  ## sig.mtx.2d[1,2] <- NA
  .Call("backtest_loop",instr.p, tickers.instrp.idx, instr.q, prices.instrpq.idx
        , dates, as.matrix(pq.factor.list), as.matrix(prices)
        , as.matrix(positions), as.matrix(sig.mtx.2d), as.matrix(sig.actions)
        , list(debug=debug, debug.name=debug.name, silent=silent
               , pos.allocation=pos.allocation, init.cash=init.cash))
  
}
