## test pair trading on financial stocks vs xlf
setwd("/home/leo/projects/finance/research/statarb-al/")
require("Revobase")
require("foreach")
require("doMC")
registerDoMC()
source("functions.r")  ##this also sources f_signals_gen.r
source("f_trading_sim.r") ## for the trading simulation
source("f_trading_sim_cpp.r") ## for the trading simulation
source("tr_signals_processing_fns.r")  ## for (at least) the following:
       ## position.signal, interval.lengths, action.times, get.signal.returns
       ## ret.to.prices, mn.returns (actual returns of b-neutral portf.), mn.returns.periods

## if running interactively, set the default returns matrix here
ret.mtx.file <- "univ1_ret_mtx"

## --- batch mode argument processing
## arguments: -saveSigFile [FALSE] -retMtxFilename [ret.mtx.file] -filename [sig.file.RObj'
##            -Rprof [FALSE] -offsetYear [2009] -yearsBack [7 for 2009, 8 for 2008, etc]
##            -subtractAvg [TRUE for more than 20 instr]
save.sig.file <- as.logical(getCmdArgs("-saveSigFile"))
if(is.na(save.sig.file)) save.sig.file <- FALSE
ret.mtx.filename <- as.character(getCmdArgs("-retMtxFilename"))
if(is.na(ret.mtx.filename))
  if(testObject(ret.mtx.file) && file.exists(ret.mtx.file)){
    ret.mtx.filename <- ret.mtx.file
  } else { stop("ret.mtx.file variable is undefined or return matrix file does not exists") }
save.sig.filename <- as.character(getCmdArgs("-filename"))
if(is.na(save.sig.filename)) save.sig.filename <- "sig.file.RObj"
profiler.on <- as.logical(getCmdArgs("-Rprof"))
if(is.na(profiler.on)) { profiler.on <- FALSE }
profiler.filename <- paste(save.sig.filename,".Rprof",sep="")
## --- further arguments processed after offset definitions
## (-yearsBack and -offsetYear
etf.ret.mtx.filename <- "etf_ret_mtx"
tic.classified.filename <- "ticker_to_sec_etf.csv"

## -----
tickers.classified <-
  sort.data.frame(get.classified.tickers(tic.classified.filename), by=~TIC)
row.names(tickers.classified) <- tickers.classified$TIC



dates.vector <- get.dates.vec(ret.mtx.filename)
dates.vector.etf <- get.dates.vec(etf.ret.mtx.filename)
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
ret.s <- get.stock.returns(ret.mtx.filename,M=(yrs.bk+1)*252,offset=this.offset,na.pct.cutoff=0.0,file=TRUE)
ret.e <- get.etf.returns(etf.ret.mtx.filename,M=(yrs.bk+1)*252,offset=this.offset,file=TRUE)
stopifnot(all(row.names(ret.e)==row.names(ret.s)))

## limit the ticker DB to the entries that we have in the price matrix
tc.subset <- subset(tickers.classified,TIC %in% names(ret.s))

subtract.average <- as.logical(getCmdArgs("-subtractAvg"))
if(is.na(subtract.average)){ 
  if(length(names(ret.s)) > 20){ subtract.average = TRUE } else { subtract.average = FALSE }  }

#### signal generation -----------------------
if(profiler.on){ cat("Profiler output:",profiler.filename,"\n"); Rprof(profiler.filename) }

cat("getMKLthreads returns ",getMKLthreads(),"\n")
cat("getDoParWorkers returns ",getDoParWorkers(),"\n")

N <- nrow(ret.s)
est.win <- 60
if(save.sig.file){
  sig.f <- stock.etf.signals(ret.s,ret.e,tc.subset,num.days=N-est.win+1,compact.output=T,subtract.average=subtract.average)
  save(sig.f,file=save.sig.filename)
}else{
  cat("note: not generating signals by default\n")
}
if(profiler.on){ Rprof(NULL) }

##signals$sig.dates list will be reverse-chronological and might contain mor dates 
##than what we want for a trading simulation.  The dates for backtesting are determined
##by signals$sig.dates (prices data frame is subset accordingly)

