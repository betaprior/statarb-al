if (!exists("statarb.al.proj")) stop("Need project metadata file to proceed")
source.files <- c("functions.R",
                  "f_trading_sim.R", ## for the trading simulation
                  "f_trading_sim_cpp.R", ## for the trading simulation
                  "tr_signals_processing_fns.R"  ## for (at least) the following:
                  ## position.signal, interval.lengths, action.times, get.signal.returns
                  ## ret.to.prices, mn.returns (actual returns of b-neutral portf.), mn.returns.periods
                  )
source(paste(statarb.al.proj$src.path, source.files, ""))
setwd(statarb.al.proj$workspace.path)

## require("Revobase")
require("foreach")
require("doMC")
registerDoMC()

filenames.default <-
  list(stock.ret.mtx="univ1_ret_mtx",
       etf.ret.mtx="etf_ret_mtx",
       tic.classified="ticker_to_sec_etf.csv",
       save.sig="sig.file.RObj")
                          
filenames <- filenames.default

## specify the etf list here
etf.list <- c("HHH","IYR","IYT","OIH","RKH","RTH"
                                ,"SMH","UTH","XLE","XLF","XLI","XLK","XLP","XLV","XLY","SPY","QQQQ")

cmdargs.default <-
  list(`-saveSigFile`=list(
         val=FALSE,
         class="logical"),
       `-retMtxFilename`=list(
         val=filenames.default$stock.ret.mtx,
         class="character"),
       `-filename`=list(
         val=filenames.default$save.sig,
         class="character"),
       `-Rprof`=list(
         val=FALSE,
         class="logical"),
       `-offsetYear`=list(
         val="2009",
         class="character"),
       `-yearsBack`=list(
         val=NA, # depends on offset.arg
         class="numeric"),
       `-subtractAvg`=list(
         val=TRUE,
         class="logical"))


## --- batch mode argument processing
## arguments: -saveSigFile [FALSE] -retMtxFilename [ret.mtx.file] -filename [sig.file.RObj'
##            -Rprof [FALSE] -offsetYear [2009] -yearsBack [7 for 2009, 8 for 2008, etc]
##            -subtractAvg [TRUE for more than 20 instr]


save.sig.file <- set.from.cmdargs(cmdargs.default, "-saveSigFile")
filenames$stock.ret.mtx <- set.from.cmdargs(cmdargs.default, "-retMtxFilename")
if (! file.exists(filenames$stock.ret.mtx))
  stop(paste("Cannot find file", filenames$stock.ret.mtx))
filenames$save.sig <- set.from.cmdargs(cmdargs.default, "-filename")
profiler.on <- set.from.cmdargs(cmdargs.default, "-Rprof")
offset.arg <- set.from.cmdargs(cmdargs.default, "-offsetYear")
cat("using",offset.arg,"as offset\n")
yrs.bk <- set.from.cmdargs(cmdargs.default, "-yearsBack")
if (is.na(yrs.bk)) yrs.bk <- as.numeric(offset.arg)-2002 ## want this to be 7 for 2009
cat("going",yrs.bk,"years back\n")
subtract.average <- set.from.cmdargs(cmdargs.default, "-subtractAvg")

filenames$profiler <- paste(filenames$save.sig, ".Rprof", sep="")

tickers.classified <-
  sort.data.frame(get.classified.tickers(filenames$tic.classified), by=~TIC)
row.names(tickers.classified) <- tickers.classified$TIC

dates.vector <- get.dates.vec(filenames$stock.ret.mtx)
dates.vector.etf <- get.dates.vec(filenames$etf.ret.mtx)
stopifnot(all(dates.vector==dates.vector.etf))
dates.vector <- rev(dates.vector)

## nb: dates.vector is reverse-sorted wrt the original file 
working.years <- seq(2009, 2003, by=-1)
offset <- as.list(get.year.offsets(dates.vector, working.years))
names(offset) <- as.character(working.years)

this.offset <- offset[[offset.arg]]

num.days <- 252 * yrs.bk + 30
get.returns <- function(type=c("stock","etf")) {
  type <- match.arg(type)
  filter.fn = switch(type,
    stock = na.pct.cutoff.filter(0.0),
    etf = select.tickers.filter(etf.list))
  get.stock.returns(ret.mtx.filename
                           , M=(yrs.bk + 1) * 252
                           , offset=this.offset
                           , file=TRUE
                           , filter.fn=filter.fn)
}

ret.s <- get.returns("stock")
ret.e <- get.returns("etf")

stopifnot(all(names(ret.e)==etf.list))
stopifnot(all(row.names(ret.e)==row.names(ret.s)))

if (length(names(ret.s)) <= 20) subtract.average = FALSE


## limit the ticker DB to the entries that we have in the price matrix
tc.subset <- subset(tickers.classified,TIC %in% names(ret.s))

#### signal generation -----------------------
if (profiler.on) {
  cat("Profiler output:",profiler.filename,"\n")
  Rprof(profiler.filename)
}

cat("getMKLthreads returns ",getMKLthreads(),"\n")
cat("getDoParWorkers returns ",getDoParWorkers(),"\n")

N <- nrow(ret.s)
est.win <- 60
if (save.sig.file) {
  sig.f <- stock.etf.signals(ret.s
                             , ret.e
                             , tc.subset
                             , num.days=N-est.win+1
                             , compact.output=T
                             , subtract.average=subtract.average)
  save(sig.f, file=save.sig.filename)
} else {
  cat("note: not generating signals by default\n")
}
if (profiler.on) Rprof(NULL) 

##signals$sig.dates list will be reverse-chronological and might contain mor dates 
##than what we want for a trading simulation.  The dates for backtesting are determined
##by signals$sig.dates (prices data frame is subset accordingly)

