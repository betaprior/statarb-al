if (!exists("statarb.al.proj")) stop("Need project metadata file to proceed")
source.files <- c("utils.R", "functions.R")
for (f in paste(statarb.al.proj$src.path, source.files, sep="")) source(f)
setwd(statarb.al.proj$workspace.path)
require("timeSeries")
require("xts")

## test pair trading on financial stocks vs xlf

## * load the data

filenames.default <-
  list(stock.ret.mtx="univ1_ret_mtx.gz",
       etf.ret.mtx="etf_ret_mtx.gz",
       tic.classified="ticker_to_sec_etf.csv.gz",
       save.sig="sig.file.RObj")
                          
filenames <- filenames.default

etf.list <- c("HHH","IYR","IYT","OIH","RKH","RTH",
              "SMH","UTH","XLE","XLF","XLI","XLK","XLP","XLV","XLY","SPY","QQQQ")

tickers.classified <-
  sort.data.frame(get.classified.tickers(filenames$tic.classified), by=~TIC)
row.names(tickers.classified) <- tickers.classified$TIC

dates.vector <- get.dates.vec(filenames$stock.ret.mtx)
dates.vector.etf <- get.dates.vec(filenames$etf.ret.mtx)
stopifnot(all(dates.vector==dates.vector.etf))
dates.vector <- rev(dates.vector)

working.years <- seq(2009, 2003, by=-1)
offset <- as.list(get.year.offsets(dates.vector, working.years))
names(offset) <- as.character(working.years)

this.offset <- offset[["2008"]]
num.days <- 252 * 5


ret.s <- { filter.fn <- na.pct.cutoff.filter(0.0)
           fname <- filenames$stock.ret.mtx
           get.stock.returns(fname, M=num.days, offset=this.offset, file=TRUE,
                             filter.fn=filter.fn) }


ret.e <- { filter.fn <- select.tickers.filter(etf.list)
           fname <- filenames$etf.ret.mtx
           get.stock.returns(fname, M=num.days, offset=this.offset, file=TRUE,
                             filter.fn=filter.fn) }

stopifnot(all(etf.list %in% names(ret.e)))
stopifnot(all(row.names(ret.e)==row.names(ret.s)))

tc.xlf <- subset(tickers.classified, SEC_ETF=="XLF") # pick financials
tc.xlf <- subset(tc.xlf, TIC %in% intersect(tc.xlf$TIC, names(ret.s))) 
                                        # ensure that we have the data for those stocks

ret.s.fin <- ret.s[, "JPM", drop=F]
N <- nrow(ret.s.fin)
est.win <- 60
system.time(sig.jpm <-
            stock.etf.signals(ret.s.fin, ret.e, tc.xlf,
                              num.days=N-est.win+1, compact.output=T, subtract.average=F))

s.jpm.inv.ts <- as.timeSeries(-sig.jpm[, "s", 1, drop=T])
## plot.xts(as.xts(s.jpm.inv.ts), main="JPM vs XLF s-signal")

## X11()
## plot.xts(as.xts(s.jpm.inv.ts)["2006/2007"], main="JPM vs XLF s-signal")
