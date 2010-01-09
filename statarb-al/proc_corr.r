#Tally up the number of NAs:
spx.ret.mtx.full <- read.csv("spx_ret_mtx",row.names=1)
#> spx.ret.mtx.na.tally <- is.na(spx.ret.mtx.full)
spx.ret.mtx.na.totals <- apply(is.na(spx.ret.mtx.full),2,sum)
spx.ret.mtx.na.pct <- spx.ret.mtx.na.totals / dim(spx.ret.mtx.full)[1]
# pick the tickers where num NAs < 5%
na.pct.cutoff <- 0.05
good.names <- names(spx.ret.mtx.na.pct[spx.ret.mtx.na.pct <= na.pct.cutoff])
good.name.idxs <- as.numeric(na.omit(match(good.names,names(spx.ret.mtx.na.pct))))
spx.ret.mtx.compl <- spx.ret.mtx.full[,good.name.idxs]
dim(spx.ret.mtx.compl)
## [1] 3272  357

