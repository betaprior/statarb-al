## test pair trading on financial stocks vs xlf
setwd("/home/leo/projects/finance/research/statarb-al/")
source("functions.r")
source("f_trading_sim.r") ## for the trading simulation

tickers.classified <-
  sort.data.frame(get.classified.tickers("ticker_to_sec_etf.csv"), by=~TIC)
row.names(tickers.classified) <- tickers.classified$TIC

tc.xlf <- subset(tickers.classified,SEC_ETF=="XLF")


dates.vector <- get.dates.vec("spx_ret_mtx")
dates.vector.etf <- get.dates.vec("etf_ret_mtx")
stopifnot(all(dates.vector==dates.vector.etf))
dates.vector <- rev(dates.vector)

## limit the ticker DB to the entries that we have in the price matrix
tc.xlf <- subset(tc.xlf,TIC %in% intersect(tc.xlf$TIC,names(ret.s)))

## nb: dates.vector is reverse-sorted wrt the original file 
offset.2005 <- which(as.logical(match(dates.vector,20050103)))
num.days <- 252*4
ret.s <- get.stock.returns("spx_ret_mtx",M=5*252,offset=offset.2005,na.pct.cutoff=0.0,file=TRUE)
ret.e <- get.etf.returns("etf_ret_mtx",M=5*252,offset=offset.2005,file=TRUE)
stopifnot(all(row.names(ret.e)==row.names(ret.s)))
ret.s.fin <- ret.s[,tc.xlf$TIC]
N <- nrow(ret.s.fin)
est.win <- 60
##sig.f <- stock.etf.signals(ret.s.fin,ret.e,tc.xlf,num.days=N-est.win+1,compact.output=T)
## save(sig.f,file="sig.financials1.RObj")
load("sig.financials1.RObj")
sig.mtx.f <- get.signals.mtx(sig.f)
##sig.mtx usage: > head(sig.mtx.f[,,"JPM"])
##sig.actions.f <- get.signals.actions(sig.mtx.f[,,"JPM"])
##sig.mtx.dbg <- as.data.frame(sig.mtx.f[,,this.instr])
## (w/o casting to data frame can't subset by column names)

##signals$sig.dates list will be reverse-chronological and might contain mor dates 
##than what we want for a trading simulation.  The dates for backtesting are determined
##by signals$sig.dates (prices data frame is subset accordingly)

instr.p.all <- sig.f$tickers
instr.q.all <- names(ret.e)
load("univ1.mid.price.RObj") # loads univ1.master.price
## current issue: duplcate names in the price dataframe
dups <- names(univ1.master.price)[duplicated(names(univ1.master.price))]
c(instr.p.all,instr.q.all)[which(c(instr.p.all,instr.q.all) %in% dups)]
## don't deal with this for the time being
price.df.f <- univ1.master.price[,c(instr.p.all,"XLF")]

num.days.bt <- 600
sig.f.bt <- list(sig.dates=sig.f$sig.dates[1:num.days.bt],tickers=sig.f$tickers)
sig.mtx.f.bt <- get.signals.mtx(sig.f.bt)
instr <- "JPM"
sim.trades.f <- run.trading.simulation(  sig.f.bt, price.df.f
                                       , instr, c(instr,"XLF"), tc.xlf
                                       , debug=FALSE, silent=FALSE)

if(testObject(trading.f.list)) rm(trading.f.list)
trading.f.list <- list()
for(i in seq(along=instr.p.all)){
  instr <- instr.p.all[i]
  trading.f.list[[instr]] <- run.trading.simulation(  sig.f.bt, price.df.f
                                                    , instr, c(instr,"XLF"), tc.xlf
                                                    , debug=FALSE, silent=T)
  cat(instr,":",last(trading.f.list[[instr]]$equity),"\n")
}

fin.pnl <- numeric(0)
for(i in seq(along=instr.p.all))
  fin.pnl[i] <- last(trading.f.list[[i]]$equity)

##Now we try to trade all of them in the same simulation
source("f_trading_sim.r") ## for the trading simulation
sim.trades.f.all <- run.trading.simulation(  sig.f.bt, price.df.f
                                           , instr.p.all, c(instr.p.all,"XLF"), tc.xlf
                                           , debug=FALSE, silent=FALSE)

## running this on all financials loses 16% in 600 days!
## ACAS : 98101.96 
## AFL : 108767.7 
## AIG : 107003.8 
## ALL : 100346.2 
## AOC : 94916.84 
## AXP : 98161.39 
## BAC : 97241.95 
## BEN : 102173.7 
## BK : 93119.79 
## C : 100213.7 
## CB : 96651.05 
## CINF : 98959.34 
## CMA : 101172.3 
## COF : 99483.34 
## DFS : 99356.69 
## FII : 98335.45 
## GS : 94857.8 
## HCBK : 101570.8 
## HIG : 101519 
## JPM : 98966.99 
## LM : 106065.3 
## LNC : 91956.99 
## LUK : 101797.9 
## MBI : 96803.55 
## MER : 97863.91 
## MMC : 102809.9 
## NTRS : 98631.32 
## PGR : 88882.02 
## SLM : 100105.5 
## STT : 111677.7 
## TMK : 98949.95 
## TROW : 99844.54 
## UNM : 94329.85 
## USB : 101388.1 
## WB : 104909.9 
## WFC : 99540.78 
## XL : 97140.68 

this.instr <- "PGR"
sig.mtx.dbg <- as.data.frame(sig.mtx.f.bt[,,this.instr])
sig.actions.dbg <- get.signals.actions(sig.mtx.dbg)
dates.dbg <- rownames(sig.mtx.dbg)
prices.dbg <- price.df.f[dates.dbg,c(this.instr,"XLF")]

par.save()
par(mfrow=c(2,1))
plot(sig.mtx.dbg$s,type='l')
draw.thresholds()
draw.signal.lines(sig.actions.dbg)
lines((trading.f.list[[this.instr]]$equity/100000)^2,col=6)
plot(prices.dbg$PGR,type='l',ylim=c(20,90))
lines(prices.dbg$XLF,col=2)
par.restore()

plot(sig.mtx.dbg$beta,type='l')
