## reproduce the AL paper figure 3 (look at the evolution of the principal
## eigenportfolio)
require(timeSeries)
require(xts)

## ret.mtx.file <- "spx_ret_mtx"
## source("tr_test_generic_batch.r")

load("univ1.mid.price.RObj") # loads univ1.master.price
## current issue: duplcate names in the price dataframe
dups <- names(univ1.master.price)[duplicated(names(univ1.master.price))]
## find duplicated names in price file:
which(names(univ1.master.price)=="HHH")
## [1]  201 1621
## > head(univ1.master.price[,c(201,1621)])
##             HHH HHH.1
## 20081231 31.900    NA
univ1.master.price <- univ1.master.price[,-which(names(univ1.master.price)=="HHH")[2]]
price.df.f <- univ1.master.price[,names(univ1.master.price) %w/o% (dups %w/o% "HHH")]
instr.p.all <- instr.p.all[-which(instr.p.all %in% (instr.p.all %w/o% names(price.df.f)))]
## gets rid of JCI, PXD

num.stocks <- 408
num.factors <- 15
load("pca_spx_eig_mtx_spx.RObj") # loads eig.mtx
plot.prices.window <- c("20060501","20070501")
eig.mtx.returns <- window(as.timeSeries(eig.mtx[,(num.stocks*(num.factors+1)+1):(num.stocks*(num.factors+1)+num.factors)]), plot.prices.window[1], plot.prices.window[2])

eig.mtx.eigvals <-
  window(as.timeSeries(eig.mtx[,(num.stocks*(num.factors+1)+num.factors+1):(num.stocks*(num.factors+1)+2*num.factors)]), plot.prices.window[1], plot.prices.window[2])

source("time_series_functions.r") ## for ret.to.prices(ret.ser,p0)
## eig.evolution <- timeSeries(ret.to.prices(eig.mtx.returns[,1],1),rownames(eig.mtx.returns))
eig.evolution <- timeSeries(cumsum(eig.mtx.returns[,1]),rownames(eig.mtx.returns))
eig.evolution.pct <- (eig.evolution-first(eig.evolution))/eig.evolution

plot.xts(as.xts(eig.evolution*0.005))

plot.prices.instr <- "SPY"
this.price.df <- reverse.rows(price.df.f[,plot.prices.instr,drop=F])
plot.prices.ts <- window(as.timeSeries(this.price.df)
                         , plot.prices.window[1], plot.prices.window[2])
plot.pct.ts <- (plot.prices.ts-first(plot.prices.ts))/plot.prices.ts
plot.xts(as.xts(plot.pct.ts), main=paste("Evolution of",plot.prices.instr,"from"
                                   ,plot.prices.window[1],"to",plot.prices.window[2]))


##plot(cbind(eig.evolution*0.005,plot.pct.ts),plot.type="single")
plot.xts(as.xts(plot.pct.ts))
lines(as.xts(eig.evolution*0.005),col=2)
