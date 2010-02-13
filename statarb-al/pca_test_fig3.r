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

eig.mtx.eigvec1 <- window(as.timeSeries(eig.mtx[,1:num.stocks]), plot.prices.window[1], plot.prices.window[2])

inv.amounts <- rowSums(eig.mtx.eigvec1)/sqrt(eig.mtx.eigvals[,1])

source("time_series_functions.r") ## for ret.to.prices(ret.ser,p0)
## eig.evolution <- timeSeries(ret.to.prices(eig.mtx.returns[,1],1),rownames(eig.mtx.returns))
eig.evolution <-
  timeSeries(cumsum(eig.mtx.returns[,1]),rownames(eig.mtx.returns))
## what was the original amount invested in the principal eigenvector?..
eigenvector.1 <- eig.mtx[plot.prices.window[1],1:num.stocks]
eig1.inv.amounts <- eigenvector.1/sqrt(eig.mtx.eigvals[plot.prices.window[1],1])
eig.evolution.1 <- (inv.amounts[1]+eig.evolution)/inv.amounts[1]
eig.evolution.pct <- eig.mtx.returns[,1]/inv.amounts

plot.xts(as.xts(eig.evolution-first(eig.evolution)+1))

plot.prices.instr <- "SPY"
this.price.df <- reverse.rows(price.df.f[,plot.prices.instr,drop=F])
plot.prices.ts <- window(as.timeSeries(this.price.df)
                         , plot.prices.window[1], plot.prices.window[2])
plot.pct.ts <- (plot.prices.ts-first(plot.prices.ts))/plot.prices.ts
plot.xts(as.xts(plot.pct.ts), main=paste("Evolution of",plot.prices.instr,"from"
                                   ,plot.prices.window[1],"to",plot.prices.window[2]))


## Going back to getting prices from returns:
this.ret.df <- reverse.rows(ret.e[,plot.prices.instr,drop=F])
plot.ret.ts <- window(as.timeSeries(this.ret.df)
                         , plot.prices.window[1], plot.prices.window[2])
plot(as.xts(timeSeries(ret.to.prices(plot.ret.ts,1),rownames(plot.ret.ts))))
lines(as.xts(timeSeries(1-first(cumsum(plot.ret.ts))+cumsum(plot.ret.ts),rownames(plot.ret.ts))),col=2)
lines(as.xts(plot.prices.ts/first(plot.prices.ts)),col=3)


foo <- timeSeries(cumsum(plot.ret.ts),rownames(plot.ret.ts))
bar <- timeSeries(1.1*ret.to.prices(plot.ret.ts,1),rownames(plot.ret.ts))

##plot(cbind(eig.evolution*0.005,plot.pct.ts),plot.type="single")
plot.xts(as.xts(plot.pct.ts))
lines(as.xts(eig.evolution*0.005),col=2)
