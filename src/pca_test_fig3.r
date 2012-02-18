## reproduce the AL paper figure 3 (look at the evolution of the principal
## eigenportfolio)
require(timeSeries)
require(xts)

ret.mtx.file <- "spx_ret_mtx"
source("tr_test_generic_batch.r")

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


## these numbers and the indexes for accesing matrix parts should now be accessed via
## the attributes of the eigenvector matrix
load("pca_spx_eig_mtx_spx.RObj") # loads eig.mtx
num.factors <- attributes(eig.mtx)$num.eigs
num.stocks <- length(attributes(eig.mtx)$tickers)
plot.prices.window <- c("20060501","20070501")

## matrix of eigenportf. returns
eig.mtx.returns <- window(as.timeSeries(eig.mtx[,(num.stocks*(num.factors+1)+1):(num.stocks*(num.factors+1)+num.factors)]), plot.prices.window[1], plot.prices.window[2])

## matrix of eigenvalues
eig.mtx.eigvals <-
  window(as.timeSeries(eig.mtx[,(num.stocks*(num.factors+1)+num.factors+1):(num.stocks*(num.factors+1)+2*num.factors)]), plot.prices.window[1], plot.prices.window[2])

## matrix of principal eigenvectors (in rows)
eig.mtx.eigvec1 <- window(as.timeSeries(eig.mtx[,1:num.stocks]), plot.prices.window[1], plot.prices.window[2])

## total amount "invested" on that day into the market eigenportfolio
inv.amounts <- rowSums(eig.mtx.eigvec1) 

source("time_series_functions.r") ## for ret.to.prices(ret.ser,p0)
## cumulative sum of daily percent returns is a good proxy for n-day returns (see below)
eig.evolution <-
  timeSeries(cumsum(eig.mtx.returns[,1]),rownames(eig.mtx.returns))
eig.evolution.pct <- eig.mtx.returns[,1]/inv.amounts

## compare the evolution of the market eigenportfolio vs SPY
plot.prices.instr <- "SPY"
this.price.df <- reverse.rows(price.df.f[,plot.prices.instr,drop=F])
plot.prices.ts <- window(as.timeSeries(this.price.df)
                         , plot.prices.window[1], plot.prices.window[2])
plot.pct.ts <- (plot.prices.ts-first(plot.prices.ts))/plot.prices.ts
plot(as.xts(timeSeries(0-first(cumsum(eig.evolution.pct))+cumsum(eig.evolution.pct),rownames(eig.evolution.pct)))
     ,main=paste("Evolution of",plot.prices.instr,"and market eigenportfolio:\n"
                                   ,plot.prices.window[1],"to",plot.prices.window[2]),col=2,lty=2)
lines(as.xts(plot.pct.ts))
legend("topleft", legend = c("SPY", "eigenportfolio"),
               lty = 1:2, col = 1:2, xjust = 1, yjust = 1,
               title = NULL, inset=0.) 
## savePlot(file="spy_vs_market_eigenportf.png")

## Prices from returns: cumsum vs exact computation vs actual price data
## using SPY as an example
this.ret.df <- reverse.rows(ret.e[,plot.prices.instr,drop=F])
plot.ret.ts <- window(as.timeSeries(this.ret.df)
                         , plot.prices.window[1], plot.prices.window[2])
plot(as.xts(timeSeries(ret.to.prices(plot.ret.ts,1),rownames(plot.ret.ts))),main="Actual prices vs prices from returns")
lines(as.xts(timeSeries(1-first(cumsum(plot.ret.ts))+cumsum(plot.ret.ts),rownames(plot.ret.ts))),col=2)
lines(as.xts(plot.prices.ts/first(plot.prices.ts)),col=3)
## you can see that the exact computation vs cumsum are almost identical, but
## both deviate from the actual prices (green) 


## to check if there are negative components in the eigenportfolio, set
## appropriate window and compute sapply(1:252,function(x) length(which(eig.mtx.eigvec1[x,]<0)))


## check eigenvector behavior + clustering


this.date <- last(rownames(eig.mtx))
## this.date <- rownames(eig.mtx)[1300]
eig.mtx.eigenvecs <-
  t(matrix(eig.mtx[this.date,attributes(eig.mtx)$subdivision.idxs$eigvecs],nrow=num.factors,ncol=num.stocks,byrow=T)
* eig.mtx[this.date,attributes(eig.mtx)$subdivision.idxs$sdevs] * sqrt(eig.mtx[this.date,attributes(eig.mtx)$subdivision.idxs$eigvals]))

## reproducing figure 4 from the paper (the numbers seem off but match Lecture2Risk2010.pdf)
require("ggplot2")
corr.etfs <- function(x)(tc.subset[attributes(eig.mtx)$tickers,2])[rev(sorted.vector.indices(eig.mtx.eigenvecs[,x]))]
sorted.ev <- function(x) data.frame(ev=rev(sort(eig.mtx.eigenvecs[,x])),ind=corr.etfs(x))
plot.sorted.ev <- function(x)
  qplot(1:nrow(sorted.ev(x)),ev,data=sorted.ev(x),color=ind
        , main=paste("Sorted eigenvector",x),xlab="Sorted eigenvector index",ylab="Eigenvector component")
plot.sorted.ev(1)
plot.sorted.ev(2)

