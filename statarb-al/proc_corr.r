#Tally up the number of NAs:
spx.ret.mtx.full <- read.csv("spx_ret_mtx",row.names=1)
spx.ret.mtx.full <- spx.ret.mtx.full[nrow(spx.ret.mtx.full):1,]
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

get.stock.returns <- function(ret.mtx, M=252, offset=0, na.pct.cutoff=0.01, file=FALSE){
## reads in files if requested; performs data cleanup (removes symbols with excessive number of NAs)
  if(file){ ret.mtx <- read.csv(ret.mtx,row.names=1); ret.mtx <- ret.mtx[nrow(ret.mtx):1,]}
  if(M+offset > dim(ret.mtx)[1]){ stop("Requested index exceeds num. of rows") }
  ret.mtx <- ret.mtx[(1+offset):(M+offset),]
  ret.mtx.na.totals <- apply(is.na(ret.mtx),2,sum)
  ret.mtx.na.pct <- ret.mtx.na.totals / dim(ret.mtx)[1]
                                        # pick the tickers where num NAs < cutoff
  good.names <- names(ret.mtx.na.pct[ret.mtx.na.pct <= na.pct.cutoff])
  good.name.idxs <- as.numeric(na.omit(match(good.names,names(ret.mtx.na.pct))))
  ret.mtx[,good.name.idxs] #filtered
}

get.adjusted.returns <- function(ret.mtx, M=252, offset=0, na.pct.cutoff=0.01, file=FALSE){
  ret.mtx <- get.stock.returns(ret.mtx,M,offset,na.pct.cutoff,file)
  vars <- apply(ret.mtx,2,function(x){ var(x,use="complete.obs") })
  means <- apply(ret.mtx,2,function(x){ mean(x,na.rm=TRUE) })
  return(t(apply(ret.mtx,1,function(x){(x-means)/sqrt(vars)})))
}

get.emp.corr <- function(ret.mtx, M=252, offset=0, na.pct.cutoff=0.01, file=FALSE){
  Y <- get.adjusted.returns(ret.mtx,M,offset,na.pct.cutoff,file)
  rho <- matrix(0.0,dim(Y)[2],dim(Y)[2])
  for(k in 1:M){ rho <- rho + (Y[k,] %o% Y[k,]) }
  rho/(M-1)
}


get.etf.returns <- function(ret.mtx, M=252, offset=0, na.pct.cutoff=0.01, file=FALSE){
  if(file){ ret.mtx <- read.csv(ret.mtx,row.names=1); ret.mtx <- ret.mtx[nrow(ret.mtx):1,]}
  if(M+offset > dim(ret.mtx)[1]){ stop("Requested index exceeds num. of rows") }
  ret.mtx <- ret.mtx[(1+offset):(M+offset),]
  good.names <- c("HHH","IYR","IYT","OIH","RKH","RTH","SMH","UTH","XLE","XLF","XLI","XLK","XLP","XLV","XLY")
  good.name.idxs <- as.numeric(na.omit(match(good.names,names(ret.mtx))))
  ret.mtx <- ret.mtx[,good.name.idxs] #filtered
}

## testing, testing:
test.etfs1 <- get.etf.returns("etf_ret_mtx",M=10,offset=0,file=TRUE)

test1 <- get.adjusted.returns("spx_ret_mtx",M=10,offset=0,na.pct.cutoff=0.01,file=TRUE)

head(get.adjusted.returns("spx_ret_mtx",M=10,offset=2,na.pct.cutoff=0.01,file=TRUE))
spx.ret.mtx.full <- read.csv("spx_ret_mtx",row.names=1)
spx.ret.mtx.full <- spx.ret.mtx.full[nrow(spx.ret.mtx.full):1,]

test2 <- get.adjusted.returns(spx.ret.mtx.full,M=10,offset=0,na.pct.cutoff=0.01,file=FALSE)


## figure out which offset corresponds to May 1 (the date used in the paper)
data.offset <- which(as.logical(match(as.numeric(row.names(spx.ret.mtx.full)),20070501)))
##[1] 547
old.etf.offset <- which(as.logical(match(as.numeric(row.names(test.etfs1)),20070501)))


corr1 <- get.emp.corr(spx.ret.mtx.full,M=252,offset=data.offset,na.pct.cutoff=0.01,file=FALSE)


## general scratch code:

unlist(get.s.score(fit.ar1(as.data.frame(get.ou.series(ret.s.jpm[1:61,,drop=F],ret.e.xlf[1:61,,drop=F])))))

jpm.resid <- as.data.frame(get.ou.series(ret.s.jpm[1:61,,drop=F],ret.e.xlf[1:61,,drop=F]))

ret.jpm.60 <- ret.s.jpm[1:61,,drop=T]
ret.xlf.60 <- ret.e.xlf[1:61,,drop=T]


plot(ret.jpm.60,type='l',col=1)

lines(ret.xlf.60,type='l',col=2)

beta.fit <- fit.stock(ret.s.jpm[1:61,,drop=F],ret.e.xlf[1:61,,drop=F],get.fit=TRUE)
x11(); par(mfrow=c(2,2))
plot(beta.fit)
 
x11();
plot(ret.jpm.60,type='l',col=1)
lines(beta.fit$fitted.values,type='l',col=3)

lines(1.064445042*ret.xlf.60+0.000075156,type='l',col=3)

x11()
plot(1.064445042*ret.xlf.60+0.000075156,type='l',col=3)
lines(ret.jpm.60,col=1)

resids <- beta.fit$residuals
resids.fit <- fit.ar1(list(JPM=resids))
print.ar1.estimates(resids.fit$JPM)

xk.jpm <- get.ou.series(ret.s.jpm[1:61,,drop=F],ret.e.xlf[1:61,,drop=F])
ar1.jpm <- fit.ar1(xk.jpm)
print.ar1.estimates(ar1.jpm$JPM)
get.s.score(ar1.jpm,subtract.average=F)


png(filename = "z2.png", width = 1000, height = 500)
plot(s.fac.df[,4],type='l',col=4)
dev.off()





## generate the s-score using old get.s.score function
dates.range <- row.names(ret.s.jpm)[1:502]
win <- 60 
s.fac.df <- NULL
for(i in seq(along=dates.range)){
  s.fac.df <- rbind(s.fac.df,
                    unlist(get.s.score(fit.ar1(get.ou.series(ret.s.jpm[i:(i+win),,drop=F],ret.e.xlf[i:(i+win),,drop=F]),method="yw"),subtract.average=F)))
}
row.names(s.fac.df) <- dates.range



for(i in seq(along=dates.range)){
  s.fac.df <- rbind(s.fac.df,
                    unlist(get.s.score(fit.ar1(get.ou.series(ret.s.jpm[i:(i+win),,drop=F],ret.e.xlf[i:(i+win),,drop=F]),method="yw"),subtract.average=F)))
}
row.names(s.fac.df) <- dates.range



## #png(filename = "jpm.sig.png", width = 1000, height = 500)
plot(as.numeric(s),type='l')
thresholds=c(sbo=1.25,sso=1.25,sbc=0.75,ssc=0.5,kmin=8.4)
abline(h=-thresholds["sbo"],lty=2)
abline(h=thresholds["sso"],lty=2)
abline(h=thresholds["sbc"],lty=2)
abline(h=-thresholds["ssc"],lty=2)
lines(-as.numeric(s.bto)*abs(thresholds["sbo"]),col=2)
lines(as.numeric(s.sto)*abs(thresholds["sso"]),col=3)
lines(as.numeric(s.close.short)*abs(thresholds["sbc"]),col=4)
lines(-as.numeric(s.close.long)*abs(thresholds["ssc"]),col=5)
## #dev.off()
