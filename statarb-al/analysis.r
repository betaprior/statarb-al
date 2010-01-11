source("functions.r")
default.par <- par(no.readonly = TRUE)

get.dates.vec <- function(fname){
  con <- pipe(paste("cut -d',' -f1 ",fname,sep=""))
  vec <- scan(con,skip=1);  close(con); return(vec)
}
dates.vector <- get.dates.vec("spx_ret_mtx")
dates.vector.etf <- get.dates.vec("etf_ret_mtx")
stopifnot(all(dates.vector==dates.vector.etf))
dates.vector <- rev(dates.vector)
## get offset:
which(as.logical(match(dates.vector,20070501)))
## [1] 547
offset <- 547

ret.s <- get.stock.returns("spx_ret_mtx",M=252,offset=offset,na.pct.cutoff=0.01,file=TRUE)
ret.e <- get.etf.returns("etf_ret_mtx",M=252,offset=offset,file=TRUE)

ret.s.60 <- ret.s[1:60,]
ret.e.60 <- ret.e[1:60,]
stopifnot(all(row.names(ret.e.60)==row.names(ret.s.60)))

fit.stock <- function(r.s,r.e,enforce.df=TRUE,get.fit=FALSE){
##  if enforcing df, r.s must be generated using [,1,drop=F]
  if(enforce.df){
    stopifnot(is.data.frame(r.s),is.data.frame(r.e),all(row.names(r.e)==row.names(r.s)))
  }else{ #assume r.s is a list and have to convert to df
    r.s <- data.frame("RS"=r.s,row.names=row.names(r.e))
#    names(r.s) <- r.s.name
  }
  fml <- as.formula(paste(c(paste(names(r.s),"~ 1"),names(r.e)),collapse=" + "))
  fit <- lm(fml,data=cbind(r.s,r.e))
  if(get.fit) fit else fit$residuals
}

fit1 <- fit.stock(ret.s.60[,1,drop=F],ret.e.60)
as.numeric(fit1)

test.df <- ret.s.60[,1:2]

get.ou.series <- function(r.s,r.e){
  stopifnot(is.data.frame(r.s),is.data.frame(r.e),all(row.names(r.e)==row.names(r.s)))
  lapply(r.s,function(x){
    beta.fit <- fit.stock(x,r.e,enforce.df=FALSE,get.fit=TRUE)
    list(beta.fit=beta.fit,ou=cumsum(rev(beta.fit$residuals))) })
}

## res is a list returned by get.ou.series
## each element of res is a list(beta.fit,ou)
fit.ar1 <- function(res, method="mle"){
  lapply(res,function(x){
    list(  beta.fit=x$beta.fit
         , ar.fit=ar(x$ou, aic = F, order.max = 1, method=method)) })
}

print.ar1.estimates <- function(r.fit){
  cat('order', r.fit$order, ', m =',r.fit$x.mean,', a[1] =',r.fit$ar,', sigma =',sqrt(r.fit$var.pred),'\n')
}

## list.of.fits is a list returned by fit.ar1
## each element of res is a list(beta.fit,ar.fit)
get.s.score <- function(list.of.fits,subtract.average=TRUE,avg.mod=0){
  m.avg <- mean(as.numeric(lapply(list.of.fits,function(x) x$ar.fit$x.mean)))
  if(!subtract.average) m.avg <- avg.mod
  lapply(list.of.fits,function(xx){
    x <- xx$ar.fit
    c(  a=x$x.mean*(1-x$ar)
      , b=x$ar
      , m=x$x.mean
      , k=-log(x$ar)*252
      , varz=x$var.pred
      , s=(x$x.mean-m.avg)*(-sqrt((1-x$ar^2)/x$var.pred))
      , xx$beta.fit$coefficients) })
}

test.r <- as.data.frame(get.ou.series(test.df,ret.e.60))
test.ar1 <- fit.ar1(test.r)

## check to see that we get roughly the same results using various estimation methods:
invisible(lapply(fit.ar1(test.r,method="mle"),print.ar1.estimates))
invisible(lapply(fit.ar1(test.r,method="yw"),print.ar1.estimates))
invisible(lapply(fit.ar1(test.r,method="ols"),print.ar1.estimates))

get.s.score(fit.ar1(as.data.frame(get.ou.series(ret.s.60[,1:10],ret.e.60))))

## after failing to find sense in current numbers, try to replicate fig 7
## use updated ETF data; take Jan 06 to Dec 07 and get s-score of JPM vs XLF

which(as.logical(match(dates.vector,20071231)))
## [1] 378
offset <- 377


which(as.logical(match(dates.vector,20070601)))
## [1] 525
##offset <- 514

ret.s <- get.stock.returns("spx_ret_mtx",M=3*252,offset=offset,na.pct.cutoff=0.01,file=TRUE)
ret.e <- get.etf.returns("etf_ret_mtx",M=3*252,offset=offset,file=TRUE)
stopifnot(all(row.names(ret.e)==row.names(ret.s)))

ret.s.jpm <- ret.s["JPM"]

ret.e.xlf <- ret.e["XLF"]

ret.s.jpm <- ret.s[c("JPM","XOM")]
ret.e.xlf <- ret.e



dates.range <- row.names(ret.s.jpm)[1:502]
win <- 60 
s.fac.df <- NULL
for(i in seq(along=dates.range)){
  s.fac.df <- rbind(s.fac.df,
                    unlist(get.s.score(fit.ar1(get.ou.series(ret.s.jpm[i:(i+win),,drop=F],ret.e.xlf[i:(i+win),,drop=F]),method="yw"),subtract.average=F)))
}
row.names(s.fac.df) <- dates.range

s.fac.df.mle <- s.fac.df
s.fac.df.yw <- s.fac.df

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

