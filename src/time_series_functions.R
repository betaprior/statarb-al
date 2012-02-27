## {{{ garch fit functions:

fGarch.obj.to.model <- function(fgarch.obj){
  params <- fgarch.obj@fit$params$params
  ar.idxs <- grep("^ar",names(params))
  ma.idxs <- grep("^ma",names(params))
  alpha.idxs <- grep("^alpha",names(params))
  beta.idxs <- grep("^beta",names(params))
  model <- list()
  if(!zero.length(ar.idxs)) model$ar <- params[ar.idxs]
  if(!zero.length(ma.idxs)) model$ma <- params[ma.idxs]
  model$omega <- params["omega"]
  model$alpha <- params[alpha.idxs]
  model$beta <- params[beta.idxs]
  model$mu <- params["mu"]
  if(!is.na(params["skew"])) model$skew=params["skew"]
  if(!is.na(params["shape"])) model$shape=params["shape"]
  return(model)
}

build.sim.spec <- function(fgarch.fit){
  garchSpec(model=fGarch.obj.to.model(fgarch.fit)
            , cond.dist=fgarch.fit@fit$params$cond.dist
            , presample=cbind(residuals(fgarch.fit,standardize=T),fgarch.fit@h.t,fgarch.fit@data)) }
  
plot.sim.results <- function(ser.list,title.list=rep(NULL,length(ser.list)),xlim.d=c(-0.2,0.2),ylim.d=NULL,xlim.q=NULL,ylim.q=c(-0.3,0.3)){
  stopifnot(class(ser.list)=="list")
  par.save()
  par(mfcol=c(length(ser.list),2))
  mapply(function(x,y)  my.densityPlot(x,xlim=xlim.d,ylim=ylim.d,main=y),ser.list,title.list)
  mapply(function(x,y){ qqnorm(x,xlim=xlim.q,ylim=ylim.q); qqline(x) }, ser.list)
  par.restore()
}

## }}}

## AR fitting/simulation functions:
fit.ar1.series <- function(x,method="yw") ar(x, aic=F, order.max=1, method=method)
ar.params.from.fit <- function(x){ ## x is ar fit
  stopifnot(class(x)=="ar" || class(x)=="Arima")
  if(class(x)=="ar"){
    return(lapply(list(m=x$x.mean
                       , a=x$x.mean*(1-x$ar)
                       , b=x$ar
                       , varz=x$var.pred),unname))
  }else{
    return(lapply(list(m=x$coef["intercept"]
                       , a=x$coef["intercept"]*(1-x$coef["ar1"])
                       , b=x$coef["ar1"]
                       , varz=x$sigma2),unname))
  } }
sim.ar1.series <- function(ar,mean,innov.var,n=1000){ ## mean here is series mean=a/(1-b)
  arima.sim(list(order=c(1,0,0), ar=ar), n=n, rand.gen = function(n,...) rnorm(n,mean=mean*(1-ar),sd=sqrt(innov.var)))
}
## this assumes ret[i+1]=(S[i+1]-S[i])/S[i]
ret.to.prices <- function(ret,p0){
  x <- rep(0,length(ret))
  x[1] <- p0
  for(i in seq(along=ret)[c(-1)])
    x[i] <- x[i-1]*(ret[i]+1)
  x
}
