source("f_signals_gen.r")  ## signal generation (some signals post-processing fns
                                        # are in this file though)
options(stringsAsFactors = FALSE)
##~ quick-and-dirty utility functions

encode <- function(...) apl.encode(...)

##~ functions related to AL paper analysis
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


get.etf.returns <- function(ret.mtx, M=252, offset=0, file=FALSE
                            , tickers=c("HHH","IYR","IYT","OIH","RKH","RTH"
                                ,"SMH","UTH","XLE","XLF","XLI","XLK","XLP","XLV","XLY")){
  if(file){ ret.mtx <- read.csv(ret.mtx,row.names=1); ret.mtx <- ret.mtx[nrow(ret.mtx):1,]}
  if(M+offset > dim(ret.mtx)[1]){ stop("Requested index exceeds num. of rows") }
  ret.mtx <- ret.mtx[(1+offset):(M+offset),]
  good.names <- tickers
  good.name.idxs <- as.numeric(na.omit(match(good.names,names(ret.mtx))))
  ret.mtx <- ret.mtx[,good.name.idxs] #filtered
}

get.mtx.gen <- function(ret.mtx, M=252, offset=0, file=FALSE){
  if(file){ ret.mtx <- read.csv(ret.mtx,row.names=1); ret.mtx <- ret.mtx[nrow(ret.mtx):1,]}
  if(M+offset > dim(ret.mtx)[1]){ stop("Requested index exceeds num. of rows") }
  ret.mtx[(1+offset):(M+offset),]
}

get.classified.tickers <- function(fname){
  con <- pipe(paste("cut -d',' -f1,8 ",fname,sep=""))
  read.csv(con,as.is=TRUE)
}

get.dates.vec <- function(fname){
  con <- pipe(paste("cut -d',' -f1 ",fname,sep=""))
  vec <- scan(con,skip=1);  close(con); return(vec)
}



fit.stock <- function(r.s,r.e,enforce.df=TRUE,get.fit=FALSE,refit.with.pos.betas=FALSE){
##  if enforcing df, r.s must be generated using [,1,drop=F]
  if(enforce.df){
    stopifnot(is.data.frame(r.s),is.data.frame(r.e),all(row.names(r.e)==row.names(r.s)))
  }else{ #assume r.s is a list and have to convert to df
    r.s <- data.frame("RS"=r.s,row.names=row.names(r.e))
#    names(r.s) <- r.s.name
  }
  fml <- as.formula(paste(c(paste(names(r.s),"~ 1"),names(r.e)),collapse=" + "))
  fit <- lm(fml,data=cbind(r.s,r.e))
  if(refit.with.pos.betas){
    betas <- fit$coefficients[!(names(fit$coefficients) %in% c("(Intercept)"))]
    betas.pos <- betas[betas>0]
    if(length(betas.pos)>0){
      fml <- as.formula(paste(c(paste(names(r.s),"~ 1"),names(betas.pos)),collapse=" + "))
      fit <- lm(fml,data=cbind(r.s,r.e))
    }else warning("Failed to find positive betas to re-fit")
  }
  if(get.fit) fit else fit$residuals
}



## res is a list returned by get.ou.series
## each element of res is a list(beta.fit,ou)
fit.ar1 <- function(res, method="mle"){  
  lapply(res,function(x){
    if(!is.na(x$ou)[1]){
      ar.fit <- ar(x$ou, aic = F, order.max = 1, method=method)
    }else{ ar.fit <- NA }
    list(  beta.fit=x$beta.fit
         ## , ar.fit=list(x.mean=ar.fit$x.mean,ar=ar.fit$ar,var.pred=ar.fit$var.pred)) })
        , ar.fit=ar.fit) })
}



## functions that manipulate the signals list structure
get.signals.mtx <- function(sig.list){
  tmp.mtx <- prealloc.mtx(  length(sig.list$sig.dates)
                          , ncol(sig.list$sig.dates[[1]])
                          , rownames=rev(names(sig.list$sig.dates)))
  sig.mtx <- array(dim=c(dim(tmp.mtx),length(sig.list$tickers))
                   , dimnames=list(rev(names(sig.list$sig.dates))
                       , c("action","s","k","m","mbar","a","b","varz","beta")
                       , sig.list$tickers))
  stopifnot(!is.unsorted(rev(names(sig.list$sig.dates)))) ##o/w next line is wrong
  for(j in seq(along=sig.list$tickers)){
    for(i in rev(seq(along=sig.list$sig.dates))){
      tmp.mtx[1+length(sig.list$sig.dates)-i,] <- sig.list$sig.dates[[i]][j,]
    }
    sig.mtx[,,j] <- tmp.mtx
  }
  if(j==1)
    as.data.frame(sig.mtx[,,1])
  else
    sig.mtx
}
get.signals.actions <- function(sig.mtx){
  sim.actions <- lapply(sig.mtx[,"action"],decode.signals)
  sim.actions <- as.data.frame(do.call("rbind",sim.actions))
  row.names(sim.actions) <- row.names(sig.mtx)
  return(sim.actions)
}


## plotting routines
draw.thresholds <- function(){
  abline(h=-thresholds["sbo"],lty=2)
  abline(h=thresholds["sso"],lty=2)
  abline(h=thresholds["sbc"],lty=2)
  abline(h=-thresholds["ssc"],lty=2)
}
draw.signal.lines <- function(act.mtx){
  lines(-as.numeric(act.mtx$bto)*abs(thresholds["sbo"]),col=2)
  lines(as.numeric(act.mtx$sto)*abs(thresholds["sso"]),col=3)
  lines(as.numeric(act.mtx$close.short)*abs(thresholds["sbc"]),col=4)
  lines(-as.numeric(act.mtx$close.long)*abs(thresholds["ssc"]),col=5)
}
draw.actions.lines <- function(a){ abline(v=which(as.numeric(a)==1),lty=3) }


stock.pca.signals <-
  function(ret.s,classified.stocks.list,num.days,win=60,win.pca=252,num.eigs=15
           , subtract.average=TRUE, ar.method="yw"){
    ## -- sanity checks and data cleanup: -------------------------
    stopifnot(num.days > 1 && win>10)
    stopifnot(nrow(ret.s) >= num.days + win - 1)
    dates.range <- row.names(ret.s)[1:num.days]
    if(is.unsorted(rev(dates.range)))
      if(!is.unsorted(dates.range)){
        ret.s <- reverse.rows(ret.s); ret.e <- reverse.rows(ret.e)
        dates.range <- row.names(ret.s)[1:num.days]
      }
    stopifnot(!is.unsorted(rev(dates.range))) ## rev. dates must be chron sorted
    stocks.list <- classified.stocks.list$TIC
    stock.names <- names(ret.s)[names(ret.s) %in% stocks.list]
    ret.s <- as.matrix(ret.s[stock.names],names=stock.names)
    omitted.stocks <- stocks.list %w/o% stock.names
    if(length(omitted.stocks)>0)
      warning(paste(length(omitted.stocks),"stocks omitted from the provided list (most likely due to bad data)"))
    ## browser()
    ## -- preallocations for fit coeff matrices: ------------------
    num.factors <- num.eigs ##number of eigenportfolios
    factor.names <- c("beta1","beta2")
    num.beta.fit.coefs <- length(factor.names)+1
    num.ar.fit.coefs <- 3
    num.stocks <- length(stock.names)
    sig.param.names <- c("action","s","k","m","mbar","a","b","varz",factor.names)
    beta.fit.mtx <- array(dim=c(length(dates.range),num.beta.fit.coefs,num.stocks)
                          , dimnames=list(rev(dates.range),NULL,stock.names))
    ar.fit.mtx <- array(dim=c(length(dates.range),num.ar.fit.coefs,num.stocks)
                        , dimnames=list(rev(dates.range),NULL,stock.names))
    combined.fit.mtx <- array(dim=c(length(dates.range),num.beta.fit.coefs+num.ar.fit.coefs,num.stocks)
                               , dimnames=list(rev(dates.range),NULL,stock.names))
    eigenportf.weights.mtx <- array(dim=c(length(dates.range),num.stocks*num.factors,num.stocks)
                                    , dimnames=list(rev(dates.range),NULL,stock.names))
    eigenportf.returns.mtx <- array(dim=c(length(dates.range),num.factors,num.stocks)
                                    , dimnames=list(rev(dates.range),NULL,stock.names))
    ## rows are m inline-combined weights vectors followed by m returns
    eigenportf.combined.mtx <- array(dim=c(length(dates.range),num.factors+num.stocks*num.factors,num.stocks)
                                    , dimnames=list(rev(dates.range),NULL,stock.names))

    cfun <- function(...) abind(...,along=3)
   ## eigenportf.combined.mtx <- 
    foreach(i = seq(along=stock.names), .combine = "cfun") %dopar% {
      cat(i,"-")
      run.pca.analysis(ret.s, num.dates=length(dates.range)
                       , num.eigs=num.factors, win.pca=win.pca)
    }
  }

run.pca.analysis <- function(ret.s, num.dates, num.eigs, win.pca){
  accum.mtx <- matrix(nrow=num.dates,ncol=dim(ret.s)[2]*(num.eigs+1) + 2*num.eigs)
  rho <- matrix(0.0,dim(ret.s)[2],dim(ret.s)[2])
  ## cat("|")
  for(i in 1:num.dates){     # pass-by-reference semantics, what do you expect?
    j <- num.dates-i+1
    ret.mtx <- ret.s[i:(i+win.pca-1), ,drop=F]
    vars <- apply(ret.mtx,2,function(x){ var(x,use="complete.obs") })
    means <- apply(ret.mtx,2,function(x){ mean(x,na.rm=TRUE) })
    Y <- t(apply(ret.mtx,1,function(x){(x-means)/sqrt(vars)})) ##ret.mtx.std
    rho <- ( t(Y) %*% Y )/(win.pca-1)
    ## cat(".")
    eigs <- eigen(rho, symmetric=TRUE)
    ## cat(".")
    eigenportfolio.amounts <- eigs$vectors[,1:num.eigs]/sqrt(vars[1:num.eigs])
    ## scaled by lambda:
   eigenportfolio.amounts <- eigenportfolio.amounts/matrix(sqrt(eigs$values[1:num.eigs]),dim(rho)[1],num.eigs,byrow=T) 

    ##eigenportfolio.amounts <- eigenportfolio.amounts/matrix(colSums(eigenportfolio.amounts),dim(rho)[1],num.eigs,byrow=T) ## normalize columns
    ## note that normalization should also take care of the sign
    if(all(eigs$vectors[,1] < 0)) {
      eigenportfolio.amounts <- -(eigenportfolio.amounts)
    } else if(all(eigs$vectors[,1] >= 0)) {
    } else {
      warning("Non-uniform signs of leading eigenvector")
      if(sum(as.numeric(eigs$vectors[,1] < 0)) > (dim(rho)[1])/2) {
        eigenportfolio.amounts <- -(eigenportfolio.amounts)
      } }
  ##  all signs of the leading e-vector have to be +ve
    eigenportfolio.returns <- sapply(1:num.eigs,function(x)sum(ret.mtx[i,]*eigenportfolio.amounts[,x]))
    accum.mtx[j,] <- c(as.numeric(eigenportfolio.amounts),sqrt(vars),eigenportfolio.returns,eigs$values[1:num.eigs])
        ## cat("|")

  }
  return(accum.mtx)
}
