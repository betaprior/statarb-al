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



get.ou.series <- function(r.s,r.e){
  stopifnot(is.data.frame(r.s),is.data.frame(r.e),all(row.names(r.e)==row.names(r.s)))
  lapply(r.s,function(x){
    beta.fit <- fit.stock(x,r.e,enforce.df=FALSE,get.fit=TRUE)
    list(beta.fit=beta.fit,ou=cumsum(rev(beta.fit$residuals))) })
}



## get.ou.series <- function(r.s,r.e){
##   stopifnot(is.data.frame(r.s),is.data.frame(r.e),all(row.names(r.e)==row.names(r.s)))
##   stock.names <- names(r.s)
##   out.list <- vector('list',length(stock.names))
##   for(i in seq(along=stock.names)){
##     r.s.i <- r.s[stock.names[i]] #nb: don't need to do [,...,drop=F]
##     beta.fit <- fit.stock(r.s.i,r.e,enforce.df=FALSE,get.fit=TRUE)
##     out.list[[i]] <-
##       list(beta.fit=beta.fit,ou=cumsum(rev(beta.fit$residuals))) }
##   names(out.list) <- stock.names
##   return(out.list)
## }

## nb: hardcoded SEC_ETF field name in tickers.classified df
## this also does error checking (NA results if any NA present)
get.ou.series.etf <- function(r.s,r.e,tickers.classified,lm.fit.here=TRUE){
  stopifnot(is.data.frame(r.s),is.data.frame(r.e),all(row.names(r.e)==row.names(r.s)))
  stock.names <- names(r.s)
  out.list <- vector('list',length(stock.names))
#  r.s <- reverse.rows(r.s); r.e <- reverse.rows(r.e)
#  browser()
  for(i in seq(along=stock.names)){
    r.s.i <- r.s[stock.names[i]] #nb: don't need to do [,...,drop=F]
    r.e.i <- r.e[tickers.classified[stock.names[i],]$SEC_ETF]
    if(!any(is.na(r.s.i)) & !any(is.na(r.e.i))){
      if(lm.fit.here) {
        design.mtx <- cbind(rep(1,length(r.s.i)),r.e.i)
        beta.fit <- lm.fit(as.matrix(design.mtx), unlist(r.s.i))
      } else {
        beta.fit <- fit.stock(r.s.i,r.e.i,enforce.df=FALSE,get.fit=TRUE)
      }
        ou <- cumsum(rev(beta.fit$residuals))
    }else{
      beta.fit <- lm(a~b,data=data.frame(a=0,b=0)) #dummy fit object
      ou <- NA
    }
    out.list[[i]] <-
      list(beta.fit=list(coefficients=beta.fit$coefficients),ou=ou) }
#      list(beta.fit=beta.fit,ou=ou) }
  names(out.list) <- stock.names
  return(out.list)
}

## res is a list returned by get.ou.series
## each element of res is a list(beta.fit,ou)
fit.ar1 <- function(res, method="mle"){  
  lapply(res,function(x){
    if(!is.na(x$ou)[1]){
      ar.fit <- ar(x$ou, aic = F, order.max = 1, method=method)
    }else{ ar.fit <- NA }
    list(  beta.fit=x$beta.fit
         , ar.fit=list(x.mean=ar.fit$x.mean,ar=ar.fit$ar,var.pred=ar.fit$var.pred)) })
#         , ar.fit=ar.fit) })
}


## list.of.fits is a list returned by fit.ar1
## each element of res is a list(beta.fit,ar.fit)
## compact output format:
## matrix with rows corresponding to stocks; each row is an unnamed numeric array A
## int2logical(A[1],5) gives logical w/ names corr to
## c("model.valid", "bto", "sto", "close.short", "close.long")
## A[2:8] are mr.params, names c("s","k","m","mbar","a","b","varz")
## A[9...] are betas (determined from stock names)
get.signals <- function(list.of.fits,subtract.average=TRUE,avg.mod=0
                        , thresholds=c(sbo=1.25,sso=1.25,sbc=0.75,ssc=0.5,kmin=8.4)
                        , compact.output=FALSE, debug=FALSE,flipsign=FALSE){
  if(!flipsign){ sign <- 1 }else{ sign <- -1 }
  m.avg <- mean(as.numeric(lapply(list.of.fits,function(x) tryCatch(x$ar.fit$x.mean,error=function(e) NA))),na.rm=T)
  if(!subtract.average) m.avg <- avg.mod
  res <- 
  lapply(list.of.fits,function(xx){
    x <- xx$ar.fit
    if(!all(is.na(x))){
      mr.params <- c(  s=(x$x.mean-m.avg)*(-sqrt((1-x$ar^2)/x$var.pred))*sign
                     , k=-log(x$ar)*252
                     , m=x$x.mean
                     , mbar=m.avg
                     , a=x$x.mean*(1-x$ar)
                     , b=x$ar
                     , varz=x$var.pred)
      signal <- c(  model.valid=(mr.params[["k"]] > thresholds[["kmin"]]) #will be NA if k is NaN
                  , bto=(mr.params[["s"]] < (-thresholds[["sbo"]]))
                  , sto=(mr.params[["s"]] > (+thresholds[["sso"]]))
                  , close.short=(mr.params[["s"]] < (+thresholds[["sbc"]]))
                  , close.long=(mr.params[["s"]] > (-thresholds[["ssc"]])))
    }else{
      mr.params <- rep(NA,7)
      signal <- rep(FALSE,5)
    }
    names(mr.params) <- c("s","k","m","mbar","a","b","varz")
    if(!compact.output){
      out <- list(  mr.params=mr.params
                  , signals=signal
                  , b.portfolio=xx$beta.fit$coefficients)
       }else{
#      betas <- xx$beta.fit$coefficients[!(names(xx$beta.fit$coefficients) %in% c("(Intercept)"))]
      betas <- xx$beta.fit$coefficients[-1]
      out <- unname(c(logical2int(signal),mr.params,betas))
     }
    return(out)
  })
  if(!compact.output){
    return(res)
  }else{
    return(matrix(unlist(res),ncol=length(res[[1]]),byrow=TRUE))
  }
}
decode.signals <- function(y){
  x <- int2logical(y[1],5)
  names(x) <- c("model.valid", "bto", "sto", "close.short", "close.long")
  x
}
decode.params <- function(y){
  x <- y[2:8]
  names(x) <- c("s","k","m","mbar","a","b","varz")
  x
}
decode.betas <- function(y){ y[9:length(y)] }

## nb: field names in tickers classification df are hardcoded as
## TIC and SEC_ETF
## input parameters: ret.s and ret.e must be dataframes
## reverse-chron. sorted with dates as row.names
stock.etf.signals <-
  function(ret.s,ret.e,classified.stocks.list,num.days,win=60
           , compact.output=TRUE, flipsign=FALSE, subtract.average=TRUE){
    ## sanity checks
    stopifnot(num.days > 1 && win>10)
    stopifnot(all(row.names(ret.e)==row.names(ret.s)))
    stopifnot(nrow(ret.s)==nrow(ret.e) && nrow(ret.s) >= num.days + win - 1)
    dates.range <- row.names(ret.s)[1:num.days]
    if(is.unsorted(rev(dates.range)))
      if(!is.unsorted(dates.range)){
        ret.s <- reverse.rows(ret.s); ret.e <- reverse.rows(ret.e)
        dates.range <- row.names(ret.s)[1:num.days]
      }
    stopifnot(!is.unsorted(rev(dates.range))) ## rev. dates must be chron sorted
    sig.list <- vector('list',length(dates.range))
    stocks.list <- classified.stocks.list$TIC
    ret.s <- ret.s[names(ret.s) %in% stocks.list]
    omitted.stocks <- stocks.list %w/o% names(ret.s)
    if(length(omitted.stocks)>0)
      warning(paste(length(omitted.stocks),"stocks omitted from the provided list (most likely due to bad data)"))
#    ret.e <- ret.e[tickers.classified["JPM",]$SEC_ETF]
    for(i in seq(along=dates.range)){
#      if(i >= 124){ browser() }
      win.idx <- i:(i+win-1)
      sig.list[[i]] <- 
        get.signals(fit.ar1(
                            get.ou.series.etf(ret.s[win.idx,,drop=F],ret.e[win.idx,,drop=F]
                                              , classified.stocks.list)
                            , method="yw")
                    ,subtract.average=subtract.average,compact.output=compact.output,flipsign=flipsign)
      
    }
    names(sig.list) <- dates.range
    return(list(sig.dates=sig.list,tickers=names(ret.s)))
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
