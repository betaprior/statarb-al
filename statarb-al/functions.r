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
