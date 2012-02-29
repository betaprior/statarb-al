if (!exists("statarb.al.proj")) stop("Need project metadata file to proceed")
source.files <- c("f_signals_gen.R")    # signal generation (some signals post-processing fns
                                        # are in this file though)
for (f in paste(statarb.al.proj$src.path, source.files, sep="")) source(f)
options(stringsAsFactors = FALSE)

##~ -----------

##~ quick-and-dirty utility functions

encode <- function(...) apl.encode(...)


set.from.cmdargs <- function(cmdargs.default, vstr) {
  if (! vstr %in% names(cmdargs.default))
    stop(paste(vstr, "not found in the supplied cmdargs list"))
  args.cmdstring <- vstr
  arg.class <- cmdargs.default[[vstr]]$class
  args.default <- cmdargs.default[[vstr]]$val
  arg.val <- as(getCmdArgs(args.cmdstring), arg.class)

  if (is.na(arg.val)) 
    arg.var <- if (!is.null(args.default)) args.default
    else
      stop("Can't find default arguments")

  arg.val
}


##~ functions related to AL paper analysis

## get.stock.returns
##
## reads in files if requested; performs data cleanup (removes symbols with excessive number of NAs)
## reads M rows with offset offset; assumes that the input matrix has data in rows
## chronologically, thus, read offset...(M+offset) from the end
## returns a matrix with M rows
get.stock.returns <- function(ret.mtx, M=252, offset=0, file=FALSE, filter.fn=NULL) {
  if (file) 
    ret.mtx <- read.csv(ret.mtx, row.names=1)
  stopifnot(is.data.frame(ret.mtx))
  stopifnot(M + offset <= nrow(ret.mtx)) # requested index must be in the matrix
  ret.mtx <- ret.mtx[nrow(ret.mtx) - seq(offset, len=M), ] # read rows off...(M+off) from the end
  if (!is.null(filter.fn)) {
    select.names <- filter.fn(ret.mtx)
  } else {
    select.names <- names(ret.mtx)
  }
  ret.mtx[names(ret.mtx) %in% select.names]
}

select.tickers.filter <- function(nameList) {
  function(ret.mtx) nameList
}

na.pct.cutoff.filter <- function(pct.cutoff=0.01) {
  function(ret.mtx) {
    ret.mtx.na.totals <- apply(is.na(ret.mtx), 2, sum)
    ret.mtx.na.pct <- ret.mtx.na.totals / nrow(ret.mtx)
                                        # pick the tickers where num NAs < cutoff
    names(ret.mtx.na.pct[ret.mtx.na.pct <= pct.cutoff])
  }
}


## Compute empirical correlation matrix (A.L. Eq. 8)
## Note that unlike A.L., we keep dates in rows and stocks in columns
## This is exactly the kind of correlation matrix one usually computes for PCA
## Note that by construction (e.g. since observations in Y are normalized)
## this matrix has 1's on the diagonal
get.emp.corr <- function(ret.mtx, M=252, ...) {
  Y <- scale(get.stock.returns(ret.mtx, M, ...)) # demean / divide by variance
  rho <- matrix(0.0, ncol(Y), ncol(Y))
  for (k in 1:M) 
    rho <- rho + (Y[k, ] %o% Y[k, ])
  rho / (M - 1)
}

get.classified.tickers <- function(fname){
  read.csv(fname, as.is=T)[c(1,8)]
}

get.dates.vec <- function(fname){
  read.csv(fname, skip=1)[1]
}

## returns POSIXt output
## assumes input is a vector of numeric dates, e.g. 20080304
## Doesn't assume a particular sorting of the dates vector
first.trading.days <- function(dates.vector) {
  dates.vector <- rev(sort(dates.vector))
  d <- strptime(dates.vector, "%Y%m%d")
  d[cumsum(rle(d$year)$lengths)]
}

## returns offsets into the dates.vector
## corresponding to the first trading date of each year specified
## in the years parameter
get.year.offsets <- function(dates.vector, years) {
  ftd <- first.trading.days(dates.vector)
  ftd.years <-
    strftime(ftd[match(as.character(years), strftime(ftd, "%Y"))],
             "%Y%m%d")
  offs <- which(as.logical(match(dates.vector, as.numeric(ftd.years))))
  if (length(offs) != length(years))
    stop("Unable to get first trading date for every year requested")
  offs
}



fit.stock <- function(r.s, r.e, enforce.df=TRUE, get.fit=FALSE, refit.with.pos.betas=FALSE) {
##  if enforcing df, r.s must be generated using [,1,drop=F]
  if(enforce.df) {
    stopifnot(is.data.frame(r.s), is.data.frame(r.e), all(row.names(r.e)==row.names(r.s)))
  } else { #assume r.s is a list and have to convert to df
    r.s <- data.frame("RS"=r.s,row.names=row.names(r.e))
                                        # names(r.s) <- r.s.name
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
           , subtract.average=TRUE, ar.method="yw",scale.by.sqrt.lambda=TRUE
           , eigenstuff.file="", flipsign=FALSE, save.eigenm.fn="eigenmatrix.RObj"){
    ## -- sanity checks and data cleanup: -------------------------
    stopifnot(num.days > 1 && win>10)
    stopifnot(nrow(ret.s) >= num.days + win + win.pca - 1)
    dates.range <- row.names(ret.s)[1:(num.days+win)]
    if(is.unsorted(rev(dates.range)) && (!is.unsorted(dates.range))){
        ret.s <- reverse.rows(ret.s)
        dates.range <- row.names(ret.s)[1:(num.days+win)]
      }
    stopifnot(!is.unsorted(rev(dates.range))) ## rev. dates must be chron sorted
    stocks.list <- classified.stocks.list$TIC
    stock.names <- names(ret.s)[names(ret.s) %in% stocks.list]
    ret.s <- as.matrix(ret.s[stock.names],colnames=stock.names)
    omitted.stocks <- stocks.list %w/o% stock.names
    if(length(omitted.stocks)>0)
      warning(paste(length(omitted.stocks),"stocks omitted from the provided list (most likely due to bad data)"))
    ## browser()
    ## -- preallocations for fit coeff matrices: ------------------
    num.factors <- num.eigs ##number of eigenportfolios
    num.stocks <- length(stock.names)
    ## eigenportf.weights.mtx <- array(dim=c(length(dates.range),num.stocks*num.factors,num.stocks)
    ##                                 , dimnames=list(rev(dates.range),NULL,stock.names))
    ## eigenportf.returns.mtx <- array(dim=c(length(dates.range),num.factors,num.stocks)
    ##                                 , dimnames=list(rev(dates.range),NULL,stock.names))
    ## rows are m inline-combined weights vectors followed by m returns
    stopifnot(num.stocks==dim(ret.s)[2]) ## since using dim(ret.s)[2] as proxy
                                         ## for no. stocks in the eigenvector generating function

    ## compute the eigenstuff
    if(file.exists(eigenstuff.file)){
      load(eigenstuff.file) ## loads eig.mtx
      eigenreturns.mtx <- eig.mtx[ ,attributes(eig.mtx)$subdivision.idxs$returns]
    } else {
    date.piece.length <- 442
    date.cuts.idxs <-as.numeric(cut(1:length(dates.range),seq(0,length(dates.range)+date.piece.length+1,by=date.piece.length),include.lowest=T))
    ## eig.mtx <- array(dim=c(date.piece.length,num.stocks*(num.factors+1)+2*num.factors,num.stocks)
                                    ## , dimnames=list(NULL,NULL,stock.names))
    eig.mtx <- foreach(ii = rev(unique(date.cuts.idxs)), .combine = "rbind",
                       .multicombine = TRUE) %dopar% {
      dates.chunk.idxs <- which(as.numeric(date.cuts.idxs)==ii,arr.ind=T)
      dates.est <- c(dates.chunk.idxs,seq(last(dates.chunk.idxs),length=win.pca))
      ## browser()
      cat(ii,"-")
      run.pca.analysis(ret.s[dates.est, ], num.dates=length(dates.chunk.idxs)
                       , num.eigs=num.factors, win.pca=win.pca
                       , scale.by.sqrt.lambda = scale.by.sqrt.lambda)
        
    }
    rownames(eig.mtx) <- rev(dates.range)
    attr(eig.mtx,"tickers") <- stock.names
    attr(eig.mtx,"num.eigs") <- num.factors
    attr(eig.mtx,"subdivision.idxs") <-
    list(eigvecs=1:(num.stocks*num.factors),sdevs=(num.stocks*(num.factors)+1):(num.stocks*(num.factors)+num.factors),returns=(num.stocks*(num.factors+1)+1):(num.stocks*(num.factors+1)+num.factors),eigvals=(num.stocks*(num.factors+1)+num.factors+1):(num.stocks*(num.factors+1)+2*num.factors))

    if(!is.null(save.eigenm.fn)){ save(eig.mtx,file=save.eigenm.fn) }
    ## cat("Wrote file:",paste("pca_spx_eig_mtx",ii,".RObj",sep=""),"\n")
    eigenreturns.mtx <- eig.mtx[ ,attributes(eig.mtx)$subdivision.idxs$returns]
  }
    if(is.unsorted(rev(rownames(eigenreturns.mtx))) && (!is.unsorted(rownames(eigenreturns.mtx)))){
      eigenreturns.mtx <- reverse.rows(eigenreturns.mtx)
    }
    stopifnot(all(rownames(ret.s)[1:nrow(eigenreturns.mtx)] == rownames(eigenreturns.mtx)))
    factor.names <- paste("beta",1:num.factors,sep="")
    stock.etf.signals(  ret.s[1:nrow(eigenreturns.mtx), ]
                      , eigenreturns.mtx[,1:num.factors,drop=F],classified.stocks.list,num.days,win,factor.names=factor.names
                      , select.factors=FALSE)
  }

run.pca.analysis <- function(ret.s, num.dates, num.eigs, win.pca,
                             corr.reg=1e-7, scale.by.sqrt.lambda=TRUE){
  num.stocks <- dim(ret.s)[2]
  accum.mtx <- matrix(nrow=num.dates,ncol=num.stocks*(num.eigs+1) + 2*num.eigs)
  rho <- matrix(0.0,num.stocks,num.stocks)
  ## cat("|")
  for(i in 1:num.dates) {     
    j <- num.dates-i+1
    ret.mtx <- ret.s[i:(i+win.pca-1), ,drop=F]
    vars <- apply(ret.mtx,2,function(x){ var(x,use="complete.obs") })
    means <- apply(ret.mtx,2,function(x){ mean(x,na.rm=TRUE) })
    Y <- t(apply(ret.mtx,1,function(x){(x-means)/sqrt(vars)})) ##ret.mtx.std
    rho <- ( t(Y) %*% Y )/(win.pca-1) + corr.reg*diag(num.stocks) ##corr.reg ~1e-7 for numerical stability
    ## cat(".")
    eigs <- eigen(rho, symmetric=TRUE)
    ## cat(".")
    eigenportfolio.amounts <- eigs$vectors[,1:num.eigs]/sqrt(vars[1:num.eigs])
    ## scaled by lambda:
    if(scale.by.sqrt.lambda){
      eigenportfolio.amounts <- eigenportfolio.amounts/matrix(sqrt(eigs$values[1:num.eigs]),num.stocks,num.eigs,byrow=T) 
    }

    ##eigenportfolio.amounts <- eigenportfolio.amounts/matrix(colSums(eigenportfolio.amounts),dim(rho)[1],num.eigs,byrow=T) ## normalize columns
    ## note that normalization should also take care of the sign
    if(all(eigs$vectors[,1] < 0)) {
      eigenportfolio.amounts <- -(eigenportfolio.amounts)
    } else if(all(eigs$vectors[,1] >= 0)) {
    } else {
      ## warning("Non-uniform signs of leading eigenvector")
      if(sum(as.numeric(eigs$vectors[,1] < 0)) > num.stocks/2) {
        eigenportfolio.amounts <- -(eigenportfolio.amounts)
      } }
  ##  all signs of the leading e-vector have to be +ve
    eigenportfolio.returns <- sapply(1:num.eigs,function(x)sum(ret.s[i,]*eigenportfolio.amounts[,x]))
    accum.mtx[j,] <- c(as.numeric(eigenportfolio.amounts),sqrt(vars),eigenportfolio.returns,eigs$values[1:num.eigs])
        ## cat("|")

  }
  return(accum.mtx)
}
