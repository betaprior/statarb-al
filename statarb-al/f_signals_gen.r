require("abind")

gen.fits.pq <- function(p1q.matrix.alldates, classified.stocks.list, num.dates, win, ar.method){
  p1q.col.num <- ncol(p1q.matrix.alldates)
  num.ar.params <- 3                    # infer # of beta coeffs from mtx col num
  combo.fit.2d <- matrix(0, num.dates, (p1q.col.num - 1 + num.ar.params) )
  for(i in 1:num.dates){     # pass-by-reference semantics, what do you expect?
    j <- num.dates-i+1
    p1q.matrix <- p1q.matrix.alldates[i:(i+win-1), ,drop=F]

    if(!any(is.na(p1q.matrix))){
      ##      design.mtx <- p1q.matrix[,2:ncol(p1q.natrix)]
      beta.fit <- lm.fit(p1q.matrix[,2:ncol(p1q.matrix)], p1q.matrix[,1])
      ou <- cumsum(rev(beta.fit$residuals))
      ar.fit <- ar(ou, aic = F, order.max = 1, method=ar.method)
      combo.fit.2d[j, ] <- c(beta.fit$coefficients, ar.fit$x.mean,ar.fit$ar,ar.fit$var.pred)
    }else{
      combo.fit.2d[j, ] <- rep(NA,ncol(combo.fit.2d))
    }
  }
  return(combo.fit.2d)
}


## output: 3d matrix (dates,params,tickers)
## params is an array A: int2logical(A[1],5) gives logical w/ names corr to
## c("model.valid", "bto", "sto", "close.short", "close.long")
## A[2:8] are mr.params, names c("s","k","m","mbar","a","b","varz")
## A[9...] are betas (determined from stock names)
## beta, ar fit matrices have dimensions 1:dates,2:fit.params,3:instrument
gen.signals <- function(beta.fit.mtx,ar.fit.mtx,subtract.average, avg.mod=0
                        , thresholds=c(sbo=1.25,sso=1.25,sbc=0.75,ssc=0.5,kmin=8.4)){
  ## if(!flipsign){ sign <- 1 }else{ sign <- -1 }
  param.names <- c("s","k","m","mbar","a","b","varz") #NB: doesn't incl. action field
  exclude.alpha <- 1 ##must be either 1 or 0!
  num.betas <- dim(beta.fit.mtx)[2]-exclude.alpha
  num.sig.fields <- 1+length(param.names)+num.betas
  sig.mtx.loc <- matrix(0,num.sig.fields,dim(beta.fit.mtx)[3]) #dim[3] is num.tkrs

  ## sig.mtx <- array(dim=c(dim(beta.fit.mtx)[1],num.sig.fields,dim(beta.fit.mtx)[3]))

  cfun <- function(...) abind(...,rev.along=3)
  ## sig.mtx <-
  foreach(i = 1:(dim(beta.fit.mtx)[1]), .combine = "cfun", .multicombine = TRUE) %dopar%{
    m.avg <- mean(ar.fit.mtx[i,1, ,drop=F],na.rm=T)
    if(!subtract.average) m.avg <- avg.mod
    sig.mtx.loc <- 
      apply(ar.fit.mtx[i, , ,drop=F],3,function(x){
        x.mean <- x[1]; ar <- x[2]; var.pred <- x[3]
        c(  0                                              #space for signal code
          , (x.mean-m.avg)*(-sqrt((1-ar^2)/var.pred))      #s
          , -log(ar)*252                                   #k
          , x.mean                                         #m
          , m.avg                                          #mbar
          , x.mean*(1-ar)                                  #a
          , ar                                             #b
          , var.pred                                       #varz
          , rep(0,num.betas) )                             #space for betas
      } )
    sig.code <- apply(sig.mtx.loc,2,function(x){ ##ATTN: critically depends on k being 3rd idx, s being 2nd
      logical2int(
                  c(  (x[3] > thresholds[["kmin"]]) #will be NA if k is NaN
                    , (x[2] < (-thresholds[["sbo"]]))
                    , (x[2] > (+thresholds[["sso"]]))
                    , (x[2] < (+thresholds[["sbc"]]))
                    , (x[2] > (-thresholds[["ssc"]])))
                  )} )
    sig.mtx.loc[1,] <- sig.code
    sig.mtx.loc[(2+length(param.names)):num.sig.fields, ] <- beta.fit.mtx[i,-1, ,drop=F] ##assumes throwing away alpha
    sig.mtx.loc
  }
  ## return(sig.mtx)
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
           , compact.output=TRUE, subtract.average=TRUE
           , ar.method="yw", factor.names=c("beta"), select.factors=TRUE){
    ## -- sanity checks and data cleanup: -------------------------
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
    ret.s <- ret.s[ ,colnames(ret.s) %in% stocks.list]
    stock.names <- colnames(ret.s)
    omitted.stocks <- stocks.list %w/o% colnames(ret.s)
    if(length(omitted.stocks)>0)
      warning(paste(length(omitted.stocks),"stocks omitted from the provided list (most likely due to bad data)"))

    ## -- preallocations for fit coeff matrices: ------------------
    num.beta.fit.coefs <- length(factor.names)+1
    num.ar.fit.coefs <- 3
    sig.param.names <- c("action","s","k","m","mbar","a","b","varz",factor.names)
    beta.fit.mtx <- array(dim=c(length(dates.range),num.beta.fit.coefs,length(stock.names))
                          , dimnames=list(rev(dates.range),NULL,stock.names))
    ar.fit.mtx <- array(dim=c(length(dates.range),num.ar.fit.coefs,length(stock.names))
                        , dimnames=list(rev(dates.range),NULL,stock.names))
    combined.fit.mtx <- array(dim=c(length(dates.range),num.beta.fit.coefs+num.ar.fit.coefs,length(stock.names))
                               , dimnames=list(rev(dates.range),NULL,stock.names))

    ## -- parallel loop to generate the signals: ------------------
    cfun <- function(...) abind(...,along=3)
    combined.fit.mtx <-
      foreach(i = seq(along=stock.names), .combine = "cfun", .multicombine = TRUE) %dopar% {
        if(select.factors){
          factor.returns <- as.matrix(ret.e[ ,as.character(classified.stocks.list[stock.names[i],][-1]) ] )
        } else {
          stopifnot(num.beta.fit.coefs == 1+ncol(ret.e))
          factor.returns <- as.matrix(ret.e)
        }
        gen.fits.pq(  cbind(  as.matrix(ret.s[, stock.names[i]])
                            , as.matrix(rep(1,nrow(ret.s))) ##to ease the construction of fit design mtx
                            , factor.returns)
                    , classified.stocks.list=classified.stocks.list
                    , num.dates=length(dates.range)
                    , win=win, ar.method=ar.method)
      }
    beta.fit.mtx <- combined.fit.mtx[ ,1:num.beta.fit.coefs, ]
    ar.fit.mtx <- combined.fit.mtx[ ,(num.beta.fit.coefs+1):(num.beta.fit.coefs+num.ar.fit.coefs), ]

    sig.mtx <- gen.signals(beta.fit.mtx, ar.fit.mtx, subtract.average=subtract.average)
    dimnames(sig.mtx) <- list(rev(dates.range), sig.param.names, stock.names)
    return(sig.mtx)
  }
