##~ quick-and-dirty utility functions
reverse.rows <- function(d){ d[nrow(d):1,,drop=F] }
"%w/o%" <- function(x,y) x[!x %in% y] #--  x without y (defined in help for %in%)

logical2int <- function(x) { sum(x * 2^(rev(seq(along=x)) - 1)) }
encode <- function(number, base) {
  # simple version of APL-encode / APL-representation "T", pw 10/02
  # "encode" converts the numbers "number" using the radix vector "base"
  n.base <- length(base); result <- matrix(0, length(base), length(number))
  for(i in n.base:1){
    result[i,] <- if(base[i]>0) number %% base[i] else number
    number     <- ifelse(rep(base[i]>0,length(number)),
                         floor(number/base[i]), 0)
  }
  return( if(length(number)==1) result[,1] else result )
}
int2logical <- function(x,num.bits) { as.logical(encode(x,rep(2,num.bits))) }

##~ utility functions (to be placed in their own file)
## {{{
testObject <- function(object){ exists(as.character(substitute(object))) }

sort.data.frame <- function(x, by){
    # Author: Kevin Wright
    # with some ideas from Andy Liaw
    # http://tolstoy.newcastle.edu.au/R/help/04/07/1076.html
 
    # x: A data.frame
    # by: A one-sided formula using + for ascending and - for descending
    #     Sorting is left to right in the formula
  
    # Useage is:
    # library(nlme);
    # data(Oats)
    # sort(Oats, by= ~nitro-Variety)
 
    if(by[[1]] != "~")
        stop("Argument 'by' must be a one-sided formula.")
 
    # Make the formula into character and remove spaces
    formc <- as.character(by[2]) 
    formc <- gsub(" ", "", formc) 
    # If the first character is not + or -, add +
    if(!is.element(substring(formc, 1, 1), c("+", "-")))
        formc <- paste("+", formc, sep = "")
 
    # Extract the variables from the formula
    vars <- unlist(strsplit(formc, "[\\+\\-]"))    
    vars <- vars[vars != ""] # Remove any extra "" terms
 
    # Build a list of arguments to pass to "order" function
    calllist <- list()
    pos <- 1 # Position of + or -
    for(i in 1:length(vars)){
        varsign <- substring(formc, pos, pos)
        pos <- pos + 1 + nchar(vars[i])
        if(is.factor(x[, vars[i]])){
            if(varsign == "-") {
                calllist[[i]] <- -rank(x[, vars[i]])
            } else {
                calllist[[i]] <- rank(x[, vars[i]])
            }
        } else {
            if(varsign == "-") {
                calllist[[i]] <- -x[, vars[i]]
            } else {
                calllist[[i]] <- x[,vars[i]]
            }
        }
    }
    return(x[do.call("order", calllist), ])
}

yapply <- function(X,FUN, ...) {
  ## cf http://tolstoy.newcastle.edu.au/R/e4/help/08/04/8720.html
  index <- seq(length.out=length(X))
  namesX <- names(X)
  if(is.null(namesX)) namesX <- rep(NA,length(X))

  FUN <- match.fun(FUN)
  fnames <- names(formals(FUN))
  if( ! "INDEX" %in% fnames ){
    formals(FUN) <- append( formals(FUN), alist(INDEX=) )   }
  if( ! "NAMES" %in% fnames ){
    formals(FUN) <- append( formals(FUN), alist(NAMES=) )   }
  mapply(FUN,X,INDEX=index, NAMES=namesX,MoreArgs=list(...)) } 
## }}}

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
get.ou.series.etf <- function(r.s,r.e,tickers.classified){
  stopifnot(is.data.frame(r.s),is.data.frame(r.e),all(row.names(r.e)==row.names(r.s)))
  stock.names <- names(r.s)
  out.list <- vector('list',length(stock.names))
  for(i in seq(along=stock.names)){
    r.s.i <- r.s[stock.names[i]] #nb: don't need to do [,...,drop=F]
    r.e.i <- r.e[tickers.classified[stock.names[i],]$SEC_ETF]
    beta.fit <- fit.stock(r.s.i,r.e.i,enforce.df=FALSE,get.fit=TRUE)
    out.list[[i]] <-
      list(beta.fit=beta.fit,ou=cumsum(rev(beta.fit$residuals))) }
  names(out.list) <- stock.names
  return(out.list)
}

## res is a list returned by get.ou.series
## each element of res is a list(beta.fit,ou)
fit.ar1 <- function(res, method="mle"){
  lapply(res,function(x){
    list(  beta.fit=x$beta.fit
         , ar.fit=ar(x$ou, aic = F, order.max = 1, method=method)) })
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
                        , compact.output=FALSE){
  m.avg <- mean(as.numeric(lapply(list.of.fits,function(x) x$ar.fit$x.mean)))
  if(!subtract.average) m.avg <- avg.mod
  res <- 
  lapply(list.of.fits,function(xx){
    x <- xx$ar.fit
    mr.params <- c(  s=(x$x.mean-m.avg)*(-sqrt((1-x$ar^2)/x$var.pred))
                   , k=-log(x$ar)*252
                   , m=x$x.mean
                   , mbar=m.avg
                   , a=x$x.mean*(1-x$ar)
                   , b=x$ar
                   , varz=x$var.pred)
    signal <- c(  model.valid=(mr.params[["k"]] > thresholds[["kmin"]])
                , bto=(mr.params[["s"]] < (-thresholds[["sbo"]]))
                , sto=(mr.params[["s"]] > (+thresholds[["sso"]]))
                , close.short=(mr.params[["s"]] < (+thresholds[["sbc"]]))
                , close.long=(mr.params[["s"]] > (-thresholds[["ssc"]])))
    if(!compact.output){
      out <- list(  mr.params=mr.params
                  , signals=signal
                  , b.portfolio=xx$beta.fit$coefficients)
       }else{
      betas <- xx$beta.fit$coefficients[!(names(xx$beta.fit$coefficients) %in% c("(Intercept)"))]
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
stock.etf.signals <-
  function(ret.s,ret.e,classified.stocks.list,num.days,win=60,compact.output=FALSE){
    stopifnot(all(row.names(ret.e)==row.names(ret.s)))
    dates.range <- row.names(ret.s)[1:num.days]
    sig.list <- vector('list',length(dates.range))
    stocks.list <- classified.stocks.list$TIC
    ret.s <- ret.s[names(ret.s) %in% stocks.list]
    omitted.stocks <- stocks.list %w/o% names(ret.s)
    if(length(omitted.stocks)>0)
      warning(paste(length(omitted.stocks),"stocks omitted from the provided list (most likely due to bad data)"))
#    ret.e <- ret.e[tickers.classified["JPM",]$SEC_ETF]
    for(i in seq(along=dates.range)){
      sig.list[[i]] <- 
        get.signals(fit.ar1(
                            get.ou.series.etf(ret.s[i:(i+win),,drop=F],ret.e[i:(i+win),,drop=F]
                                              , classified.stocks.list)
                            , method="yw"),subtract.average=F,compact.output=compact.output)
      
    }
    names(sig.list) <- dates.range
    return(list(sig.dates=sig.list,tickers=names(ret.s)))
}
