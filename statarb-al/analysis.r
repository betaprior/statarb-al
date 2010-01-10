source("functions.r")

old.etf.offset <- 423
stock.offset <- 547

ret.s <- get.stock.returns("spx_ret_mtx",M=252,offset=stock.offset,na.pct.cutoff=0.01,file=TRUE)
ret.e <- get.etf.returns("etf_ret_mtx",M=252,offset=old.etf.offset,file=TRUE)

ret.s.60 <- ret.s[1:60,]
ret.e.60 <- ret.e[1:60,]

fit.stock <- function(r.s,r.e,enforce.df=TRUE,r.s.name=""){
##  if enforcing df, r.s must be generated using [,1,drop=F]
  if(enforce.df){
    stopifnot(is.data.frame(r.s),is.data.frame(r.e),all(row.names(r.e)==row.names(r.s)))
  }else{ #assume r.s is a list and have to convert to df
    r.s <- data.frame("RS"=r.s,row.names=row.names(r.e))
#    names(r.s) <- r.s.name
  }
  fml <- as.formula(paste(c(paste(names(r.s),"~ 1"),names(ret.e)),collapse=" + "))
  lm(fml,data=cbind(r.s,r.e))$residuals
}

fit1 <- fit.stock(ret.s.60[,1,drop=F],ret.e.60)
as.numeric(fit1$residuals)

test.df <- ret.s.60[,1:2]

get.residuals <- function(r.s,r.e){
  stopifnot(is.data.frame(r.s),is.data.frame(r.e),all(row.names(r.e)==row.names(r.s)))
  lapply(r.s,function(x){ fit.stock(x,r.e,enforce.df=FALSE) })
}

fit.ar1 <- function(res){
  lapply(res,function(x){ ar.mle(x, aic = F, order.max = 1) })}

test.r <- as.data.frame(get.residuals(test.df,ret.e.60))
test.ar1 <- fit.ar1(test.r)
