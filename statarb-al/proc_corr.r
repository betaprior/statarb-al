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


get.adjusted.returns <- function(ret.mtx, M=252, offset=0, na.pct.cutoff=0.01, file=FALSE){
  if(file){ ret.mtx <- read.csv(ret.mtx,row.names=1); ret.mtx <- ret.mtx[nrow(ret.mtx):1,]}
  if(M+offset > dim(ret.mtx)[1]){ stop("Requested index exceeds num. of rows") }
  ret.mtx <- ret.mtx[(1+offset):(M+offset),]
  ret.mtx.na.totals <- apply(is.na(ret.mtx),2,sum)
  ret.mtx.na.pct <- ret.mtx.na.totals / dim(ret.mtx)[1]
                                        # pick the tickers where num NAs < cutoff
  good.names <- names(ret.mtx.na.pct[ret.mtx.na.pct <= na.pct.cutoff])
  good.name.idxs <- as.numeric(na.omit(match(good.names,names(ret.mtx.na.pct))))
  ret.mtx <- ret.mtx[,good.name.idxs] #filtered
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



## testing, testing:
test1 <- get.adjusted.returns("spx_ret_mtx",M=10,offset=0,na.pct.cutoff=0.01,file=TRUE)

head(get.adjusted.returns("spx_ret_mtx",M=10,offset=2,na.pct.cutoff=0.01,file=TRUE))
spx.ret.mtx.full <- read.csv("spx_ret_mtx",row.names=1)
spx.ret.mtx.full <- spx.ret.mtx.full[nrow(spx.ret.mtx.full):1,]

test2 <- get.adjusted.returns(spx.ret.mtx.full,M=10,offset=0,na.pct.cutoff=0.01,file=FALSE)


## figure out which offset corresponds to May 1 (the date used in the paper)
data.offset <- which(as.logical(match(as.numeric(row.names(spx.ret.mtx.full)),20070501)))
##[1] 547

corr1 <- get.emp.corr(spx.ret.mtx.full,M=252,offset=data.offset,na.pct.cutoff=0.01,file=FALSE)
