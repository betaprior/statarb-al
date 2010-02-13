ret.mtx.file <- "spx_ret_mtx"
source("tr_test_generic_batch.r")


est.win <- 252
num.eigs <- 15 
source("functions.r")
#Rprof("pcastuff.Rprof")
registerDoMC(2)
getDoParWorkers()
stock.pca.signals(ret.s,tc.subset,num.days=N-est.win+1,num.eigs=num.eigs)
#Rprof(NULL)
num.stocks <- dim(mtx1)[3]
q.factors <-
  matrix(mtx1[1,1:(num.eigs*num.stocks),1],nrow=num.stocks,ncol=num.eigs,byrow=F)
q.sdevs <- mtx1[1,(num.eigs*num.stocks+1):(num.eigs*(num.stocks+1)),1]
q.evals <- mtx1[1,(num.eigs*(num.stocks+1)+1):(num.eigs*(num.stocks+2)),1]
