ret.mtx.file <- "spx_ret_mtx"
source("tr_test_generic_batch.r")


corr.est.win <- 252
fit.est.win <- 60
num.eigs <- 15  
source("functions.r")
## Rprof("pcastuff.Rprof")
mtx1 <- stock.pca.signals(ret.s,tc.spx,num.days=N-est.win+1,num.eigs=num.eigs)
## Rprof(NULL)


load("pca_one_eigen_record.RObj") ## loads q.factors, q.sdevs, q.evals
q.eigenvecs <- q.factors*q.sdevs
plot(rev(sort(q.eigenvecs[,1])),type='l'); abline(h=0)
corr.etfs <- tc.spx$SEC_ETF[rev(sorted.vector.indices(q.eigenvecs[,1]))]
require("ggplot2")
sorted.ev <- data.frame(ev=rev(sort(q.eigenvecs[,1])),ind=corr.etfs)
p <- ggplot(sorted.ev)
p+aes(x=ind,y=ev)


qplot(1:nrow(sorted.ev),ev,data=sorted.ev,color=ind,xlab="Sorted eigenvector index",ylab="Eigenvector component")

plot(rev(sort(q.eigenvecs[,2])),type='l'); abline(h=0)

plot(rev(sort(q.eigenvecs[,3])),type='l'); abline(h=0)

#Rprof("pcastuff.Rprof")
registerDoMC(4)
getDoParWorkers()
stock.pca.signals(ret.s,tc.subset,num.days=N-corr.est.win-fit.est.win+1,num.eigs=num.eigs,eigenstuff.file="")
#Rprof(NULL)
num.stocks <- dim(mtx1)[3]
q.factors <-
  matrix(mtx1[1,1:(num.eigs*num.stocks),1],nrow=num.stocks,ncol=num.eigs,byrow=F)
q.sdevs <- mtx1[1,(num.eigs*num.stocks+1):(num.eigs*(num.stocks+1)),1]
q.evals <- mtx1[1,(num.eigs*(num.stocks+1)+1):(num.eigs*(num.stocks+2)),1]

