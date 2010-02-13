source("tr_test_spx1_batch.r")
require("Revobase")
require("foreach")
require("doMC")

N <- 80
est.win <- 60
num.eigs <- 2
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
