source("tr_test_spx1_batch.r")
require("Revobase")
require("foreach")
require("doMC")

N <- 80
est.win <- 60
num.eigs <- 2
source("functions.r")
#Rprof("pcastuff.Rprof")
mtx1 <- stock.pca.signals(ret.s,tc.spx,num.days=N-est.win+1,num.eigs=num.eigs)
#Rprof(NULL)
