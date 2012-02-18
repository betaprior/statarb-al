my.matrix <- matrix(c(NA,2:4),2,2)
my.vector <- c(9,8,7,6)
my.number <- 42
rcpp.test3 <- function(num=my.number, vec=my.vector, mat=my.matrix, verbose=TRUE){
  parms <- list(verbose=verbose)
  .Call("rcpp_test3",num,vec,mat,parms)
}

rcpp.test2 <- function(num=my.number, vec=my.vector, mat=my.matrix, verbose=TRUE){
  parms <- list(verbose=verbose)
  .Call("rcpp_test2",num,vec,mat,parms)
}
rcpp.test2a <- function(num=my.number, vec=my.vector, mat=my.matrix, verbose=TRUE){
  parms <- list(verbose=verbose)
  .Call("rcpp_test2a",num,vec,mat,parms)
}

rcpp.test <- function(mat){
  .Call("rcpp_test",mat)
}

rcpp.test.2x2 <- function(mat){
  .Call("rcpp_test_2x2",mat)
}


dyn.load("rcpp_test.so")
M <- matrix(1:6,3,2,byrow=TRUE)
M[2,1] <- NA
M[3,1] <- NaN
cat("matrix M is ",M,"\n")
res1 <- rcpp.test(M)
MM <- matrix(1:4,2,2,byrow=TRUE)
MM[2,1] <- NA
cat("matrix MM is ",MM,"\n")
res1.2 <- rcpp.test.2x2(MM)


res2 <- rcpp.test2(my.number, my.vector, MM, list())
cat("2x2 with (1,1)=NA:\n")
res2a <- rcpp.test2a(my.number, my.vector, my.matrix, list())

na.mtx <- M; na.number <- NA; na.vector <- c(NA,1:3)
res3 <- rcpp.test3(na.number,na.vector,na.mtx)
