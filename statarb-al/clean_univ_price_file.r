load("univ1.mid.price.RObj") # loads univ1.master.price
## current issue: duplcate names in the price dataframe
dups <- names(univ1.master.price)[duplicated(names(univ1.master.price))]
drop.idx <- numeric(0)
dups2 <- character(0) ##with diff.data
for(i in seq(along=dups)){
  curr.dup.idx <- which(names(univ1.master.price)==dups[i])
  tmp <- as.list(univ1.master.price[,curr.dup.idx])
  cat("all.equal on",dups[i],":",do.call("all.equal",unname(tmp)),"\n")
  all.equal.res <- all(as.logical(do.call("all.equal",unname(tmp))))
  if(!is.na(all.equal.res) && all.equal.res){
    drop.idx <- c(drop.idx,which(names(univ1.master.price)==dups[i])[-1])
  } else {
    all.na.res <- unlist(lapply(tmp,function(x)all(is.na(x))))
    if(any(all.na.res)){
      drop.idx <- c(drop.idx,curr.dup.idx[all.na.res])
      next
    }
    dups2 <- c(dups2,dups[i])
  }
}
## now have to manually check dups2
## [1] "CIV" "DNY" "IEI" "ION" "PAX"
## for now thow them all out (CIV - diff prices, DNY/IEI mostly NA)
drop.idx <- c(drop.idx,which(names(univ1.master.price) %in% dups2))
drop.idx <- sort(drop.idx)
univ1.master.price <- univ1.master.price[-drop.idx]
save(univ1.master.price,file="univ1.mid.price.nodups.RObj")
