## Trading simulation function
## input: signals object, prices df



## the signals object here is assumed to be a list of dates
## positions matrix is a matrix of instruments / pairs
## rows ("P") indicate instruments for which signals are generated
## cols ("Q") indicate intruments acting as pairs (beta factors)
## NB: q should always also include the underlying p
run.trading.simulation <- function(  signals.struct, prices
                                   , instr.p, instr.q, pq.classifier
                                   , debug=FALSE, warn=FALSE, stop.on.wrn=FALSE
                                   , silent=FALSE, outfile="", debug.name=instr.p[1]
                                   , dbg.transactions=FALSE
                                   , init.cash=100000){
  ## equity.blown.thr <- 10000
  if(outfile!="")
    if(file.exists(outfile)) { file.remove(outfile) }
  stopifnot(!is.unsorted(rev(names(signals.struct$sig.dates)))) ##o/w next line is wrong
  signals <- rev(signals.struct$sig.dates)
  dates <- names(signals)
  stopifnot(all(dates %in% row.names(prices))) ##prices dates range
                                        # must include all signals+more
  stopifnot(all(instr.p %in% instr.q))
  stopifnot(all(instr.q %in% names(prices)))
  prices <- prices[dates,] ## align the data frames
  stopifnot(all(row.names(prices)==dates))
  positions <-as.data.frame(matrix(0,length(instr.p),length(instr.q)))
  names(positions) <- instr.q;  row.names(positions) <- instr.p

  long.shr.amounts <- function(rat,tot,S,b){
    rat.thr <- 0.01
    if(abs(rat-1)<rat.thr){ if((rat-1)>0){ rat <- 1+rat.thr }else{rat <- 1-rat.thr} }
    c(s.shares=round(rat*tot/(S*(rat-1))), b.shares=round(tot/(b*(rat-1)))) }
  prealloc.signal.mtx <- function(stocks,dates){
    x <- array(0.0,c(length(stocks),length(dates)))
    colnames(x) <- dates; rownames(x) <- stocks
    return(x) }

  s.action <- prealloc.signal.mtx(instr.p,dates)
  k <- 0
  lambda <- 0.01 #for single-instr debugging
  nav <- 0; cash <- init.cash; equity <- rep(0.,length(dates))
  
  for(i in seq(along=dates)){
    if(!silent) { cat(i," ") }
    net.positions <- apply(positions,2,sum)
    prices.0na <- prices[i,names(net.positions)]; prices.0na[is.na(prices.0na)] <- 0
    nav <- sum(prices.0na*net.positions)
    equity[i] <- cash + nav
    for(j in seq(along=row.names(positions))){
      this.name <- row.names(positions)[j]
      pair.name <- pq.classifier[this.name,]$SEC_ETF
      if(!(this.name %in% instr.p)) next
      if(any(is.na(prices[i,c(this.name,pair.name)]))) next
      sig.idx <- which(signals.struct$tickers==this.name)
##    if(dates[i]=="20030328") { browser() }
      sig <- decode.signals(signals[[i]][sig.idx,])
      params <- decode.params(signals[[i]][sig.idx,])
      k <- match(this.name,instr.p)
      ## s.id[k] <- this.name
      ## s[k,i] <- params["s"]
      ## s.a[k,i] <- params["a"]
      ## s.b[k,i] <- params["b"]
      ## s.varz[k,i] <- params["varz"]
      ## s.k[k,i] <- params["k"]
      ## s.sto[k,i] <- sig["sto"]
      ## s.bto[k,i] <- sig["bto"]
      ## s.close.short[k,i] <- sig["close.short"]
      ## s.close.long[k,i] <- sig["close.long"]
      betas <- decode.betas(signals[[i]][sig.idx,])
      ## s.betas[k,i] <- betas
      this.p <- positions[j,this.name]
      if(!sig["model.valid"]){
        if(debug && this.name==debug.name) cat(i,"pos:",this.p,"inv.targ:",tot,"ratio ",rat," prices: ",price.s.b," num shares: ",num.shrs,"INVALID\n",file=outfile,append=TRUE)
      }else{
        tot <- lambda*equity[i] # investment amount
        rat <- 1/betas
        price.s.b <- c(prices[i,this.name], prices[i,pair.name])
        num.shrs <- long.shr.amounts(rat,tot, price.s.b[1],
                                     price.s.b[2])*c(1,-1)
        if(betas >=1 && num.shrs[1] <=0){ num.shrs <- -num.shrs }
        if(debug && this.name==debug.name) cat(i,"pos:",this.p,"inv.targ:",tot,"ratio ",rat," prices: ",price.s.b," num shares: ",num.shrs,"\n",file=outfile,append=TRUE)
        if(sig["sto"]){
          if(!(this.p<0)&& (-num.shrs["s.shares"])<0){ #flat or long (but shouldn't be long here)
            ##	sell stock, buy factors #opening short (if flat before, as we should
            ##be)
            ## num.shrs has the correct signs for long, this is short though
            num.shrs <- -num.shrs
            positions[j,this.name] <- positions[j,this.name] + num.shrs["s.shares"]
            positions[j,pair.name] <- positions[j,pair.name] + num.shrs["b.shares"]
            cash <- cash - sum(price.s.b * num.shrs)
            s.action[k,i] <- 1
            if((debug && this.name==debug.name)||dbg.transactions)
              cat(i,this.name,"STO: 'acquiring'",num.shrs,"paying ",sum(price.s.b * num.shrs),"cash=",cash,"\n")
            if(this.p>0 && warn) { cat(paste("\nSTO tripped while long, on day",i,"for stock",this.name),"\n"); if(stop.on.wrn) stop() }
          }
        } #else do nothing #already short 
        if(sig["close.short"]){
          if(this.p<0){
            ## buy stock, sell factors #closing short
            cash <- cash +
              sum(price.s.b*c(positions[j,this.name],positions[j,pair.name]))
            s.action[k,i] <- 1
            if((debug && this.name==debug.name)||dbg.transactions)
              cat(i,this.name,"CLOSING SHORT: paying ",-sum(price.s.b*c(positions[j,this.name],positions[j,pair.name])),"cash=",cash,"\n")
            positions[j,this.name] <- 0
            positions[j,pair.name] <- 0
            
          }
        }#else: do nothing
        if(sig["bto"]){
          if(!(this.p>0) && num.shrs["s.shares"]>0){ #flat or short (but shouldn't be short here)
            ##        buy stock, sell factors #opening long
            positions[j,this.name] <- positions[j,this.name] + num.shrs["s.shares"]
            positions[j,pair.name] <- positions[j,pair.name] + num.shrs["b.shares"]
            cash <- cash - sum(price.s.b * num.shrs)
            s.action[k,i] <- 1
            if(this.p<0 && warn){ cat(paste("\nBTO tripped while short, on day",i,"for stock",this.name,"\n")); if(stop.on.wrn) stop() }
            if((debug && this.name==debug.name)||dbg.transactions)
              cat(i,this.name,"BTO: 'acquiring'",num.shrs," paying ",sum(price.s.b * num.shrs),"cash=",cash,"\n")
          }# else: do nothing #already long
        }
        if(sig["close.long"]){
          if(this.p>0){
            ##          sell stock, buy factors #closing long
            cash <- cash +
              sum(price.s.b*c(positions[j,this.name],positions[j,pair.name]))
            s.action[k,i] <- 1
            if((debug && this.name==debug.name)||dbg.transactions)
              cat(i,this.name,"CLOSING LONG: paying ",-sum(price.s.b*c(positions[j,this.name],positions[j,pair.name])),"cash=",cash,"\n")
            positions[j,this.name] <- 0
            positions[j,pair.name] <- 0

          }# else: do nothing
        }
      }
    }
  }
  return(list(cash=cash,nav=nav,equity=equity,log=list(actions=s.action)))
}
