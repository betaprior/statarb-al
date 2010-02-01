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
                                   , silent=FALSE, outfile="", debug.name=instr.p[1]){
  if(outfile!="")
    if(file.exists(outfile)) { file.remove(outfile) }
  signals <- rev(signals.struct$sig.dates)
  dates <- names(signals)
  stopifnot(all(dates %in% row.names(prices))) ##prices dates range
                                        # must include all signals+more
  prices <- prices[dates,] ## align the data frames
  stopifnot(all(row.names(prices)==dates))
  positions <-as.data.frame(matrix(0,length(instr.p),length(instr.q)))
  names(positions) <- instr.q;  row.names(positions) <- instr.p

  long.shr.amounts <- function(rat,tot,S,b){
    c(s.shares=round(rat*tot/(S*(rat-1))), b.shares=round(tot/(b*(rat-1)))) }
  prealloc.signal.mtx <- function(stocks,dates){
    x <- array(0.0,c(length(stocks),length(dates)))
    colnames(x) <- dates; rownames(x) <- stocks
    return(x) }

  s.action <- prealloc.signal.mtx(instr.p,dates)
  k <- 0
  lambda <- 0.01 #for single-instr debugging
  
  for(i in seq(along=dates)){
    if(!silent) { cat(i," ") }
    net.positions <- apply(positions,2,sum)
    nav <- sum(prices[i,names(net.positions)]*net.positions)
    equity[i] <- cash + nav
    for(j in seq(along=row.names(positions))){
      this.name <- row.names(positions)[j]
      if(!(this.name %in% instr.p)) next
##    if(dates[i]=="20030328") { browser() }
      sig <- decode.signals(signals[[i]][j,])
      params <- decode.params(signals[[i]][j,])
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
      betas <- decode.betas(signals[[i]][j,])
      ## s.betas[k,i] <- betas
      this.p <- positions[j,this.name]
      pair.name <- pq.classifier[this.name,]$SEC_ETF
      if(!sig["model.valid"] || !all(!is.na(prices[i,]))){
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
            if(debug && this.name==debug.name) cat("STO: 'acquiring'",num.shrs,"paying ",sum(price.s.b * num.shrs),"\n")
            if(this.p>0) { cat(paste("\nSTO tripped while long, on day",i,"for stock",this.name),"\n"); if(stop.on.wrn) stop }
          }
        } #else do nothing #already short 
        if(sig["close.short"]){
          if(this.p<0){
            ## buy stock, sell factors #closing short
            cash <- cash +
              sum(price.s.b*c(positions[j,this.name],positions[j,pair.name]))
            s.action[k,i] <- 1
            if(debug && this.name==debug.name) cat("CLOSING SHORT: paying ",-sum(price.s.b*c(positions[j,this.name],positions[j,pair.name])),"\n")
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
            if(this.p<0){ cat(paste("\nBTO tripped while short, on day",i,"for stock",this.name,"\n")); if(stop.on.wrn) stop }
            if(debug && this.name==debug.name) cat("BTO: 'acquiring'",num.shrs," paying ",sum(price.s.b * num.shrs),"\n")
          }# else: do nothing #already long
        }
        if(sig["close.long"]){
          if(this.p>0){
            ##          sell stock, buy factors #closing long
            cash <- cash +
              sum(price.s.b*c(positions[j,this.name],positions[j,pair.name]))
            s.action[k,i] <- 1
            if(debug && this.name==debug.name) cat("CLOSING LONG: paying ",-sum(price.s.b*c(positions[j,this.name],positions[j,pair.name])),"\n")
            positions[j,this.name] <- 0
            positions[j,pair.name] <- 0

          }# else: do nothing
        }
      }
    }
  }
  return(list(cash=cash,nav=nav,equity=equity,log=list(actions=s.action)))
}
