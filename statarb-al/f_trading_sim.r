## Trading simulation function
## input: signals object, prices df


k <- 0
#lambda <- lambda/100


## the signals object here is assumed to be a list of dates
run.trading.simulation <- function(  signals, prices
                                   , debug=FALSE, warn=FALSE
                                   , outfile=""){
  if(outfile!="")
    if(file.exists(outfile)) { file.remove(outfile) }
  dates <- names(signals)
  for(i in seq(along=dates)){
    cat(i," ")
    net.positions <- apply(positions,2,sum)
    nav <- sum(current.univ.price[i,names(net.positions)]*net.positions)
    equity[i] <- cash + nav
    for(j in seq(along=row.names(positions))){
      this.name <- row.names(positions)[j]
      if(!(this.name %in% stocks)) next
                                        #    if(dates[i]=="20030328") { browser() }
      sig <- decode.signals(signals[[i]][j,])
      params <- decode.params(signals[[i]][j,])
      k <- match(this.name,stocks)
      s.id[k] <- this.name
      s[k,i] <- params["s"]
      s.a[k,i] <- params["a"]
      s.b[k,i] <- params["b"]
      s.varz[k,i] <- params["varz"]
      s.k[k,i] <- params["k"]
      s.sto[k,i] <- sig["sto"]
      s.bto[k,i] <- sig["bto"]
      s.close.short[k,i] <- sig["close.short"]
      s.close.long[k,i] <- sig["close.long"]
      betas <- decode.betas(signals[[i]][j,])
      s.betas[k,i] <- betas
      this.p <- positions[j,this.name]
      pair.name <- tickers.classified[this.name,]$SEC_ETF
      if(!sig["model.valid"] || !all(!is.na(current.univ.price[i,]))){
        if(debug && this.name==debug.name) cat(i,"pos:",this.p,"inv.targ:",tot,"ratio ",rat," prices: ",price.s.b," num shares: ",num.shrs,"INVALID\n",file=outfile,append=TRUE)
      }else{
        tot <- lambda*equity[i] # investment amount
        rat <- 1/betas
        price.s.b <- c(current.univ.price[i,this.name], current.univ.price[i,pair.name])
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
            if(this.p>0) if(this.name==debug.name) cat(paste("\nSTO tripped while long, on day",i,"for stock",this.name))
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
            if(this.p<0) cat(paste("\nBTO tripped while short, on day",i,"for stock",this.name))
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
