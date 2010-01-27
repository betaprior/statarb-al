source("functions.r")

tickers.classified <-
  sort.data.frame(get.classified.tickers("ticker_to_sec_etf.csv"), by=~TIC)
row.names(tickers.classified) <- tickers.classified$TIC

dates.vector <- get.dates.vec("spx_ret_mtx")
dates.vector.etf <- get.dates.vec("etf_ret_mtx")
stopifnot(all(dates.vector==dates.vector.etf))
dates.vector <- rev(dates.vector)

offset.2009 <- 124 #jan 09, jan 08, etc
offset.2008 <- 377
offset.2007 <- 628
offset.2006 <- 879
offset.2005 <- 1131
offset.2004 <- 1383
offset.2003 <- 1635

date.offset <- offset.2005-offset.2009

load("sig.dbg1.RObj")
stocks <- sig.list.dbg1$tickers
stocks <- "JPM"
debug.name <- "JPM"
#stocks <- c(debug.name)
##portfolio.stocks <- c("~S~")
portfolio.stocks <- stocks
signals <- rev(sig.list.dbg1$sig.dates)
dates <- names(signals)

sector.etfs <-
  c("HHH","IYR","IYT","OIH","RKH","RTH","SMH","UTH","XLE","XLF","XLI","XLK","XLP","XLV","XLY")
sector.etfs <- sector.etfs %w/o% c("IYT") ##somehow have NAs in price records

sector.etfs <- c("XLF")

load("univ1.mid.price.RObj") # loads univ1.master.price
univ1.master.price <- univ1.master.price[-c(1621)]
univ1.master.price <- univ1.master.price[-c(1816)]
current.univ.price <- univ1.master.price[names(univ1.master.price) %in% c(stocks,sector.etfs)]
current.univ.price <- reverse.rows(current.univ.price[(1+date.offset):(length(dates)+date.offset),])

## set up a price file with simulated prices
current.univ.price.old <- current.univ.price
current.univ.price <- data.frame(XLF=current.univ.price$XLF,JPM=jpm.pr.sim,row.names=rownames(current.univ.price))

positions <-
  as.data.frame(matrix(0,length(stocks),length(c(portfolio.stocks,sector.etfs))))
names(positions) <- c(portfolio.stocks,sector.etfs)
row.names(positions) <- stocks

cash <- 100000
lambda <- 2/length(stocks)
lambda <- 0.01 #for single-instr debugging

prealloc.signal.mtx <- function(stocks,dates){
  x <- array(0.0,c(length(stocks),length(dates)))
  colnames(x) <- dates
  rownames(x) <- stocks
  return(x)
}

s.betas <- prealloc.signal.mtx(stocks,dates)
s.k <- prealloc.signal.mtx(stocks,dates)

equity <- rep(0,length(dates))
s <- array(0,c(length(stocks),length(dates)))
colnames(s) <- dates
rownames(s) <- stocks

s.a <- array(0,c(length(stocks),length(dates)))
colnames(s.a) <- dates
rownames(s.a) <- stocks
s.b <- array(0,c(length(stocks),length(dates)))
colnames(s.b) <- dates
rownames(s.b) <- stocks
s.varz <- array(0,c(length(stocks),length(dates)))
colnames(s.varz) <- dates
rownames(s.varz) <- stocks

s.action <- array(0,c(length(stocks),length(dates)))
colnames(s.action) <- dates
rownames(s.action) <- stocks
s.bto <- array(0,c(length(stocks),length(dates)))
s.sto <- array(0,c(length(stocks),length(dates)))
s.close.short <- array(0,c(length(stocks),length(dates)))
s.close.long <- array(0,c(length(stocks),length(dates)))
s.id <- rep("",length(stocks))

## final sanity checks, and let's go!
stopifnot(all(row.names(current.univ.price)==dates))
stopifnot(all(names(signals)==dates))

long.shr.amounts <- function(rat,tot,S,b){
  c(s.shares=round(rat*tot/(S*(rat-1))), b.shares=round(tot/(b*(rat-1))))
}

s.jpm <- rep(0,length(dates))
for(i in seq(along=dates)){
  this.name <- "JPM"
  pair.name <- tickers.classified[this.name,]$SEC_ETF
  params <-
    decode.params(signals[[i]][match(this.name,row.names(positions)),])
  s.jpm[i] <- params["s"]
}

## #png(filename = "jpm.sig.png", width = 1000, height = 500)
## plot(as.numeric(s),type='l')
## thresholds=c(sbo=1.25,sso=1.25,sbc=0.75,ssc=0.5,kmin=8.4)
## abline(h=-thresholds["sbo"],lty=2)
## abline(h=thresholds["sso"],lty=2)
## abline(h=thresholds["sbc"],lty=2)
## abline(h=-thresholds["ssc"],lty=2)
## lines(-as.numeric(s.bto)*abs(thresholds["sbo"]),col=2)
## lines(as.numeric(s.sto)*abs(thresholds["sso"]),col=3)
## lines(as.numeric(s.close.short)*abs(thresholds["sbc"]),col=4)
## lines(-as.numeric(s.close.long)*abs(thresholds["ssc"]),col=5)
## #dev.off()

k <- 0
#lambda <- lambda/100
debug <- FALSE
#debug.name <- "AA"
outfile <- "jpm.xlf.f.tmp"
outfile <- ""
warn <- FALSE
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
