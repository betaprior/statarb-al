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

stocks <- sig.list.test$tickers
##portfolio.stocks <- c("~S~")
portfolio.stocks <- stocks
signals <- rev(sig.list.test[[1]]$sig.dates)
dates <- names(signals)

sector.etfs <-
  c("HHH","IYR","IYT","OIH","RKH","RTH","SMH","UTH","XLE","XLF","XLI","XLK","XLP","XLV","XLY")

load("univ1.mid.price.RObj") # loads univ1.master.price
current.univ.price <- univ1.master.price[names(univ1.master.price) %in% c(stocks,sector.etfs)]
current.univ.price <- reverse.rows(current.univ.price[1:length(dates),])

  
positions <-
  as.data.frame(matrix(0,length(stocks),length(c(portfolio.stocks,sector.etfs))))
names(positions) <- c(portfolio.stocks,sector.etfs)
row.names(positions) <- stocks

cash <- 100000
lambda <- 2/length(stocks)

equity <- rep(0,length(dates))


## final sanity checks, and let's go!
stopifnot(all(row.names(current.univ.price)==dates))

long.shr.amounts <- function(rat,tot,S,b){
  c(s.shares=round(rat*tot/(S*(rat-1))), b.shares=round(tot/(b*(rat-1))))
}

for(i in seq(along=dates)){
  net.positions <- apply(positions,2,sum)
  nav <- sum(current.univ.price[i,names(net.positions)]*net.positions)
  equity[i] <- cash + nav
  for(j in seq(along=row.names(positions))){
    sig <- decode.signals(signals[[i]][j,])
    params <- decode.params(signals[[i]][j,])
    betas <- decode.betas(signals[[i]][j,])
    this.name <- row.names(positions)[j]
    this.p <- positions[j,this.name]
    pair.name <- tickers.classified[this.name,]$SEC_ETF
    if(sig["model.valid"]){
      tot <- lambda*equity[i] # investment amount
      rat <- 1/betas
      price.s.b <- c(current.univ.price[i,this.name], current.univ.price[i,pair.name])
      num.shrs <- long.shr.amounts(rat,tot, price.s.b[1],
                                    price.s.b[2])*c(1,-1)
      if(sig["sto"]){
        if(!(this.p<0)){ #flat or long (but shouldn't be long here)
          ##	sell stock, buy factors #opening short (if flat before, as we should
          ##be)
          ## num.shrs has the correct signs for long, this is short though
          num.shrs <- -num.shrs
          positions[j,this.name] <- positions[j,this.name] + num.shrs["s.shares"]
          positions[j,pair.name] <- positions[j,pair.name] + num.shrs["b.shares"]
          cash <- cash - price.s.b * num.shrs
          if(this.p>0) warning("STO tripped while long, weird jump?")
        }
      } #else do nothing #already short 
      if(sig["close.short"]){
        if(this.p<0){
          ## buy stock, sell factors #closing short
          cash <- cash + price.s.b*c(positions[j,this.name],positions[j,pair.name])
          positions[j,this.name] <- 0
          positions[j,pair.name] <- 0
          
        }
      }#else: do nothing
      if(sig["bto"]){
        if(!(this.p>0)){ #flat or short (but shouldn't be short here)
          ##        buy stock, sell factors #opening long
          positions[j,this.name] <- positions[j,this.name] + num.shrs["s.shares"]
          positions[j,pair.name] <- positions[j,pair.name] + num.shrs["b.shares"]
          cash <- cash - price.s.b * num.shrs
          if(this.p<0) warning("BTO tripped while short, weird jump?")
        }# else: do nothing #already long
      }
      if(sig["close.long"]){
        if(this.p>0){
##          sell stock, buy factors #closing long
          cash <- cash + price.s.b*c(positions[j,this.name],positions[j,pair.name])
          positions[j,this.name] <- 0
          positions[j,pair.name] <- 0

        }# else: do nothing
      }
    }
  }
}

