createOption <- function( buyingPrice, type, strike, direction ){
  return(list(buyingPrice=buyingPrice, type=type, strike=strike, direction=direction))
}


calculateProfit <- function(opcja,expPrice) {
  if(opcja$direction=="long"){
    if(opcja$type=="call"){
      return(max(expPrice - opcja$strike - opcja$buyingPrice,-opcja$buyingPrice))
    }
    else{ # put
      return(max(opcja$strike - expPrice - opcja$buyingPrice,-opcja$buyingPrice))
    }
  }
  else { #short
    if(opcja$type=="call"){
      return(min(opcja$strike - expPrice + opcja$buyingPrice,opcja$buyingPrice))
    }
    else {   #put
      return(min(expPrice - opcja$strike + opcja$buyingPrice,opcja$buyingPrice))
    }
  }
}


plotOption <- function(opcja,from = 2000,to = 4000,step = 1){
  totalProfit <- vector()
  a <- seq(from = from,to = to, by = step)
  for ( i in a){
    profit <- calculateProfit(opcja,i)
    totalProfit <- c(totalProfit,profit)
  }
  plot(from:to,totalProfit)
}


