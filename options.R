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


calculateProfitArray <- function(opcja,from,to,step){
  totalProfit <- vector()
  a <- seq(from = from,to = to, by = step)
  for ( i in a){
    profit <- calculateProfit(opcja,i)
    totalProfit <- c(totalProfit,profit)
  }
  return(totalProfit)
}

calculateProfitArrayForManyOptions <- function(listaOpcji,from,to,step){
  totalProfit <- 0
  for ( i in listaOpcji ){
    if(typeof(i)=="list"){totalProfit <- totalProfit + calculateProfitArray(i,from,to,step)}
    else {return(calculateProfitArray(listaOpcji,from,to,step))}
  }
  return(totalProfit)
}

calculateProfitVolumeForManyOptions <- function(listaOpcji,from = 2000,to = 4000,step = 1){
  
  totalProfit <- calculateProfitArrayForManyOptions(listaOpcji,from,to,step)
  sum(totalProfit)
  
}

optionsStrategy <- function(expirations){
  

  expirations = expDates
  type <- c("call","put")
  dir <- c("long","short")
  strike <- seq(2000,4000,50)
  max = -10000000
  maxOp = 0
  expir = 0
  for ( k in expirations){
    for ( j in dir){
      for ( i in strike){
        for( l in type){
          price = 0
          if(l=="call")price = as.numeric(get(k,eurex_stoxx50_call) %>% 
                                                     filter(Strike_price==i) %>% 
                                                     select(Ask_price))
          if(l=="put")price = as.numeric(get(k,eurex_stoxx50_put) %>%
                                                    filter(Strike_price==i) %>% 
                                                    select(Ask_price))
          
          
          if(!is.na(price)){
              if ( price !=0 ){
                  a <- createOption(price,l,i,j)
                      profitA <- calculateProfitVolumeForManyOptions(a,2000,4000,1)
                      if(profitA>max){
                        max = profitA
                        maxOp = a
                        expir = k
                      }
                      cat(paste("."," "))
              }
          }
        }
      }
    }
  }
  
  return(c(maxOp,expir))
  
}


optionsStrategy2 <- function(expirations){
  
  expirations = expDates
  type <- c("call","put")
  dir <- c("long","short")
  strike <- seq(2000,4000,50)
  
  max = -10000000
  maxOp = 0
  expir = 0
  
  a <- list()
  
  for ( i in expirations){
    
    for ( j in 1:10){
      
      listaOpcji <- list()
      k <- 2
      while( k > 0 ){
        
        type_temp <-sample(type,1)
        dir_temp <- sample(dir,1)
        strike_temp <- sample(strike,1)        
        
        if(type_temp=="call")price = as.numeric(get(i,eurex_stoxx50_call) %>% 
                                          filter(Strike_price==strike_temp) %>% 
                                          select(Ask_price))
        if(type_temp=="put")price = as.numeric(get(i,eurex_stoxx50_put) %>%
                                         filter(Strike_price==strike_temp) %>% 
                                         select(Ask_price))
        
        if(!is.na(price)){
          if ( price !=0 ){
        a[k] <- list(createOption(price,type_temp,strike_temp,dir_temp))
          }
        }
      } ## k in 1:2
      
      if(!is.na(price)){
        if ( price !=0 ){
          profitA <- calculateProfitVolumeForManyOptions(a,2000,4000,1)
          if(profitA>max){
            max = profitA
            maxOp = a
            expir = i
          }
          cat(paste("."," "))
        }
      }
    }
    
  }
  return(c(maxOp,expir))
  
}


plotOption <- function(opcja,from = 2000,to = 4000,step = 1){
  totalProfit <- calculateProfitArrayForManyOptions(opcja,from,to,step)
  plot(from:to,totalProfit)
}


a <- optionsStrategy2(expDates)
print(a)


