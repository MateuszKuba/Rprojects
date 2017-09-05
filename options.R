createOption <- function( buyingPrice, type, strike, direction ){
  return(list(buyingPrice=buyingPrice, type=type, strike=strike, direction=direction))
}


## oblicza zysk dla pojedynczej opcji dla danej zapadalnosci

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


## zwraca tablice z wartosciami zysku/straty dla danej opcji w zakresie od from to ( zapadalnosc ) uzywajac funkcji calculateProfit()

calculateProfitArray <- function(opcja,from,to,step){
  totalProfit <- vector()
  a <- seq(from = from,to = to, by = step)
  for ( i in a ) {
            profit <- calculateProfit(opcja,i)
            totalProfit <- c(totalProfit,profit)
  }
  return(totalProfit)
}

calculateProfitArrayForManyOptions <- function(listaOpcji,from,to,step){
  totalProfit <- 0
  dlugoscListy <- lengths(listaOpcji)
  if( dlugoscListy[1] == 4) for ( i in head(listaOpcji,-1)) totalProfit <- totalProfit + calculateProfitArray(i,from,to,step)
  else return(calculateProfitArray(listaOpcji,from,to,step))
  
  return(totalProfit)
}

## uzywa funkcji calculateProfitArrayForManyOptions, roznica jest dodatkowo taka ze sumuje wartosci pojedynczych opcji w calosc

calculateProfitVolumeForManyOptions <- function(listaOpcji,from = 2000,to = 4000,step = 1){
  
  totalProfit <- calculateProfitArrayForManyOptions(listaOpcji,from,to,step)
  sum(totalProfit)
  
}



## przeszuka wszystkie zapadalnosci i wszystkie strike a nastepnie obliczy pole powierzchni wskazane w funkcji
## calculateProfitVolumeForManyOptions i poda parametry opcji oraz jej date zapadalnosci ktora ma te pole
## najwieksze

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


## przeszuka wszystkie zapadalnosci i znajdzie taka pare opcji wskazana w parametrze k ktora da lacznie najwiekszy zysk
## a nastepnie zwroci liste w postaci tych opcji wraz z ich data zapadalnosci, daty zapadalnosci nie sa mieszane
## zmiana jest taka, ze parametry: type, dir, strike sa losowane i wybierane jest "iterations" opcji z kazdej zapadalnosci

optionsStrategy2 <- function(expirations,from,to,step=50,liczbaOpcji = 3,iterations=10){
  
  expirations<- expDates
  type <- c("call","put")
  dir <- c("long","short")
  strike <- seq(from,to,step)
  
  max = -10000000
  maxOp = 0
  expir = 0
  
  a <- list()
  
  for ( i in expirations){
    
    for ( j in 1:iterations){
      
      listaOpcji <- list()
      k <- 1 ## parametr k narazie ustawiony sztywno na 2
      while( k < liczbaOpcji + 1 ){
        
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
        k = k + 1
          }
        }
      } ## k in 1:2
      
      if(!is.na(price)){
        if ( price !=0 ){
          profitA <- calculateProfitVolumeForManyOptions(a,from,to,1)
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
  return(c(maxOp,list(expir)))
  
}


plotOption <- function(opcja,from = 2000,to = 4000,step = 1){
  totalProfit <- calculateProfitArrayForManyOptions(opcja,from,to,step)
  plot(from:to,totalProfit)
}


a <- optionsStrategy2("201806",1500,4500,liczbaOpcji = 5)
print(a)

plotOption(a,1500,4500,1)
