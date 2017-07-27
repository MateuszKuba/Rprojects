  options(stringsAsFactors = FALSE)
  
  require(rvest)
  
  scrapeJSSite <- function(type,expire){
    
    url <- paste0("http://www.eurexchange.com/exchange-en/products/idx/stx/blc/19068!quotesSingleViewOption?callPut=",
                  type,"&maturityDate=",expire)
    
    lines <- readLines("scrape_final.js")
    lines[1] <- paste0("var url ='",url,"';")
    writeLines(lines,"scrape_final.js")   ## zapis do pliku scrape_final.js do pierwszej linijki by
                                          ##skrypt js wiedzial co robic
    
    system("phantomjs scrape_final.js")
    
    pg <- read_html("1.html")
    
    table <- pg %>% html_node("tableWrapper")
    
    return(pg)
  }
  
  dfCols_toDouble <- function(df){
    for(i in ncol(df))df[,i] < as.double(df[,i])
    return(df)
  }
  
  scrape <- function(type,expire){
    
    url <- paste0("http://www.eurexchange.com/exchange-en/products/idx/stx/blc/19068!quotesSingleViewOption?callPut=",
                  type,"&maturityDate=",expire)
    
    page <- url %>% read_html() %>% html_nodes("span") %>% html_text()
    page <- page[12:match("Total",page)-1] ## wyrzucenie smieci z przodu
    eurex = matrix(page, ncol = 18, byrow = TRUE)
    eurex_df <- as.data.frame(eurex)
    colnames(eurex_df) = eurex[1,]
    eurex_df = eurex_df[-1,]
    eurex_df = eurex_df[,c(1,6:9,17)]  # pozostaw interesujace kolumny
    
    eurex_df <- dfCols_toDouble(eurex_df)
    
    return(eurex_df)
  }
  
  
  x <- -100:100
  y <- -100:100
  
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
  
 
  