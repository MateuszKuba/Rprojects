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
  
  
  
  scrape <- function(type,expire){
    
    url <- paste0("http://www.eurexchange.com/exchange-en/products/idx/stx/blc/19068!quotesSingleViewOption?callPut=",
                  type,"&maturityDate=",expire)
    
    page <- url %>% read_html() %>% html_nodes("span") %>% html_text()
    page <- page[12:match("Total",page)-1] ## wyrzucenie smieci z przodu
    eurex = matrix(page, ncol = 18, byrow = TRUE)
    eurex_df <- as.data.frame(eurex)
    colnames(eurex_df) = eurex[1,]
    eurex_df = eurex_df[-1,]
    eurex_df = eurex_df[,c(1,6:9,16)]  # pozostaw interesujace kolumny
    names(eurex_df)[6] <- "open_interest" 
    eurex_df$open_interest <- as.integer(sub(",","",(eurex_df$open_interest)))
    
    eurex_df$'Strike price' = as.numeric(sub(",","",sub("\\..*","",eurex_df$`Strike price`)))
    names(eurex_df) <- gsub(" ","_",names(eurex_df))
    
    eurex_df$Ask_price <- as.numeric(eurex_df$Ask_price)
    eurex_df$Ask_price[is.na(eurex_df$Ask_price)] <- 0
    
    return(eurex_df)
  }
  
  
 
  
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
  
  getExpDatesEurex <- function(){
    month <- as.numeric(format(Sys.Date(), "%m"))
    year <- as.numeric(format(Sys.Date(), "%Y"))
    
    expMonths <- month:(month+6)
    expDates <- vector()
    for (i in expMonths){
      month = i
      if(i>12){
        month= i-12
        year = year + 1
      }
      if(month<10)month<-paste0("0",as.character(month))
        expDates <- c(expDates,paste0(as.character(year),as.character(month)))
    }
    return(expDates)
  }
  
  expDates <- getExpDatesEurex()   
  expiration = 201812
  eurex_df_call = scrape("Call",expiration)
  eurex_df_put = scrape("Put",expiration)
  
  scrapeFromYahoo <- function(date){
    
    require(TTR)
    symbols <- stockSymbols()
    
    require(anytime)
    as.numeric(as.POSIXct("2013-09-16 2:13:46 EST"))
    
    
    require(jsonlite)
    company_name <- paste0("https://query2.finance.yahoo.com/v7/finance/options/"+symbol)
    aapl_expDates <- fromJSON(company_name)
    aapl_expDates <- aapl_expDate$optionChain$result$expirationDates
    require(rjson)
    require(RCurl)
    raw_data <- getURL("https://query2.finance.yahoo.com/v7/finance/options/aapl?date=1501200000")
    options_data <- fromJSON(raw_data)
    options_data$optionChain$result[[1]]$options[[1]]$calls
    options_data$optionChain$result[[1]]$options[[1]]$puts
    
    
    
  }
  