source("scraping_site.R")

expDates <- getExpDatesEurex()   

eurex_stoxx50_call <- list()
eurex_stoxx50_put <- list()


    
    for ( i in expDates ){
      eurex_df_call = scrape("Call",i)
      eurex_df_put= scrape("Call",i)
      eurex_stoxx50_call <- append(eurex_stoxx50_call,list(eurex_df_call))
      eurex_stoxx50_put <- append(eurex_stoxx50_put,list(eurex_df_put))
      print(".")
    }
    names(eurex_stoxx50_call) <- paste0(expDates,"_stoxx50","_call")


