source("scraping_site.R")

expDates <- getExpDatesEurex()   

eurex_stoxx50_call <- list()
eurex_stoxx50_put <- list()
expDates <- expDates[expDates!="201902"]

    
    for ( i in expDates ){
      print(i)
      eurex_df_call = scrape("Call",i)
      eurex_df_put= scrape("Put",i)
      eurex_stoxx50_call <- append(eurex_stoxx50_call,list(eurex_df_call))
      eurex_stoxx50_put <- append(eurex_stoxx50_put,list(eurex_df_put))
      print(".")
    }
    names(eurex_stoxx50_call) <- expDates
    names(eurex_stoxx50_put) <- expDates


