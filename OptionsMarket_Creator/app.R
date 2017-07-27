#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
source("../scraping_site.R")
library(shiny)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Structured Product Creator: "),
   
   # Sidebar with a slider input for number of bins 
   
      column(12,
        selectInput("dir", "Option Direction", 
                    choices=c("long","short"))
        
        ,
         selectInput("type", "Option Type", 
                      choices=c("call","put"))

        ,
         sliderInput("strike",
                     "Option Strike:",
                     min = 2000,
                     max = 4000,
                     value = 3000,step = 50)
      ),
      
      # Show a plot of the generated distribution
      fluidRow(
         column(6,
         plotOutput("plot")),
         column(6,
         plotOutput("plot4")),
         column(6,
         plotOutput("plot2")),
         column(6,
         plotOutput("plot3"))
      )
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$plot <- renderPlot({
      price = 0
      if(input$type=="call")price = as.numeric(eurex_df_call %>% filter(Strike_price==input$strike) %>% select(Ask_price))
      if(input$type=="put")price = as.numeric(eurex_df_put %>% filter(Strike_price==input$strike) %>% select(Ask_price))
     
      a <- createOption(price,type = input$type, strike = input$strike,direction = input$dir )
      plotOption(a)
   })
   output$plot2 <- renderPlot({
      eurex_df_call_temp <- eurex_df_call %>% filter(open_interest > 10000)
      plot(x = eurex_df_call_temp$Strike_price,y = eurex_df_call_temp$open_interest)
   })
   output$plot3 <- renderPlot({
     eurex_df_put_temp <- eurex_df_put %>% filter(open_interest > 10000)
     plot(x = eurex_df_put_temp$Strike_price,y = eurex_df_put_temp$open_interest)
   })
   output$plot4 <- renderPlot({
     eurex_df_temp <- eurex_df_call
     eurex_df_temp$open_interest <- (eurex_df_call$open_interest*eurex_df_call$Ask_price
                                     - eurex_df_put$open_interest*eurex_df_put$Ask_price)
     eurex_df_temp <- eurex_df_temp %>% filter(open_interest > 10000)
     plot(x = eurex_df_temp$Strike_price,y = eurex_df_temp$open_interest,pch=21, col="red")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

