#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
source("../scraping_site.R")
source("../options.R")
library(shiny)
library(dplyr)

library(ggplot2)
library(pastecs)

# Define UI for application that draws a histogram
ui <- fluidPage(style = "border: 30px solid white",
   
   # Application title
   titlePanel("Structured Product Creator: "),
   
   # Sidebar with a slider input for number of bins 
      sidebarPanel(
        fluidRow(12,
        selectInput("dir", "Option Direction", 
                    choices=c("long","short"))
        
        ,
         selectInput("type", "Option Type", 
                      choices=c("call","put"))

        ,
        selectInput("expDate", "Option Type", 
                    choices=getExpDatesEurex())
        
        ,
         sliderInput("strike",
                     "Option Strike:",
                     min = 2000,
                     max = 4000,
                     value = 3000,step = 50)
      )),
      
      # Show a plot of the generated distribution
      mainPanel(
         column(6,
         plotOutput("plot")),
         column(6,
         plotOutput("plot4")),
         column(6,
         plotOutput("plot2")),
         column(6,
         plotOutput("plot3"))
      ),
   
       fluidRow(
         column(6, titlePanel("Call"),
         DT::dataTableOutput("callTable")),
         column(6,titlePanel("Put"),
         DT::dataTableOutput("putTable")),
         column(3,titlePanel("Call"),DT::dataTableOutput("callTableSumm")),
         column(3,titlePanel("Put"),DT::dataTableOutput("putTableSumm"))
       )
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$plot <- renderPlot({
      price = 0
      if(input$type=="call")price = as.numeric(get(input$expDate,eurex_stoxx50_call) %>% filter(Strike_price==input$strike) %>% select(Ask_price))
      if(input$type=="put")price = as.numeric(get(input$expDate,eurex_stoxx50_put) %>% filter(Strike_price==input$strike) %>% select(Ask_price))

      a <- createOption(price,type = input$type, strike = input$strike,direction = input$dir )
      
      plotOption(a)
   })
   output$plot2 <- renderPlot({
      eurex_stoxx50_call_temp <- get(input$expDate,eurex_stoxx50_call) %>% filter(open_interest > 5000)
      barplot(names.arg = eurex_stoxx50_call_temp$Strike_price,eurex_stoxx50_call_temp$open_interest,main="Call Option Open Interest vs Strike")
      
      output$callTable <- DT::renderDataTable(DT::datatable({
        as.data.frame(eurex_stoxx50_call_temp)
      },rownames = FALSE))
      
      output$callTableSumm <- DT::renderDataTable(DT::datatable({
        as.data.frame(stat.desc(eurex_stoxx50_call_temp[6]))
      },rownames = TRUE))
   })
   output$plot3 <- renderPlot({
     eurex_stoxx50_put_temp <- get(input$expDate,eurex_stoxx50_put) %>% filter(open_interest > 5000)
     barplot(names.arg = eurex_stoxx50_put_temp$Strike_price,eurex_stoxx50_put_temp$open_interest,main="Pall Option Open Interest vs Strike")
     
     output$putTable <- DT::renderDataTable(DT::datatable({
       as.data.frame(eurex_stoxx50_put_temp)
     },rownames = FALSE,filter = 'bottom'))
     
     output$putTableSumm <- DT::renderDataTable(DT::datatable({
       as.data.frame(stat.desc(eurex_stoxx50_put_temp[6]))
     },rownames = TRUE))
   })
   output$plot4 <- renderPlot({
     eurex_df_temp <- get(input$expDate,eurex_stoxx50_call)
     eurex_df_temp$open_interest <- (get(input$expDate,eurex_stoxx50_call)$open_interest*get(input$expDate,eurex_stoxx50_call)$Ask_price
                                     - get(input$expDate,eurex_stoxx50_put)$open_interest*get(input$expDate,eurex_stoxx50_put)$Ask_price)
     eurex_df_temp <- eurex_df_temp %>% filter(open_interest > 10000)
     rows <- nrow(eurex_df_temp)
     if(rows>0)plot(x = eurex_df_temp$Strike_price,y = eurex_df_temp$open_interest,pch=21, col="red")
     else plot(x = 0,y = 0,main = "Not enough data for plotting")
     
   })
   
   
   
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

