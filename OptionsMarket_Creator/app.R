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
   titlePanel("Structured Product Creator"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
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
                     value = 3000)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("plot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$plot <- renderPlot({
       
      #if(input$type=="call")filter(eurex_df, 'Strike Price'==input$strike)
      #if(input$type=="put")
     
      a <- createOption(150,type = input$type, strike = input$strike,direction = input$dir )
      plotOption(a)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

