###########10.1.3: Shiny: App 1###########

library(shiny)
library(lubridate)

server<- function(input, output){
  
}

ui<- fluidPage(
  mainPanel(paste("Becky's Shiny App At", now()))
)
shinyApp(ui, server)