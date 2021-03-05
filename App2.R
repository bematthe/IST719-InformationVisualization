###########10.1.4: Shiny: App 2###########
library(shiny)
fname <- "C:\\Users\\becky\\Desktop\\art.csv"

artServer <- function(input, output){
  art <- read.csv(fname, header = TRUE, stringsAsFactors = FALSE)
  
  output$yearlyReceipts <- renderPlot({
    #plotOutput("yearlyReceipts")
    print("Inside yearlyReceipts")
    my.title <- "Number of Sales per Year"
    barplot(table(art$year), main = my.title, border = "white"
            , col = "chartreuse4")
  })
}
  
artUI <- fluidPage(
  titlePanel("ACME Art Company Dashboard"),
  mainPanel(
    plotOutput("yearlyReceipts")
  )
)

shinyApp(uI = artUI, server = artServer)

