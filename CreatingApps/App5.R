library(shiny)
fname <- "C:\\Users\\becky\\Desktop\\art.csv"

artServer <- function(input, output){
  art<-read.csv(fname, header = TRUE, stringsAsFactors = FALSE)
  watercolor.col <- "cadetblue1"
  drawing.col <- "antiquewhite"
  #plotOutput("yearlyReceipts")
  print("yearlyReceipts:: start")
  output$yearlyReceipts <- renderPlot({
    my.title <- "Number of Sales per Year"
    barplot(table(art$year), main = my.title, border = "white", col = "chartreuse4")
  })
  output$storePaper <- renderPlot({
    my.title <- "Inside storePaper"
    if(input$store!= "None"){
      print(paste("storePaper:: store:", input$store))
      sub.index <- which(art$store == input$store)
      tmp.data <- art[sub.index,]
      if(input$year != "All"){
        print(paste("storePaper:: year:", input$year))
        sub.index.2 <- which(tmp.data$year==as.numeric(input$year))
        tmp.data = tmp.data[sub.index.2,]
      }
      par(mfrow=c(1,2))
      sales.by.paper <- tapply(tmp.data$total.sale, list(tmp.data$paper), sum)
      barplot(sales.by.paper, beside = TRUE, main = "Income by paper type", col = c(watercolor.col,drawing.col), border = NA)
      sales.by.rep <- tapply(tmp.data$total.sale, list(tmp.data$rep), sum)
      pie(sales.by.rep, col = terrain.colors((length(sales.by.rep))), border = NA)    }
    
    
  })
  output$storeEmployee <- renderPlot({
    print("storeEmployee:: start")
    if(input$store!= "None"){
      print(paste("storeEmployee:: store:", input$store))
      sub.index <- which(art$store == input$store)
      
      tmp.data <- art[sub.index,]
      if(input$year != "All"){
        print(paste("storeEmployee:: year:", input$year))
        sub.index.2 <- which(tmp.data$year==as.numeric(input$year))
        tmp.data = tmp.data[sub.index.2,]
      }
      par(mfrow = c(1,2))
      sales.by.paper <- tapply(tmp.data$total.sale, list(tmp.data$paper), sum)
      barplot(sales.by.paper, beside = TRUE, main = "Income by Paper Type", col = c(watercolor.col,drawing.col), border = NA)
      sales.by.rep <- tapply(tmp.data$total.sale, list(tmp.data$rep), sum)
      pie(sales.by.rep, col=terrain.colors((length(sales.by.rep))), border = NA)    }
    
    
  })
}
artUI <- fluidPage(
  titlePanel("ACME Art Company Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      plotOutput("yearlyReceipts"),
      selectInput("store", "Select Store:", choices = c("None", "Portland","Syracuse", "Davenport", "Dublin")),
      selectInput("year", "Select Store:", choices = c("All", "2012","2013", "2014", "2015"))
      
    ),
    mainPanel(
      plotOutput("storePaper"),
      plotOutput("storeEmployee")
    )
  )
)
shinyApp(ui = artUI, server = artServer)
