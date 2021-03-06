#Becky Matthews-Pease
#IST 719: Information Visualization
#Homework 1, Week 2


## Part I
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(vioplot)

winners <- read.csv("C:\\Users\\becky\\Downloads\\hot-dog-contest-winners.csv", header = TRUE, stringsAsFactors = FALSE)
view(winners)


#Figure 4-11: Winners by Country
fill_colors <- c()
for (i in 1: length(winners$Country)){
  if(winners$Country[i] == "United States"){
    fill_colors <-c(fill_colors, "#821122")
  }else{
    fill_colors<-c(fill_colors, "#cccccc")
  }
}
barplot(winners$Dogs.eaten, names.arg=winners$Year, col = fill_colors, border=NA, space=0.3, main = "Nathan's Hot Dog Eating Contest Results, 1980-2010", xlab = "Year", ylab = "Hot Dogs And Buns (HCB) Eaten", )


#Figure 4-22: Contest Results
hot_dog_places <- read.csv("C:\\Users\\becky\\Downloads\\hot-dog-places.csv", header = TRUE, stringsAsFactors = FALSE)
names(hot_dog_places) <- c("2000", "2001", "2002", "2003", "2004",    "2005", "2006", "2007", "2008", "2009", "2010")
hot_dog_matrix <- as.matrix(hot_dog_places)
barplot(hot_dog_matrix, border=NA, space=0.25, ylim=c(0, 200),    xlab="Year", ylab="Hot dogs and buns (HDBs) eaten",    main="Hot Dog Eating Contest Results, 1980-2010")


#Figure 4-28: Subscribers
subscribers <- read.csv("http://datasets.flowingdata.com/flowingdata_subscribers.csv", sep=",", header=TRUE)
subscribers

plot(subscribers$Subscribers, type="p", ylim=c(0, 300000))
plot(subscribers$Subscribers, type="h", ylim=c(0, 300000))
plot(subscribers$Subscribers, type="h", ylim=c(0, 300000), xlab = "Day", ylab = "Subscribers")
points(subscribers$Subscribers, pch = 19, col = "black")


#Figure 4-34: Timeseries
population <- read.csv("http://datasets.flowingdata.com/world-population.csv", sep=",", header=TRUE)
plot(population$Year, population$Population, type="l",  ylim=c(0, 7000000000), xlab="Year", ylab="Population")


#Figure 4-43: Step Chart
postage <- read.csv("http://datasets.flowingdata.com/us-postage.csv", sep = ",", header = TRUE)
plot(postage$Year, postage$Price, type = "s", main = "US Postage Rates for Letters, First Ounce, 1991-2010", xlab = "Year", ylab = "Postage Rate (Dollars)")



## Part II

art <- read.csv("~/A-Grad School/IST 719 - Information Visualization/04-DataSets/art.csv",
                header = TRUE, stringsAsFactors = FALSE)
view(art)

par(mfrow=c(2,2))

hist(art$total.sale, col = "green", main = "Distribution of Total Sales", ylab = "Total Sales (in USD)")
boxplot(art$total.sale, col = "green", main = "Distribution of Total Sales", ylab = "Total Sales (in USD)")

artDrawing <- art[art$paper == "drawing", ]
vioplot(artDrawing$total.sale, col = "green", main = "Distribution of Total Sales for Drawing Paper", ylab = "Total Number of Sales (in USD)")

artWatercolor <- art[art$paper == "watercolor", ]
vioplot(artWatercolor$total.sale, col = "green", main = "Distribution of Total Sales for Watercolor Paper", ylab = "Total Number of Sales (in USD)")


##Part III

#1. Is there a replationship between the unit price of art goods and their units sold?"
plot(art$unit.price, art$units.sold, pch=18, cex = 3, main = "Unit Price x Units Sold", col = "darkgreen")
#The lower the unit price, the more total units are sold. Most fall between a unit price of <$35, with one outlier closer to $100.


#2. Does the company sell more units of watercolor or drawing paper?
Watercolor <- sum(art$paper == "watercolor")
Drawing <- sum(art$paper == "drawing")
df <- data.frame(Watercolor, Drawing)
datafr <- as.matrix(df)
barplot(datafr, col = "darkred", xlab = "Paper Type", ylab = "Number of Units Sold")
#The watercolor paper has sold nearly 2,000 more units than the drawing paper


#3. Does the Company bring in more money(income) selling drawing paper or watercolor paper?
df2 <- tapply(art$total.sale, list(art$paper), sum)
df2 <- as.matrix(df2)
dollar(df2)
barplot(tapply(art$total.sale, list(art$paper), sum))
plot(df2)
#More money was made selling watercolor that drawing paper. Because I was unable to convert the numbers on the y-axis to USD format, I also created a matrix showing these totals to confirm.


#Combine Plots
par(mfrow = c(3,1))
plot(art$unit.price, art$units.sold, pch=18, cex = 3, main = "Unit Price x Units Sold", col = "darkgreen")
barplot(datafr, col = "darkred", xlab = "Paper Type", ylab = "Number of Units Sold")
barplot(tapply(art$total.sale, list(art$paper), sum))



