##Becky Matthews-Pease
##IST: Information Visualization
##Homework 03, Week 04

library(lattice)

##Figure 4.5##
hotdogs <- read.csv("http://datasets.flowingdata.com/hot-dog-contest-winners.csv", sep = ",", header = TRUE)

hotdogs$Dogs.eaten
barplot(hotdogs$Dogs.eaten)
barplot(hotdogs$Dogs.eaten, names.arg = hotdogs$Year, col = "red", border = NA, xlab = "Year", ylab = "Hot dogs and buns (HDB) eaten")
fill_colors<- c()
for( i in 1:length(hotdogs$Country)){
  if (hotdogs$Country[i]== "United States"){
    fill_colors<- c(fill_colors, "#821122")
  } else {
    fill_colors<- c(fill_colors, "#cccccc")
  }
}
barplot(hotdogs$Dogs.eaten, names.arg = hotdogs$Year, col = fill_colors, border = NA, xlab = "Year", ylab = "Hot Dogs and Buns (HDB) Eaten")
fill_colors<- c()
for( i in 1:length(hotdogs$New.record)){
  if (hotdogs$New.record[i]==1){
    fill_colors<- c(fill_colors, "#821122")
  } else {
    fill_colors<- c(fill_colors, "#cccccc")
  }
}
barplot(hotdogs$Dogs.eaten, names.arg = hotdogs$Year, col = fill_colors, border = NA, xlab = "Year", ylab = "Hot Dogs and Buns (HDB) Eaten")


##Figure 4-21##
hot_dog_places<- read.csv("http://datasets.flowingdata.com/hot-dog-places.csv", sep=",", header = TRUE)
#head(hot_dog_places)
names(hot_dog_places)<- c("2000", "2001","2002","2003","2004","2005","2006","2007","2008","2009","2010")
hot_dog_matrix<- as.matrix(hot_dog_places)
barplot(hot_dog_matrix, border = NA, space = 0.25, ylim = c(0,200)
        , xlab = "Year", ylab = "Hot dogs and buns (HDBs) eaten"
        , main = "Hot Dog Eating Contest Results, 1980-2010")



##Figure 4-28: Subscribers##
subscribers <- read.csv("http://datasets.flowingdata.com/flowingdata_subscribers.csv", sep=",", header=TRUE)
subscribers
plot(subscribers$Subscribers)
plot(subscribers$Subscribers, type="p", ylim=c(0, 30000))
plot(subscribers$Subscribers, type="h", ylim=c(0, 30000))
plot(subscribers$Subscribers, type="h", ylim=c(0, 30000), xlab = "Day", ylab = "Subscribers")
points(subscribers$Subscribers, pch = 19, col = "black")


##Figure 4-34: Timeseries##
population <- read.csv("http://datasets.flowingdata.com/world-population.csv", sep=",", header=TRUE)
plot(population$Year, population$Population, type="l",  ylim=c(0, 7000000000), xlab="Year", ylab="Population")


##Figure 4-43: Step Chart##
postage <- read.csv("http://datasets.flowingdata.com/us-postage.csv", sep = ",", header = TRUE)
plot(postage$Year, postage$Price, type = "s", main = "US Postage Rates for Letters, First Ounce, 1991-2010", xlab = "Year", ylab = "Postage Rate (Dollars)")


##Figure 4-40: LOESS##
unemployment <- read.csv("http://datasets.flowingdata.com/unemployment-rate-1948-2010.csv", sep = ",")
unemployment[1:10,]
plot(1:length(unemployment$Value), unemployment$Value)
scatter.smooth(x=1:length(unemployment$Value), y=unemployment$Value)
scatter.smooth(x=1:length(unemployment$Value), y=unemployment$Value, ylim=c(0,11), degree = 2, col = "#CCCCCC", span = 0.5)


##
##Part 2##
##

##Figure 6-9: Scatterplot Matrix ##
crime <- read.csv("http://datasets.flowingdata.com/crimeRatesByState2005.csv", sep=",", header = TRUE)
plot(crime$murder, crime$burglary)
crime2 <- crime[crime$state != "District of Columbia",]
crime2 <- crime2[crime$state != "United States",]
plot(crime2$murder, crime2$burglary)
plot(crime2$murder, crime2$burglary, xlim = c(0,10), ylim = c(0,1200))
scatter.smooth(crime2$murder, crime2$burglary, xlim = c(0,10), ylim = c(0,1200))
plot(crime2[,2:9]
plot(crime2[,2:9], panel=panel.smooth)
pairs(crime2[,2:9], panel = panel.smooth)



##Figure 6-15: Bubble chart 
crime <- read.csv("http://datasets.flowingdata.com/crimeRatesByState2005.tsv", sep="\t", header = TRUE)
radius<-sqrt(crime$population/pi)
symbols(crime$murder, crime$burglary, circles = crime$radius)
symbols(crime$murder, crime$burglary, circles = crime$radius, inches = 0.35, fg = "white", bg = "red", xlab = "Murder Rate", ylab = "Burglary Rate")
symbols(crime$murder, crime$burglary, squares = sqrt(crime$population), inches = 0.5)
text(crime$murder, crime$burglary, crime$state, cex = 0.5)


##Figure 6-24: Histogram 
birth <- read.csv("http://datasets.flowingdata.com/birth-rate.csv")
hist(birth$X2008)
hist(birth$X2008, breaks = 5)
hist(birth$X2008, breaks = 20)


##Figure 6-32: Density plot
birth <- read.csv("http://datasets.flowingdata.com/birth-rate.csv")
birth2008 <- birth$X2008[!is.na(birth$X2008)]
d2008 <- density(birth2008)
density.default(x = birth2008)
d2008$x
d2008$y
d2008frame <- data.frame(d2008$x, d2008$y)
write.table(d2008frame, "birthdensity.txt", sep = "\t")
write.table(d2008frame, "birthdensity.txt", sep = ",", row.names = FALSE)
plot(d2008)
plot(d2008, type = "n")
polygon(d2008, col = "#821122", border = "#cccccc")


##
##Part 2##
##
sales <- ("C:\\Users\\becky\\Desktop\\sales.csv")
breaks = seq(0,1000, by = 100)
par(mfrow = c(3,2))

hist(sales[sales$year == 2010,]$income, breaks = breaks)
hist(sales[sales$year == 2011,]$income, breaks = breaks)
hist(sales[sales$year == 2012,]$income, breaks = breaks)
hist(sales[sales$year == 2013,]$income, breaks = breaks)
hist(sales[sales$year == 2014,]$income, breaks = breaks)

