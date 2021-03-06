#Becky Matthews_Pease
#IST 719: Information Visualization
#Lab Week 05


tweet <- file.choose()
tweets <- read.csv(tweet , header = TRUE,stringsAsFactors = FALSE ,quote = "\"")
View(tweets)

my.media <- tweets$media
table(my.media)
my.media[my.media == ""] <- "text only"
my.media <- gsub("\\|photo", "", my.media) 

pie(100* round(table(my.media)/sum(table(my.media)), 4)) # save pie chart 7.09 * 4.67

tweets$created_at[1:3]

conversion.string <- "%a %b %d %H:%M:%S +0000 %Y"

tmp <- strptime(tweets$created_at[1:3],conversion.string)
class(tmp) #POSIX comes from UNIX

is.na(tmp)
any(is.na(tmp))
tmp <- na.omit(tmp)

#######Dates##########
rm(tmp)
tweets$date<- strptime(tweets$created_at,conversion.string)

tmp <-"10AM and 27 minutes, on June 22, 1999"
str
strptime(tmp, "%H%p and %M minutes, on %B %d, %Y")

rm(tmp)
tweets$date<- strptime(tweets$created_at,conversion.string)

min(tweets$date)
max(tweets$date)
range(tweets$date)
summary(tweets$date)

difftime(min(tweets$date), max(tweets$date))
difftime(min(tweets$date), max(tweets$date), units ="min")
difftime(min(tweets$date), max(tweets$date), units ="weeks")

library(lubridate)
wday(tweets$date[1:3], label = TRUE, abbr = TRUE)
barplot(table(wday(tweets$date, label = TRUE, abbr = TRUE)))

#Convert Time
tmp <- tweets$user_utc_offset
tweets$date[7:10] + tmp[7:10]

known.times <- tweets$date + tweets$user_utc_offset
index <- which(is.na(known.times))
known.times <- known.times[-index]
barplot(table(hour(known.times)))

start.date <- as.POSIXct("2016-06-24 23:59:19")
end.date <- as.POSIXct("2016-06-26 00:00:00")

index <- which((tweets$date > start.date) & (tweets$date < end.date))
tweets.25th <- tweets$date[index]

#Returns date as a string
format.Date(tweets.25th, "%Y%m%d%H%M") 

tmp.date <- as.POSIXct(strptime(format.Date(tweets.25th, "%Y%m%d%H%M"), "%Y%m%d%H%M"))

plot(table(tmp.date))
length(table(tmp.date))
24 * 60

tmp.tab <- table(tmp.date)

plot(as.POSIXct(names(tmp.tab)), as.numeric(tmp.tab))
plot(as.POSIXct(names(tmp.tab)), as.numeric(tmp.tab), type ="h")
class(names(tmp.tab))
 
x <- seq.POSIXt(from = start.date + 1, to = end.date - 1, by ="min")
length(x)
y <- rep(0, length(x))
y[match(names(tmp.tab), as.character(x))] <- as.numeric(tmp.tab)
y

plot(x,y,type = "p", pch = 16, cex = .4)
plot(x,y,type = "p", pch = ".", cex = .4)
plot(x,y,type = "l")

barplot(table(hour(known.times)))
plot(x,y, type = "p", pch = 16, cex = .4)
################SAVEPLOT############################


#############################
tweets$text[5:10]
library(stringr)                                
tags<- str_extract_all(tweets$text, "#\\S+", simplify = FALSE)                                          
tags<- tags[lengths(tags)>0]                                          
tags<- unlist(tags)
tags <- tolower(tags)
tags<- gsub("#|[[:punct:]]", "", tags)
tag.tab<- sort(table(tags), decreasing = TRUE)
tag.tab[1:10]

zap<- which(tag.tab < 3)
tag.tab<-tag.tab[-zap]
boxplot(as.numeric(tag.tab))
plot(as.numeric(tag.tab))

df <- data.frame(words = names(tag.tab), count = as.numeric(tag.tab), stringsAsFactors = FALSE)
par(mfrow=c(3,3))
plot(df$count, main =" raw")
y <- df$count/max(df$count)
plot(y, main = "0-1")
plot(df$count^2, main = "^2")
plot(df$count^(1/2), main = "^(1/2)")
plot(df$count^(1/2), main = "^(1/5)")
plot(log10(df$count), main = "log10")
log10(c(1, 10, 100, 1000, 1237, 10000))
plot(log10(df$count), main = "log10")

##########################################
library(wordcloud)
gc()
myPal <- colorRampPalette(c("red", "gold","orange"))

df
wordcloud(df$words, df$count, scale = c(5, .5), min.freq = 1
          , max.words = Inf, random.order = FALSE
          , random.color = FALSE, ordered.colors = TRUE)

myPal <- colorRampPalette(c("red", "gold","orange"))
index <- which(df$count > 9)          
par(mar=c(0,0,0,0), bg = "black")
my.counts<- (df$count[index]^(1/2))
wordcloud(df$words[index], my.counts, scale = c(4, .4), min.freq = 1
          , max.words = Inf, random.order = FALSE
          , random.color = FALSE, ordered.colors = TRUE
          , rot.per = 0, colors = myPal(length(df$words[index])))
##########################################
my.dir <- "C:\\Users\\becky\\Desktop\\"
sales <- read.csv(file = paste0(my.dir, "sales.csv")
                  , header = TRUE
                  , stringsAsFactors = FALSE)

library(alluvial)
dat <- as.data.frame(Titanic, stringsAsFactors = FALSE)
alluvia(dat[,1:4], freq = dat$Freq)


          


##############3
#Alluvual plot & treemap plots

sale<- file.choose()
sales<- read.csv(sale, header = TRUE, stringsAsFactors = FALSE)
library(alluvial)
dat<- as.data.frame(Titanic, stringsAsFactors = FALSE)
alluvial(dat[,1:4], freq= dat$Freq, bg="white")
alluv.df<- aggregate(sales$units.sold, list(sales$rep.region, sales$type),sum)
colnames(alluv.df)<- c("reg", "type", "units.sold")
alluvial(alluv.df[ , 1:2], freq = alluv.df$units.sold)
my.cols<- rep("gold", nrow(alluv.df))
my.cols[alluv.df$type == "red"]<- "red"
alluvial(alluv.df[ , 1:2] , freq = alluv.df$units.sold, col=my.cols)
alluvial(alluv.df[ , 1:2 ], freq = alluv.df$units.sold, col=ifelse(alluv.df$type=="red","red","gold"))
options(stringsAsFactors = FALSE)

alluv.df<- aggregate(sales$units.sold, list(sales$rep.region, sales$type, sales$wine),sum)
colnames(alluv.df)<- c("reg", "type", "wine", "units.sold")
alluvial(alluv.df[ , 1:3 ], freq = alluv.df$units.sold, col=ifelse(alluv.df$type=="red","red","gold"))

library(RColorBrewer)
library(treemap)

treemap(sales, index = c("rep.region"), vSize = "income", palette = "Greens", fontsize.labels = 18 )
treemap(sales, index = c("rep.region"), vSize = "income", vColor = "units.sold", type= "dens",palette = "Greens", fontsize.labels = 18 )
treemap(sales, index = c("rep.region"), vSize = "income", vColor = "units.sold", type= "value",palette = "OrRd", fontsize.labels = 18 )
treemap(sales, index = c("rep.region", "sales.rep", "type"), vSize = "income", vColor = "units.sold", type= "index",palette = brewer.pal(8, "Set1"))

# river plot

library (riverplot)
river<- riverplot.example()
par(mfrow=c(2,1))
plot(river, srt =90, lty =1)
class(river)
x<- river
x$edges
x$nodes
x$edges$Value[2]<- 45
x$edges$Value[1]<- 15
x$nodes$x[5]<-5
plot(x)

df<- aggregate(sales$income, list(type=sales$type, wine =sales$wine),sum)
df<- df[order(df$type, df$x),]

node.name<- c("wine", unique(df$type), df$wine)
node.position<- c(1,2,2,3,3,3,3,3,3,3)
node.color<- rep("gray", length(node.name))
node.color<- c("deepskyblue", "red", "yellow", "brown4", "firebrick3", "deeppink4", "khaki1", "lightgoldenrod1", "gold", "goldenrod1")
node<- data.frame(ID = node.name, x= node.position, col= node.color, stringsAsFactors = FALSE)
parent.nodes<- c("wine", "wine", df$type)
child.nodes<- c("red", "white", df$wine)
value <- c(sum(df$x[df$type == "red"]), sum(df$x[df$type == "white"]), df$x)
edges<- data.frame(N1 = parent.nodes, N2=child.nodes, Value=value)
r<- makeRiver(node, edges)
par(mar=c(0,0,0,0))
plot(r)

########################

# R plots and word

dat<- tapply(sales$units.sold, list(sales$type, sales$rep.region),sum)
barplot(dat, beside=TRUE, col=c("brown", "gold"), main = "Units Sold by region by type")





