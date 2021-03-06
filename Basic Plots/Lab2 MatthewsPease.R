#Becky Matthews-Pease
#IST 719
#Lab Week 2
#Data Interrogation or Data Exploration

read.csv("C:\\Users\\becky\\Documents\\A-Grad School\\IST 719 - Information Visualization\\04-DataSets\\tips.csv")
fname <- file.choose()
tips <- read.csv("C:\\Users\\becky\\Documents\\A-Grad School\\IST 719 - Information Visualization\\04-DataSets\\tips.csv", header = TRUE, stringsAsFactors = FALSE)


colnames(tips)
fix(tips)
view(tips)
str(tips)

tips[1, ]
tips[ ,1]
tips[3,3]
tips[1:3, ]

length(tips[1:3,2])

dim(tips)
dim(tips)[1]
tips$time

plot(tips$total_bill)
plot(sort(tips$total_bill))
boxplot(tips$total_bill)
hist(tips$total_bill)
d <- density(tips$total_bill)
plot(d)
polygon(d, col = "orange")

par(mfrow = c(2,2))
plot(tips$total_bill)
plot(sort(tips$total_bill))
boxplot(tips$total_bill)
hist(tips$total_bill)


library(vioplot)
vioplot(tips$total_bill)

unique(tips$sex)

tips.M <- tips[tips$sex == "Male", ]
tips.F <- tips[tips$sex == "Female", ]

par(mfrow = c(2,1), mar = c(2,3,1,2))
boxplot(tips.F$tip, horizontal = T, xlim = c(1,10))
boxplot(tips.M$tip, horizontal = T, xlim = c(1,10))

####
library(jsonlite)
fname <- ("C:\\Users\\becky\\Documents\\A-Grad School\\IST 719 - Information Visualization\\04-DataSets\\tweet.formated.json")
fname <- fromJSON("C:\\Users\\becky\\Documents\\A-Grad School\\IST 719 - Information Visualization\\04-DataSets\\tweet.formated.json")
raw.tweet <- fromJSON(fname, flatten = FALSE)
str(raw.tweet)
names(raw.tweet)

raw.tweet[["user"]]
raw.tweet[["user"]]$followers_count
raw.tweet[["user"]][["followers_count"]]

fname <- "C:\\Users\\becky\\Documents\\A-Grad School\\IST 719 - Information Visualization\\04-DataSets\\tweets5814.json"
com <- file(fname, open = "r")
tweets <- stream_in(com)
close(com)

dim(tweets)

tweets$text[1:3]
tweets$user[1]
tweets$user$followers_count

boxplot(tweets$user$followers_count, horizontal = TRUE)
boxplot(log10(tweets$user$followers_count), horizontal = TRUE)



#####
task.time <- c(rnorm(n = 30, mean = 30, sd = 2.25)
               , rnorm(n = 30, mean = 25, sd = 1.5))
hist(task.time)

df <- data.frame(time = task.time, status = status)
status <- c(rep("AMA", 30), rep("PRO", 30))
view(df)
df.grouped <- aggregate(df$time, list(df$status), mean)
df.grouped
colnames(df.grouped) <- c("stat", "time")
df.grouped
class(df.grouped)
barplot(df.grouped$time, names.arg = df.grouped$stat)

M.grouped <- tapply(df$time, list(df$status), mean)
class(M.grouped)

tapply(df$time, list(df$status), range)
range(task.time)
summary(task.timm)
aggregate(df$time, list(df$status), summary)

table(df$status)
table(df$time)
table(round(df$time, 2))

df$sex <- sample(c("M", "F"), 60, replace = T)

aggregate(df$time, list(df$status, df$sex), mean)
M <- tapply(df$time, list(df$status, df$sex), mean)
barplot(M, beside = TRUE)

M <- tapply(df$time, list(df$sex, df$status), mean)
barplot(M, beside = TRUE)
######

library(tidyr)
n <- 5
year <- 2001:(2000 + n)
q1 <- runif(n = n, min = 100, max = 120)
q2 <- runif(n = n, min = 103, max = 130)
q3 <- runif(n = n, min = 105, max = 140)
q4 <- runif(n = n, min = 108, max = 150)

df.wide <- data.frame(year, q1, q2, q3, q4)

gather(df.wide, qt, sales, q1:q4)
df.wide %>% gather(qt, sales, q1:q4)

df.long <- df.wide %>% gather(qt, sales, q1:q4)
o <- order(df.long$year, df.long$qt)
dflong <- df.long[o, ]

df <- data.frame(cat = rep(c("tap", "reg", "zed", "vum"), 3)
                 , group = rep(letters[7:9], 4)
                 , x = 1:12)

spread(df, cat, x)
#####

library(plotrix)
n <- 70
age.min <- 1
age.max <- 90
age.range <- c(age.min, age.max)
m <- round(rescale(rbeta(n, 5, 2.5), age.range), 0)
f <- round(rescale(rbeta(n, 5, 2.0), age.range), 0)
x <- age.min:age.max
f.y <- m.y <- rep(0, length(x))
           
m.tab <- table(m)
m.y[as.numeric((names(m.tab)))] <- as.numeric(m.tab)

f.tab <- table(f)
f.y[as.numeric((names(f.tab)))] <- as.numeric(f.tab)

age.freqs <- data.frame(ages = x, males = m.y, femails = f.y)

max.x <- round(1.2 - max(age.freqs[ ,2:3]), 0)
plot(c(-max.x, max.x), c(0,100), type = "n", bty = "n", xaxt = "n"
     , ylab = "age", xlab = "freq", main = "sample age distribution")


grid()
last.y <- 0
  for (i in 1:90) {
    rect(xleft = 0, ybottom = last.y, xright = -age.freqs$males[i]
         , ytop = age.freqs$ages[i], col = "lightblue2", border = NA)
    rect(xleft = 0, ybottom = last.y, xright - age.freqs$females[i]
         , ytop = age.freqs$ages[i], col = "lightpink", border = NA)
    last.y <- age.freqs$ages[i]
  }




