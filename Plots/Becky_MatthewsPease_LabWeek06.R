##Becky Matthews-Pease
##IST 719: Information Visualization
##Lab Week 6

###6.2.1: Coding  GGPlot##############################
library(ggplot2)

sales <-file.choose()
sales<- read.csv(sales, header=TRUE, stringsAsFactors = FALSE)

p <- ggplot(sales)
class(p)
attributes(p)
p$data
p$layers
p$scales
summary(p)
View(p)

ggplot(sales) + aes(x=expenses)
range(sales$expenses)

plot(sales$expenses)
ggplot(sales, aes(x=expenses))

ggplot(sales) + aes(x=expenses, y=income) + geom_point()
ggplot(sales) + 
  aes(x = sales$expenses, y = sales$income) +
  geom_point()

p <- ggplot(sales)

p <- ggplot(sales) + aes(x=expenses, y=income) + geom_point()
p + geom_point(color = "blue")
save(p, file =)

ggplot(sales) + aes(x = expenses, y = income, color = type) + geom_point()

###6.2.3: Coding  GGPlot##############################
ggplot(sales) + aes(x = expenses, y = income, color = unit.price > 14) + geom_point()

ggplot(sales) + aes(x = expenses, y = income) + geom_point(color = ifelse(sales$unit.price > 14, "red", "green"))

ggplot(sales) + aes(x = expenses, y = income, color = unit.price) + geom_point()

ggplot(sales) + aes(x = expenses, y = income
                    , color = unit.price
                    , shape = type
                    , alpha = income) + 
  geom_point()


ggplot(sales) + 
  aes(x = expenses, y = income
      , color = rep.region, shape = type
      , alpha = unit.price
      , size = units.sold) + 
  geom_point()

p1 <- ggplot(sales)
p2 <- ggplot(sales) + aes(x = expenses, y = income, shape = rep.region)
summary(p1)
attributes(p1)
p1$labels
p1$mapping
summary(p2)
attributes(p2)
p2$labels
p2$mapping

###6.2.5: Coding  GGPlot, Geoms#########################

library(ggplot2)
ggplot(sales) + aes(x = expenses, y = income) + geom_point() + geom_rug()

income.pred <- predict(lm(sales$income~sales$expenses))
ggplot(sales) + aes(x = expenses, y = income) + geom_point()+
  aes(y=income.pred) + geom_line(color = "red", lwd = 3)

income.pred <- predict(lm(sales$income~sales$expenses))
ggplot(sales) + aes(x = expenses, y = income) + geom_point()+
  aes(y=income.pred) + geom_line(color = "red", lwd = 3) + geom_rug

ggplot(sales) + aes(x = expenses, y = income) + geom_point(color = "pink") + 
  geom_rug() +
  geom_line(aes(y = income.pred)) + 
  geom_line(aes(y = income.pred + 150)) +
  geom_vline(xintercept = 10, color = "blue") +
  geom_hline(yintercept = 500 , color = "orange") +
  geom_abline(intercept = 50, slope = 100, color = "red", lty = 3, lwd = 2)

ggplot(sales) + aes(x = expenses, y = income) + geom_point(color = "pink")+ geom_rug() +
  geom_line(aes(y = income.pred)) + geom_line(aes(y = income.pred + 150)) +
  geom_vline(xintercept = 10, color = "blue") +
  geom_hline(yintercept = 500 , color = "orange") +
  geom_abline(intercept = 50, slope = 100, color = "red", lty = 3, lwd = 2)

ggplot(sales) + aes(x = income, y = expenses) + geom_point() + 
  geom_smooth(method = "loess")

ggplot(sales) + aes(x = income, y = expenses) + geom_point() + 
  geom_smooth()

ggplot(sales) + aes(x = expenses, y = income) + geom_bin2d()
ggplot(sales) + aes(x = expenses, y = income) + geom_bin2d(bins = 50)

price <- ifelse(sales$unit.price > 14, "expensive", "moderate")
price[sales$unit.price < 9] <- "cheap"

ggplot(sales) + aes(y = income, x = expenses, color = price) + 
  geom_bin2d(bins = 50)

###6.3.1: Coding  GGPlot, Geoms#########################

df <- aggregate(sales$units.sold, list (year = sales$year), sum)
df2 <- aggregate(sales$units.sold, list (year = sales$year, region = sales$rep.region), sum)


ggplot(sales) + aes(x = income) + geom_blank()
ggplot(sales) + aes(x = income) + geom_histogram()
ggplot(sales) + aes(x = income) + geom_histogram() + geom_histogram(binwidth = 10)

ggplot(sales) + aes(x = income) + 
  geom_histogram(binwidth = 10, fill="orange") + 
  geom_vline(aes(xintercept = mean(income))
             , color= "blue", linetype = "dashed", size = 1)
                           #Setting orange - not based on data

ggplot(sales) + aes(x = income) + 
  geom_histogram(binwidth = 10, fill="orange", alpha = .9) +
  aes(y = ..density..) +
  geom_density(alpha = .3, fill = "blue", color = "blue")
             #Elipsis = Stat = density curve for what was previously passed in aes for x

##Boxplots
ggplot(sales) + aes(x = "Jeff", y = income) + geom_boxplot()
ggplot(sales) + aes(x = rep.region, y = income) + geom_boxplot()

##LineCharts
ggplot(df) + aes(x = year, y = x) + geom_line() + ylim(c(0,40000))
ggplot(df) + aes(x = year, y = x)+ geom_step() + ylim(c(0,40000))

ggplot(df) + aes(x = year, y = x) + 
  geom_ribbon(aes(ymin = x - 1000, ymax = x + 1000, fill = "yellow")) + 
  geom_line() + ylim(c(0, 40000))   

df2
ggplot(df2) + aes(x = year, y = x, color = region) + 
  geom_line() + ylim(c(0, 10000))

 
###6.3.3: Coding  GGPlot, GeomBar######################

df3 <- aggregate(sales$units.sold, list(region = sales$rep.region), sum)
colnames(df3)[2] <- "sales"

ggplot(sales) + aes(x = rep.region) + geom_bar()
ggplot(sales) + aes(x = rep.region) + geom_bar(fill = "orange", width = .5) +
  ggtitle("Number of Sales by Region")

ggplot(sales) + aes(x = rep.region, fill = type) + geom_bar()
ggplot(sales) + aes(x = rep.region, fill = type) + geom_bar(position = "dodge")
ggplot(sales) + aes(x = rep.region, fill = type) + geom_bar(position = "fill")

ggplot(df3) + aes(x = region, y = sales, fill = region) +
  geom_bar(stat = "identity")

ggplot(df3) + aes(x = "", y = sales, fill = region) + 
  geom_bar(width = .3, stat = "identity") +
  coord_polar("y", start = 45)

###6.3.5: Coding  GGPlot, Stats######################

hist(sales$income)
p <- ggplot(sales) + aes(x = income)
p + geom_histogram() + stat_bin(binwidth = 20)

p + stat_density()

ggplot(sales) + aes(y = income) + geom_boxplot() + stat_boxplot()

ggplot(sales) + aes(y = income) + stat_boxplot()
ggplot(sales) + aes(x = expenses, y = income) + stat_bin2d() 
ggplot(sales) + aes(x = expenses, y = income) + stat_bin2d() + stat_density_2d(col = "red")

ggplot(sales) + aes(x = rep.region) + geom_bar()
ggplot(sales) + aes(x = rep.region) + stat_count()

ggplot(df2) + aes(x = region, y = sales) + geom_bar(stat = "identity")
ggplot(sales) + aes(x = region, y = sales) + geom_bar() + stat_identity()

ggplot(sales) + aes(x=income) +
  geom_histogram(aes(fill = ..count..)) +
  aes(y = ..density..) +
  geom_density(fill = "yellow", alpha = .1)
