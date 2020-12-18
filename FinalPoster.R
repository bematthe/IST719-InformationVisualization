library(alluvial)
library(circlepackeR)
library(colorspace)
library(data.tree)
library(dplyr)
library(easyalluvial)
library(ggalt)
library(ggalluvial)
library(ggplot2)
library(ggraph)
library(grDevices)
library(hrbrthemes)
library(igraph)
library(networkD3)
library(patchwork)
library(RColorBrewer)
library(rworldmap)
library(tidyr)
library(tidyverse)
library(treemap)
library(wordcloud)
library(wordcloud2)


####PULLDATA#######################################################
choc <- file.choose()
choc <- read.csv(choc, header = TRUE, stringsAsFactors = FALSE)
choc <- read.csv("C:\\Users\\becky\\Desktop\\choc.csv", header = TRUE, stringsAsFactors = FALSE)
chocn <- read.csv("C:\\Users\\becky\\Desktop\\chocn.csv", header = TRUE, stringsAsFactors = FALSE)
head(choc)
###################################################################
####TRANSFORMDATA##################################################
x <- table(choc$BeanMainContinent)
barplot(x[order(x, decreasing = TRUE)], xlab = "Continent", ylab = "Number of Bars", main = "Continent of Bean Origin", cex.axis = 1.0, cex.lab = 1.1)
chocdf <- data.frame(choc)
###################################################################
###HISTOGRAMS & DISTRIBUTIONS######################################
#BeanSourceContinent
p1 <- ggplot(data = choc) + 
  geom_bar(mapping = aes(x = BeanMainContinent, y = ..count..), stat = "count") +
  theme(axis.text.x = element_text(angle = 45))
#Company Location
p2 <- ggplot(data = choc) + 
  geom_bar(mapping = aes(x = CoContinent, y = ..count..), stat = "count") +
  theme(axis.text.x = element_text(angle = 45))
#CocoaPercentage
p3 <- ggplot(chocn, aes(x=CocoaPerc), stat = "count") + geom_histogram(bins=10)
#GeneralBeanType
p4 <- ggplot(data = choc) + 
  geom_bar(mapping = aes(x = BeanTypeSum, y = ..count..), stat = "count") +
  theme(axis.text.x = element_text(angle = 45))
p1 + p2 + p3 + p4

###################################################################
###DataFrameCounts#################################################
#BeanOriginContinent
bSource <- aggregate(data.frame(count = choc$BeanContinent),
                     list(value = choc$BeanContinent),
                     length)
#CompanyContinent
cLocation <- aggregate(data.frame(count = choc$CoContinent),
                     list(value = choc$CoContinent),
                     length)
#BeanTypeGeneral
bType <- aggregate(data.frame(count = choc$BeanTypeSum),
                     list(value = choc$BeanTypeSum),
                     length)
###################################################################
###GLOBALMAP-ORIGIN################################################
choc <- file.choose()
choc <- read.csv(choc, header = TRUE, stringsAsFactors = FALSE)
byCountry <- choc %>%
  count(CountryCode)
##Count.csv
byCountry <- choc
# plot data on world map
plot(worldmap, xlim = c(-80, 160), ylim = c(-50, 100), 
     asp = 1, bg = "lightblue", col = "black", fill = T)
mapped_data <- joinCountryData2Map(byCountry, joinCode = "ISO3", 
                                   nameJoinColumn = "CountryCode")

par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
mapCountryData(mapped_data, nameColumnToPlot = "CountCountryCode")
###################################################################
###BOXPLOT#########################################################
q1 <- ggplot(choc,
             aes(y = Rating, x = BeanMainContinent
                 , main = "Chocolate Ratings by Bean Source - Continent"
                 , notch = TRUE, scale_y_continuous(breaks=c(0,.5, 1, 1.5,2, 2.5, 3, 3.5, 4, 4.5, 5.0)))) + 
  theme(axis.text.x = element_text(angle = 45)) + 
  geom_boxplot()
q2 <- ggplot(choc,
             aes(y = Rating, x = CoContinent
                 , main = "Chocolate Ratings by Company Location - Continent"
                 , notch = TRUE, scale_y_continuous(breaks=c(0,.5, 1, 1.5,2, 2.5, 3, 3.5, 4, 4.5, 5.0)))) + 
  theme(axis.text.x = element_text(angle = 45)) + 
  geom_boxplot()
q3 <- ggplot(choc,
             aes(y = Rating, x = BeanTypeSum
                 , main = "Chocolate Ratings by General Bean Type"
                 , notch = TRUE, scale_y_continuous(breaks=c(0,.5, 1, 1.5,2, 2.5, 3, 3.5, 4, 4.5, 5.0)))) +  
  theme(axis.text.x = element_text(angle = 45)) + 
  geom_boxplot()
q4 <- ggplot(choc,
             aes(y = Rating, x = CocoaPerc
                 , main = "Chocolate Ratings by Cocoa Percentage"
                 , notch = TRUE, scale_y_continuous(breaks=c(0,.5, 1, 1.5,2, 2.5, 3, 3.5, 4, 4.5, 5.0)))) +  
  theme(axis.text.x = element_text(angle = 45)) + 
  geom_boxplot()
q1 + q2 + q3 + q4

choc %>%
  ggplot(aes(x=Rating, y=BeanMainContinent, fill=CocoaPerc)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Basic boxplot") +
  xlab("")

q1 <- ggplot(choc,
             aes(y = Rating, x = BeanMainContinent
                 , main = "Chocolate Ratings by Bean Source - Continent"
                 , notch = TRUE)) + 
  theme(axis.text.x = element_text(angle = 45)) + 
  geom_boxplot()
q1 + scale_y_continuous(breaks = seq(1, 5, .04))
###################################################################
###CIRCLEPLOT######################################################
data(choc)
choc$pathString <- paste("world",
                         choc$BeanTypeSum,
                         choc$Rating,
                         sep = "/")
Rating <- as.Node(choc)
circlepackeR(Rating, size = count("Rating")
# save the widget
# library(htmlwidgets)
# saveWidget(p, file=paste0( getwd(), "/HtmlWidget/circular_packing_circlepackeR1.html"))
# 2
data(choc)
choc <- count(choc$BeanTypeSum, choc$Rating)
choc$pathString <- paste("world",
                                      choc$BeanTypeSum,
                                      choc$Rating,
                                      sep = "/")
Rating <- as.Node(choc)
circlepackeR(Rating, size = count("Rating")
###################################################################
###TreeMaps########################################################
choc <- data.frame(choc$BeanMainContinent, choc$BeanTypeSum, choc$Rating)
group <- choc$choc.BeanMainContinent
subgroup <- choc$choc.BeanTypeSum
value <- choc$choc.Rating
data <- data.frame(group, subgroup, value)
#Treemap 1
treemap(data,
        index=c("group","subgroup"),
        vSize="value",
        type="value"
) 
#Treemap 2
treemap(data,
        index=c("group","subgroup"),
        vSize="value",
        type="index"
) 
#Treemap 3
group <- choc$Main.Continent
subgroup <- choc$Rating
value <- choc$Rating
data <- data.frame(group,subgroup,value)
#Treemap
treemap(data,
        index=c("group","subgroup"),
        vSize="value",
        type="value",
        title = "Ratings by Continent of Bean Origin"
) 
###################################################################
####DUMBBELL#######################################################

choc$Main.Continent <- factor(choc$BeanMainContinent, levels = as.character(choc$BeanMainContinent))
ggplot(choc, aes(x = Rating, xend = Rating, y = BeanOrigin)) +
  geom_dumbbell()
###################################################################
###WORDCLOUD#######################################################
chocdesc <- file.choose()
choc <- read.csv(chocdesc, header = TRUE, stringsAsFactors = FALSE)
choct <- table(choc)
chocdf <- data.frame(choct)
wordcloud(chocdf$choc, scale = c(3, .3), min.freq = 2
          , max.words = Inf, random.order = FALSE
          , random.color = FALSE, ordered.colors = TRUE, rot.per = 0, colors = "green")

###################################################################
####SCATTER########################################################
#Bin size
choc <- file.choose()
choc <- read.csv(choc, header = TRUE, stringsAsFactors = FALSE)
head(choc)
ggplot(choc, aes(x=Rating, y=CocoaPerc) ) +
  geom_hex(bins = 10) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()
ggplot(choc, aes(x = Rating, y = CocoaPerc)) +
  geom_hex() +
  theme_bw() +
  scale_colour_gradient2(choc,
                         low = "plum1",
                         mid = "plum",
                         high = "plum4",
                         guide = "colourbar",
                         aesthetics = "colour")
###################################################################
###HEATMAP#########################################################
choc <- read.csv("C:\\Users\\becky\\Desktop\\Chocal.csv", header = TRUE, stringsAsFactors = FALSE)
chocdfr <- data.frame(choc$BeanMainContinent,choc$CocoaPerc, choc$Rating)
chocmatrix <- as.matrix(chocdfr)
heatmap(chocmatrix, scale = "column")
heatmap(chocmatrix, Colv = NA, Rowv = NA, scale="column", col = cm.colors(256))
heatmap(chocmatrix, Colv = NA, Rowv = NA, scale="column", cm.colors(256), xlab="Choc.Cocoa.Percent", ylab="choc.Rating", main="heatmap")


###################################################################
###Alluvial########################################################
#Interactive #1
links <- data.frame(
  source=choc$BeanMainContinent, 
  target=choc$BeanTypeSum,
  value=choc$Rating
)
# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)
# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1
# Make the Network
p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   sinksRight=FALSE)
p

#Interactive #2
set.seed(1)
data <- matrix(sample( seq(0,40), 49, replace=T ), 7, 7)
data[data < 35] <- 0
colnames(data) = rownames(data) = c("group_A", "group_B", "group_C", "group_D", "group_E", "group_F", "group_G")
# Transform it to connection data frame with tidyr from the tidyverse:
links <- data %>% 
  as.data.frame() %>% 
  rownames_to_column(var="source") %>% 
  gather(key="target", value="value", -1) %>%
  filter(value != 0)
# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), as.character(links$target)) %>% 
    unique()
)
# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1
# Make the Network
p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   sinksRight=FALSE)
p

##Continent - Bean Type
library(alluvial)
choc <- as.data.frame(choc)
# only two variables: class and survival status
choc2d <- aggregate( Rating ~ BeanMainContinent + BeanTypeSum, data=choc, sum)
alluvial( choc2d[,1:2], freq=choc2d$Rating, xw=0.0, alpha=0.8,
          gap.width=0.1, col= "steelblue", border="white",
          layer = choc2d$BeanTypeSum != "Blend" )
###################################################################
###ScatteGraphs####################################################
CocoaContent <- aggregate(x = choc$Rating, 
                          by = list(choc$CocoaPerc), FUN = mean)
ggplot(CocoaContent, aes(x = x, y = Group.1)) + 
  geom_point(
    color="orange",
    fill="#69b3a2",
    shape=21,
    alpha=0.5,
    size=6,
    stroke = 2
  )

ggplot(CocoaContent, aes(x = x, y = Group.1)) + 
  geom_point(
    color="black",
    fill="#69b3a2",
    shape=22,
    alpha=0.5,
    size=6,
    stroke = 1
  ) +
  theme_ipsum()
###################################################################

