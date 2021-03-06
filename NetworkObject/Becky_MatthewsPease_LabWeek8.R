#Becky Matthews-Pease
#IST 719: Information Visualization
#Week 8 Lab

### 8.2.1 Visualizing Social Networks ###

library(igraph)

link.data <- read.csv("C://Users//becky//Desktop//links-421-719network.csv"
                      , header = TRUE, stringsAsFactors = FALSE)
node.data <- read.csv("C://Users//becky//Desktop//nodes-421-719network.csv"
                      , header = TRUE, stringsAsFactors = FALSE)
View(node.data)
View(link.data)

dim(node.data)
colnames(link.data) <- gsub("\\.", "", colnames(link.data))

link.data$X <- gsub(" |-", "", link.data$X)
cbind(link.data$X, colnames(link.data)[-1])

node.data$Name <- gsub(" |-", "", node.data$Name)
cbind(node.data$Name, link.data$X)

M <- as.matrix(link.data[ , -1])
rownames(M) <- colnames(M)
dim(M)
any(is.na(M))
M[is.na(M)] <- 0
M[M > 1] 

g <- graph_from_adjacency_matrix(M)

### 8.2.3 Visualizing Social Networks ###

vcount(g)
ecount(g)
plot.igraph(g)

g <- simplify(g)  #Suppress self loops
par(mar = c(0, 0, 0, 0))
plot.igraph(g, edge.arrow.size = 0, edge.arrow.width = 0)

E(g)$arrow.size <- 0
E(g)$arrow.width <- 0

plot.igraph(g)
g

V(g)$color <- "gold"
V(g)$frame.color <- "white"
V(g)$label.color <- "black"
E(g)$color <- "cadetblue"
V(g)$size <-5

plot.igraph(g)

#For list of attributes:
?igraph.plotting

E(g)$curved <- .4
plot.igraph(g)
#########################################
### 8.3.1 Visualizing Social Networks ###
### Visualizing Centrality and Centrality Measurement ###

plot(degree(g))

par(mar = c(3,10,1,1))
barplot(sort(degree(g)), main = "Number of Links", horiz = TRUE, las = 2)
V(g)$degree <- degree(g)

V(g)$deg.out <- degree(g, mode = "out")
V(g)$deg.in <- degree(g, mode = "in")
barplot(sort(V(g)$deg.out), horiz = TRUE, las = 2, names.arg = V(g)$name)
barplot(sort(V(g)$deg.in), horiz = TRUE, las = 2, names.arg = V(g)$name)

#g.bak <- g
#g <- as.undirected(g)
#g <- g.bak

V(g)$close <- closeness(g, normalized = TRUE, mode = "all")
V(g)$bet <- betweenness(g, directed = FALSE)

library(plotrix)
my.pallet <- colorRampPalette(c("steelblue", "violet", "tomato", "red", "red"))

V(g)$color <- rev(
  my.pallet(200))[round(1+ rescale(V(g)$close, c(1,199)),0)]
plot.igraph(g)

V(g)$size <- 2+ rescale(V(g)$degree, c(0,13))
V(g)$label.cex <- .7 + rescale(V(g)$bet, c(0,1.25))
plot.igraph(g)

#########################################
### 8.3.3 Visualizing Social Networks ###

cbind(V(g)$name, node.data$Name)
V(g)$class <- node.data$Class
V(g)$country <- node.data$Country
V(g)$year <- node.data$year
g <- delete_vertices(g,("JoHunter"))
plot.igraph(g)

V(g)$shape <- "circle"
V(g)$shape[V(g)$class=="Wednesday"] <- "square"
V(g)$shape[V(g)$class=="Both"] <- "rectangle"

V(g)$color <- "gold"
V(g)$color[V(g)$country=="India"] <- "springgreen4"
V(g)$color[V(g)$country=="China"] <- "red"
V(g)$color[V(g)$country=="Both"] <- "purple"
plot.igraph(g)

V(g)$label.color <- "blue"
V(g)$label.color[V(g)$year==1] <- "black"
plot.igraph(g)

fc <- cluster_fast_greedy(as.undirected(g))
print(modularity(fc))
membership(fc)
V(g)$cluster <- membership(fc)
length(fc)
sizes(fc)

par(mar = c(0,0,0,0))
plot_dendrogram(fc, palette = rainbow(7))
?plot_dendrogram

#########################################
### 8.3.3 Visualizing Social Networks ###

mydoc <- read.csv("C:\\Users\\becky\\Desktop\\ist719networkobject.rda")
par(mar = c(0, 0, 0, 0))
plot.igraph(g)

l <- layout_in_circle(g)

V(g)$x <- l[,1]
V(g)$y <- l[,2]
l <- layout_with_fr(g)
l <- layout_as_star(g, center = "LeelaDeshmukh")
l <- layout_with_kk(g)

V(g)$x <- 0
V(g)$y <- 0
plot.igraph(g)
coord <- cbind(V(g)$x, V(g)$y)

iteratation <- c(500, 100, 20, 10, 5, 3, 2, 1)
for(i in 1: length(iteratation)){
  l <-layout_with_fr(g, coords = coord, dim = 2, niter = iteratation[i])
  V(g)$x <- l[,1]
  V(g)$y <- l[,2]
  plot.igraph(g)
  mtext(paste("Layout FR:", iteratation[i]), side = 3, line = 0, cex = 1.5, adj = 0)
}

l <- layout_with_gem(g)
V(g)$x <- l[,1]
V(g)$y <- l[,2]
plot.igraph(g)

l <- layout_with_dh(g)
V(g)$x <- l[,1]
V(g)$y <- l[,2]
plot.igraph(g)

l <- layout_on_grid(g)
V(g)$x <- l[,1]
V(g)$y <- l[,2]
plot.igraph(g)

#########################################
### 8.4.3 Visualizing Social Networks ###
########BipartiteNetwork#################

my.linked.list <- data.frame(person = V(g)$name, event = V(g)$country)
g <- graph_from_data_frame(my.linked.list, directed = FALSE)

#node = 80
#edges = 72
V(g)$type <- FALSE
V(g)$type[V(g)$name %in% node.data$Name] <- TRUE

l <- layout_as_bipartite(g, types = V(g)$type)
V(g)$x <- l[,2]
V(g)$y <- l[,1]

par(mar = c(0,0,0,0))
plot.igraph(g)
V(g)$size <- 0
plot.igraph(g)

