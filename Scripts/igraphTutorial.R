#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#'
#' igraph Social Network Analysis Tutorial
#' @author Cooper Kimball-Rhines
#' 
#' Following this guide: https://finnstats.com/2021/04/22/social-network-analysis-in-r/
#' Data downloaded from: https://github.com/finnstats/finnstats
#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

#### Libraries and Data ####
#install.packages("igraph")
#?igraph
library(igraph)
library(tidyverse)

#Load data
SNAdata <- read.csv("Data/igraphTutorial.csv", header=T)
summary(SNAdata)
head(SNAdata)

#Select columns that we're interested in
SNAtut <- SNAdata |>
  select(first, second)

#### Network Construction ####
#Construct network with igraph
net <- graph.data.frame(SNAtut, directed = T)
V(net) #Show at network vertices
E(net) #Show network edges

#Assign labels to networks and vertices
V(net)$label <- V(net)$name
V(net)$degree <- degree(net)

#Illustrate node degrees (with a weird non-ggplot method)
hist(V(net)$degree,
     col = 'green',
     main = 'Histogram of Node Degree',
     ylab = 'Frequency',
     xlab = 'Degree of Vertices')
#The histogram shows the frequency of nodes by number of vertices

#Graph Network Diagram (again not using ggplot)
set.seed(111)
plot(net,
     vertex.color = 'green',
     vertext.size = 2,
     edge.arrow.size = 0.1,
     vertex.label.cex = 0.8)

#Highlight degrees and layouts of the nodes (larger circle = more connected)
plot(net,
     vertex.color = rainbow(52),
     vertex.size = V(net)$degree*0.4,
     edge.arrow.size = 0.1,
     layout = layout.fruchterman.reingold)
