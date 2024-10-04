install.packages("igraph")
library(igraph)
#loading data
nodes <-read.csv("C:/Users/sunva/Desktop/COSC421A4/vertex911.csv", header = T)
adj <- read.csv("C:/Users/sunva/Desktop/COSC421A4/adj911.csv", header=T)
head(adj)

adj <- as.matrix(adj)

g <- graph_from_adjacency_matrix(adj, mode="undirected")

# assigning colours
V(g)$color <-"blue"
V(g)$color[1:19]<-"red"

#ploting the graph
plot(g, vertex.size=5, edge.arrow.size=0.5, vertex.label=NA)

#eigenvalue centrality 
eigen.cent <- eigen_centrality(g)

eigen.cent$vector
eigenSort<- sort(eigen.cent$vector, decreasing = T)
head(eigenSort,5)

#Computing closeness centrality and between-ness centrality scores
gBetween <- betweenness(g)
gBetSort <- sort(gBetween, decreasing = T)
head(gBetSort,5)

gClose <- closeness(g)
gCloseSort <- sort(gClose, decreasing = T)
head(gCloseSort,5)

#counts of cliques
cliCount<-clique_size_counts(g)
maxSize <- max(cliCount)
listClique<-cliques(g)[sapply (cliques(g),length)== cliCount]
listClique


# Creating a sub graph
g2<- graph_from_adjacency_matrix(adj[1:19,1:19],mode="undirected")
V(g2)$color <- nodes[1:19,3]
plot(g2, vertex.size=8, edge.arrow.size=0.5, vertex.label=NA)

#calculating cliques in subgraph
G2cliques = clique_size_counts(g2)
G2cliques_size = sapply(G2cliques,length)
listCliqueG2 = cliques(g2)[G2cliques_size]
listCliqueG2
