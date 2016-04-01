from <- c(1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,4,5,5,5,5,5,5,6,6,6,6,6,6)
to <-   c(1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6)
value<- c(0,0.1,0,0,0,0,0,0,0.5,0.5,0.2,0,0,0.3,0,0,0.2,0,0,0.3,0,0,0.6,0,0,0.3,0.5,0.5,0,1.0,0,0,0,0,0,0) 
example<- cbind(from,to,value)


#G<- graph(from,to,value)
#plot(G,'XData',from,'YData',to,'EdgeLabel',G.Edges.value)

install.packages("igraph")
library(igraph)
#example[,1]=as.character(example[,1]) 
#example[,2]=as.character(example[,2]) 
g=graph.edgelist(example[,1:2]) #We first greate a network from the first two columns, which has the list of vertices
E(g)$weight=as.numeric(example[,3])
betweenness(g)
plot(g, layout = layout.fruchterman.reingold, 
     main ="Example from thesis with links >= 0",
     vertex.size = 20,
     vertex.color="red",
     vertex.frame.color= "white",
     vertex.label.color = "white",
     vertex.label.family = "sans",
     edge.width=E(g)$weight,  
     edge.color="blue")


from <- c(1,2,2,2,3,3,4,4,5,5,5,5)
to <-   c(2,3,4,5,2,5,2,5,2,3,4,6)
value<- c(0.1,0.5,0.5,0.2,0.3,0.2,0.3,0.6,0.3,0.5,0.5,1.0) 
example1<- cbind(from,to,value)

g1=graph.edgelist(example1[,1:2]) #We first greate a network from the first two columns, which has the list of vertices
E(g1)$weight=as.numeric(example1[,3])
plot(g1, layout = layout.fruchterman.reingold, 
     main ="Example from thesis with links>0.0",
     vertex.size = 20,
     vertex.color="red",
     vertex.frame.color= "white",
     vertex.label.color = "white",
     vertex.label.family = "sans",
     edge.width=E(g1)$weight,  
     edge.color="blue")
betweenness(g1)

#plot(g, layout=layout.fruchterman.reingold,edge.width=E(g)$weight/2)
Edge_data<- data.frame(example1[,1],example1[,2])
Network<- graph.data.frame(d = Edge_data, directed = TRUE)
betweenness(Network)
max(betweenness(Network))
which.max(betweenness(Network)))

#plot(g, vertex.label=NA, vertex.size=3, edge.width=E(g)$width)
