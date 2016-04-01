library(igraph)

mynetwork<-erdos.renyi.game(1000, 10000, type=c("gnm"), directed = FALSE, loops = FALSE)

#Degree Centrality
max_degree <-max_degree
cat("\nNode ",max_degree ,"has the maximum numberof links", max(degree(mynetwork)))
    
cat("\nNode ", which.min(degree(mynetwork)),"has the minimum numberof links", min(degree(mynetwork)))

cat("\nThe average number of links to all the nodes is ", mean(degree(mynetwork)))
lowerlinks_count<-0
hubs_count<-0
total_links<- degree(mynetwork)
for( i in 1:1000)
  {
  if( total_links[i] < mean(total_links)){
    lowerlinks_count=lowerlinks_count+1
  }
  if(total_links[i] == max(total_links)){
    hubs_count= hubs_count+1
  }
   
}
cat("\nNodes having their total links below the average:", lowerlinks_count)
cat("\nNumber of larger hubs in given network", hubs_count) 

##Betweenness Centrality
max_betweenness <- which.max(betweenness(mynetwork))
cat("\nNode having the best location in network acting as the broker: ",max_betweenness)
which.min(betweenness(mynetwork))
#Maximum betweeeness is at differnt node than max degree but min degree and between ness are at same node


##Closeness Centrality
closeness(mynetwork)
quick_access <- which.max(closeness(mynetwork))
cat("\nNode having maximum closeness: ", quick_access)

## In my example node having max links and max closeness is same

## Network Reach
cat("\nNeighbouring nodes of one of the largest hub:", neighbors(mynetwork, max_degree))
cat("\nCheck if given two nodes with max degree and max betweenness are connected or not:" , are.connected(mynetwork,max_degree,max_betweenness))

cat("\nIncident links to one of the largest hub:", incident(mynetwork,max_degree))

##Network Integration
cat("\nDistance between given two largest hubs:", distances(mynetwork,max_degree,max_betweenness))
cat("\nShortest path between given two largest hubs")
print(shortest_paths(mynetwork,max_degree,max_betweenness)$vpath)

# visualization
plot(mynetwork, layout = layout.fruchterman.reingold, 
     main ="Random Graph of 1000 nodes and 10000 links",
     vertex.size = 2,
     vertex.color="red",
     vertex.frame.color= "white",
     vertex.label.color = "white",
     vertex.label.family = "sans",
     edge.width=1,  
     edge.color="blue")