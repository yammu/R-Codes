
library(data.table)
graphData_mainVar <- fread("C:/Users/Yamuna/Downloads/data_main_variables.csv", header =T, sep = ",")
#nrow(graphData_mainVar)
db <- data.frame(graphData_mainVar)

##Getting some information from dataset
db[1:5,]
db[1]
db[,4]


##list of shareholder type = 10 types
unique(db$hol_shr_type)

## Merging all finance type under one Finance type = 7 types
new_shr_type <- replace(db$hol_shr_type, db$hol_shr_type == "Finance: Pension fund/Mutual fund/Nominee/Trust/Trustee.", "Finance")
new_shr_type <- replace(new_shr_type, new_shr_type == "Finance: Other financial company.", "Finance")
new_shr_type <- replace(new_shr_type, new_shr_type == "Finance: Bank.", "Finance")
new_shr_type <- replace(new_shr_type, new_shr_type == "Finance: Insurance company.", "Finance")
unique(new_shr_type)
db$hol_shr_type <- new_shr_type

#Total Links with industrail firms = 4904503
db_indus <- subset(db, db$hol_shr_type == "Industrial firm.")
nrow(db_indus)

boxplot(db_indus$link_Dcf, main= "Boxplot of directlinks in industrial type", xlab = "Direct Links")
points(mean(db_indus$link_Dcf),pch=8,col="red")

#Total Links with financial firms = 5760508
db_financial <- subset(db, db$hol_shr_type == "Finance")
nrow(db_financial)

boxplot(db_financial$link_Dcf, main= "Boxplot of directlinks in Financial type")
points(mean(db_financial$link_Dcf),pch=8,col="red")

##Number of total shareHolders = 1007258
total_shareHolders <- length(unique(db[,1]))
total_shareHolders


## Number of total subsidiaries = 1507967
total_Subsidiaries <- length(unique(db[,2]))
total_subsidiaries

##shareholders and subsidiary = 202617
length(intersect(Edge_data[,1], Edge_data[,2]))


##Extracting first two columns and creating a graph with this edge
Edge_data<- data.frame(db[,1],db[,2])
colnames(Edge_data) <- c("ShareHol","Subsidiaries")
Edge_data[1:5,]

## Creating small network
install.packages("igraph")
library(igraph)
graph_Edge_data<- graph.data.frame(d = Edge_data[1:5,], directed = TRUE)
plot(graph_Edge_data, vertex.label = V(graph_Edge_data)$name)

## Now the whole network
SSNetwork<- graph.data.frame(d = Edge_data, directed = TRUE)
max(degree(SSNetwork, mode = "out"))
#Ans  55632
which.max(degree(SSNetwork))
## CH46911 
## 814040 

nrow(db[db$hol_id == "CH46911",])
##Ans 55632
nrow(db[db$sub_id == "CH46911",])
## Ans 3


## Number of indegrees, outdegrees, and total degrees

max(degree(SSNetwork,mode="in"))
which.max(degree(SSNetwork,mode="in"))
##ITTO0702122 has max incoming links and is a subsidiary

db_max_sub <- db[db$sub_id == "ITTO0702122",]
db_max_sub[1:5,]
nrow(db_max_sub[db_max_sub$hol_shr_type == "Finance",])
## Ans 615
nrow(db_max_sub[db_max_sub$hol_shr_type == "Industrial firm.",])
## Ans 1271

db[which(db$hol_id=="ITTO0702122"),]
##ITTO0702122 is not a shareholder
are.connected(SSNetwork,"ITTO0702122","CH46911")
##are.connected(SSNetwork,which.max(degree(SSNetwork,mode="in"))
##             ,which.max(degree(SSNetwork,mode="out")))
max(degree(SSNetwork,mode="out"))

## Degree Analysis
deg <- degree(SSNetwork)
boxplot(deg, main="Boxplot of degree")
hist(deg)
#length(which(deg==1))
length(deg[which(deg > 29000)])
top20DegreeNames <- names(deg[which(deg > 29000)])

##deg[which(deg<=40000 && deg>30000)]
db[which(db$hol_id=="ES22628"),]
db[which(db$hol_id=="FR11792"),]
db[which(db$hol_id=="FR11804"),]
db[which(db$hol_id=="FR17481"),]
db[which(db$hol_id=="FR439208190"),]
db[which(db$hol_id=="FR444369607"),]
db[which(db$hol_id=="GB04303322"),]

db_PT500097488 <-db[which(db$hol_id=="PT500097488"),]
db_PT500097488[db_PT500097488$link_Dcf>0.00,]

##Betweenness Analysis
bet <- betweenness(SSNetwork)
boxplot(bet, main = "Boxplot of betweenness")

max(betweenness(SSNetwork))
which.max(betweenness(SSNetwork))

maxBetNames <- names(bet[which(bet > 100000)])
db[which(db$hol_id=="PT22541"),]
db[which(db$hol_id=="PT500697256"),]
db[which(db$hol_id=="PT206934"),]


## Closeness Analysis
clos <- closeness(SSNetwork)
boxplot(clos, main= "Boxplot of Closeness")
max(clos)
which.max(clos)

are.connected(SSNetwork,which.max(deg),which.max(bet))
are.connected(SSNetwork,which.max(deg),which.max(clos))

neighbors(SSNetwork, which.max(deg))
length(neighbors(SSNetwork, which.max(deg)))
length(unique(neighbors(SSNetwork, which.max(deg))))

##Analysis of only financial firms

Edge_data_financial<- data.frame(db_financial[,1],db_financial[,2])
colnames(Edge_data_financial) <- c("ShareHol","Subsidiaries")
Edge_data_financial[1:5,]

## Creating  network of financial firm

SSNetwork_fin <- graph.data.frame(d = Edge_data_financial, directed = TRUE)
max(degree(SSNetwork_fin))
which.max(degree(SSNetwork_fin))

max(degree(SSNetwork_fin,mode = "in"))
which.max(degree(SSNetwork_fin,mode = "in"))

max(degree(SSNetwork_fin,mode = "out"))
which.max(degree(SSNetwork_fin,mode = "out"))

degree(SSNetwork_fin,mode = "out")[1:5]



#################################### Week 2 #########################
## Extracting datasets which link_Dcf value greater than 0
db_dcf_nonZero <- db[db$link_Dcf > 0.00,]
Edge_data_nonZero<- data.frame(db_dcf_nonZero[,1],db_dcf_nonZero[,2])
colnames(Edge_data_nonZero) <- c("ShareHol","Subsidiaries")
Edge_data_nonZero[1:5,]

install.packages("igraph")
library(igraph)
SSNetwork_nonZero<- graph.data.frame(d = Edge_data_nonZero, directed = TRUE)
inDegree_NonZero<-degree(SSNetwork_nonZero, mode = "in")
outDegree_NonZero<-degree(SSNetwork_nonZero, mode = "out")
boxplot(outDegree_NonZero,inDegree_NonZero,main = "Data with NonZeroLinks", names= c("outDegree","inDegree"))
inDegree_NonZero[which(inDegree_NonZero > 160)]

aa <- outDegree_NonZero[which(outDegree_NonZero > 900)]


