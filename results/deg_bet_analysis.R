
library(data.table)
graphData_mainVar <- fread("C:/Users/Yamuna/Downloads/data_main_variables.csv", header =T, sep = ",")
#nrow(graphData_mainVar)
db <- data.frame(graphData_mainVar)

## Merging all finance type under one Finance type = 7 types
new_shr_type <- replace(db$hol_shr_type, db$hol_shr_type == "Finance: Pension fund/Mutual fund/Nominee/Trust/Trustee.", "Finance")
new_shr_type <- replace(new_shr_type, new_shr_type == "Finance: Other financial company.", "Finance")
new_shr_type <- replace(new_shr_type, new_shr_type == "Finance: Bank.", "Finance")
new_shr_type <- replace(new_shr_type, new_shr_type == "Finance: Insurance company.", "Finance")
unique(new_shr_type)
db$hol_shr_type <- new_shr_type


##Extracting first two columns and creating a graph with this edge
Edge_data<- data.frame(db[,1],db[,2])
colnames(Edge_data) <- c("ShareHol","Subsidiaries")
Edge_data[1:5,]

install.packages("igraph")
library(igraph)

## Now the whole network
SSNetwork<- graph.data.frame(d = Edge_data, directed = TRUE)

## Degree Analysis
deg <- degree(SSNetwork, mode = "out")
boxplot(deg, main="Boxplot of degree")
hist(deg)


top20Deg<-length(deg[which(deg > 29000)])
#top20DegreeNames <- names(deg[which(deg > 29000)])
dSorted<-sort.int(deg[which(deg > 29000)],decreasing=TRUE,index.return=FALSE)

top20DegreeNames <- names(dSorted)

## Find how many are sole shareholders
sub_names <- unique(db$sub_id)
length(intersect(sub_names, top20DegreeNames))
setdiff(top20DegreeNames, intersect(sub_names, top20DegreeNames))

dbHol_Type <- data.frame(db[,1],db[,4])
colnames(dbHol_Type)<-c("Hol_id","Shr_Type")
dbHol_Type[1:5,]
dbHol_Type_unique <- unique(dbHol_Type)
dbHol_Type_unique[1:5,]

for( i in 1:20)
{
  share_hol_type[i] <- dbHol_Type_unique$Shr_Type[dbHol_Type_unique$Hol_id == top20DegreeNames[i]]
}

data.frame(top20DegreeNames,share_hol_type)

## Extracting datasets which link_Dcf value greater than 0

db_dcf_nonZero <- db[db$link_Dcf > 0.00,]
Edge_data_nonZero<- data.frame(db_dcf_nonZero[,1],db_dcf_nonZero[,2])
colnames(Edge_data_nonZero) <- c("ShareHol","Subsidiaries")
Edge_data_nonZero[1:5,]

install.packages("igraph")
library(igraph)
SSNetwork_nonZero<- graph.data.frame(d = Edge_data_nonZero, directed = TRUE)
outDegree_NonZero<-degree(SSNetwork_nonZero, mode = "out")
boxplot(deg, outDegree_NonZero,main = "Comapring original network with the network having nonzero links", names= c("outDegree_Original","outDegree_NonZero"))

dSorted_NonZero<- sort.int(outDegree_NonZero[which(outDegree_NonZero > 900)],decreasing=TRUE,index.return=FALSE)
ll <- length(dSorted_NonZero)
top20DegreeNames_nonZero <- names(dSorted_NonZero)
for( i in 1:ll)
{
  type[i]<- dbHol_Type_unique$Shr_Type[dbHol_Type_unique$Hol_id == top20DegreeNames_nonZero[i]]
}

data.frame(top20DegreeNames_nonZero,type)

## Find how many are sole shareholders
sub_names <- unique(db$sub_id)
length(intersect(sub_names, top20DegreeNames_nonZero))
setdiff(top20DegreeNames, intersect(sub_names, top20DegreeNames_nonZero))

##Indegree Analysis in whole dataset

indeg <- degree(SSNetwork, mode = "in")
boxplot(indeg, main ="Boxplot of indegrees")
top20InDeg<-indeg[which(indeg > 1200)]

indSorted<-sort.int(top20InDeg,decreasing=TRUE,index.return=FALSE)

top20inDegreeNames <- names(indSorted)

## Find how many are sole shareholders
sub_names <- unique(db$sub_id)
length(intersect(sub_names, top20inDegreeNames))

## Indegree Analysis for dataset havingdirect link greater than zero
inDegree_NonZero<-degree(SSNetwork_nonZero, mode = "in")
boxplot(inDegree_NonZero)
inSorted_NonZero<- sort.int(inDegree_NonZero[which(inDegree_NonZero > 160)],decreasing=TRUE,index.return=FALSE)
ll <- length(inSorted_NonZero)
sub_names <- unique(db$sub_id)
hol_names <- unique(db$hol_id)
names1 <-names(inSorted_NonZero)
hol<-intersect(hol_names, names(inSorted_NonZero))
sub<-intersect(sub_names, names(inSorted_NonZero))
setdiff(names(inSorted_NonZero), hol)
setdiff(names(inSorted_NonZero), sub)

for( i in 1:length(hol))
{
  yamuna[i]<- dbHol_Type_unique$Shr_Type[dbHol_Type_unique$Hol_id == hol[i]]
}

data.frame(hol,yamuna)



##Betweenness Analysis
bet <- betweenness(SSNetwork)
boxplot(bet, main = "Boxplot of betweenness")

maxBetNames <- bet[which(bet > 7000)]

betSorted<- sort.int(maxBetNames,decreasing=TRUE,index.return=FALSE)
len <- length(betSorted)
top20BetweenNames <- names(betSorted)
for( i in 1:len)
{
  new_type[i]<- dbHol_Type_unique$Shr_Type[dbHol_Type_unique$Hol_id == top20BetweenNames[i]]
}

data.frame(top20BetweenNames,new_type)


##Betweenness Analysis
bet_nozero <- betweenness(SSNetwork_nonZero)
boxplot(bet_nozero, main = "Boxplot of betweenness")

maxBetNames <- bet_nozero[which(bet_nozero > 70000000)]

betSorted<- sort.int(maxBetNames,decreasing=TRUE,index.return=FALSE)
len <- length(betSorted)
top20BetweenNames <- names(betSorted)
for( i in 1:len)
{
  new_type[i]<- dbHol_Type_unique$Shr_Type[dbHol_Type_unique$Hol_id == top20BetweenNames[i]]
}

data.frame(top20BetweenNames,new_type)









