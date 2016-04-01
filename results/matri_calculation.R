library(data.table)
graphdatawith5var <- fread("E:/Graph_Project/datasets/data1.csv", header =T, sep = ",")
#nrow(graphData_mainVar)
dataf <- data.frame(graphdatawith5var)
dataf[1:5,]


library(data.table)
graphdatawith6var <- fread("E:/Graph_Project/datasets/data2.csv", header =T, sep = ",")
#nrow(graphData_mainVar)
dataf6 <- data.frame(graphdatawith6var)
dataf6[1:5,]


newData<-subset(dataf6,!(is.na(dataf6["hol_toas"])))
which.max(newData$hol_toas)
max(newData$hol_toas)
edgeList <- data.frame(newData$hol_id,newData$sub_id,newData$link_Dcf)
colnames(edgeList)<- c("from", "to","weight")
edgeList[1:5,]

edgeList[,1]=as.character(edgeList[,1])
edgeList[,2]=as.character(edgeList[,2])
el <- as.matrix(edgeList)
library(igraph)
g<-graph.edgelist(el[,1:2])
E(g)$weight = as.double(el[,3])
adj <- get.adjacency(g, attr = 'weight')
adj[1:5,]

dim(adj)
nrow(adj)

W <- adj
v<-as.matrix(newData$hol_toas)
v[1:5]
nrow(W)
I <- diag(nrow(W))

############Data set filterd for shareholder of GreatBritian only######
filterData<- newData[grep("GB",newData$hol_id ), ]
edgeListGB <- data.frame(filterData$hol_id,filterData$sub_id,filterData$link_Dcf)
colnames(edgeListGB)<- c("from", "to","weight")
edgeListGB[1:5,]

edgeListGB[,1]=as.character(edgeListGB[,1])
edgeListGB[,2]=as.character(edgeListGB[,2])
eGB <- as.matrix(edgeListGB)
library(igraph)
gGB<-graph.edgelist(eGB[,1:2])
E(gGB)$weight = as.double(eGB[,3])
adjGB <- get.adjacency(gGB, attr = 'weight')
adjGB[1:5,]
dim(adjGB)

############Data set filterd for shareholder starting with CH ######
DataCH<- newData[grep("CH00",newData$hol_id ), ]
DataCH[,1]
edgeListCH <- data.frame(DataCH$hol_id,DataCH$sub_id,DataCH$link_Dcf)
colnames(edgeListCH)<- c("from", "to","weight")
edgeListCH[1:5,]

#edgeListGB[,1]=as.character(edgeListGB[,1])
#edgeListGB[,2]=as.character(edgeListGB[,2])
eCH <- as.matrix(edgeListCH)
library(igraph)
gCH<-graph.edgelist(eCH[,1:2])
E(gCH)$weight = as.double(eCH[,3])

adjCH <- get.adjacency(gCH, attr = 'weight', sparse=TRUE)
dim(adjCH)
W <- adjCH

v<-as.matrix(DataCH$hol_toas)
length(v)

I <- diag(nrow(W))
WW <- I-W
W_I_inv <- solve(WW)  ## Inverse of I-W
Int_W <- W_I_inv%*% W 
VG <- W_I_inv%*% V





temp<-dataf6[dataf6$hol_toas ==292252.4,]
temp[1:10,]
newdata <- na.omit(dataf6)
max(dataf6$hol_toas,na.rm=TRUE)

##B = matrix( c(2, 4, 3, 1, 5, 7), nrow=3,ncol=2) 
W = matrix( c(0.00,0.2,0.0,0.0,0.0,0.0,0.3,0.2,0.0,0.4,0.0,0.8,0.0,0.4,0.7,0.0), nrow=4,ncol=4)
d = matrix( c(0.2,0.0,0.0),nrow = 1, ncol =3)
I<-diag(4)
WW <- I-W
W_I_inv <- solve(WW)  ## Inverse of I-W
Int_W <- W_I_inv%*% W ## Multuplication of  Inverse of I-W times W


V<- matrix( c(0.0,0.2,0.06,0.04),nrow = 4, ncol =1)
VG <- W_I_inv%*% V
VG


VG <- matrix( c(4.00,15.00,28.64,27.05),nrow = 4, ncol =1)
WG <- matrix( c(0.0,0.4,0.4,0.3,0.0,0.7,0.2,0.8,0.0),nrow = 3, ncol =3)
VG


##d_Var=d*inv(I-WG)
d<- matrix( c(0.2, 0.0, 0.0),nrow = 1, ncol =3)
diag(3)-WG


##Example B
W <- matrix( c(0,0,0,0,0,0,0.1,0,0.3,0.3,0.3,0,0,0.5,0,0,0.5,0,0,0.5,0,0,0.5,0,0,0.2,0.2,0.6,0,0,0,0,0,0,1,0),nrow = 6, ncol =6)
IminusW_inv <- solve(diag(6)-W)
v<- matrix( c(1,1,1,1,1,1),nrow =1, ncol = 6)
v_net <- IminusW_inv %*% t(v)

v_int_temp<- IminusW_inv %*% W
v_int <- v_int_temp %*% t(v)

ddiag<-diag(IminusW_inv)
diag(ddiag)

D<-solve(diag(ddiag))
v_cap_net <- D %*% v_net
v_cap_net

v_cap_int <- D %*% v_int
v_cap_int


W_curl <- IminusW_inv %*% W

W_cap <- D %*% W_curl
W_cap

W_bar <- W %*% (W_cap +D)

v_bar_int <- W_bar %*% t(v)

v_bar_net <- v_bar_int +D %*% t(v)
