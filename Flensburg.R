rm(list = ls())
#libraries
library(readr)
library(igraph)
library(igraphdata)
library(ergm)
library(intergraph)
library(gridExtra)
library(grid)


####Import Data####
#node list
Nodes_Copy_Data <- read_csv("C:/Users/nicco/Desktop/UNI/00Esami in corso/Social Network Analysis/Project/All_Flensburg_Data_Files/Flensburg_Data_Nodes.csv")
Flensburg_Data_Nodes <- read_csv("C:/Users/nicco/Desktop/UNI/00Esami in corso/Social Network Analysis/Project/All_Flensburg_Data_Files/Flensburg_Data_Nodes.csv")
summary(Flensburg_Data_Nodes)

#missing values
NAcols <- colSums(is.na( Flensburg_Data_Nodes ))
NAcols[(NAcols<=10)]
Flensburg_Data_Nodes <- Flensburg_Data_Nodes[, (NAcols<=10) ]
summary(Flensburg_Data_Nodes)

Flensburg_Data_Nodes[is.na(Flensburg_Data_Nodes$LifestyleStage),] #carogne e detriti

Flensburg_Data_Nodes[is.na(Flensburg_Data_Nodes$Kingdom),] #alge batteri ecc..

Flensburg_Data_Nodes[is.na(Flensburg_Data_Nodes)] <- "Other"

##subsets of animals, adults and adults animal
Flensburg_Animals <- Flensburg_Data_Nodes[Flensburg_Data_Nodes$Kingdom=="Animalia", ]
#Flensburg_Adult <- Flensburg_Data_Nodes[Flensburg_Data_Nodes$Stage=="Adult",]
#Flensburg_Adult_Animals <- Flensburg_Data_Nodes[Flensburg_Data_Nodes$Kingdom=="Animalia" & Flensburg_Data_Nodes$Stage=="Adult",]
#Flensburg_Predator <- Flensburg_Data_Nodes[Flensburg_Data_Nodes$ConsumerStrategyStage=="predator",]

##some tables
table( Flensburg_Data_Nodes$Stage, Flensburg_Data_Nodes$`Stage ID` )

grid.table( cbind( table( Flensburg_Data_Nodes$Kingdom ) ) )
grid.table( cbind( table( Flensburg_Data_Nodes$ConsumerStrategyStage ) ) )
grid.table( cbind( table( Flensburg_Data_Nodes$"Stage ID" ) ) )
grid.table( cbind( table( Flensburg_Data_Links$LinkType ) ) )


table( Flensburg_Adult$Kingdom )
table( Flensburg_Adult$ConsumerStrategyStage )

table( Flensburg_Animals$ConsumerStrategyStage )


##edge list
Link_Copy_Data <- read_csv("C:/Users/nicco/Desktop/UNI/00Esami in corso/Social Network Analysis/Project/All_Flensburg_Data_Files/Flensburg_Data_Links.csv")
Flensburg_Data_Links <- read_csv("C:/Users/nicco/Desktop/UNI/00Esami in corso/Social Network Analysis/Project/All_Flensburg_Data_Files/Flensburg_Data_Links.csv")
summary(Flensburg_Data_Links)

NAcols2 <- colSums(is.na( Flensburg_Data_Links ))
NAcols2[(NAcols2<=10)]
Flensburg_Data_Links <- Flensburg_Data_Links[, (NAcols2<=10) ]
summary(Flensburg_Data_Links)

#Flensburg_Links_Animals <- Flensburg_Data_Links[(Flensburg_Data_Links$ConsumerNodeID %in% Flensburg_Animals$`Node ID` & Flensburg_Data_Links$ResourceNodeID %in% Flensburg_Animals$`Node ID` ),]

####Descriptive Analysis ####

table(Nodes_Copy_Data$OrganismalGroup)

####Graph####
#nodes.attr <- names(Flensburg_Data_Nodes[,c(1:4,6,7,10:15)]) 
nodes.attr <- names(Flensburg_Data_Nodes) 
graph <- graph_from_data_frame(Flensburg_Data_Links, vertices = Flensburg_Data_Nodes[,nodes.attr], directed=T )
E(graph)[which_loop(graph)]
V(graph)$WorkingName[V(r2)$name==7] #zooplankton
V(graph)$WorkingName[V(r2)$name==8] #meiofauna

graph <- delete_edges(graph , E(graph)[which_loop(graph)]) #remove loops
#plot(graph,edge.arrow.size = 0.05, vertex.size = 5, vertex.label.cex = 0.5)


#only animals subgraph
subg_animals <- induced.subgraph( graph , V(graph)$Kingdom=="Animalia" )
V(subg_animals)[which( degree(subg_animals, mode="all")==0 )] #no isolated nodes
#plot(subg_animals, edge.arrow.size = 0.05, vertex.size = 5, vertex.label.cex = 0.5)

#components
components(subg_animals) #connected graph
strong_components <- components(subg_animals, mode="strong")
which(strong_components$membership==23) #25 vertices
which(strong_components$membership==95) #3 vertices

#strong component 1 with 3 nodes
subg_strongComp1 <- induced.subgraph( subg_animals , 
                                     V(subg_animals)[which(strong_components$membership==95)] )

plot( main="Main Strong Connected Component,size=degree" ,subg_strongComp1, edge.arrow.size = 0.3, vertex.label = NA, vertex.size=degree(subg_strongComp1))

grid.table( as.data.frame(vertex.attributes(subg_strongComp1)[c(6,7,13)]) )

#strong component 2 with 25 nodes
subg_strongComp <- induced.subgraph( subg_animals , 
                                  V(subg_animals)[which(strong_components$membership==23)] )

V(subg_strongComp)$color <- "orange"
V(subg_strongComp)[OrganismalGroup=="Bird"]$color <- "wheat"
V(subg_strongComp)[OrganismalGroup=="Fish"]$color <- "lightblue"

par(mfrow=c(1,2))
plot( main="Main Strong Connected Component,size=degree" ,subg_strongComp, edge.arrow.size = 0.3, vertex.label = NA, vertex.size=degree(subg_strongComp))
legend(x=-2,y=1, c("Bird", "Fish", "Other"), pch=c(21), col="#777777", pt.bg=c("wheat", "lightblue", "orange" ), pt.cex=5, cex=2, bty="n", ncol=1)

animals_component <- cbind( as.data.frame(vertex.attributes(subg_strongComp)[c(6,7,13)]), degree(subg_strongComp) )
grid.table( animals_component[order(animals_component$`degree(subg_strongComp)`,decreasing = T),] )

#subgraph_animal plot with lgl algorithm
V(subg_animals)$shape <- "circle"
V(subg_animals)$component <- strong_components$membership
V(subg_animals)$color <- "orange"
V(subg_animals)[component==23]$color <- "black"
l <- layout_with_lgl(subg_animals)
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)
set.seed(21)
plot(subg_animals, edge.arrow.size = 0.05, vertex.size = 5, vertex.label = NA,
      layout=l*1.06, rescale=F, main="Biggest Component's Nodes Position in the Graph (lgl algorithm)")


####Subgraph_animals Graphical Representation####

#subgraph_animal plot with lgl algorithm
V(subg_animals)$shape <- "circle"
l <- layout_with_lgl(subg_animals)
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)
set.seed(21)
plot(subg_animals, edge.arrow.size = 0.05, vertex.size = 5, vertex.label = NA,
     vertex.color="orange", layout=l*1.06, rescale=F, main="First Look at Animals Subgraph (lgl algorithm)")



##plot by ConsumerStrategyStage if predator
#out
deg <- degree(subg_animals, mode="out")
V(subg_animals)$size <- deg*0.4
V(subg_animals)$shape <- "circle"
V(subg_animals)[LifestyleStage=="Infectious"]$shape <- "square"
V(subg_animals)$color <- "orange"
V(subg_animals)[ConsumerStrategyStage=="predator"]$color <- "deepskyblue4"
set.seed(21)
plot(subg_animals, edge.arrow.mode = 0, vertex.label = NA,
     edge.color="grey60", layout=l*1.13, rescale=F, main="Predator VS Parasite/non-feeding and Infectious vs Freeliving by Out Degree")
legend(x=-2,y=1, c("Predator","Parasite/non-feeding", "Infectious", "Freeliving"), pch=c(21,21,22,21), col="#777777", pt.bg=c("deepskyblue4","orange","white","white"), pt.cex=2.5, cex=1, bty="n", ncol=1)

#in
deg <- degree(subg_animals, mode="in")
V(subg_animals)$size <- deg*0.4
V(subg_animals)$shape <- "circle"
V(subg_animals)[LifestyleStage=="Infectious"]$shape <- "square"
V(subg_animals)$color <- "orange"
V(subg_animals)[ConsumerStrategyStage=="predator"]$color <- "deepskyblue4"
set.seed(21)
plot(subg_animals, edge.arrow.mode = 0, vertex.label = NA,
     edge.color="grey60", layout=l*1.13, rescale=F, main="Predator VS Parasite/non-feeding and Infectious vs Freeliving by In Degree")
legend(x=-2,y=1, c("Predator","Parasite/non-feeding", "Infectious", "Freeliving"), pch=c(21,21,22,21), col="#777777", pt.bg=c("deepskyblue4","orange","white","white"), pt.cex=2.5, cex=1, bty="n", ncol=1)

##plot by Stage ID
V(subg_animals)$size <- 6
V(subg_animals)$shape <- "circle"
V(subg_animals)$color <- c("grey20", "grey40", "grey60", "grey100" )[V(subg_animals)$"Stage ID"]
plot(subg_animals, edge.arrow.mode = 0, vertex.label = NA,
     edge.color="grey60", layout=l*1.13, rescale=F, main="Animals Live Stage")
legend(x=-2,y=1, 1:4, pch=c(21), col="#777777", pt.bg=c("grey20", "grey40", "grey60", "grey100" ), pt.cex=2.5, cex=1, bty="n", ncol=1)


##color by organismal-group
#organism_group_id <-as.numeric( factor( V(subg_predator)$OrganismalGroup, ordered=T, levels=unique(V(subg_predator)$OrganismalGroup) ))
#organism_group_color <- cbind( unique(organism_group_id), unique(V(subg_predator)$OrganismalGroup), c( "navy","yellow","red","orange","green","white","pink","seagreen","tan","grey40","oldlace","orchid","peru","black","turquoise","sienna","brown","lightgoldenrod","khaki3","greenyellow" ) )
#V(subg_predator)$color <- organism_group_color[,3][organism_group_id]
#plot(subg_predator, edge.arrow.size = 0.05, vertex.size = 5, vertex.label.cex = 0.5)
#
##node size by in degree(prey)
#deg <- degree(subg_predator, mode="in")
#V(subg_predator)$size <- deg*0.7
#
#
##no labels
#V(subg_predator)$label <- NA
#
##change arrow siaze and edge color
#E(subg_predator)$arrow.size <- .1
#E(subg_predator)$edge.color <- "gray80"
#set.seed(21)
#plot(subg_predator, edge.arrow.size = 0.05)
#legend(x=-2.8, y=1,a , pch=21,
#       col="#777777", pt.bg=b, pt.cex=2.5, cex=1, bty="n", ncol=2)
#
#a <- unique( V(subg_predator)$OrganismalGroup[which(deg>5)] )
#b <- unique( V(subg_predator)$color[which(deg>5)] )


####Subgraph_animals Network Statistics####

#reload graph due to previous errors
subg_animals <- induced.subgraph( graph , V(graph)$Kingdom=="Animalia" )


###Q1: DENSITY###

graph.density(subg_animals) #sparse

###Q2: RECIPROCITY###

# subgraph on two vertices
# (A,B) -- three possible relations between them
# null -- (0,0)
# asymmetric -- (0,1), (1,0)
# mutual -- (1,1)

# dyad census
grid.table( cbind( unlist(dyad.census(subg_animals)) ) )

# RECIPROCITY 
reciprocity(subg_animals) #relations observed in the network are not reciprocated at all

### Q3: is there a tendency of the nodes in the network to cluster together?###

# TRIADS

# Let us create a vector of labels with all possible relations
census.labels = c('empty',
                  'A->B, C',
                  'A<->B, C',
                  'A<-B->C',
                  'A->B<-C',
                  'A->B->C',
                  'A<->B<-C',
                  'A<->B->C',
                  'A->B<-C, A->C',
                  'A<-B<-C, A->C',
                  'A<->B<->C',
                  'A<-B->C, A<->C',
                  'A->B<-C, A<->C',
                  'A->B->C, A<->C',
                  'A->B<->C, A<->C',
                  'A<->B<->C, A<->C')

subg_animals.tri = triad.census(subg_animals)
grid.table( data.frame(census.labels, subg_animals.tri  ) ) #no clusters

# TRANSITIVITY COEFFICIENT
# ATT: directed networks are transformed in undirected ones with mode = "collapse"

tr.subg_animals = transitivity(subg_animals)
tr.subg_animals

# How to normalize the transitivity index?  
# let us compare it with the density
dens.subg_animals = graph.density(subg_animals) #sparse

# compute the log-odds ratio
odd.dens.subg_animals = dens.subg_animals/(1-dens.subg_animals)
odd.dens.subg_animals

odd.tr.subg_animals = tr.subg_animals/(1-tr.subg_animals)
odd.tr.subg_animals

# odds ratio
odd.tr.subg_animals/odd.dens.subg_animals
# the chance of observing a relation between nodes sharing a common relation
# is more than 4 times higher than that of observing a relation between two randomly selected nodes

### Q4: are highly connected nodes similar to each other? ###

#ASSORTATIVE MIXING
#attributes
names(vertex.attributes(subg_animals))

subg_parasite <- induced.subgraph( subg_animals , V(subg_animals)$predator==0 )
table( V(subg_parasite)$ConsumerStrategyStage )

#LifestyleStage
table(V(subg_animals)$LifestyleStage)
assortativity.nominal(subg_animals, as.numeric(as.factor(V(subg_animals)$LifestyleStage)) )
assortativity(subg_animals, as.numeric(as.factor(V(subg_animals)$LifestyleStage)) )

#Phylum
table(V(subg_animals)$Phylum)
assortativity.nominal(subg_animals, as.numeric(as.factor(V(subg_animals)$Phylum)) )
assortativity(subg_animals, as.numeric(as.factor(V(subg_animals)$Phylum)) )

#ConsumerStrategyStage
table(V(subg_animals)$ConsumerStrategyStage)
assortativity.nominal(subg_animals, as.numeric(as.factor(V(subg_animals)$ConsumerStrategyStage)) )
assortativity(subg_animals, as.numeric(as.factor(V(subg_animals)$ConsumerStrategyStage)) )


#Predator/Non-predator
V(subg_animals)$predator=0
V(subg_animals)[ConsumerStrategyStage=="predator"]$predator=1
table(V(subg_animals)$predator)
assortativity.nominal(subg_animals, as.numeric(as.factor(V(subg_animals)$predator)) )
assortativity(subg_animals, as.numeric(as.factor(V(subg_animals)$predator)) )

#"Stage ID"
table(V(subg_animals)$"Stage ID")
assortativity.nominal(subg_animals, as.numeric(as.factor(V(subg_animals)$"Stage ID")) )
assortativity(subg_animals, as.numeric(as.factor(V(subg_animals)$"Stage ID")) )

#Feeding
table(V(subg_animals)$Feeding)
assortativity.nominal(subg_animals, as.numeric(as.factor(V(subg_animals)$Feeding)) )
assortativity(subg_animals, as.numeric(as.factor(V(subg_animals)$Feeding)) )


#OrganismalGroup
table(V(subg_animals)$OrganismalGroup)
assortativity.nominal(subg_animals, as.numeric(as.factor(V(subg_animals)$OrganismalGroup)) )
assortativity(subg_animals, as.numeric(as.factor(V(subg_animals)$OrganismalGroup)) )

#disassortative mixing tendency


####Subgraph_animals Nodal Statistics####

# 1) DEGREE CENTRALITY

# A node is central if it is connected to many other nodes

##ALL degree centrality
graph_degree <- degree(subg_animals, mode="all")
graph_degree_std <- degree(subg_animals, normalized = T, mode="all")
ord = order(graph_degree, decreasing = T)
ord_std = order(graph_degree_std, decreasing = T)
summary(graph_degree_std)
summary(graph_degree)

#degree centrality histogram
par(mfrow=c(1,2))
hist(graph_degree, breaks = 10)
hist(graph_degree_std, breaks = 10)

#vertex dataframe
vertex_data <- as.data.frame( vertex.attributes(subg_animals) )
vertex_data[which(graph_degree_std>0.25),c("name","WorkingName","OrganismalGroup")] # more trophic
vertex_data[ord_std[1:3],c("name","WorkingName","OrganismalGroup")] # highly central

#graphical rep of all degree centr.
V(subg_animals)$color <- "orange"
V(subg_animals)$color[which(graph_degree_std>0.25)] = "lightblue"
par(mfrow=c(1,1))
l <- layout_with_lgl(subg_animals)
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)
set.seed(21)
plot(subg_animals, edge.arrow.size = 0.05, vertex.size = graph_degree_std*20, vertex.label = NA,
     layout=l*1.06, rescale=F, main="Animals Subgraph by ALL degree centr. (lgl algorithm)")

##IN degree centrality (popularity/prey)
graph_degree <- degree(subg_animals, mode="in")
graph_degree_std <- degree(subg_animals, normalized = T, mode="in")
ord = order(graph_degree, decreasing = T)
ord_std = order(graph_degree_std, decreasing = T)
summary(graph_degree_std)
summary(graph_degree)

#degree centrality histogram
par(mfrow=c(1,2))
hist(graph_degree, breaks = 10, main="Histogram of In-Degree")
hist(graph_degree_std, breaks = 10, main="Histogram of Stadardized In-Degree")

#vertex dataframe
vertex_data <- as.data.frame( vertex.attributes(subg_animals) )
vertex_data$INdegree <- graph_degree
vertex_data$INdegreeSTD <- graph_degree_std
grid.table( vertex_data[ord_std[1:5],c("name","WorkingName","OrganismalGroup","ConsumerStrategyStage","INdegree","INdegreeSTD")] )# highly central

#graphical rep of all degree centr.
V(subg_animals)$color <- "orange"
par(mfrow=c(1,1))
l <- layout_with_lgl(subg_animals)
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)
set.seed(21)
plot(subg_animals, edge.arrow.size = 0.05, vertex.size = graph_degree_std*40, vertex.label = NA,
     layout=l*1.06, rescale=F, main="Animals Subgraph by IN degree centr. (lgl algorithm)")

##plot by ConsumerStrategyStage if predator
#in
deg <- degree(subg_animals, mode="out")
V(subg_animals)$size <- graph_degree_std*50
V(subg_animals)$shape <- "circle"
V(subg_animals)[LifestyleStage=="Infectious"]$shape <- "square"
V(subg_animals)$color <- "orange"
V(subg_animals)[ConsumerStrategyStage=="predator"]$color <- "deepskyblue4"
set.seed(21)
plot(subg_animals, edge.arrow.mode = 0, vertex.label = NA,
     edge.color="grey60", layout=l*1.13, rescale=F, main="Predator VS Parasite/non-feeding and Infectious vs Freeliving by Standard IN Degree Centrality")
legend(x=-2,y=1, c("Predator","Parasite/non-feeding", "Infectious", "Freeliving"), pch=c(21,21,22,21), col="#777777", pt.bg=c("deepskyblue4","orange","white","white"), pt.cex=2.5, cex=1, bty="n", ncol=1)



##OUT degree centrality (expansiveness/predator)
graph_degree <- degree(subg_animals, mode="out")
graph_degree_std <- degree(subg_animals, normalized = T, mode="out")
ord = order(graph_degree, decreasing = T)
ord_std = order(graph_degree_std, decreasing = T)
summary(graph_degree_std)
summary(graph_degree)

#degree centrality histogram
par(mfrow=c(1,2))
hist(graph_degree, breaks = 10)
hist(graph_degree_std, breaks = 10, main="Histogram of Stadardized Out-Degree")

#vertex dataframe
vertex_data <- as.data.frame( vertex.attributes(subg_animals) )
vertex_data$OUTdegree <- graph_degree
vertex_data$OUTdegreeSTD <- graph_degree_std
grid.table( vertex_data[ord_std[1:5],c("name","WorkingName","OrganismalGroup","ConsumerStrategyStage","OUTdegree","OUTdegreeSTD")] )# highly central
grid.table( cbind(vertex_data[which(graph_degree_std>0.2),c("name","WorkingName","OrganismalGroup","ConsumerStrategyStage","OUTdegree")],graph_degree[which(graph_degree_std>0.2)]) ) # more trophic


#graphical rep of all degree centr.
V(subg_animals)$color <- "orange"
V(subg_animals)[OrganismalGroup=="Bird"]$color <- "lightblue"
V(subg_animals)[OrganismalGroup=="Crab"]$color <- "red4"
V(subg_animals)[OrganismalGroup=="Fish"]$color <- "gold"
V(subg_animals)[OrganismalGroup=="Shrimp"]$color <- "green"

par(mfrow=c(1,1))
l <- layout_with_lgl(subg_animals)
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)
set.seed(21)
plot(subg_animals, edge.arrow.size = 0.05, vertex.size = graph_degree_std*30, vertex.label = NA,
     layout=l*1.06, rescale=F, main="Animals Subgraph by ALL degree centr. (lgl algorithm)")

##plot by ConsumerStrategyStage if predator
#out
deg <- degree(subg_animals, mode="out")
V(subg_animals)$size <- graph_degree_std*30
V(subg_animals)$shape <- "circle"
V(subg_animals)[LifestyleStage=="Infectious"]$shape <- "square"
V(subg_animals)$color <- "orange"
V(subg_animals)[ConsumerStrategyStage=="predator"]$color <- "deepskyblue4"
set.seed(21)
plot(subg_animals, edge.arrow.mode = 0, vertex.label = NA,
     edge.color="grey60", layout=l*1.13, rescale=F, main="Predator VS Parasite/non-feeding and Infectious vs Freeliving by Standard Out Degree Centrality")
legend(x=-2,y=1, c("Predator","Parasite/non-feeding", "Infectious", "Freeliving"), pch=c(21,21,22,21), col="#777777", pt.bg=c("deepskyblue4","orange","white","white"), pt.cex=2.5, cex=1, bty="n", ncol=1)


# 2) CLOSENESS CENTRALITY
# A node is central if it is "close" to many other nodes
# z_i^c = 1/sum_j d_ij

closOUT <- closeness(subg_animals, mode="out")
closOUT_std<- closeness(subg_animals, normalized = T, mode="out")
closIN_std<- closeness(subg_animals, normalized = T, mode="in")
closALL_std<- closeness(subg_animals, normalized = T, mode="all")
ordOUT <-  order(closOUT_std,decreasing = T)
ordIN <- order(closIN_std, decreasing = T)
ordALL <- order(closALL_std, decreasing = T)
cbind(ordOUT, ordIN,  ordALL) #different results

grid.table( cbind( vertex_data[ordALL[1:5],c("name","WorkingName","OrganismalGroup","ConsumerStrategyStage")], closOUT_std[ordALL[1:5]] ) ) # highly central
grid.table( cbind( vertex_data[ordOUT[1:5],c("name","WorkingName","OrganismalGroup","ConsumerStrategyStage")], closOUT_std[ordOUT[1:5]] ) )# highly central
grid.table( cbind( vertex_data[ordIN[1:5],c("name","WorkingName","OrganismalGroup","ConsumerStrategyStage")],  closIN_std[ordIN[1:5]] ) )# highly central


par(mfrow=c(2,1))
hist(closOUT_std, breaks=10)
hist(closIN_std, breaks=10)
#hist(closALL_std, breaks=10)

##plot by ConsumerStrategyStage if predator
#out
deg <- degree(subg_animals, mode="out")
V(subg_animals)$size <- closOUT_std*30
V(subg_animals)$shape <- "circle"
V(subg_animals)[LifestyleStage=="Infectious"]$shape <- "square"
V(subg_animals)$color <- "orange"
V(subg_animals)[ConsumerStrategyStage=="predator"]$color <- "deepskyblue4"
set.seed(21)
plot(subg_animals, edge.arrow.mode = 0, vertex.label = NA,
     edge.color="grey60", layout=l*1.13, rescale=F, main="Predator VS Parasite/non-feeding and Infectious vs Freeliving by Standard Out Degree Centrality")
legend(x=-2,y=1, c("Predator","Parasite/non-feeding", "Infectious", "Freeliving"), pch=c(21,21,22,21), col="#777777", pt.bg=c("deepskyblue4","orange","white","white"), pt.cex=2.5, cex=1, bty="n", ncol=1)

##plot by ConsumerStrategyStage if predator
#in
deg <- degree(subg_animals, mode="out")
V(subg_animals)$size <- closIN_std*30
V(subg_animals)$shape <- "circle"
V(subg_animals)[LifestyleStage=="Infectious"]$shape <- "square"
V(subg_animals)$color <- "orange"
V(subg_animals)[ConsumerStrategyStage=="predator"]$color <- "deepskyblue4"
set.seed(21)
plot(subg_animals, edge.arrow.mode = 0, vertex.label = NA,
     edge.color="grey60", layout=l*1.13, rescale=F, main="Predator VS Parasite/non-feeding and Infectious vs Freeliving by Standard Out Degree Centrality")
legend(x=-2,y=1, c("Predator","Parasite/non-feeding", "Infectious", "Freeliving"), pch=c(21,21,22,21), col="#777777", pt.bg=c("deepskyblue4","orange","white","white"), pt.cex=2.5, cex=1, bty="n", ncol=1)


# 3) BETWEENNESS CENTRALITY
# a node is central if it is located between many nodes
bt_centr = betweenness(subg_animals, directed = T, normalized = F)
summary(bt_centr)

bt_centr_std = betweenness(subg_animals, directed = T, normalized = T)
summary(bt_centr_std)

par(mfrow=c(1,2))
hist(bt_centr, breaks=10)
hist(bt_centr_std, breaks=10, main="Histogram of Stadardized Betweenness Centrality")

bt_ord_std <- order(bt_centr_std, decreasing = T)
vertex_data[bt_ord_std[1:5],c("name","WorkingName","OrganismalGroup","ConsumerStrategyStage")] # highly central
grid.table( cbind( vertex_data[bt_ord_std[1:10],c("name","WorkingName","OrganismalGroup","ConsumerStrategyStage")],  bt_centr_std[bt_ord_std[1:10]] ) )# highly central

##plot by ConsumerStrategyStage if predator
#out
deg <- degree(subg_animals, mode="out")
V(subg_animals)$size <- bt_centr_std*300
V(subg_animals)$shape <- "circle"
V(subg_animals)[LifestyleStage=="Infectious"]$shape <- "square"
V(subg_animals)$color <- "orange"
V(subg_animals)[ConsumerStrategyStage=="predator"]$color <- "deepskyblue4"
set.seed(21)
plot(subg_animals, edge.arrow.mode = 0, vertex.label = NA,
     edge.color="grey60", layout=l*1.07, rescale=F, main="Predator VS Parasite/non-feeding and Infectious vs Freeliving by Standard Betweenness Centrality")
legend(x=-2,y=1, c("Predator","Parasite/non-feeding", "Infectious", "Freeliving"), pch=c(21,21,22,21), col="#777777", pt.bg=c("deepskyblue4","orange","white","white"), pt.cex=2.5, cex=1, bty="n", ncol=1)



# 4) EIGENVECTOR CENTRALITY
# a node is central if it is connected to other central nodes

ei_centr <- eigen_centrality(subg_animals, directed = T, scale = F)$vector
summary(ei_centr)

ei_centr_std <- eigen_centrality(subg_animals, directed = T, scale = T)$vector
summary(ei_centr_std)

ei_centr_undirected <- eigen_centrality(subg_animals, directed = F, scale = F)$vector
summary(ei_centr_undirected)

ei_list <- cbind(ei_centr, ei_centr_std, ei_centr_undirected , V(subg_animals)$WorkingName)
ei_list[order(ei_list[,1], decreasing = T),] #directed metrhod
ei_list[order(ei_list[,3], decreasing = T),] #undirected method

hist(ei_centr_std, breaks=10)

ord_ei_centr_std <- order(ei_centr_std, decreasing = T)
vertex_data[ord_ei_centr_std[1:5],c("name","WorkingName","OrganismalGroup","ConsumerStrategyStage")] # highly central
grid.table( cbind( vertex_data[ord_ei_centr_std[1:5],c("name","WorkingName","OrganismalGroup","ConsumerStrategyStage")],  ei_centr_std[ord_ei_centr_std[1:5]] ) )# highly central


#plot according to eigen centrality
V(subg_animals)$color = "orange"
V(subg_animals)$color[ord_ei_centr_std[1:3]] = "lightblue"

par(mfrow=c(1,1))
l <- layout_with_lgl(subg_animals)
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)
set.seed(21)
plot(subg_animals, edge.arrow.size = 0.05, vertex.size = ei_centr_std*30, vertex.label = NA,
     layout=l*1.06, rescale=F, main="Animals Subgraph by eigenvector centrality (lgl algorithm)")


## NETWORK CENTRALIZATION
centr_degree(subg_animals,mode="all", loops = F)$centralization
centr_degree(subg_animals,mode="out", loops = F)$centralization
centr_degree(subg_animals,mode="in", loops = F)$centralization

centr_clo(subg_animals, mode="all" )$centralization
centr_clo(subg_animals, mode="out" )$centralization
centr_clo(subg_animals, mode="in" )$centralization



centr_betw(subg_animals, directed = T)$centralization

centr_eigen(subg_animals, directed = T )$centralization
centr_eigen(subg_animals, directed = T, scale = F )$centralization
#0.4136262

#### Simple Random Graph ####

# let us extract the adjacency matrix
Y = get.adjacency(subg_animals, sparse = F)
diag(Y) = NA

# number of nodes
n = nrow(Y)

# let us compare the observed network with the null model via the density statistic
rho.obs = mean(Y, na.rm = T)
rho.obs

# let us consider the in-degree centralization and the clustering coefficient statistic
CId.obs = centr_degree(subg_animals, mode = "in", loops = F)$centralization
CId.obs  

COd.obs  <- centr_degree(subg_animals,mode="out", loops = F)$centralization
COd.obs

C.obs = transitivity(subg_animals)
C.obs

R.obs = reciprocity(subg_animals)
R.obs


# 1.Is the observed network coherent with the family of Binomial random graph models G(n, p)?
# a naive approach based on the best case scenario

# maximum likelihood estimate of p
p.MLE = mean(Y, na.rm = T)

B = 1000
rho.sim = CId.sim = C.sim = COd.obs.sim = R.obs.sim = c()
for(b in 1:B){
        tmp = rbinom(n^2,1,p.MLE)  
        Y.sim = matrix(tmp, n,n)
        diag(Y.sim) = NA
        rho.sim[b] = mean(Y.sim, na.rm = TRUE)
        g.sim = graph_from_adjacency_matrix(Y.sim)
        CId.sim[b] = centr_degree(g.sim, mode = "in", loops = F)$centralization
        C.sim[b] = transitivity(g.sim)
        COd.obs.sim[b] = centr_degree(g.sim, mode = "out", loops = F)$centralization
        R.obs.sim[b]= reciprocity(g.sim)
}


# Graphical comparison
par(mfrow = c(2,3))
low = pmin(min(rho.sim), rho.obs) - 0.05
up = pmax(max(rho.sim), rho.obs) + 0.05
hist(rho.sim, col = "lightgray", main = "Null distribution", xlim = c(low, up))
abline(v = rho.obs, col = "red", lwd=2)


low = pmin(min(CId.sim), CId.obs) - 0.05
up = pmax(max(CId.sim), CId.obs) + 0.05
hist(CId.sim, col = "lightgray", main = "Null distribution", xlim = c(low, up))
abline(v = CId.obs, col = "red", lwd=2)


low = pmin(min(C.sim), C.obs) - 0.05
up = pmax(max(C.sim), C.obs) + 0.05
hist(C.sim, col = "lightgray", main = "Null distribution", xlim = c(low, up))
abline(v = C.obs, col = "red", lwd=2)

low = pmin(min(COd.obs.sim), COd.obs) - 0.05
up = pmax(max(COd.obs.sim), COd.obs) + 0.05
hist(COd.obs.sim, col = "lightgray", main = "Null distribution", xlim = c(low, up))
abline(v = COd.obs, col = "red", lwd=2)

low = pmin(min(R.obs.sim), R.obs) - 0.05
up = pmax(max(R.obs.sim), R.obs) + 0.05
hist(R.obs.sim, col = "lightgray", main = "Null distribution", xlim = c(low, up))
abline(v = R.obs, col = "red", lwd=2)

# compute an approximate p-value
mean(rho.sim >= rho.obs)
mean(CId.sim >= CId.obs)
mean(C.sim >= C.obs)
mean(COd.obs.sim >= COd.obs)
mean(R.obs.sim <= R.obs)
# is the network coherent with the family of BRG models [G(n, p)] in terms of the above statistics?




# 3. Is the observed network coherent with a Binomial random graph model G(n, p)?
# a more formal approach based on the conditional uniform distribution

# let us consider the degree centralization index and the transitivity coefficient
# NB. the density statistic makes no sense -- by fixing m, then rho = p is constant 
B = 5000
m =  sum(Y, na.rm = TRUE)
rho.sim = CId.sim = C.sim = COd.obs.sim = R.obs.sim = c()
for(b in 1:B){
        Y.sim = matrix(, n, n) 
        ones = rep(1, m)
        zeros = rep(0, n*(n-1) - m)
        all = c(ones, zeros)
        Y.sim[col(Y.sim) != row(Y.sim)] = sample(all, n*(n-1))
        g.sim = graph_from_adjacency_matrix(Y.sim)
        CId.sim[b] = centr_degree(g.sim, mode = "in", loops = F)$centralization
        C.sim[b] = transitivity(g.sim)
        COd.obs.sim[b] = centr_degree(g.sim, mode = "out", loops = F)$centralization
        R.obs.sim[b]= reciprocity(g.sim)
}


# Graphical comparison
par(mfrow = c(2,2))

low = pmin(min(CId.sim), CId.obs) - 0.05
up = pmax(max(CId.sim), CId.obs) + 0.05
hist(CId.sim, col = "lightgray", main = "Null distribution", xlim = c(low, up))
abline(v = CId.obs, col = "red", lwd=2)


low = pmin(min(C.sim), C.obs) - 0.05
up = pmax(max(C.sim), C.obs) + 0.05
hist(C.sim, col = "lightgray", main = "Null distribution", xlim = c(low, up))
abline(v = C.obs, col = "red", lwd=2)

low = pmin(min(COd.obs.sim), COd.obs) - 0.05
up = pmax(max(COd.obs.sim), COd.obs) + 0.05
hist(COd.obs.sim, col = "lightgray", main = "Null distribution", xlim = c(low, up))
abline(v = COd.obs, col = "red", lwd=2)

low = pmin(min(R.obs.sim), R.obs) - 0.05
up = pmax(max(R.obs.sim), R.obs) + 0.05
hist(R.obs.sim, col = "lightgray", main = "Null distribution", xlim = c(low, up))
abline(v = R.obs, col = "red", lwd=2)

#p-values
mean(CId.sim >= CId.obs)
mean(C.sim >= C.obs)
mean(COd.obs.sim >= COd.obs)
mean(R.obs.sim <= R.obs)




####Exponential Random Graph Model####

# response variable 
Y = as.matrix(get.adjacency(subg_animals), sparse = F)
diag(Y) = NA
y = c(Y)
# number of nodes
n = nrow(Y)
n


# covariates -- sender and receiver effects
rowIdx = row(Y)
colIdx = col(Y)
rowidx = c(rowIdx)
colidx = c(colIdx)

# estimate the parameters
mod = glm(y ~ factor(rowidx) + factor(colidx), family = "binomial")
mod

# more..
summary(mod)

# compute tie probability
pij = mod$fitted.values
# ATT: when using this latter approach, only off diagonal elements are given!

##assessing significance for the non-homogeneous SRG model
din = degree(subg_animals, mode = "in")

dout = degree(subg_animals, mode = "out")


# null distribution
sdIn.sim = sdOut.sim = recip.sim = c()
for(b in 1:1000){
        tmp = rbinom(n*(n-1),1,pij)
        Y.sim = matrix(NA, n,n); Y.sim[row(Y.sim) != col(Y.sim)] = tmp
        g.sim = graph_from_adjacency_matrix(Y.sim)
        sdIn.sim[b] = sd(degree(g.sim, mode = "in"))
        sdOut.sim[b] = sd(degree(g.sim, mode = "out"))
        recip.sim[b] = reciprocity(g.sim)
}

# graphical comparison
par(mfrow = c(1,3))
hist(sdIn.sim, breaks = 20, col = "lightgray", main = "", 
     prob = TRUE, xlab = "sd(in-degree)")
abline(v = sd(din), col = "red", lwd = 2)

hist(sdOut.sim, breaks = 20, col = "lightgray", main = "", 
     prob = TRUE, xlab = "sd(out-degree)")
abline(v = sd(dout), col = "red", lwd = 2)

# graphical comparison
hist(recip.sim, breaks = 20, col = "lightgray", main = "", 
     prob = TRUE, xlab = "reciprocity", xlim=c(0,.12))
abline(v = reciprocity(subg_animals), col = "red", lwd = 2)

# p-value
mean(sdIn.sim < sd(din))
mean(sdOut.sim < sd(dout))
mean(recip.sim < reciprocity(subg_animals))

# --------- formal approach -- conditional uniform distribution
# we need to condition on the sufficient statistics
# 1) m = y..
# 2) y_i., i = 1, ..., n
# 3) y_.j, j = 1, ..., n

# simulate from the conditional uniform distribution
# simulating function -- alternating rectangles
aRect.fnc = function(Y, k){
        
        # Y = adjacency matrix
        # k = n. of steps in the alternating rectangles algorithm 
        
        Y1 = matrix(c(0,1,1,0), 2, 2)
        Y2 = 1 - Y1
        
        n = nrow(Y)
        
        for(s in 1:k){
                # draw 4 distinct indexes
                # two rows and two columns
                ij = sample(1:n,4,replace = F)
                
                # select the corresponding sub-matrix
                rows = ij[1:2]
                cols = ij[3:4]
                Yij = Y[rows, cols]
                
                # perturbation
                if(all(Yij == Y1)) Yij = Y2 else if(all(Yij == Y2))  Yij = Y1
                
                Y[rows, cols] = Yij
        }
        
        return(Y)
}

sdIn.sim = sdOut.sim = recip.sim = trans.sim = c()
for(b in 1:1000){
        Y.sim = aRect.fnc(Y, 10000)
        # print number of perturbed elements in Y.sim 
        cat(sum(Y != Y.sim, na.rm = T), "*", sep="")
        g.sim = graph_from_adjacency_matrix(Y.sim)
        sdIn.sim[b] = sd(degree(g.sim, mode = "in"))
        sdOut.sim[b] = sd(degree(g.sim, mode = "out"))
        recip.sim[b] = reciprocity(g.sim)
        trans.sim[b] = transitivity(g.sim)
        
}

# graphical comparison
par(mfrow = c(1,2))
#hist(sdIn.sim)
#abline(v = sd(din), col = "red", lty = 2)
#
#hist(sdOut.sim)
#abline(v = sd(dout), col = "red", lty = 2)

hist(trans.sim)
abline(v = transitivity(subg_animals), col = "red", lty = 2)

hist(recip.sim)
abline(v = reciprocity(subg_animals), col = "red", lty = 2)

# p-value
mean(sdIn.sim > sd(din))
mean(sdOut.sim > sd(dout))
mean(recip.sim < reciprocity(subg_animals))
mean(recip.sim <= reciprocity(subg_animals))
mean(trans.sim < transitivity(subg_animals))
mean(trans.sim <= transitivity(subg_animals))


#### Model Selection ####

#get adj marix
Y = get.adjacency(subg_animals, sparse = F)
diag(Y) = NA
p.MLE = mean(Y, na.rm = T)
p.MLE

#network object
net = network(Y,vertex.attr = vertex.attributes(subg_animals) ,directed = T)
net %e% "LinkNumber" = E(subg_animals)$LinkNumber
net %e% "LinkType" = E(subg_animals)$LinkType
class(net)
net

# -------- mod1: homogeneous binomial random graph model

brg = ergm(net ~ edges, estimate = "MPLE")
# as usual, some more results?
summary(brg)

odds = exp(brg$coef)
odds 
# the risk of not observing a tie is 95% lower than that of observing a tie

# furhtermore, by applying the expit transform, we have
exp(brg$coef)/(1+exp(brg$coef))
# which is exactly equal to the density of the network
# that is it is equal to p.MLE
p.MLE

# ------ mod 2: non-homogeneous BRG

nh_brg = ergm(net ~ edges + sender + receiver, estimate = "MPLE")

# looking at the estimated parameters
head(nh_brg$coef)

# to interpret these parameters, let us consider the exponential transform
exp(nh_brg$coef)

#  models selection via AIC or BIC
AIC(brg, nh_brg)
BIC(brg, nh_brg) #brg much much better

#  ----------- mod 3:  p1 model
# which are the sufficient statistics? 
# y.. -> that is, the number of edges
# yi. -> that is, the set of out-degrees
# y.j -> that is, the set of in-degrees
# gamma --> that is, the number of reciprocal ties

p1 = ergm(net ~ edges + sender + receiver + mutual, estimate = "MPLE")
summary(p1)
p1$coef[length(p1$coef)] #mutual
coef(summary(p1))[344,] #significant, ties not reciprocated
exp(p1$coef[length(p1$coef)])
AIC(p1)
BIC(p1)


# ----- mod 4 -- Markov graph model

mark3 = ergm(net ~ edges +  istar(2) + ostar(2) + istar(3) + ostar(3), estimate = "MPLE")
summary(mark3)

mark4 = ergm(net ~ edges +  istar(2) + ostar(2), estimate = "MPLE")
summary(mark4)

# which model should we prefer? 
AIC(mark3, mark4)
BIC(mark3, mark4) #mark 3 nodoubt

# what about triangles? 
mark5 = ergm(net ~ edges +  istar(2) + ostar(2)+ istar(3) + ostar(3) + triangles, estimate = "MPLE")
summary(mark5)


mark6 = ergm(net ~ edges +  istar(2) + ostar(2)+ istar(3) + ostar(3) + mutual+ triangles, estimate = "MPLE")
summary(mark6)

mark7 = ergm(net ~ edges +triangles+  istar(2) + istar(3) + ostar(3) + mutual+ triadcensus(3:6), estimate = "MPLE")
summary(mark7)


# which model should we prefer? 
AIC(mark3, mark5, mark6, mark7)
BIC(mark3, mark5, mark6, mark7) # mark 6 slighlty better



####MCMC MODEL ESTIMATION####

##### ! ATT -- MCMCMLE used for estimation 

#------ let us now consider the dyad independence model(p1)
#Note: receiver and sender to many cannot estimate
mod5 = ergm(net ~ edges + mutual ,control = control.ergm(seed = 1))
summary(mod5)

# has the model converged?
dev.new()
mcmc.diagnostics(mod5)

#--- include nodal attributes
# main effects 
# quantitative attributes -- nodecov(attr)
# qualitative attributes -- nodefactor(attr)

# homophily effects
# quantitative attributes -- absdiff(attr) - sum_ij[abs(attr_i - attr_j)y_ij]
# qualititave attributes -- nodematch(attr) 
mod6 = ergm(net ~ edges + mutual + nodefactor("predator") + 
                    nodefactor("Stage ID") + 
                    nodematch("predator")+ nodematch("Stage ID")+
                    nodematch("Phylum")+nodefactor("Phylum"),
            control = control.ergm(seed = 1)) 
summary(mod6)

dev.new()
mcmc.diagnostics(mod6)
# again, convergence seems to be acheaved


###---let us move towards the Markov graph model
# Let us add to the model the triangle term, the in- and the out-stars of order 2
# (indicating the tendency to form clusters in the network)

mod10 = ergm(net ~ edges + mutual + istar(2) +
                      ostar(2) + ostar(3) + istar(3) + triangle, 
             control = control.ergm(seed = 1))

summary(mod10)
#degeneracy issues

mod10.2= ergm(net ~ edges +  istar(2) + ostar(2)  , control = control.ergm(seed = 1))
summary(mod10.2)
#degeneracy issues

# let us try to remove triangles
mod11 = ergm(net ~ edges + mutual + nodefactor("predator") + 
                     nodefactor("Stage ID") + nodefactor("Phylum") + 
                     nodematch("predator")+ nodematch("Stage ID")+
                     nodematch("Phylum") + istar(2) + ostar(2), 
             control = control.ergm(seed = 1))
#degeneracy issues

# let us try to solve the issue by considering the alternating k-star term
# ergm implements some modified forms of such statistics

# for directed networks 
# gwidegree(decay, fixed = FALSE) -- decay = log(lambda/(lambda-1))
# gwodegree(decay, fixed = FALSE) -- decay = log(lambda/(lambda-1))
# gwidegree/gwodegree -- geometrically weighted in-/out- degree distribution

# positive estimates -- centralized network --  few nodes with high degree
# negative estimates -- non centralized network

# a standard choice for decay is 1, but model selection can be used!
mod10 = ergm(net ~ edges + triangle + nodefactor("predator") + 
                     nodefactor("Stage ID") +  
                     nodematch("predator")+ nodematch("Stage ID")+
                     gwidegree(decay = 2, fixed = TRUE), 
             control = control.ergm(seed = 100))

mod11 = ergm(net ~ edges + triangle + nodefactor("predator") + 
                     nodefactor("Stage ID") + nodefactor("Phylum") + 
                     nodematch("predator")+ nodematch("Stage ID")+
                     nodematch("Phylum") + gwodegree(decay = 1, fixed = TRUE), 
             control = control.ergm(seed = 1))
#degeneracy issues


mod12 = ergm(net ~ edges + mutual +  ostar(2)+ ostar(3)+ 
             + gwodegree(decay = log(1.3), fixed = TRUE) , 
             control = control.ergm(seed = 1))

summary(mod12)

mod12.2 = ergm(net ~ edges + mutual +  ostar(2)+ ostar(3)+ 
                     + gwodegree(decay = log(1.3), fixed = TRUE)+
                       nodefactor("predator") + 
                       nodefactor("Stage ID") + nodefactor("Phylum") + 
                       nodematch("predator")+ nodematch("Stage ID")+
                       nodematch("Phylum"), 
             control = control.ergm(seed = 1))

summary(mod12.2)

dev.new()
mcmc.diagnostics(mod12.2)

--#-- let us consider the social circuit model 
# alternating k-triangles --> gwesp(decay = 0, fixed = FALSE) 
# geometrically weighted edge-wise shared partners
# the corresponding parameter expresses the tendency for tied nodes 
# to have multiple shared partners

# alternating k-2-paths --> gwdsp(decay = 0, fixed = FALSE)
# geometrically weighted dyad-wise shared partners
# the corresponding parameter expresses the tendency for dyads 
# (whether tied or not) to have multiple shared partners

mod13 = ergm(net ~ edges + mutual + nodefactor("predator") + 
                     nodefactor("Stage ID") + nodefactor("Phylum") + 
                     nodematch("predator")+ nodematch("Stage ID")+
                     nodematch("Phylum") + gwesp(fixed = T) + 
                     gwdsp(fixed = T), control = control.ergm(seed=1))
summary(mod13)


# let us remove the gwesp term
mod14 = ergm(net ~ edges + mutual + nodefactor("predator") + 
                     nodefactor("Stage ID") +  
                     nodematch("predator")+ nodematch("Stage ID")
             +gwdsp(fixed = T), control = control.ergm(seed=1))

summary(mod14)



mod15 = ergm(net ~ edges + mutual + nodefactor("predator") + 
                     nodefactor("Stage ID") +  
                     nodematch("predator")+ nodematch("Stage ID")+
                     nodefactor("Phylum")+nodematch("Phylum")+
                     gwdsp(fixed = T), control = control.ergm(seed=1))

summary(mod15)

mod16 = ergm(net ~ edges + mutual + nodefactor("predator") + 
                     nodefactor("Stage ID") +  
                     nodematch("predator")+ nodematch("Stage ID")+
                     nodefactor("Phylum")+nodematch("Phylum")+
                     gwdsp(fixed = T)+ gwesp(fixed = T), control = control.ergm(seed=1))


summary(mod16)

mod17 = ergm(net ~ edges + mutual + nodefactor("predator") + 
                     nodefactor("Stage ID") +  
                     nodematch("predator")+ nodematch("Stage ID")+
                     nodefactor("Phylum")+nodematch("Phylum")+
                     dgwdsp(fixed = T, type="OSP"), control = control.ergm(seed=1))

summary(mod17)

AIC( mod13, mod14, mod15, mod16, mod17  )
BIC( mod13, mod14, mod15, mod16, mod17  )



#mod18 = ergm(net ~ edges + mutual + nodefactor("predator") + 
#                     nodefactor("Stage ID") +  
#                     nodematch("predator")+ nodematch("Stage ID")+
#                     nodefactor("Phylum")+nodematch("Phylum")+
#                     dgwdsp(fixed = T, type="OSP")+gwdsp(fixed = T) , control = control.ergm(seed=1))
#
#summary(mod18)

dev.new()
mcmc.diagnostics(mod17)

significant_groups=c("Annelida","Chordata", "Ciliophora", "Crustacea", "Mollusca", "Platyhelminthes")

mod19 = ergm(net ~ edges + mutual + nodefactor("predator") + 
                     nodefactor("Stage ID") +  
                     nodematch("predator")+ nodematch("Stage ID")+
                     nodefactor("Phylum", levels=significant_groups)+nodematch("Phylum")+
                     dgwdsp(fixed = T, type="OSP") , control = control.ergm(seed=1))

summary(mod19)

mod20= ergm(net ~ edges + mutual + nodefactor("predator") + 
                     nodefactor("Stage ID") +  
                     nodematch("predator")+ nodematch("Stage ID")+
                     nodefactor("Phylum", levels=significant_groups)+nodematch("Phylum")+
                    dgwesp(fixed=T, type="ISP")+dgwdsp(fixed = T, type="OSP") , control = control.ergm(seed=1))

summary(mod20)

mod21= ergm(net ~ edges + mutual + nodefactor("predator") + 
                    nodefactor("Stage ID") +  
                    nodematch("predator")+ nodematch("Stage ID")+
                    nodefactor("Phylum")+nodematch("Phylum")+
                    dgwesp(fixed=T, type="ISP")+dgwdsp(fixed = T, type="OSP") , control = control.ergm(seed=1))

summary(mod21)

mod22= ergm(net ~ edges + mutual + nodefactor("predator") + 
                    nodefactor("Stage ID", levels=c(1,2,3)) +  
                    nodematch("predator")+ nodematch("Stage ID")+
                    nodefactor("Phylum", levels=significant_groups)+nodematch("Phylum")+
                    dgwesp(fixed=T, type="ISP")+dgwdsp(fixed = T, type="OSP") , control = control.ergm(seed=1))

summary(mod22)

mod172 = ergm(net ~ edges + mutual + nodefactor("predator") + 
                     nodefactor("Stage ID") +  
                     nodematch("predator")+ nodematch("Stage ID")+
                     nodefactor("Phylum")+nodematch("Phylum")+
                     dgwdsp(fixed = T, type="OSP")+ dgwesp(fixed = T, type="ISP")
              , control = control.ergm(seed=1))

summary(mod172)


AIC( mod15, mod17, mod19, mod20, mod21, mod22, mod172 )
BIC( mod15, mod17, mod19, mod20, mod21, mod22, mod172 )

# simulate from the model
#sim = simulate(mark7, burnin = 1000, nsim = 100, verbose = TRUE, seed = 1)
#sim = simulate(mod13, burnin = 1000, nsim = 100, verbose = TRUE, seed = 1)
sim = simulate(mod12.2, burnin = 1000, nsim = 100, verbose = TRUE, seed = 1)

# let us assume we want to verify whether the model is appropriate to represent the degree and the 
# transitivity in the network

fnc = function(xx){
        ig = asIgraph(xx)
        tr = transitivity(ig)
        rep = reciprocity(ig)
        ideg = sd(degree(ig, mode = "in"))
        odeg = sd(degree(ig, mode = "out"))
        return(list(tr, ideg, odeg, rep))
}
prova = as.matrix(t(sapply(sim, function(xx){fnc(xx)})))
dev.new()
par(mfrow = c(2,2))
hist(unlist(prova[,1]), xlab = "transitivity"); abline(v = transitivity(subg_animals), col = "red")
hist(unlist(prova[,2]), xlab = "in-degree"); abline(v = sd(degree(subg_animals, mode = "in")), col = "red")
hist(unlist(prova[,3]), xlab = "out-degree"); abline(v = sd(degree(subg_animals, mode = "out")), col = "red")
hist(unlist(prova[,4]), xlab = "reprocity"); abline(v = reciprocity(subg_animals), col = "red")

#pvalues
mean(unlist(prova[,1])> transitivity(subg_animals))
mean(unlist(prova[,4])< reciprocity(subg_animals))
 sd ( degree(subg_animals, mode = "in") )
sd(degree(subg_animals, mode = "out") )