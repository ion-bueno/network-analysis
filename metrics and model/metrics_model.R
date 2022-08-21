#########################################
# This work has been done by:

# Ion Bueno Ulacia    NIA: 100364530
# Daniel Martín Cruz  NIA: 100384121

#########################################


## --------- Load data ---------

links = read.csv('dataset/links.csv')
nodes = read.csv('dataset/nodes.csv', sep = '\t')
topics = read.csv('dataset/topics.csv', sep = '\t')


## --------- Pre-processing data ---------

# Adjust indexes to R
# First node has 0 as id, so we must add 1
links = links + 1
nodes$Page.ID = nodes$Page.ID + 1
topics$Page.ID = topics$Page.ID + 1

n.nodes = length(nodes$Page.ID)
n.edges = length(links[,1])


## --------- Graph ---------

library("igraph")

# Edge list
el = data.matrix(links) 

# Graph declaration
g = graph.edgelist(el, directed=T) # directed graph

# Plot graph, better with Gephi
plot(g)


## --------- Graph statistics ---------

# Number of vertices/nodes
vcount(g) 
E(g) # list of edges

# Number of edges/links
ecount(g) 
V(g) # list of vertices

# Degree 
dg = degree(g)
mean(dg)
V(g)[degree(g)>30] # get most connected nodes
dg = data.frame(name=as.integer(V(g)), degree=degree(g)) 
dg_order = dg[order(dg$degree,decreasing=T),] # order nodes wrt their degree
head(dg_order)

# Degree distribution
plot(degree_distribution(g,cumulative=F),type="s") # degree distribution
plot(degree_distribution(g,cumulative=T),type="s") # cumulative degree distribution

M=(rbind(as.matrix(el[,1:2]),as.matrix(el[,2:1])))
nodes=unique(nodes)


friends = function(x) as.character(M[which(M[,1]==x),2])
nb_friends = Vectorize(function(x) length(friends(x)))

friends_of_friends = function(y) (Vectorize(function(x) length(friends(x)))(friends(y)))
nb_friends_of_friends = Vectorize(function(x) mean(friends_of_friends(x)))

Nb  = nb_friends(nodes$Page.ID)
Nb2 = nb_friends_of_friends(nodes$Page.ID)
hist(Nb,breaks=seq(0,500,10),col=rgb(1,0,0,.2),border="white",probability = TRUE)
hist(Nb2,breaks=seq(0,500,10),col=rgb(0,0,1,.2),border="white",probability = TRUE,add=TRUE)

legend('topright', legend=c("# Friends distribution", "# Friends of friends distribution"),
       col=c("red", "blue"), lty=1, cex=0.8)

mean(Nb)
mean(Nb2[!is.na(Nb2)])

# Diameter 
# length of the longest shortest path, or the largest eccentricity among all nodes
diameter(g)

# Assortativity (Pearson) 
# Pearson correlation coefficient [-1,1] between the degrees of connected nodes
assortativity_degree(g)


transitivity(g)

# Average shortest path length
mean_distance(g)


## --------- Centralities ---------

library("CINNA")

# Betweenness
bw = betweenness(g, directed = TRUE)
# Edge Betweenness
edge.betweenness(g, directed = TRUE)

# Closeness
cl = closeness(g)

# Harmonic centrality
hc = harmonic_centrality(g)

# Page rank
pr_obj = page_rank(g, directed = TRUE)
pr = pr_obj$vector

# Summary table
library("dplyr")
tabla_cent = data.frame(id=as.integer(V(g)), dg, bw, cl, hc, pr)
sort_centrality = pr # sort rows by centrality 
head(arrange(tabla_cent, desc(sort_centrality))) 


## --------- Communities ---------



## --------- Network models ---------


### ------- Erdos Renyi

g.er <- erdos.renyi.game(n.nodes, n.edges, type = 'gnm') # ER

dg.er = degree(g.er)
mean(dg.er)

plot(degree_distribution(g.er,cumulative=F),type="s") # degree distribution
plot(degree_distribution(g.er,cumulative=T),type="s") # cumulative degree distributio


# Diameter 
# length of the longest shortest path, or the largest eccentricity among all nodes
diameter(g.er)

# Assortativity (Pearson) 
# Pearson correlation coefficient [-1,1] between the degrees of connected nodes
assortativity_degree(g.er)


transitivity(g.er)

# Average shortest path length
mean_distance(g.er)


### ------- Configuration model

g.cm <- sample_degseq(degree(g, mode='out'), degree(g, mode='in')) # ER

dg.cm = degree(g.cm)
mean(dg.cm)

plot(degree_distribution(g.cm,cumulative=F),type="s") # degree distribution
plot(degree_distribution(g.cm,cumulative=T),type="s") # cumulative degree distributio


# Diameter 
# length of the longest shortest path, or the largest eccentricity among all nodes
diameter(g.cm)

# Assortativity (Pearson) 
# Pearson correlation coefficient [-1,1] between the degrees of connected nodes
assortativity_degree(g.cm)


transitivity(g.cm)

# Average shortest path length
mean_distance(g.cm)


## ---------Graph visualization ---------

# Gephi files 
write.graph(g, format="edgelist",file="wikipedia.edges")
write.graph(g, format="graphml",file="wikipedia.graphml")
