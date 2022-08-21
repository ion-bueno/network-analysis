#########################################
# This work has been done by:

# Ion Bueno Ulacia    NIA: 100364530
# Daniel Martín Cruz  NIA: 100384121

#########################################


## --------- Used libraries ---------

library("igraph")
library(tidyverse)


## --------- Load data ---------

links = read.csv('dataset/links.csv')
names = read.csv('dataset/nodes.csv', sep = '\t')
topics = read.csv('dataset/topics.csv', sep = '\t')


## --------- Pre-processing data ---------

# Adjust indexes to R
# First node has 0 as id, so we must add 1
links = links + 1
names$Page.ID = names$Page.ID + 1
topics$Page.ID = topics$Page.ID + 1


## --------- Graph ---------

# Edge list
el = data.matrix(links) 
el_names = apply(el, 2, function(x)names$Name[x])

# Graph declaration
g = graph.edgelist(el_names, directed=T) # directed graph



## --------- Preliminary cleaning ---------

any_multiple(g)

is_simple(g)

is_connected(g)

cc <- clusters(g)

cc$csize
cc$no
cc$membership

# remove non-connected component and use the giant component
gGC <- induced.subgraph(g,vids = which(cc$membership==which.max(cc$csize)))

radius(g)
radius(gGC)

diameter(g)
diameter(gGC)


## --------- Link prediction algortihms ---------


# DEGREE HOMOPHILY

source = names[names$Page.ID==1,]

neighbors = neighbors(gGC, source$Page.ID)

potential_links_hom <- data.frame("id"=0,"name"="","proximity"=0,stringsAsFactors = FALSE)

for (i in 1:vcount(gGC)) { # for all possible links from the source we measure proximity
  
  if (are_adjacent(gGC,source$Page.ID,names$Page.ID[i]) == FALSE & source$Page.ID != names$Page.ID[i]) {
    
    proximity = -abs(degree(gGC,source$Page.ID)-degree(gGC,names$Page.ID[i])) # evaluate proximity for each potential link
    
    potential_links_hom = rbind(potential_links_hom,list(names$Page.ID[i],names$Name[i],proximity))
    
  }
  
}

potential_links_hom <- potential_links_hom[-1,]

potential_links_hom <- potential_links_hom[order(potential_links_hom$proximity,decreasing = TRUE),]

potential_links_hom[1:10,]


# GRAPH DISTANCE

source = names[names$Page.ID==1,]

potential_links_dist <- data.frame("id"=0,"name"="","proximity"=0,stringsAsFactors = FALSE)

for (i in 1:vcount(gGC)) { # for all possible links from the source we measure proximity
  
  if (are_adjacent(gGC,source$Page.ID,names$Page.ID[i]) == FALSE & source$Page.ID != names$Page.ID[i]) {
    
    proximity = -abs(distances(gGC, source$Page.ID, to = names$Page.ID[i], mode = "all")[1,1]) # evaluate proximity for each potential link
    
    potential_links_dist = rbind(potential_links_dist,list(names$Page.ID[i],names$Name[i],proximity))
    
  }
  
}

potential_links_dist <- potential_links_dist[-1,]

potential_links_dist <- potential_links_dist[order(potential_links_dist$proximity,decreasing = TRUE),]

potential_links_dist[1:15,]



# COMMON NEIGHBORS

source = names[names$Page.ID==1,]

neighbors = neighbors(gGC, source$Page.ID)

potential_links_nei <- data.frame("id"=0,"name"="","proximity"=0,stringsAsFactors = FALSE)

for (i in 1:vcount(gGC)) { # for all possible links from the source we measure proximity as a function of common neighbors
  
  if (are_adjacent(gGC,source$Page.ID,names$Page.ID[i]) == FALSE & source$Page.ID != names$Page.ID[i]) {
    
    proximity = length(intersect(neighbors(gGC,source$Page.ID),neighbors(gGC,names$Page.ID[i])))
    
    potential_links_nei = rbind(potential_links_nei,list(names$Page.ID[i],names$Name[i],proximity))
    
  }
  
}

potential_links_nei <- potential_links_nei[-1,]

potential_links_nei <- potential_links_nei[order(potential_links_nei$proximity,decreasing = TRUE),]

potential_links_nei[1:15,]



# COMMON NEIGHBORS 2 (looking at neighbors at distance 2)


source = names[names$Page.ID==1,]

neighbors = neighbors(gGC, source$Page.ID)

potential_links_nei2 <- data.frame("id"=0,"name"="","proximity"=0,stringsAsFactors = FALSE)

g1 <- make_ego_graph(gGC,order=2,source$Page.ID) # second order neighbors

for (i in 1:vcount(gGC)) {
  
  if (are_adjacent(gGC,source$Page.ID,names$Page.ID[i]) == FALSE & source$Page.ID != names$Page.ID[i]) {
    
    g2 <- make_ego_graph(gGC,order=2,names$Page.ID[i])
    
    proximity = length(intersect(vertex.attributes(g1[[1]])$Page.ID,vertex.attributes(g2[[1]])$Page.ID))
    
    potential_links_nei2 = rbind(potential_links_nei2,list(names$Page.ID[i],names$Name[i],proximity))
    
  }
  
}

potential_links_nei2 <- potential_links_nei2[-1,]

potential_links_nei2 <- potential_links_nei2[order(potential_links_nei2$proximity,decreasing = TRUE),]

potential_links_nei2[1:15,]




# JACCARD'S ONE NODE

source = names[names$Page.ID==1,]

neighbors = neighbors(gGC, source$Page.ID)

potential_links_jac <- data.frame("id"=0,"name"="","proximity"=0,stringsAsFactors = FALSE)


if(length(neighbors) != 0){
  
  for (i in 1:length(neighbors)) {
    
    v2 = neighbors[i]
    
    neighbors2 = neighbors(gGC, v2)
    
    if(length(neighbors2) != 0){
      
      for (j in 1:length(neighbors2)) {
        
        v3 = neighbors2[j]
        id3= names[names$Page.ID==v3,]$Page.ID
        
        if (are_adjacent(gGC,source$Page.ID,v3) == FALSE & source$Page.ID != v3) {
          
          common_neighbors = length(intersect(neighbors(gGC,source$Page.ID),neighbors(gGC,v3)))
          
          union = length(neighbors(gGC,source$Page.ID))+length(neighbors(gGC,v3))-length(intersect(neighbors(gGC,source$Page.ID),neighbors(gGC,v3)))
          
          potential_links_jac = rbind(potential_links_jac,list(id3,names[names$Page.ID==v3,]$Name,union))
          
        }
        
      }
    }
  }
}

potential_links_jac <- potential_links_jac[-1,]

potential_links_jac <- potential_links_jac[order(potential_links_jac$proximity,decreasing = TRUE),]

potential_links_jac <- potential_links_jac[!duplicated(potential_links_jac$id),]

potential_links_jac[1:15,]





# JACCARD'S ADJUSTED ONE NODE

source = names[names$Page.ID==1,]

neighbors = neighbors(gGC, source$Page.ID)
size = length(neighbors)

potential_links_jac_adj <- data.frame("id"=0,"name"="","proximity"=0,stringsAsFactors = FALSE)


if(size != 0){
  
  for (j in 1:size) {
    
    v2 = neighbors[j]
    
    neighbors2 = neighbors(gGC, v2)
    
    if(length(neighbors2) != 0){
      
      for (k in 1:length(neighbors2)) {
        
        v3 = neighbors2[k]
        id3= names[names$Page.ID==v3,]$Page.ID
        size3 = length(neighbors(gGC, v3))
        
        if (are_adjacent(gGC,source$Page.ID,v3) == FALSE & source$Page.ID != v3 & size > 2 & size3 > 2) {
          
          common_neighbors = length(intersect(neighbors(gGC,source$Page.ID),neighbors(gGC,v3)))
          
          union = length(neighbors(gGC,source$Page.ID))+length(neighbors(gGC,v3))-length(intersect(neighbors(gGC,source$Page.ID),neighbors(gGC,v3)))
          
          potential_links_jac_adj = rbind(potential_links_jac,list(id3,names[names$Page.ID==v3,]$Name,union))
          
        }
        
      }
    }
  }
}

potential_links_jac_adj <- potential_links_jac_adj[-1,]

potential_links_jac_adj <- potential_links_jac_adj[order(potential_links_jac_adj$proximity,decreasing = TRUE),]

potential_links_jac_adj <- potential_links_jac_adj[!duplicated(potential_links_jac_adj$id),]

potential_links_jac_adj[1:15,]


# ADAMIC-ADAR ONE NODE

source = names[names$Page.ID==1,]

neighbors = neighbors(gGC, source$Page.ID)

potential_links_ada <- data.frame("id"=0,"name"="","proximity"=0,stringsAsFactors = FALSE)


if(length(neighbors) != 0){
  
  for (j in 1:length(neighbors)) {
    
    v2 = neighbors[j]
    
    neighbors2 = neighbors(gGC, v2)
    
    if(length(neighbors2) != 0){
      
      for (k in 1:length(neighbors2)) {
        
        v3 = neighbors2[k]
        id3= names[names$Page.ID==v3,]$Page.ID
        #size3 = length(neighbors(gGC, v3))
        
        if(v3!=source$Page.ID & are_adjacent(gGC,source$Page.ID,v3) == FALSE) { # if (v3!=source$Page.ID) {
          
          sum = 0
          neighbors3 = neighbors(gGC, v3)
          
          if(length(neighbors3) != 0){
            
            for (l in 1:length(neighbors3)) {
              
              v4 = neighbors3[l]
              
              if (are_adjacent(gGC,source$Page.ID,v4) == TRUE & source$Page.ID != v4) {
                
                sum = sum + 1/log(degree(gGC,v4))
                
              }
              
              potential_links_ada = rbind(potential_links_ada,list(id3,names[names$Page.ID==v3,]$Name,sum))
              
            }
          }
        }
        
      }
    }
  }
}


potential_links_ada <- potential_links_ada[-1,]

potential_links_ada <- potential_links_ada[order(potential_links_ada$proximity,decreasing = TRUE),]

potential_links_ada <- potential_links_ada[!duplicated(potential_links_ada$id),]

potential_links_ada[1:15,]



# HITTING TIME

randomWalk<-function(graph, start, end, count=0, max_steps=1000){
  
  if(i%%100==0) {print(i)}
  
  if(start==end || count==max_steps){
    return(count)
  }
  neis = neighbors(gGC, v2, mode='out')
  
  next_step = neis[sample(1:length(neis), 1)]
  
  return(randomWalk(graph, next_step, end, count+1, max_steps))
  
}




source = names[names$Page.ID==1,]
potential_links_hit <- data.frame("id"=0,"name"="","proximity"=0,stringsAsFactors = FALSE)
n=5

for (i in 1:vcount(gGC)) { # for all possible links from the source we measure proximity
  
  if(i%%100==0) {print(i)}
  
  if (are_adjacent(gGC,source$Page.ID,names$Page.ID[i]) == FALSE & source$Page.ID != names$Page.ID[i]) {
    
    proximity = 0
    
    for(j in 1:n){
      proximity = proximity-abs(randomWalk(gGC, source, names$Page.ID[i], max_steps=200)) # evaluate proximity for each potential link
    }  
    proximity = proximity/n
    potential_links_hit = rbind(potential_links_hit,list(names$Page.ID[i],names$Name[i], proximity))
  }
  
}

potential_links_hit <- potential_links_hit[-1,]

potential_links_hit <- potential_links_hit[order(potential_links_hit$proximity,decreasing = TRUE),]

potential_links_hit[1:15,]



## WHOLE NETWORK


# ALL COMMON NEIGHBORS

common <- matrix(0,vcount(gGC),vcount(gGC))

for (i in 1:vcount(gGC)) {
  
  if(i%%100==0) {print(i)}
  
  v1 = names$Page.ID[i]
  id1= names[names$Page.ID==v1,]$Page.ID
  
  neighbors = neighbors(gGC, v1)
  
  if(length(neighbors) != 0){
    
    for (j in 1:length(neighbors)) {
      
      v2 = neighbors[j]
      
      neighbors2 = neighbors(gGC, v2)
      
      if(length(neighbors2) != 0){
        
        for (k in 1:length(neighbors2)) {
          
          v3 = neighbors2[k]
          id3= names[names$Page.ID==v3,]$Page.ID
          
          if (are_adjacent(gGC,v1,v3) == FALSE & v1 != v3) {
            
            common[id1,id3] = length(intersect(neighbors(gGC,v1),neighbors(gGC,v3)))
            
          }
        }
      }
    }
  }
}

max(common)

which(common == max(common), arr.ind = TRUE)

names[names$Page.ID==13,]$Name


which(common == 3, arr.ind = TRUE)



potential_links_1 <- order(common[1,],decreasing = TRUE)

potential_links_1[1:15]
common[1,98]
common[1,542]



# ALL COMMON NEIGHBORS 2

common2 <- matrix(0,vcount(gGC),vcount(gGC))

for (i in 1:vcount(gGC)) {
  
  if(i%%100==0) {print(i)}
  
  v1 = names$Page.ID[i]
  id1= names[names$Page.ID==v1,]$Page.ID
  
  neighbors = neighbors(gGC, v1)
  
  g1 <- make_ego_graph(gGC,order=2,v1) # second order neighbors
  
  if(length(neighbors) != 0){
    
    for (j in 1:length(neighbors)) {
      
      v2 = neighbors[j]
      
      neighbors2 = neighbors(gGC, v2)
      
      if(length(neighbors2) != 0){
        
        for (k in 1:length(neighbors2)) {
          
          v3 = neighbors2[k]
          id3= names[names$Page.ID==v3,]$Page.ID
          
          if (are_adjacent(gGC,v1,v3) == FALSE & v1 != v3) {
            
            g3 <- make_ego_graph(gGC,order=2,v3)
            
            common2[id1,id3] = length(intersect(vertex.attributes(g1[[1]])$Page.ID,vertex.attributes(g3[[1]])$Page.ID))
            
          }
        }
      }
    }
  }
  
}

max(common2)

which(common2 == max(common2), arr.ind = TRUE)


list_common2=as.array(common2)

sort(list_common2,decreasing=TRUE)[1:15]

which(common2 == 52, arr.ind = TRUE)





# JACCARD'S COEFFICIENT

jaccard <- matrix(0,vcount(gGC),vcount(gGC))

for (i in 1:vcount(gGC)) {
  
  if(i%%100==0) {print(i)}
  
  v1 = names$Page.ID[i]
  id1= names[names$Page.ID==v1,]$Page.ID
  
  neighbors = neighbors(gGC, v1)
  
  if(length(neighbors) != 0){
    
    for (j in 1:length(neighbors)) {
      
      v2 = neighbors[j]
      
      neighbors2 = neighbors(gGC, v2)
      
      if(length(neighbors2) != 0){
        
        for (k in 1:length(neighbors2)) {
          
          v3 = neighbors2[k]
          id3= names[names$Page.ID==v3,]$Page.ID
          
          if (are_adjacent(gGC,v1,v3) == FALSE & v1 != v3) {
            
            common_neighbors = length(intersect(neighbors(gGC,v1),neighbors(gGC,v3)))
            
            union = length(neighbors(gGC,v1))+length(neighbors(gGC,v3))-length(intersect(neighbors(gGC,v1),neighbors(gGC,v3)))
            
            jaccard[id1,id3] = common_neighbors/union
            
          }
          
        }
      }
    }
  }
}


max(jaccard)

head(which(jaccard == max(jaccard), arr.ind = TRUE))




# JACCARD'S COEFFICIENT (ADJUSTED)

jaccard2 <- matrix(0,vcount(gGC),vcount(gGC))

for (i in 1:vcount(gGC)) {
  
  if(i%%100==0) {print(i)}
  
  v1 = names$Page.ID[i]
  id1= names[names$Page.ID==v1,]$Page.ID
  
  neighbors = neighbors(gGC, v1)
  size = length(neighbors)
  
  if(size != 0){
    
    for (j in 1:size) {
      
      v2 = neighbors[j]
      
      neighbors2 = neighbors(gGC, v2)
      
      if(length(neighbors2) != 0){
        
        for (k in 1:length(neighbors2)) {
          
          v3 = neighbors2[k]
          id3= names[names$Page.ID==v3,]$Page.ID
          size3 = length(neighbors(gGC, v3))
          
          if (are_adjacent(gGC,v1,v3) == FALSE & v1 != v3 & size > 2 & size3 > 2) {
            
            common_neighbors = length(intersect(neighbors(gGC,v1),neighbors(gGC,v3)))
            
            union = length(neighbors(gGC,v1))+length(neighbors(gGC,v3))-length(intersect(neighbors(gGC,v1),neighbors(gGC,v3)))
            
            jaccard2[id1,id3] = common_neighbors/union
            
          }
          
        }
      }
    }
  }
}

max(jaccard2)

head(which(jaccard2 == max(jaccard2), arr.ind = TRUE))


list_jaccard2=as.array(jaccard2)

sort(list_jaccard2,decreasing=TRUE)[1:15]

head(which(jaccard2 == 0.6, arr.ind = TRUE))


# ADAMIC-ADAR

adamic <- matrix(0,vcount(gGC),vcount(gGC))

for (i in 1:vcount(gGC)) {
  
  if(i%%100==0) {print(i)}
  
  v1 = names$Page.ID[i]
  id1= names[names$Page.ID==v1,]$Page.ID
  
  neighbors = neighbors(gGC, v1)
  #size = length(neighbors)
  
  if(length(neighbors) != 0){
    
    for (j in 1:length(neighbors)) {
      
      v2 = neighbors[j]
      
      neighbors2 = neighbors(gGC, v2)
      
      if(length(neighbors2) != 0){
        
        for (k in 1:length(neighbors2)) {
          
          v3 = neighbors2[k]
          id3= names[names$Page.ID==v3,]$Page.ID
          #size3 = length(neighbors(gGC, v3))
          
          if(v3!=v1 & are_adjacent(gGC,v1,v3) == FALSE) { # if (v3!=v1) {
            
            sum = 0
            neighbors3 = neighbors(gGC, v3)
            
            if(length(neighbors3) != 0){
              
              for (l in 1:length(neighbors3)) {
                
                v4 = neighbors3[l]
                
                if (are_adjacent(gGC,v1,v4) == TRUE & v1 != v4) {
                  
                  sum = sum + 1/log(degree(gGC,v4))
                  
                }
                
                adamic[id1,id3] = sum
                
              }
            }
          }
          
        }
      }
    }
  }
}

max(adamic)

head(which(adamic == max(adamic), arr.ind = TRUE))





## --------- Communities detection ---------

# A lot of algorithms cannot be used because our network is directed!
# For example the cluster_louvain or the multilevel.community


# remove edges of high betweenness since they seem to be between communities
bet_com = edge.betweenness.community(gGC)
bet_com_len = sort(c(sizes(bet_com)), decreasing=TRUE, index.return=TRUE)
length(bet_com_len$x)
bet_com_len$x[1:10]

# find consensus in majority voting of labels in the neighborhood of a vertex
label_com = label.propagation.community(gGC)
label_com_len = sort(c(sizes(label_com)), decreasing=TRUE, index.return=TRUE)
length(label_com_len$x)
label_com_len$x[1:10]

# short random walks tend to stay in the same community
rw_com = walktrap.community(gGC)
rw_com_len = sort(c(sizes(rw_com)), decreasing=TRUE, index.return=TRUE)
length(rw_com_len$x)
rw_com_len$x[1:10]

# find community structure that minimizes the expected description length of random walker trajectories
minrw_com = infomap.community(gGC) 
minrw_com_len = sort(c(sizes(minrw_com)), decreasing=TRUE, index.return=TRUE)
length(minrw_com_len$x)
minrw_com_len$x[1:10]

# The most suitable according to the previous analysis in the first assignment could be the rw_com or minrw_com
com = rw_com
com_len = rw_com_len

com            # the communities
com_len$x      # size communities sorted
com_len$ix     # indexes size communities sorted
com$membership # corresponding community per node

# Communities to analyze
n_com = 7
com_len$x[1:n_com]
com_ids_len = com_len$x[1:n_com]
com_ids = com_len$ix[1:n_com]



## --------- Plots network statistics ---------

#  Barplot
df = data.frame(out_degree=degree(gGC, mode = c("out")))
df['community'] = com$membership
df_grouped = df %>% 
  filter(community %in% com_ids) %>% 
  group_by(community) %>% 
  summarise(avg_out_degree = mean(out_degree)) %>%
  slice(match(com_ids, community))

df_grouped['size'] = com_ids_len
df_grouped['number'] = 1:length(com_ids)

ggplot(data=df_grouped, 
       aes(x=factor(number), 
           y=avg_out_degree, 
           fill=as.factor(community))) +
  geom_bar(stat="identity") +
  scale_fill_hue(c = 40) +
  theme(legend.position="none") +
  geom_text(aes(label=round(avg_out_degree, 2)), 
            vjust=1.6, color="white",
            position = position_dodge(0.9), 
            size=3.5) +
  labs(x='Communities', 
       y='Average out degree',
       title='Barplot average out degree',
       subtitle = 'Communitites sorted by size, from larger to smaller')



# Bubble plot
df = data.frame(degree=degree(gGC, mode = c("all")))

df['community'] = com$membership
df['betweenness'] = betweenness(gGC)
df['closeness'] = closeness(gGC)

df_grouped = df %>% 
  group_by(community) %>% summarise(avg_degree = mean(degree))

df_grouped['size'] = com_len$x

df_bet = df %>% 
  group_by(community) %>% summarise(avg_betweenness = mean(betweenness))

df_clo = df %>% 
  group_by(community) %>% summarise(avg_closeness= mean(closeness))

df_grouped['avg_betweenness'] = df_bet$avg_betweenness
df_grouped['Average_closeness'] = df_clo$avg_closeness

df_grouped %>% ggplot(aes(x=avg_betweenness, y=avg_degree, size = log(size), color=Average_closeness)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 6), name="Size in logarithmic scale") +
  xlab("Average betweenness") + ylab("Average degree") +
  labs(color="Average closeness", title="Bubble plot of network statistics per community")



## --------- Spreading process - SIRS model ---------

# Init values
init_nodes = function(gGC, communities){
  nodes = data.frame("id"=0,"name"="","degree"=0,"state"=FALSE,"next_state"=FALSE,
                     stringsAsFactors = FALSE)
  names = vertex_attr(gGC)$name
  # Loop to fill nodes
  for(i in 1:vcount(gGC)) {
    nodes <- rbind(nodes, list(i,names[i],as.numeric(degree(gGC,i)), 'Susceptible', 'Susceptible'))
  }
  nodes = nodes[-1,]
  nodes$community = communities$membership
  return(nodes)
}

# Calculate statistics by comunity and in total
get_stats = function(com_ids, nodes){
  
  results = array(dim = c(1,3,length(com_ids)+1))
  
  inf_nodes = nodes[nodes$state=='Infectious',]
  rec_nodes = nodes[nodes$state=='Recovered',]
  sus_nodes = nodes[nodes$state=='Susceptible',]
  
  # By selected communities
  i = 1
  for(id in com_ids){
    results[1,1,i] = nrow(inf_nodes[inf_nodes$community==id,]) # infected
    results[1,2,i] = nrow(rec_nodes[rec_nodes$community==id,]) # recovered
    results[1,3,i] = nrow(sus_nodes[sus_nodes$community==id,]) # susceptible
    i = i + 1
  }
  
  # Total
  results[1,1,i] = nrow(inf_nodes) # infected
  results[1,2,i] = nrow(rec_nodes) # recovered
  results[1,3,i] = nrow(sus_nodes) # susceptible
  
  return(results)
}

# Spreading SIRS model
spreading_sirs_model = function(gGC, nodes, com_ids, 
                                beta, gamma, xi, steps=30){
  
  # To store results (steps+1 x states x communities+1)
  results = array(dim = c(steps+1, 3, length(com_ids)+1))
  
  # Get initial statistics
  results[1, ,] = get_stats(com_ids, nodes)
  
  # Number of steps
  for(step in 1:steps) {
    
    print(step)
    
    # Iterate through nodes
    for(i in 1:vcount(gGC)){
      
      node  = nodes$name[i]
      state = nodes$state[i]
      
      
      # INFECTED
      if(state == 'Infectious'){
        
        # Infect other nodes
        neighbors = neighbors(gGC, node)
        if(length(neighbors)){
          for(j in 1:length(neighbors)){
            
            neighbor = neighbors[j]$name
            state_neigh = nodes[nodes$name==neighbor,]$state
            
            # Infect by beta
            if(beta > runif(1) & (state_neigh == 'Susceptible')){
              nodes[nodes$name == neighbor,]$next_state = 'Infectious'
            }
          }
        }
        
        # SUSCEPTIBLE
        if(gamma > runif(1)){
          nodes[nodes$name == node,]$next_state = 'Recovered'
        }
      } # end infected
      
      
      # RECOVERED
      else if(state == 'Recovered'){
        if(xi > runif(1)){
          nodes[nodes$name == node,]$next_state = 'Susceptible'
        }
      }
      
    } # end nodes
    
    # Update state
    nodes$state = nodes$next_state
    
    # Update statistics
    results[step+1, ,] = get_stats(com_ids, nodes)
    
    # Show evolution of total network
    print(table(nodes$state)/vcount(gGC))
    
  }
  return(results)
}

# Parameters
beta = 0.9
gamma = 0.6
xi = 0.3
steps = 5


# Init nodes
nodes = init_nodes(gGC, com)

# Select random initial infected nodes
n = 3
ids = sample(1:vcount(gGC), n)
for(i in ids){
  nodes[nodes$id==i,]$state = 'Infectious'
  nodes[nodes$id==i,]$next_state = 'Infectious'
}

# Spreading process
results = spreading_sirs_model(gGC, nodes, com_ids,
                               beta, gamma, xi, steps=steps)


# Spreading results

# Results in dataframe per community
get_df = function(results, idx){
  df = data.frame(infectious = results[,1,idx], 
                  recovered = results[,2,idx], 
                  susceptible = results[,3,idx])
  return(df)
}

df_total = get_df(results, dim(results)[3]) # results total network
df_total



# Obtain the fraction of infected nodes per community in one dataframe
get_df_frac = function(results){
  n_com = dim(results)[3]-1
  df = data.frame(matrix(nrow=dim(results)[1], ncol=n_com))
  for(com in 1:n_com){
    inf_nodes = results[,1,com]
    n_nodes = rowSums(results[,,com])[1]
    df[com] = inf_nodes/n_nodes
    colnames(df)[com] =  com
  }
  return(df)
}

df_com_infect = get_df_frac(results)
df_com_infect


## --------- Spreading plots --------- 

# Evolution spreading
show_sirs_spreading = function(df, normalize=TRUE){
  steps = nrow(df)
  if(normalize){
    df = df/rowSums(df)[1]
  }
  df$steps = 0:(steps-1)
  df %>%
    pivot_longer(c(infectious, recovered, susceptible),
                 names_to='nodes') %>%
    ggplot(aes(x=steps, y=value, colour=nodes)) +
    geom_line() +
    scale_color_manual(labels = c('infectious', 'recovered', 'susceptible'), 
                       values = c('red2', 'deepskyblue3', 'seagreen3')) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
    labs(x = "Time steps", 
         y='Population ratio',
         title='Evolution spreading process',
         subtitle='SIRS model')
}

show_sirs_spreading(df_total)


# Evolution infected fraction per community
show_com_infect = function(df, com_ids){
  steps = nrow(df)
  df$steps = 0:(steps-1)
  df %>%
    pivot_longer(colnames(df)[colnames(df) != "steps"], 
                 names_to='communities') %>%
    ggplot(aes(x=steps, y=value, colour=communities)) +
    geom_line() +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
    ylim(0, 1) +
    labs(x = "Time steps", 
         y='Infected ratio',
         title='Evolution infected ratio by community',
         subtitle='SIRS model')
}

show_com_infect(df_com_infect)


# Smoothing average
show_smooth_avg = function(df){
  steps = nrow(df)
  df$steps = 0:(steps-1)
  df %>%
    pivot_longer(colnames(df)[colnames(df) != "steps"], 
                 names_to='communities') %>%
    ggplot(aes(x=steps, y=value)) +
    #geom_point(color='darkblue') + 
    geom_line(aes(col=communities)) +
    geom_smooth(color='black', lty='dotdash') +
    ylim(0,1) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
    labs(x = "Time steps", 
         y='Infected ratio',
         title='Evolution infected ratio by community',
         subtitle='SIRS model')
}

show_smooth_avg(df_com_infect)



## --------- Spreading experiments ---------


pipeline = function(gGC, nodes, params,
                    initial_nodes,
                    communities, com_ids){
  
  # Parameters
  beta = params$beta
  gamma = params$gamma
  xi = params$xi
  steps = params$steps
  
  # Init nodes
  nodes = init_nodes(gGC, communities)
  
  # Initial infected nodes
  for(i in initial_nodes){
    nodes[nodes$id==i,]$state = 'Infectious'
    nodes[nodes$id==i,]$next_state = 'Infectious'
  }
  
  # Spreading process
  results = spreading_sirs_model(gGC, nodes, com_ids, beta, gamma, xi, steps=steps)
  
  # Spreading results
  df_total = get_df(results, dim(results)[3])
  df_com_infect = get_df_frac(results)
  
  out = list('df_total'=df_total, 'df_com_infect'=df_com_infect)
  return(out)
}


# Parameters
params = list('beta' = 0.9, 
              'gamma'= 0.6, 
              'xi'   = 0.3, 
              'steps'= 30
)


# Experiments respect initial nodes


# 1) Minimum eccentricity
min_ecc = sort(eccentricity(gGC), index.return=TRUE)
ids_min_ecc = min_ecc$ix[1:100]
out1 = pipeline(gGC, nodes, params, ids_min_ecc, com, com_ids)

show_sirs_spreading(out1$df_total)
show_com_infect(out1$df_com_infect)
show_smooth_avg(out1$df_com_infect)


# 2) Maximum eccentricity
max_ecc = sort(eccentricity(gGC), decreasing=TRUE, index.return=TRUE)
ids_max_ecc = max_ecc$ix[1:100]
out2 = pipeline(gGC, nodes, params, ids_max_ecc, com, com_ids)

show_sirs_spreading(out2$df_total)
show_com_infect(out2$df_com_infect)
show_smooth_avg(out2$df_com_infect)


# 3) Minimum eccentricity per community
df = data.frame(ecc=eccentricity(gGC))
df['id'] = 1:nrow(df)
df['community'] = com$membership
df_grouped = df %>% 
  filter(community %in% com_ids) %>%
  group_by(community) %>%
  slice(which.min(ecc))

ids_min_ecc_com = df_grouped$id
out3 = pipeline(gGC, nodes, params, ids_min_ecc_com, com, com_ids)

show_sirs_spreading(out3$df_total)
show_com_infect(out3$df_com_infect)
show_smooth_avg(out3$df_com_infect)



# 4) Minimum outdegree
min_degree = sort(degree(gGC, mode = c("out")), index.return=TRUE)
ids_min_degree = min_degree$ix[1:100]
out4 = pipeline(gGC, nodes, params, ids_min_degree, com, com_ids)

show_sirs_spreading(out4$df_total)
show_com_infect(out4$df_com_infect)
show_smooth_avg(out4$df_com_infect)


# 5) Maximum outdegree
max_degree = sort(degree(gGC, mode = c("out")), decreasing=TRUE, index.return=TRUE)
ids_max_degree = max_degree$ix[1:100]
out5 = pipeline(gGC, nodes, params, ids_max_degree, com, com_ids)

show_sirs_spreading(out5$df_total)
show_com_infect(out5$df_com_infect)
show_smooth_avg(out5$df_com_infect)


# 6) Maximum outdegree per community
df = data.frame(out_degree=degree(gGC, mode = c("out")))
df[
  'id'] = 1:nrow(df)
df['community'] = com$membership
df_grouped = df %>% 
  filter(community %in% com_ids) %>%
  group_by(community) %>%
  slice(which.max(out_degree))

ids_max_degree_com = df_grouped$id
out6 = pipeline(gGC, nodes, params, ids_max_degree_com, com, com_ids)

show_sirs_spreading(out6$df_total)
show_com_infect(out6$df_com_infect)
show_smooth_avg(out6$df_com_infect)


# 7) Minimum betweenness
min_bet = sort(betweenness(gGC), index.return=TRUE)
ids_min_bet = min_bet$ix[1:100]
out7 = pipeline(gGC, nodes, params, ids_min_bet, com, com_ids)

show_sirs_spreading(out7$df_total)
show_com_infect(out7$df_com_infect)
show_smooth_avg(out7$df_com_infect)


# 8) Maximum betweenness
max_bet = sort(betweenness(gGC), decreasing=TRUE, index.return=TRUE)
ids_max_bet = max_bet$ix[1:100]
out8 = pipeline(gGC, nodes, params, ids_max_bet, com, com_ids)

show_sirs_spreading(out8$df_total)
show_com_infect(out8$df_com_infect)
show_smooth_avg(out8$df_com_infect)


# 9) Maximum betweenness per community
df = data.frame(bet=betweenness(gGC))
df['id'] = 1:nrow(df)
df['community'] = com$membership
df_grouped = df %>% 
  filter(community %in% com_ids) %>%
  group_by(community) %>%
  slice(which.max(bet))

ids_max_bet_com = df_grouped$id
out9 = pipeline(gGC, nodes, params, ids_max_bet_com, com, com_ids)

show_sirs_spreading(out9$df_total)
show_com_infect(out9$df_com_infect)
show_smooth_avg(out9$df_com_infect)


# 10) Minimum closeness
min_clos = sort(closeness(gGC), index.return=TRUE)
ids_min_clos = min_clos$ix[1:100]
out10 = pipeline(gGC, nodes, params, ids_min_clos, com, com_ids)

show_sirs_spreading(out10$df_total)
show_com_infect(out10$df_com_infect)
show_smooth_avg(out10$df_com_infect)


# 11) Maximum closeness
max_clos = sort(closeness(gGC), decreasing=FALSE, index.return=TRUE)
ids_max_clos = max_clos$ix[1:100]
out11 = pipeline(gGC, nodes, params, ids_max_clos, com, com_ids)

show_sirs_spreading(out11$df_total)
show_com_infect(out11$df_com_infect)
show_smooth_avg(out11$df_com_infect)


# 12) Maximum closeness per community
df = data.frame(clos=closeness(gGC))
df['id'] = 1:nrow(df)
df['community'] = com$membership
df_grouped = df %>% 
  filter(community %in% com_ids) %>%
  group_by(community) %>%
  slice(which.max(clos))

ids_max_clos_com = df_grouped$id
out12 = pipeline(gGC, nodes, params, ids_max_clos_com, com, com_ids)

show_sirs_spreading(out12$df_total)
show_com_infect(out12$df_com_infect)
show_smooth_avg(out12$df_com_infect)











