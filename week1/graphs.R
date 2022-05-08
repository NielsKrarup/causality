# Copyright (c) 2017--2021  Jonas Peters [jonas.peters@math.ku.dk] and Jan Ernest
# All rights reserved.  See the file COPYING for license terms.
#
library(Matrix)

plotting <- TRUE # put to FALSE the following line if you have problems with the package.

if(plotting){ 
  # install.packages("BiocManager")
  # BiocManager::install("Rgraphviz")
  library(Rgraphviz)
  plotCausalOrderedDAGfromAdj <- function(Adj, labels = 1:dim(Adj)[1], main=NULL){
    G <- as(Adj, "graphNEL")
    z <- labels
    names(z) = nodes(G)
    nAttrs <- list()
    nAttrs$label <- z
    attrs <- list(node = list(shape = "ellipse", fixedsize = FALSE))
    plot(G, nodeAttrs = nAttrs, attrs = attrs, main=main)    
  }
}

randomDAG <- function(p,probConnect,causalOrderI = sample(p,p,replace=FALSE), sparse = TRUE, silent = TRUE){
  # simulates a directed acyclic graph (DAG) and returns its adjacency matrix
  # The function is programmed to be easily understandable. It is 
  # not implemented in the most efficient way.
  # INPUT:
  #   p           number of nodes 
  #   probConnect the probability that an edge i -> j is added to the DAG
  #   causalOrder starting with sink node (also called topological order)
  #   
  # OUTPUT:
  #   DAG         Adjacency matrix of a directed acyclic graph (DAG)    
  if(sparse){
    DAG <- Matrix(0, p, p, sparse=TRUE)
  } else {
    DAG <- matrix(0, p, p)
  }
  for(i in p:3){
    node <- causalOrderI[i]
    possibleParents <- causalOrderI[1:(i-1)]
    numberParents <- rbinom(n=1, size=(i-1), prob=probConnect)
    parents <- sample(x = possibleParents, size = numberParents, replace = FALSE)
    DAG[parents,node] <- rep(1,numberParents)
  }
  # sample does not work properly when choosing from sets with one element. We thus consider the last case separately.  
  node <- causalOrderI[2]
  parentYesNo <- rbinom(n=1,size=1,prob=probConnect)
  DAG[causalOrderI[1],node] <- parentYesNo
  if(!silent){
    cat("inverse of causal order:\n")
    show(causalOrderI)        
  }
  return(DAG)
}

set.seed(123)
G <- randomDAG(p = 5, probConnect = 0.6, sparse = FALSE)
show(G)
if(plotting){
  par(mfrow = c(1,2))
  plotCausalOrderedDAGfromAdj(G)
  plotCausalOrderedDAGfromAdj(G%*%G)
}


cat("-----------------\n\n\n")
time.now <- proc.time()
G <- randomDAG(p = 2000, probConnect = 0.1, sparse = FALSE)
cat("elapsed time for generating DAG:", proc.time()[3]-time.now[3], "\n")

time.now <- proc.time()
Gsparse <- Matrix(G)
cat("elapsed time for sparsification:", proc.time()[3]-time.now[3], "\n\n")

cat("number of non-zeros in G:", sum(G), "\n\n")

cat("object size of non-sparse matrix:", object.size(G), "\n")
cat("object size of sparse matrix:", object.size(Gsparse), "\n\n")

time.now <- proc.time()
G.squared <- G %*% G
cat("elapsed time for squaring with non-sparse matrix:", proc.time()[3]-time.now[3], "\n")

time.now <- proc.time()
Gsparse.squared <- Gsparse %*% Gsparse
cat("elapsed time for squaring with sparse matrix:", proc.time()[3]-time.now[3], "\n")


