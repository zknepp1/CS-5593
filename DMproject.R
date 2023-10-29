
distances <- function(x, distance = c("euclidean", "manhattan", "minkowski"))
{
  num <- nrow(x) # number of rows
  dis <- matrix(0, nrow = num, ncol = num) # converting to a matrix

   if (distance == "euclidean") {
    for (i in 1:num) { # finding distance 
      for (j in 1:num) {
        dis[i, j] <- sqrt(sum((x[i, ] - x[j, ])^2)) # root(sum(x - y)^2)
      }
    }
    return(as.dist(dis))
   }
  
  else if(distance == "manhattan")
  {
    for (i in 1:num) {
      for(j in 1:num) {
        dis[i, j] <- sum(abs(x[i, ] - x[j, ])) # sum(abs(x - y))
      }
    }
    return(as.dist(dis))
  }
  
  else if(distance == "minkowski")
  {
    for(i in 1:num){
      for(j in 1:num){
        dis[i, j] <- (sum(abs(x[i, ] - x[j, ])^2))^(1/2) # sum(abs(x - y)^p)^1/p
      }
    }
    return(as.dist(dis))
  }
  
  else
  {
    print("Default method is Euclidean")
    
    if (distance == "euclidean") {
      for (i in 1:num) { # finding distance 
        for (j in 1:num) {
          dis[i, j] <- sqrt(sum((x[i, ] - x[j, ])^2)) # root(sum(x - y)^2)
        }
      }
      return(as.dist(dis))
    }
  }
  
}

#-------------------------------------------------------------------------------

cluster <- function(dis, rows, method_choice)
{
  #d <- d[,unlist(lapply(d, is.numeric))] # making sure matrix is numeric from errors
  # initialization
  mem = seq(-1, -rows)  # Tracks group members by making them negative, distinct from others
  mat = matrix(0, nrow = rows-1, ncol=2) # bringing everything back together
  height = rep(0, nrow(dis) - 1) # for the height output
  order <- seq(1, rows) # initialize order
  
  # trying to follow 
  # https://www.datanovia.com/en/lessons/agglomerative-hierarchical-clustering/ 
  # https://www.simplilearn.com/tutorials/data-science-tutorial/hierarchical-clustering-in-r
  # https://www.learndatasci.com/glossary/hierarchical-clustering/ 
  # https://medium.com/analytics-vidhya/hierarchical-clustering-agglomerative-f6906d440981
  
  i <- 1
  
  #for(i in seq(1, rows - 1)) 
  while(i <= (rows - 1))
  {
    # Find smallest distance using specified method
    min_val <- min(dis)
    height[i] <- min_val
    diff <- dis - height[i]
    # Finding the index of the minimum value
    index <- which(diff == min(diff), arr.ind=TRUE)
    
    # getting the first row of smallest distances 
    index <- head(index, n = 1)
    
    # assigning the index of the smallest distance in the members and reordering it to current row
    mat[i,] <- mem[index][order(mem[index])]
    
    # combine this cluster and all previous clusters they belong to into the current i'th group
    group <- c(index)

    for (m in mem[index[1, mem[index] > 0]]) { #finding clusters that belong to current cluster
      # https://www.geeksforgeeks.org/union-of-two-objects-in-r-programming-union-function/
      group <- union(group, which(mem == m)) # merging clusters that belong together
    }
    #print(group)
    
    order[group] <- i #update order at the merged cluster with row number
    
    # assigning row to the member at that particular group 
    mem[group] <- i
    # replace distances that make the pair using `specified method`like in examples at top
    #?apply
    rep = apply(dis[index, ], 2, method_choice)
    #rep = sapply(d[index, ], method_choice)
    
    # go to next min distance and update matrix with current min distances
    # https://statsandr.com/blog/clustering-analysis-k-means-and-hierarchical-clustering-by-hand-and-in-r/#solution-by-hand-1
    # trying to set the new distance matrix by separating previous links
    dis[min(index), ] = dis[, min(index)] <- rep # replacing specific row and column with the linkage choice
    # https://elki-project.github.io/tutorial/hierarchical_clustering 
    dis[min(index), min(index)] <- Inf # setting diagonal value to infinity since looking for min distance
    dis[max(index), ] = dis[ ,max(index)] <- Inf # isolating this cluster away from other clusters with max distance
    
    i <- i + 1
  }
  
  # returning the heights, matrix, and distances
  return(list(height = height, mat = mat, dis = dis, order = order))
}

#-------------------------------------------------------------------------------

hierarchical <- function(x, distance = c("euclidean", "manhattan", "minkowski"), 
                         method = c("single","complete","average"))
{
  # a little more preprocessing
  x <- x[,unlist(lapply(x, is.numeric))] # making sure I only get numeric variables
  
  set.seed(42) # make sure results are reproducible
  
  x <- x[sample(nrow(x), 200), ] # taking random sample to make dendrogram readable
  
  # Getting the distances
  dis <- distances(x, distance) # calculate the distance between points
  
  # conversions
  # https://carpentries-incubator.github.io/lc-litsearchr/07-data-frames-matrices-and-lists/index.html#:~:text=Matrices%20behave%20as%20two%2Ddimensional,%2C%20character%2C%20etc.).
  # Matrices are faster to access
  if(!is.matrix(dis))
  {
    dis <- as.matrix(dis) # convert to a matrix if it isn't
  }
  
  # setting it up
  # some methods from hclust https://r-charts.com/part-whole/hclust/ 
  # https://www-users.cse.umn.edu/~kumar001/dmbook/ch7_clustering.pdf # pg. 556
  method_choice = switch(match.arg(method),
                     single   = min,
                     complete = max,
                     average  = mean)
  
  # common variables
  rows <- nrow(dis) # number of rows in matrix
  # giving an error if set to 0
  diag(dis) <- Inf # diagonal distance between point and itself 
  
  out <- cluster(dis, rows, method_choice)
  
  height <- out$height # height
  mat <- out$mat # matrix
  dis <- out$dis # distances
  order <- out$order # order of leaves
  
  #print(height)
  #print(mat)
  #print(d)
  # output
  # hclust output : https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/hclust
  # object of class hclust to make graph
  # https://stat.ethz.ch/R-manual/R-devel/library/stats/html/hclust.html 
  put <- structure(list(merge = mat, height = height, order = order,
                 labels = rownames(dis), method = method,
                 call = match.call(), dist.method = "euclidean"), class = "hclust")
  
  print(put)
  print(summary(put))
  # need whole output from put variable to plot
  # https://r-graphics.org/recipe-miscgraph-dendrogram 
  plot(put)
  
}

#debug(hierarchical)
#hierarchical(iris[, 1:4], distance = "euclidean", method = "single")

#undebug(hierarchical)

# test <- hierarchical(iris[, 1:4], distance = "euclidean", method = "single")
# test
# summary(test)
# 
# plot(test)

# read in data
data = read.csv("/Users/jacobflynn/Documents/Grad_School/Fall_2023/Data_Mining/Ok-County-Res-Data/Prognose_S.csv")
summary(data)

data$LATITUDE <- as.numeric(data$LATITUDE) # latitude is showing as character

num <- data[,unlist(lapply(data, is.numeric))] # taking only numeric data
summary(num)

for(i in colnames(num)){
  num[,i][is.na(num[,i])] = mean(num[,i], na.rm = T) # mean value imputation
}
summary(num) # checking

sub <- num[, c("LONGITUDE", "LATITUDE", "SoldPrice", "LandActualTotal")] # subset of dataframe

housing <- hierarchical(sub, distance = "euclidean", method = "single")
# housing
# plot(housing, edgePar = list(col = 2:3, lwd = 2:1))


