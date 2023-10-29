library(readxl)
library(tidyverse)
library(corrplot)
library(ggpubr)
library(caret)
library(ggplot2)

#df_P <- read.csv("/Users/airishimamura/RStudio/DataMining/project/Prognose_P.csv")
df_S <- read.csv("/Users/airishimamura/RStudio/DataMining/project/Prognose_S.csv")
dfS <- df_S[c("LandActualTotal","Bedrooms","rooms","YearBuilt","QUALITY","Condition","LONGITUDE","LATITUDE","SoldPrice")]

# get total missing values for each column 
colSums(is.na(dfS)) 

dfS$Condition[dfS$Condition == "Poor"] <- 1
dfS$Condition[dfS$Condition == "Fair"] <- 2
dfS$Condition[dfS$Condition == "Average"] <- 3
dfS$Condition[dfS$Condition == "Good"] <- 4
dfS$Condition[dfS$Condition == "Very Good"] <- 5
dfS$Condition[dfS$Condition == "Excellent"] <- 6

dfS$QUALITY[dfS$QUALITY == "Low"] <- 1
dfS$QUALITY[dfS$QUALITY == "Low Plus"] <- 1.5
dfS$QUALITY[dfS$QUALITY == "Fair"] <- 2
dfS$QUALITY[dfS$QUALITY == "Fair Plus"] <- 2.5
dfS$QUALITY[dfS$QUALITY == "Average"] <- 3
dfS$QUALITY[dfS$QUALITY == "Average Plus"] <- 3.5
dfS$QUALITY[dfS$QUALITY == "Good"] <- 4
dfS$QUALITY[dfS$QUALITY == "Good Plus"] <- 4.5
dfS$QUALITY[dfS$QUALITY == "Very Good"] <- 5
dfS$QUALITY[dfS$QUALITY == "Very Good Plus"] <- 5.5
dfS$QUALITY[dfS$QUALITY == "Excellent"] <- 6

dfS <- na.omit(dfS)
# convert character into numeric 
dfS$Condition <- as.numeric(dfS$Condition)
dfS$QUALITY <- as.numeric(dfS$QUALITY)
dfS$Bedrooms <- as.numeric(dfS$Bedrooms)
dfS$LATITUDE <- as.numeric(dfS$LATITUDE)
dfS$YearBuilt <- as.numeric(dfS$YearBuilt)
# remove missing values 
dfS <- na.omit(dfS)
# check missing values 
colSums(is.na(dfS))

corr <- cor(dfS)
corrplot(corr, order="hclust")
dfS <- subset(dfS, select = -c(YearBuilt, Bedrooms))

# new kmeans ---------------------------------------------------------------

# function to calculate the euclidean distance
# create a function for kmeans 
kmeans_new <- function(data, k_num, max_iter = 30){

  # normalize the dataset 
  dfNorm <- as.data.frame(scale(data))
  data <- as.data.frame(apply(dfNorm, 2, function(x){x-min(x)}))
  
  # randomly get cluster centroids 
  rand_inx <- sample(1:nrow(data), k_num)
  centroids <- data[rand_inx,]
  # create empty vectors that will store the cluster label for each observations 
  clusterID <- rep(0, nrow(data))
  nearestDist <- rep(0, nrow(data))
  
  new_clusterID <- rep(0, nrow(data))
  new_nearestDist <- rep(0, nrow(data))
  
  #last_vec <- rep(1, nrow(data))
  # iteration counter
  iter_cnt <- 1
  # stop flag
  converged <- F 

  # Step 4: Calculate the distance from the origin (0) for each row 
  distances <- apply(data, 1, function(point) sqrt(sum(point^2)))
  # Step 5: Sort data points and distances
  sorted_data <- data |> cbind(distances)
  sorted_data <- sorted_data[order(sorted_data$distances, decreasing = FALSE),]
  sorted_dist <- sort(distances)
  # remove the last column
  sorted_data <- sorted_data[,-ncol(sorted_data)]
  sorted_data <- as.data.frame(sorted_data)
  # Step 6: Partition data into k equal sets
  clust_size <- nrow(data) %/% k_num
  clusters <- split(sorted_data, cut(1:nrow(sorted_data), breaks = k_num))
  # Step 7: Take the mean as the initial centroid for each cluster
  init_centroids <- sapply(clusters, function(c) colMeans(c))
  # convert columns into rows
  init_centroids <- as.data.frame(t(init_centroids))
  
  while(converged == F && iter_cnt < max_iter){
    for( i in 1:nrow(data)){
      # Step 8: Compute the distance between each data point and initial centroids
      euclidean_dis <- data[i,] |> rbind(init_centroids) |> dist(method = "euclidean")
      # Step 10-11: Assign data points to the closest cluster
      # find the index where smallest distance is stored in the list
      clus_label <- euclidean_dis[1:k_num] |> which.min()
      # Step 12: add the cluster label to for the i th observation in a vector 
      clusterID[i] <- clus_label
      # Step 12: store smallest distance (total) one distance
      nearestDist[i] <- euclidean_dis[1:k_num][clus_label]
    }
    
    # Step 13: Recalculate centroids
    new_centroids <- data |> cbind(clusterID) |> group_by(clusterID) |> summarise_all(mean)
    init_centroids <- new_centroids
    # remove the column that stores the cluster label 
    init_centroids <- init_centroids[,-1]
   
    # Step 14.1: 
    # Compute its distance from the centroid of the present nearest cluster.
     for( i in 1:nrow(data)){
      # Step 8: Compute the distance between each data point and initial centroids
      new_euclidean_dis <- data[i,] |> rbind(init_centroids) |> dist(method = "euclidean")
      # Step 10-11: Assign data points to the closest cluster
      # find the index where smallest distance is stored in the list
      new_clus_label <- new_euclidean_dis[1:k_num] |> which.min()
      # Step 12: add the cluster label to for the i th observation in a vector 
      new_clusterID[i] <- new_clus_label
      # Step 12: store smallest distance (total) one distance
      new_nearestDist[i] <- new_euclidean_dis[1:k_num][new_clus_label]
     }
    
    # Step 14.2: 
    # meet the criteria, if new nearest distance is less than or equal to the present nearest distance
    if(mean(new_nearestDist) <= mean(nearestDist)){
      converged <- T
    }

    print(paste0("iteration is: ", iter_cnt))
    # update iteration count 
    iter_cnt <- iter_cnt+1 
  }
  cluster_sizes <- data |> cbind(clusterID) |> count(clusterID) |> pull(n)
  init_centroids <- data |> cbind(clusterID) |> group_by(clusterID) |> summarise_all(mean)
  results <- list("size" = cluster_sizes,"clusterMeans" = init_centroids,"clusteringVec" = clusterID, "iterationNum" = iter_cnt)
  return(results)
}

test3 <- kmeans_new(dfS, 4)
label <- test3$clusteringVec
dfNorm |> 
  cbind(label) |> 
  ggplot(aes(x = LATITUDE, y = SoldPrice, color = as.factor(label))) + 
  geom_point() + theme_bw() + labs(color = "Cluster") 
table(label)
# 1     2     3     4 
# 13638 13859 16349 10351 
LONGITUDE <-  dfS$LONGITUDE
LATITUDE <-  dfS$LATITUDE
Label <- label
result_df <- data.frame(LONGITUDE, LATITUDE, Label)
