library(tidyverse)
library(dplyr)
library(caret)


# load datasets 
#population_path <- "https://www.dropbox.com/scl/fi/efu5kcp5zy3dxn1irlzly/Prognose_P.csv?rlkey=zm6gjdurzd014a2rt4c57bexj&dl=1"
#popdata <- read.csv(population_path)
sales_path <- "https://www.dropbox.com/scl/fi/b7l74h4ak6syr3z7lt823/Prognose_S.csv?rlkey=7sx3g0gvpel8o66l0dmkkxpmx&dl=1"
salesdata <- read.csv(sales_path)

# pre-processing ---- 
x <- salesdata['LONGITUDE']
y <- salesdata['LATITUDE']
gla <- salesdata['GLA']
yrblt <- salesdata['YearBuilt']
sprice <- salesdata['SoldPrice']
quality <- salesdata['QUALITY']
# convert quality data into numeric scale 
quality[quality == "Low"] <- 1
quality[quality == "Low Plus"] <- 1.5
quality[quality == "Fair"] <- 2
quality[quality == "Fair Plus"] <- 2.5
quality[quality == "Average"] <- 3
quality[quality == "Average Plus"] <- 3.5
quality[quality == "Good"] <- 4
quality[quality == "Good Plus"] <- 4.5
quality[quality == "Very Good"] <- 5
quality[quality == "Very Good Plus"] <- 5.5
quality[quality == "Excellent"] <- 6

# sprice => sold price 
df_cont <- data.frame(x, y, gla, yrblt, sprice, quality)
# remove missing values 
df_cont <- na.omit(df_cont)
# convert some variables into numeric 
df_cont$LONGITUDE <- as.numeric(df_cont$LONGITUDE)
df_cont$LATITUDE <- as.numeric(df_cont$LATITUDE)
df_cont$GLA <- as.numeric(df_cont$GLA)
df_cont$YearBuilt <- as.numeric(df_cont$YearBuilt)
df_cont$SoldPrice <- as.numeric(df_cont$SoldPrice)
df_cont$QUALITY <-  as.numeric(df_cont$QUALITY)
# remove missing values 
df_cont <- na.omit(df_cont)


# get some sample data 
set.seed(100)
df_cont <- df_cont[sample(nrow(df_cont), 250), ]

euclidean_distance <- function(point1, point2) {
  sqrt(sum((point1 - point2)^2))
}

##  DBSCAN algorithm implementation ---- 
# Takes input data, eps, and minpoints as parameters
dbscan <- function(data, eps, minPts) {
  n <- nrow(data)
  #preparing lists
  clusters <- rep(0, n)
  cluster_id <- 0
  
  for (i in 1:n) {
    if (clusters[i] != 0) {
      next
    }
    #neighbors list
    neighbors <- numeric(0)
    
    for (j in 1:n) {
      if (euclidean_distance(data[i,], data[j,]) < eps) {
        neighbors <- append(neighbors, j)
      }
    } 
    
    if (length(neighbors) < minPts) {
      # Im using -1 as noise because thats standard
      clusters[i] <- -1
    } else {
      cluster_id <- cluster_id + 1
      clusters <- expand_cluster(data, clusters, i, neighbors, cluster_id, eps, minPts)
    }
  }
  # returns the clusters as a list
  return(list(cluster = clusters))
}

# Function to expand a cluster
expand_cluster <- function(data, clusters, point_index, neighbors, cluster_id, eps, minPts) {
  clusters[point_index] <- cluster_id
  
  i <- 1
  while (i <= length(neighbors)) {
    current_point <- neighbors[i]
    
    if (clusters[current_point] == -1) {
      clusters[current_point] <- cluster_id
    } 
    else if (clusters[current_point] == 0) {
      clusters[current_point] <- cluster_id
      
      current_neighbors <- numeric(0)
      for (j in 1:nrow(data)) {
        if (euclidean_distance(data[current_point,], data[j,]) < eps) {
          current_neighbors <- append(current_neighbors, j)
        }
      }
      
      if (length(current_neighbors) >= minPts) {
        neighbors <- unique(c(neighbors, current_neighbors))
      }
    }
    
    i <- i + 1
  }
  return(clusters)
}



#############################################################################################
#############################################################################################
# SHINY APP  ################################################################################
#############################################################################################
#############################################################################################
#install.packages("shiny")
#install.packages("leaflet")
library(shiny)

calculate_averages <- function(df) {
  averages <- colMeans(df[sapply(df, is.numeric)])
  return(data.frame(t(averages)))
}

# create a function for kmeans 
kmeans_new <- function(orig_data, k_num = 4, max_iter = 30){
  # skip step 1-4 and do scale and normalization instead
  df <- orig_data
  dfNorm <- as.data.frame(scale(df))
  data <- as.data.frame(apply(dfNorm, 2, function(x){x-min(x)}))
  
  # randomly get cluster centroids 
  rand_inx <- sample(1:nrow(data), k_num)
  centroids <- data[rand_inx,]
  # create empty vectors that will store the cluster label for each obs 
  clusterID <- rep(0, nrow(data))
  nearestDist <- rep(0, nrow(data))
  # iteration counter
  iter_cnt <- 1
  # stop flag
  converged <- F 
  # converge counter
  cnv_cnt <- 0
  
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
  
  while(converged == F){
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
    
    curr_distance <- sapply(init_centroids,function(centroid) sqrt(rowSums((data-centroid)^2)))
    
    # Step 13: Recalculate centroids
    new_centroids <- data |> cbind(clusterID) |> group_by(clusterID) |> summarise_all(mean)
    init_centroids <- new_centroids
    # remove the column that stores the cluster label 
    init_centroids <- init_centroids[,-1]
    
    # Step 14.1: Check for convergence 
    # Compute its distance from the centroid of the present nearest cluster.
    new_distance <- sapply(init_centroids,function(centroid) sqrt(rowSums((data-centroid)^2)))
    
    # finish when exceeding the max iteration number or pass the step 14.2
    if(max_iter <= iter_cnt){
      converged <- T
    }
    for( x in 1:nrow(new_distance)){
      
      # compute the distance for every centroid if the distance is greater than the present nearest cluster 
      if(mean(new_distance[x]) > mean(curr_distance[x])){
        euclidean_dis <- data[x,] |> rbind(init_centroids) |> dist(method = "euclidean")
        clus_label <- euclidean_dis[1:k_num] |> which.min()
        clusterID[x] <- clus_label
        nearestDist[x] <- euclidean_dis[1:k_num][clus_label]
      }
      # or converge if overall mean of new distance is less than the current one 
      else if(mean(new_distance) <= mean(curr_distance)){
        converged <- T
      }
    }
    #print(paste0("iteration is: ", iter_cnt))
    
    # update iteration count 
    iter_cnt <- iter_cnt+1 
  }
  
  sd_num <- apply(orig_data, 2, sd) 
  mean_num <- apply(orig_data, 2, mean)
  # un-normalize data 
  unscaled_df <- data
  min_values <- apply(dfNorm, 2, min)
  unscaled_df$GLA <- data$GLA + min_values["GLA"]
  unscaled_df$YearBuilt <- data$YearBuilt + min_values["YearBuilt"]
  unscaled_df$QUALITY <- data$QUALITY + min_values["QUALITY"]
  unscaled_df$LONGITUDE <- data$LONGITUDE + min_values["LONGITUDE"]
  unscaled_df$LATITUDE <- data$LATITUDE + min_values["LATITUDE"]
  unscaled_df$SoldPrice  <- data$SoldPrice + min_values["SoldPrice"]
  # un-scale data 
  unscaled_df$GLA <- unscaled_df$GLA * sd_num["GLA"] + mean_num["GLA"]
  unscaled_df$YearBuilt <- unscaled_df$YearBuilt * sd_num["YearBuilt"] + mean_num["YearBuilt"] 
  unscaled_df$QUALITY <- unscaled_df$QUALITY * sd_num["QUALITY"] + mean_num["QUALITY"]
  unscaled_df$LONGITUDE <- unscaled_df$LONGITUDE * sd_num["LONGITUDE"] + mean_num["LONGITUDE"]
  unscaled_df$LATITUDE <- unscaled_df$LATITUDE * sd_num["LATITUDE"] + mean_num["LATITUDE"]
  unscaled_df$SoldPrice  <- unscaled_df$SoldPrice * sd_num["SoldPrice"] + mean_num["SoldPrice"]
  
  get_centroid <- data |> cbind(clusterID)  
  total_sse = 0
  # calculate SSE 
  for (i in 1:k_num) {
    cluster_totalpoints <- get_centroid[clusterID == i, ]
    cluster_points <- cluster_totalpoints[,-ncol(cluster_totalpoints)]
    cluster_points <- as.data.frame(cluster_points)
    center = as.data.frame(init_centroids[i,])
    for(j in 1:nrow(cluster_points)){
      total_sse <- total_sse + sum((cluster_points[j,] - center)^2)
    }
    #cat("k num: ", i , " SSE: ", total_sse, "\n")
  }
  cat("Total SSE: ", total_sse, "\n")
  
  cluster_sizes <- data |> cbind(clusterID) |> count(clusterID) |> pull(n)
  # get means of each properties in each cluster 
  init_centroids <- unscaled_df |> cbind(clusterID) |> group_by(clusterID) |> summarise_all(mean)
  
  # total results 
  results <- list("size" = cluster_sizes, "total_SSE" = total_sse,
                  "clusterMeans" = init_centroids,"clusteringVec" = clusterID, "iterationNum" = iter_cnt)
  return(results)
}


# calculate a distance between input and data (get k-means results from this function)
calculate_similarity <- function(input_property, df_cont, algoResults) {
  # get cluster labels (k-means)
  cluster_label <- algoResults$clusteringVec
  distances <- numeric(nrow(df_cont))
  # calculate distances 
  for(i in 1:nrow(df_cont)){
    distances[i] <- sqrt((df_cont$GLA[i] - input_property$squareFootage)^2 +
                           (df_cont$LONGITUDE[i] - input_property$longitude)^2 +
                           (df_cont$LATITUDE[i] - input_property$latitude)^2 +
                           (df_cont$YearBuilt[i] - input_property$YearBuilt)^2)
  }
  
  df_cont$distances <- distances
  df_cont$clusterID <- cluster_label
  # Change the order of variables
  new_df <- df_cont[, c("clusterID", "LONGITUDE", "LATITUDE", "GLA", "YearBuilt", "QUALITY", "SoldPrice", "distances")]

  return(new_df)
}

# calculate a distance between each mean of cluster and input (for k-means)
get_clusterNum <- function(input, algoResults){
  # get centroids (k-means)
  cluster_means <- algoResults$clusterMeans
  distances <- numeric(nrow(cluster_means))
  for(i in 1:nrow(cluster_means)){
    distances[i] <-    sqrt((cluster_means$GLA[i] - input$squareFootage)^2 +
                           (cluster_means$LONGITUDE[i] - input$longitude)^2 +
                           (cluster_means$LATITUDE[i] - input$latitude)^2 +
                           (cluster_means$YearBuilt[i] - input$YearBuilt)^2)
  }
  cluster_means$distances <- distances
  new_df <- cluster_means[, c("clusterID", "LONGITUDE", "LATITUDE", "GLA", "YearBuilt", "QUALITY", "SoldPrice", "distances")]
  return(new_df)
}


#sales comparison approach function
sales_comparison <- function(subject, comparables) {
  
  # Calculate adjustment factors
  subject_gla <- subject$squareFootage
  subject_yblt <- subject$YearBuilt
  subject_latitude <- subject$latitude
  subject_longitude <- subject$longitude
  
  comparables$gla_adjustment <- subject_gla / comparables$GLA
  comparables$yblt_adjustment <- subject_gla / comparables$YearBuilt
  comparables$lat_adjustment <- subject_gla / comparables$LATITUDE
  comparables$long_adjustment <- subject_gla / comparables$LONGITUDE
  
  
  # Adjust comparable sales prices
  comparables$adjusted_price <- comparables$SoldPrice * comparables$gla_adjustment * comparables$yblt_adjustment * comparables$lat_adjustment * abs(comparables$long_adjustment)
  
  # Calculate estimated sales price as the average of adjusted comparable prices
  estimated_sales_price <- mean(comparables$adjusted_price)

  
  return(estimated_sales_price)
}


ui <- fluidPage(
  titlePanel("Number Comparables Finder"),
  # Embedding JavaScript inside the UI
  tags$head(
    tags$script(HTML('
      $(document).on("click", "#submit", function() {
        $("#squareFootage").val("");
        $("#yblt").val("");
        $("#longitude").val("");
        $("#latitude").val("");
      });
    '))
  ),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Enter numbers to compare with the dataset"),
      
      # Input - Enter Square Footage
      numericInput(inputId = "squareFootage", 
                   label = "Please enter the Square Footage:", 
                   value = 500),
      
      # Input - Enter year built
      numericInput(inputId = "yblt", 
                   label = "Please enter the Year Built:", 
                   value = 2000),
      
      #Input - Slider for the number of bins
      sliderInput(inputId = "longitude",
                  label = "Please enter the Longitude:",
                  min = -97.673696,
                  max = -97.145898,
                  value = -97.4),
      
      # Input - Slider for the number of bins
      sliderInput(inputId = "latitude", 
                  label = "Please enter the Latitude:",
                  min = 35.377213,
                  max = 35.725231,
                  value = 35.5),
      actionButton(inputId = "submit", label = "Submit")
    ),
    
    mainPanel(
      # Output
      h4("K-means"),
      h5("Top6 Overall Results based on distance"),
      tableOutput("results"),
      h5("Cluster Results based on distance"),
      tableOutput("averages")
    )
  )
)

server <- function(input, output) {
  comps <- eventReactive(input$submit, {
    if (is.null(input$squareFootage) || is.null(input$longitude) || is.null(input$latitude) || is.null(input$yblt)) {
      return()
    }
    input_property <- data.frame(squareFootage = input$squareFootage,
                                 longitude = input$longitude,
                                 latitude = input$latitude,
                                 YearBuilt = input$yblt)
    
    k_results <- kmeans_new(df_cont)
    estimated_sp <- sales_comparison(input_property, df_cont)
    similar_properties <- calculate_similarity(input_property, df_cont, k_results)
    sorted_properties <- similar_properties[order(similar_properties$distances), ]
    
    top_six <- head(sorted_properties, 6)
  
    # compare each cluster 
    cmp_cluster <- get_clusterNum(input_property, k_results)
    cmp_cluster$esp <- estimated_sp
    list(top_six = top_six, averages = cmp_cluster)
    
  })
  
  # display top 6 results 
  output$results <- renderTable({comps()$top_six})
  # display average of the results 
  output$averages <- renderTable({comps()$averages})

}

shinyApp(ui, server)










