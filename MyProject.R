# cleaning data

library(shiny)

population_path <- "/Users/jacobflynn/Documents/Grad_School/Fall_2023/Data_Mining/Ok-County-Res-Data/Prognose_P.csv"
sales_path <- "/Users/jacobflynn/Documents/Grad_School/Fall_2023/Data_Mining/Ok-County-Res-Data/Prognose_S.csv"

salesdata <- read.csv(sales_path)
popdata <- read.csv(population_path)

x <- salesdata['LONGITUDE']
y <- salesdata['LATITUDE']
gla <- salesdata['GLA']
yrblt <- salesdata['YearBuilt']
sprice <- salesdata['SoldPrice']

df_cont <- data.frame(x, y, gla, yrblt, sprice)

df_cont$LONGITUDE <- as.numeric(df_cont$LONGITUDE)
df_cont$LATITUDE <- as.numeric(df_cont$LATITUDE)
df_cont$GLA <- as.numeric(df_cont$GLA)
df_cont$YearBuilt <- as.numeric(df_cont$YearBuilt)

sum(is.na(df_cont))

for(i in colnames(df_cont)){
  df_cont[,i][is.na(df_cont[,i])] = mean(df_cont[,i], na.rm = T) # mean value imputation
}

################################################################################

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
  
  #print(put)
  #print(summary(put))
  # need whole output from put variable to plot
  # https://r-graphics.org/recipe-miscgraph-dendrogram 
  #plot(put)
  #print(mat)
  #print(dis)
  return(put)
  
}

################################################################################
housing <- hierarchical(df_cont, distance = "euclidean", method = "single")

# housing$height
# housing$merge
# housing$order
# housing$labels
# housing$method
# housing
# 
# plot(housing)

closest_clusters <- function(input_property, df_cont) {
  # Perform hierarchical clustering
  #housing <- hierarchical(df_cont, distance = "euclidean", method = "single")
  
  # Cut the dendrogram to obtain clusters
  temp <- cutree(housing, h = .10)
  
  # Calculate distances to each cluster
  distances_to_clusters <- sapply(seq_along(temp), function(i) {
    cluster_data <- df_cont[temp == i, -1]
    
    # Ensure input_value has the same number of components as cluster_data
    adjusted_input <- input_property[1:length(cluster_data)]
    
    dist_to_cluster <- dist(rbind(adjusted_input, cluster_data))
    min(dist_to_cluster)
  })
  
  print("here1")
  
  # Find the indices of the top 6 closest clusters
  top_indices <- order(distances_to_clusters)[1:6]
  
  # Find the closest values within the top 6 clusters
  closest_values <- df_cont[df_cont$ID %in% top_indices, -1]
  
  print("here2")
  # Assuming distances_to_clusters and df_cont have the same number of rows
  #result <- data.frame(distances = distances_to_clusters)
  
  result <- data.frame(df_cont[top_indices, ])
  #print(result)
  #print(distances_to_clusters)
  clus_dist <- sort(distances_to_clusters)
  result$distances <- clus_dist[1:6]
  
  #print(result)
  return(result)
}


ui <- fluidPage(
  titlePanel("Comparable Property Finder"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Enter numbers to compare with the dataset"),
      
      # Input: Enter Square Footage
      numericInput("squareFootage", "Please enter the Square Footage:", value = 500),
      
      # Input: Enter year built
      numericInput("yblt", "Please enter the Year Built:", value = 2000),
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "longitude",
                  label = "Please enter the Longitude:",
                  min = -97.673696,
                  max = -97.145898,
                  value = -97.4),
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "latitude",
                  label = "Please enter the Latitude:",
                  min = 35.377213,
                  max = 35.725231,
                  value = 35.5),
      
      #selectInput("Select", "Distance Metric", choices = c("euclidean", "manhattan", "minkowski")),
      
      
      actionButton(inputId = "submit", label = "Submit")
    ),
    
    mainPanel(
      # Output: Display results
      tableOutput("results"),
      #tableOutput("averages")
    )
  )
)

top_six <- data.frame()

server <- function(input, output) {
  
  #top_six <- data.frame()
  
  comps <- eventReactive(input$submit, {
    if (is.null(input$squareFootage) || is.null(input$longitude) || is.null(input$latitude) || is.null(input$yblt)) {
      return()
    }
    
    # input_property <- data.frame(squareFootage = input$squareFootage,
    #                              longitude = input$longitude,
    #                              latitude = input$latitude,
    #                              YearBuilt = input$yblt) # Changed to match df_cont column name
    
    input_property <- as.vector(c(input$squareFootage, input$longitude, input$latitude, input$yblt))
    
    #print(input_property)
    print("here3")
    
    similar_properties <- closest_clusters(input_property, df_cont)
    
    print("here4")
    sorted_properties <- similar_properties[order(similar_properties$distances), ] # Corrected column name
    top_six <<- head(sorted_properties, 6)
    
    print("here5")
    
    #averages <- calculate_averages(top_six)
    
    #list(top_six = top_six) #, averages = averages)
    
    print("here6")
    print(top_six)
    
    #input_property <- as.vector(c())
    
  })
  
   output$results <- renderTable({
     #print('here7')
     comps()$top_six
     top_six
     
     #print(top_six)
     
     #print('here8')

   })
  #top_six <<- data.frame()
}

shinyApp(ui, server)









server <- function(input, output) {
  
  comps <- eventReactive(input$submit, {
    if (is.null(input$squareFootage) || is.null(input$longitude) || is.null(input$latitude) || is.null(input$yblt)) {
      return()
    }
    
    input_property <- data.frame(squareFootage = input$squareFootage,
                                 longitude = input$longitude,
                                 latitude = input$latitude,
                                 YearBuilt = input$yblt)
    
    
    estimated_sp <- sales_comparison(input_property, df_cont)
    
    similar_properties <- calculate_similarity(input_property, df_cont)
    
    sorted_properties <- similar_properties[order(similar_properties$distances), ]
    
    top_six <- head(df_cont, 6)
    
    averages <- calculate_averages(top_six)
    
    averages$esp <- estimated_sp
    
    list(top_six = top_six, averages = averages)
    
  })
  
  output$results <- renderTable({comps()$top_six})
  output$averages <- renderTable({comps()$averages})
}






# server <- function(input, output) {
#   
#   top_six <- data.frame()  # Initialize as a reactiveValues
#   
#   comps <- eventReactive(input$submit, {
#     if (is.null(input$squareFootage) || is.null(input$longitude) || is.null(input$latitude) || is.null(input$yblt)) {
#       return(data.frame())  # Return an empty data frame if input is incomplete
#     }
#     
#     input_property <- as.vector(c(input$squareFootage, input$longitude, input$latitude, input$yblt))
#     
#     print("here3")
#     
#     similar_properties <- closest_clusters(input_property, df_cont)
#     
#     print("here4")
#     sorted_properties <- similar_properties[order(similar_properties$distances), ] # Corrected column name
#     top_six_val <- head(sorted_properties, 6)
#     
#     print("here5")
#     
#     # Update the reactiveValues with the new value
#     top_six(top_six_val)
#     
#     print("here6")
#     print(top_six_val)
#     
#     return(list(top_six = top_six_val))
#   })
#   
#   output$results <- renderTable({
#     print('here7')
#     comps()$top_six
#   })
#   
# }
# 
# 
# 
# 
# 
# 
# # top_six <- NULL
# # 
# # server <- function(input, output) {
# #   
# #   comps <- eventReactive(input$submit, {
# #     if (is.null(input$squareFootage) || is.null(input$longitude) || is.null(input$latitude) || is.null(input$yblt)) {
# #       return()
# #     }
# #     
# #     input_property <- as.vector(c(input$squareFootage, input$longitude, input$latitude, input$yblt))
# #     print("here3")
# #     
# #     similar_properties <- closest_clusters(input_property, df_cont)
# #     
# #     print("here4")
# #     sorted_properties <- similar_properties[order(similar_properties$distances), ] # Corrected column name
# #     top_six <<- head(sorted_properties, 6)  # Use <<- to assign to the global variable
# #     
# #     print("here5")
# #     
# #     #averages <- calculate_averages(top_six)
# #     
# #     list(top_six = top_six)  #, averages = averages)
# #     
# #     print("here6")
# #     #print(top_six)
# #   })
# #   
# #   output$results <- renderTable({
# #     result <- comps()
# #     
# #     result$top_six
# #     
# #     # if (!is.null(result) && is.list(result) && "top_six" %in% names(result)) {
# #     #   result$top_six
# #     # } else {
# #     #   # Return an empty data frame or some default value if there's no valid result
# #     #   data.frame()
# #     # }
# #   })
# # }


library(shiny)

closest_clusters <- function(input_property, df_cont) {
  # Cut the dendrogram to obtain clusters
  temp <- cutree(housing, h = .10)
  
  # Calculate distances to each cluster
  distances_to_clusters <- sapply(seq_along(temp), function(i) {
    cluster_data <- df_cont[temp == i, -1]
    
    # Ensure input_value has the same number of components as cluster_data
    adjusted_input <- input_property[1:length(cluster_data)]
    
    dist_to_cluster <- dist(rbind(adjusted_input, cluster_data))
    min(dist_to_cluster)
  })
  
  print("here1")
  
  # Find the indices of the top 6 closest clusters
  top_indices <- order(distances_to_clusters)[1:6]
  
  # Find the closest values within the top 6 clusters
  closest_values <- df_cont[df_cont$ID %in% top_indices, -1]
  
  print("here2")
  
  # Assuming distances_to_clusters and df_cont have the same number of rows
  result <- df_cont[top_indices, ]
  
  # Sort distances and add to the result
  clus_dist <- sort(distances_to_clusters)
  result$distances <- clus_dist[1:6]
  
  print(result)
  return(result)
}



ui <- fluidPage(
  titlePanel("Comparable Property Finder"),
  sidebarLayout(
    sidebarPanel(
      helpText("Enter numbers to compare with the dataset"),
      numericInput("squareFootage", "Please enter the Square Footage:", value = 500),
      numericInput("yblt", "Please enter the Year Built:", value = 2000),
      sliderInput(inputId = "longitude",
                  label = "Please enter the Longitude:",
                  min = -97.673696,
                  max = -97.145898,
                  value = -97.4),
      sliderInput(inputId = "latitude",
                  label = "Please enter the Latitude:",
                  min = 35.377213,
                  max = 35.725231,
                  value = 35.5),
      actionButton(inputId = "submit", label = "Submit")
    ),
    mainPanel(
      tableOutput("results")
    )
  )
)

server <- function(input, output) {
  # Initialize reactiveValues to store top_six
  rv <- reactiveValues(top_six = data.frame())
  
  observeEvent(input$submit, {
    if (is.null(input$squareFootage) || is.null(input$longitude) || is.null(input$latitude) || is.null(input$yblt)) {
      return(NULL)  # Return NULL if inputs are null
    }
    
    input_property <- as.vector(c(input$squareFootage, input$longitude, input$latitude, input$yblt))
    
    print("here3")
    
    similar_properties <- closest_clusters(input_property, df_cont)
    
    print("here4")
    sorted_properties <- similar_properties[order(similar_properties$distances), ] # Corrected column name
    rv$top_six <- head(sorted_properties, 6)
    
    print("here5")
    
    #averages <- calculate_averages(top_six)
    
    print("here6")
    print(rv$top_six)
  })
  
  observe({
    print("observe event triggered")
    print(input$submit)
  })
  
  output$results <- renderTable({
    print('here7')
    rv$top_six
  })
}

shinyApp(ui, server)



