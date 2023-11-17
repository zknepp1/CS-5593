

library(tidyverse)


population_path <- "https://www.dropbox.com/scl/fi/efu5kcp5zy3dxn1irlzly/Prognose_P.csv?rlkey=zm6gjdurzd014a2rt4c57bexj&dl=1"
sales_path <- "https://www.dropbox.com/scl/fi/b7l74h4ak6syr3z7lt823/Prognose_S.csv?rlkey=7sx3g0gvpel8o66l0dmkkxpmx&dl=1"

salesdata <- read.csv(sales_path)
popdata <- read.csv(population_path)

dim(salesdata)
dim(popdata)


#total_appraised <- df['Total.Appraised.Value']
land_appraised <- df['Land.Appraised.Value']
sqft <- df['Total.Finished.Area']
x <- df['GIS.Coord.1']
y <- df['GIS.Coord.2']

#df_cont <- data.frame(land_appraised, sqft, x, y)
df_cont <- data.frame(x, y)
df_cont <- na.omit(df_cont)

df_cont <- df_cont[1:50,]
dim(df_cont)

df_cont



eps_list <- list(0.002, 0.02, 0.2)
minpoints <- list(2,3,4)
results_list <- list()
df_results <- data.frame()

for (eps in eps_list) {
  for (minpts in minpoints){
    result <- dbscan(df_cont, eps = eps, minPts = minpts)
    df <- data.frame(df_cont, result$cluster)
    df$eps <- eps
    df$pts <- minpts
    df_results <- rbind(df_results, df)
  
  }
}


df_results







euclidean_distance <- function(point1, point2) {
  sqrt(sum((point1 - point2)^2))
}

# DBSCAN algorithm implementation
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
      expand_cluster(data, clusters, i, neighbors, cluster_id, eps, minPts)
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
    } else if (clusters[current_point] == 0) {
      clusters[current_point] <- cluster_id
      
      current_neighbors <- numeric(0)
      for (j in 1:nrow(data)) {
        if (euclidean_distance(data[current_point,], data[j,]) < eps) {
          current_neighbors <- append(current_neighbors, j)
        }
      }
      
      if (length(current_neighbors) >= minPts) {
        neighbors <- c(neighbors, current_neighbors)
      }
    }
    
    i <- i + 1
  }
}




##############
# SHINY APP  #
##############


#install.packages("shiny")
#install.packages("leaflet")


library(shiny)
library(leaflet)
library(ggplot2)

# Sample data for demonstration
data1 <- data.frame(
  x = c(37.7749, 34.0522, 40.7128, 41.8781, 33.4484, 30.2672, 32.7157, 29.7604, 42.3601, 38.8951),
  y = c(-122.4194, -118.2437, -74.0060, -87.6298, -112.0740, -97.7431, -117.1611, -95.3698, -71.0589, -77.0364),
  group = c(1,1,1,2,2,1,2,1,1,2)
)

data2 <- data.frame(
  x = c(51.5074, 48.8566, 52.5200, 53.3498, 55.7558, 40.7128, 37.7749, 41.9028, 55.6761, 48.8566),
  y = c(-0.1278, 2.3522, 13.4050, -6.2602, 37.6176, -74.0060, -122.4194, 12.4964, 12.5683, 2.3522),
  group = c(2,2,2,1,1,2,1,2,2,2)
)

data3 <- data.frame(
  x = c(-33.8688, -27.4698, -37.8136, -34.6118, -23.5505, -26.2041, -36.8485, -28.6139, -37.7749, -41.2865),
  y = c(151.2093, 153.0251, 144.9631, 138.5921, -46.6333, 28.0473, 174.7633, 77.2090, 144.9631, 174.7762),
  group = c(1,2,1,2,1,1,1,2,2,2)
)




ui <- fluidPage(
  titlePanel("Basic Mapping App"),
  div(
    h3(
      selectInput("data_selection", "Select an Algorithm:",
                  choices = c("DBSCAN", "Kmeans", "Hierarchal"),
                  selected = "DBSCAN")
    ),
    mainPanel(
      plotOutput("scatterplot", width = "800px", height = "600px")
    )
  )
)





# Define the server
server <- function(input, output, session) {
  # Render the scatterplot based on the selected data source
  output$scatterplot <- renderPlot({
    selected_data <- switch(input$data_selection,
                            "DBSCAN" = data1,
                            "Kmeans" = data2,
                            "Hierarchal" = data3,
                            data1  # Default selection
    )
    ggplot(selected_data, aes(x = x, y = y, color = factor(group))) +
      geom_point(size = 3) +
      labs(title = "Coordinates by Group") +
      scale_color_discrete(name = "Group") +
      theme(
        text = element_text(size = 20)
      )
    #plot(selected_data$x, selected_data$y, main = "Selected Data Plot")
  })
}


shinyApp(ui, server)








