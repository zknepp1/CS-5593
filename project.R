

library(tidyverse)

# Path to the data
population_path <- "https://www.dropbox.com/scl/fi/efu5kcp5zy3dxn1irlzly/Prognose_P.csv?rlkey=zm6gjdurzd014a2rt4c57bexj&dl=1"
sales_path <- "https://www.dropbox.com/scl/fi/b7l74h4ak6syr3z7lt823/Prognose_S.csv?rlkey=7sx3g0gvpel8o66l0dmkkxpmx&dl=1"

salesdata <- read.csv(sales_path)
popdata <- read.csv(population_path)

dim(salesdata)
dim(popdata)


names(salesdata)


# Variables I decided to use
x <- salesdata['LONGITUDE']
y <- salesdata['LATITUDE']
gla <- salesdata['GLA']
yrblt <- salesdata['YearBuilt']

#df_cont <- data.frame(land_appraised, sqft, x, y)
df_cont <- data.frame(x, y, gla, yrblt)

df_cont <- na.omit(df_cont)
dim(df_cont)


# Making sure they are numeric
df_cont$LONGITUDE <- as.numeric(df_cont$LONGITUDE)
df_cont$LATITUDE <- as.numeric(df_cont$LATITUDE)
df_cont$GLA <- as.numeric(df_cont$GLA)
df_cont$YearBuilt <- as.numeric(df_cont$YearBuilt)

df_cont <- na.omit(df_cont)
dim(df_cont)





dim(df_cont)

df_cont

class(df_cont$GLA)
class(df_cont$LONGITUDE)
class(df_cont$LATITUDE)
class(df_cont$YearBuilt)



# putting data in DBSCAN algorithm
result <- dbscan(df_cont, eps = 20, minPts = 3)

result



df_cont$cluster <- result$cluster
head(df_cont)

dim(df_cont)


head(df_cont, 6)













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






##############
# SHINY APP  #
##############


install.packages("shiny")
#install.packages("leaflet")




# Table to compare
df_cont
names(df_cont)


library(shiny)

# Similarity function
# calculates similarity between the variables, squareFootage, YearBuilt, longitude, and latitude
calculate_similarity <- function(input_property, df) {
  distances <- numeric(nrow(df))
  for(i in 1:nrow(df)){
    distances[i] <- sqrt((df$GLA[i] - input_property$squareFootage)^2 +
                           (df$LONGITUDE[i] - input_property$longitude)^2 +
                           (df$LATITUDE[i] - input_property$latitude)^2 +
                           (df$YearBuilt[i] - input_property$YearBuilt)^2)
  }
  
  df$distances <- distances
  print(distances)
  return(df)
}


# The is part of the shiney app that the user will see
ui <- fluidPage(
  titlePanel("Number Comparables Finder"),
  
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
      
      
      actionButton(inputId = "submit", label = "Submit")
    ),
    
    mainPanel(
      # Output: Display results
      tableOutput("results")
    )
  )
)



# This is the backend server that controls the app
# Right now it prints out the 6 most similar properties.
# I would like to be able to value properties with a sales comparison approach.
server <- function(input, output) {
  
  data_to_display <- eventReactive(input$submit, {
    if (is.null(input$squareFootage) || is.null(input$longitude) || is.null(input$latitude) || is.null(input$yblt)) {
      return()
    }
    
    input_property <- data.frame(squareFootage = input$squareFootage,
                                 longitude = input$longitude,
                                 latitude = input$latitude,
                                 YearBuilt = input$yblt) # Changed to match df_cont column name
    
    

    similar_properties <- calculate_similarity(input_property, df_cont)
    sorted_properties <- similar_properties[order(similar_properties$distances), ] # Corrected column name
    top_six <- head(df_cont, 6)
    top_six
  })
  
  output$results <- renderTable({
    data_to_display()
  })
}


shinyApp(ui, server)


















