


library(tidyverse)


population_path <- "https://www.dropbox.com/scl/fi/efu5kcp5zy3dxn1irlzly/Prognose_P.csv?rlkey=zm6gjdurzd014a2rt4c57bexj&dl=1"
sales_path <- "https://www.dropbox.com/scl/fi/b7l74h4ak6syr3z7lt823/Prognose_S.csv?rlkey=7sx3g0gvpel8o66l0dmkkxpmx&dl=1"

salesdata <- read.csv(sales_path)
popdata <- read.csv(population_path)

dim(salesdata)
dim(popdata)


names(salesdata)



#total_appraised <- df['Total.Appraised.Value']
#land_appraised <- salesdata['Land.Appraised.Value']
#sqft <- salesdata['Total.Finished.Area']
x <- salesdata['LONGITUDE']
y <- salesdata['LATITUDE']
gla <- salesdata['GLA']
yrblt <- salesdata['YearBuilt']

#df_cont <- data.frame(land_appraised, sqft, x, y)
df_cont <- data.frame(x, y, gla, yrblt)

df_cont <- na.omit(df_cont)
dim(df_cont)

df_cont$LONGITUDE <- as.numeric(df_cont$LONGITUDE)
df_cont$LATITUDE <- as.numeric(df_cont$LATITUDE)
df_cont$GLA <- as.numeric(df_cont$GLA)
df_cont$YearBuilt <- as.numeric(df_cont$YearBuilt)

df_cont <- na.omit(df_cont)
dim(df_cont)





sub <- c(1:50, 1000:2050, 10000:10050)
df_cont <- df_cont[sub,]
dim(df_cont)

df_cont

class(df_cont$GLA)
class(df_cont$LONGITUDE)
class(df_cont$LATITUDE)


result <- dbscan(df_cont, eps = 0.2, minPts = 3)

result



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
library(shiny)

ui <- fluidPage(
  titlePanel("Number Comparables Finder"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Enter numbers to compare with the dataset"),
      
      # Input: Enter numbers
      textInput("numbers", "Please enter the Square Footage:", ""),
      # Input: Enter numbers
      textInput("numbers", "Please enter the Longitude:", ""),
      # Input: Enter numbers
      textInput("numbers", "Please enter the Latitude:", ""),

      
      submitButton("Submit")
    ),
    
    mainPanel(
      # Output: Display results
      tableOutput("results")
    )
  )
)




server <- function(input, output) {
  
  # Function to find similar data points
  find_comparables <- function(input_numbers, dataset) {
    # Logic to compare input_numbers with dataset and find comparables
    # Return the comparable data points
  }
  
  output$results <- renderTable({
    # Ensure that input is not empty
    if (input$numbers == "") {
      return()
    }
    
    # Convert input numbers into a numeric vector
    input_numbers <- as.numeric(unlist(strsplit(input$numbers, ",")))
    
    # Call the function to find comparables
    comparables <- find_comparables(input_numbers, your_dataframe)
    
    # Return the comparable data
    comparables
  })
}


shinyApp(ui, server)











