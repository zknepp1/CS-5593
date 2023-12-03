library(tidyverse)
library(dplyr)

population_path <- "https://www.dropbox.com/scl/fi/efu5kcp5zy3dxn1irlzly/Prognose_P.csv?rlkey=zm6gjdurzd014a2rt4c57bexj&dl=1"
sales_path <- "https://www.dropbox.com/scl/fi/b7l74h4ak6syr3z7lt823/Prognose_S.csv?rlkey=7sx3g0gvpel8o66l0dmkkxpmx&dl=1"

salesdata <- read.csv(sales_path)
popdata <- read.csv(population_path)

dim(salesdata)
dim(popdata)


names(salesdata)



x <- salesdata['LONGITUDE']
y <- salesdata['LATITUDE']
gla <- salesdata['GLA']
yrblt <- salesdata['YearBuilt']
sprice <- salesdata['SoldPrice']

sprice


df_cont <- data.frame(x, y, gla, yrblt, sprice)

df_cont


df_cont <- na.omit(df_cont)
dim(df_cont)

df_cont$LONGITUDE <- as.numeric(df_cont$LONGITUDE)
df_cont$LATITUDE <- as.numeric(df_cont$LATITUDE)
df_cont$GLA <- as.numeric(df_cont$GLA)
df_cont$YearBuilt <- as.numeric(df_cont$YearBuilt)
df_cont$SoldPrice <- as.numeric(df_cont$SoldPrice)

df_cont <- na.omit(df_cont)
dim(df_cont)





set.seed(100)
df_cont <- df_cont[sample(nrow(df_cont), 250), ]

dim(df_cont)

df_cont

result <- dbscan(df_cont, eps = 200, minPts = 3)

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





#############################################################################################
#############################################################################################
# SHINY APP  ################################################################################
#############################################################################################
#############################################################################################











install.packages("shiny")
#install.packages("leaflet")




# Table to compare
df_cont
names(df_cont)


library(shiny)


calculate_similarity <- function(input_property, df_cont) {
  distances <- numeric(nrow(df_cont))
  for(i in 1:nrow(df_cont)){
    distances[i] <- sqrt((df_cont$GLA[i] - input_property$squareFootage)^2 +
                           (df_cont$LONGITUDE[i] - input_property$longitude)^2 +
                           (df_cont$LATITUDE[i] - input_property$latitude)^2 +
                           (df_cont$YearBuilt[i] - input_property$YearBuilt)^2)
  }
  
  df_cont$distances <- distances
  print(distances)
  return(df_cont)
}


calculate_averages <- function(df) {
  averages <- colMeans(df[sapply(df, is.numeric)])
  return(data.frame(t(averages)))
}



#sales comparison approach function

sales_comparison <- function(subject, comparables) {
  # Calculate adjustment factors
  subject_gla <- subject$squareFootage
  comparables$gla_adjustment <- subject_gla / comparables$GLA
  
  
  
  # Adjust comparable sales prices
  comparables$adjusted_price <- comparables$SoldPrice * comparables$gla_adjustment
  
  # Calculate estimated sales price as the average of adjusted comparable prices
  estimated_sales_price <- mean(comparables$adjusted_price)
  
  
  return(estimated_sales_price)
}







ui <- fluidPage(
  titlePanel("Number Comparables Finder"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Enter numbers to compare with the dataset"),
      
      # Input - Enter Square Footage
      numericInput("squareFootage", "Please enter the Square Footage:", value = 500),
      
      # Input - Enter year built
      numericInput("yblt", "Please enter the Year Built:", value = 2000),
      
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
      # OutpuT
      print('hello'),
      tableOutput("results"),
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


shinyApp(ui, server)










