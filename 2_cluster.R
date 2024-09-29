library(googledrive)
library(googlesheets4)
library(tidyverse)
library(tidylog)
library(cluster)

# SETUP ========================================================================
drive_auth(email = "sifan1121@gmail.com")
gs4_auth(token = drive_token())
url <- "https://docs.google.com/spreadsheets/d/19NWvZ3kkDv2KYPvd9QdC6wH0vMQlXeD6NPty1OrjpUM/edit?gid=0#gid=0"


ai_results <- read_sheet(url, "ai_results_raw")
# CLUSTER =======
# use elbow method to find optimal number of clusters

# Data preprocessing
scaled_data <- ai_results %>% 
  filter(cbsa_size!='small metros') |> 
  select(starts_with('index')) %>%
  mutate_if(is.numeric, ~replace_na(., 0)) %>%
  scale()

# Function to calculate within-cluster sum of squares (wss)
wss <- function(k) {
  kmeans(scaled_data, centers = k, nstart = 10)$tot.withinss
}

# Calculate wss for a range of k values
k_values <- 1:15
wss_values <- sapply(k_values, wss)

# Plot elbow curve
plot(k_values, wss_values, type = 'b', pch = 19, frame = FALSE,
     xlab = 'Number of clusters K', ylab = 'Total within-clusters sum of squares')

# Perform k-means clustering with the optimal number of clusters (e.g., 5)
set.seed(123)  # for reproducibility
optimal_k <- 5
kmeans_result <- kmeans(scaled_data, centers = optimal_k, nstart = 10)

# Visualize the clusters
clusplot(scaled_data, kmeans_result$cluster, color = TRUE, shade = TRUE,
         labels = 2, lines = 0)

ai_results |> 
  filter(cbsa_size!='small metros') |> 
  mutate(cluster = kmeans_result$cluster) |>
  select(starts_with('cbsa'), cluster, starts_with('index')) |>
  arrange(cluster) |> 
  write_sheet(url, "ai_results_cluster_updated")
