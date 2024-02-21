#install.packages("cluster")
#install.packages("reshape2")
library(cluster)


# importing the data from the csv
data <- read.csv("data/fastfood.csv", header = TRUE)

# removing column with too much null value and with too much similar value
columns_to_delete <- c("cal_fat", "sat_fat", "trans_fat", "vit_a", "vit_c", "salad")
data <- data[, -(which(names(data) %in% columns_to_delete))]
data

# standardize the data because they come in different unit of measure
numeric_cols <- data[, -c(1, 2)] # select all the column but the first one ("restaurant" is not numerical)
data_stand_numeric <- scale(numeric_cols) # standardization
df_standard <- cbind(data[, c(1,2), drop = FALSE], data_stand_numeric) #combining the original df first column with the new numerical stardardized


item_restaurant_dendogram <- function(restaurant) {
# filter by restaurant
  data_for_clustering <- df_standard[df_standard$restaurant == restaurant,]


  data_for_clustering <- data_for_clustering[!duplicated(data_for_clustering), ] # delete duplicates
# row_names <- paste(data_for_clustering$restaurant, data_for_clustering$item, sep = "--") # set row names
row_names <- data_for_clustering$item # set row names
rownames(data_for_clustering) <- row_names


# using Gower distance because I have some null value
data_for_clustering_num <- data_for_clustering[,-c(1,2)] #excluding the first two columns because they're not significant in our analysys
gower_dist <- daisy(data_for_clustering_num, metric = "gower")


groups<-hclust(gower_dist) #clustering
png(paste("dendogram_", restaurant, ".png"), width = 1920, height = 1080)
plot(groups, main = paste(restaurant, "'s Items Dendogram")) # plot
  
dev.off()

#TRY TO MAKE THE PLOT FANCIER AND TO CLUSTER THE RESTAURANT
}

item_restaurant_dendogram("Mcdonalds")
item_restaurant_dendogram("Burger King")









restaurant_dendrogram <- function(aggre) {
  # aggregate the nutritional information by restaurant
  agg_data <- aggregate(df_standard[, -c(1, 2)], by = list(df_standard$restaurant), FUN = aggre)
  
  # extract numerical columns for clustering
  data_for_clustering <- agg_data[, -1]
  
  # calculate the similarity matrix
  gower_dist <- daisy(data_for_clustering, metric = "gower")
  
  # perform hierarchical clustering
  hclust_result <- hclust(gower_dist, method = "complete")
  
  # plot the dendrogram with restaurant labels
  png(paste("restaurant_dendogram_", as.character(substitute(aggre)), ".png"), height = 480, width = 600)
  plot(hclust_result, main = paste("Dendrogram of Restaurants based on Nutritional Similarity (", as.character(substitute(aggre)), ")"), xlab = "Restaurants", sub = "", ylab = "Distance",
       labels = agg_data$Group.1)  # Use restaurant names as labels
  dev.off()
}

restaurant_dendrogram(mean)