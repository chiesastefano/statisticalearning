#install.packages("cluster")
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
df_standard <- df_standard[df_standard$restaurant == restaurant,]


df_standard <- df_standard[!duplicated(df_standard), ] # delete duplicates
# row_names <- paste(df_standard$restaurant, df_standard$item, sep = "--") # set row names
row_names <- df_standard$item # set row names
rownames(df_standard) <- row_names


# using Gower distance because I have some null value
df_standard_num <- df_standard[,-c(1,2)] #excluding the first two columns because they're not significant in our analysys
gower_dist <- daisy(df_standard_num, metric = "gower")


groups<-hclust(gower_dist) #clustering
png(paste("dendogram_", restaurant, ".png"), width = 1920, height = 1080)
plot(groups, main = paste(restaurant, "'s Items Dendogram")) # plot
  
dev.off()

# NEXT TIME TRY TO MAKE THE PLOT FANCIER AND TO CLUSTER THE RESTAURANT
}

item_restaurant_dendogram("Mcdonalds")
item_restaurant_dendogram("Burger King")