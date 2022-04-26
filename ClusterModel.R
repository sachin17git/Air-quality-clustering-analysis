library(data.table)
library(dplyr)
library(cluster)
library(caret)
library(devtools)
library(ggbiplot)
library(cluster)
#library(lubridate)
library(ggplot2)
library(plotly)
library(naniar)
library(tidyr)
library(tidyverse)
library(hrbrthemes)
library(viridis)

consider_cols <- c('Latitude','Longitude','AQI', 'Parameter Name')

#data <- subset(df, select = consider_cols)
#data$Latitude <- scale(data$Latitude, center = TRUE, scale = TRUE)
#data$Longitude <- scale(data$Longitude, center = TRUE, scale = TRUE)
#data$AQI <- scale(data$AQI, center = TRUE, scale = TRUE)
#data$month <- scale(data$month, center = TRUE, scale = TRUE)
#data$day <- scale(data$day, center = TRUE, scale = TRUE)

################## Elbow method to find best number of clusters.################

df <- fread('./pollutant dataframes/Sulfur_dioxide.csv')
set.seed(100)
wcss <- vector()
df_elbow <- subset(df, select = consider_cols)
for (i in 1:10) {wcss[i] <- sum(kmeans(x = df_elbow$AQI, centers = i)$withinss)}

plot(x = 1:10,
     y = wcss,
     type = 'b',
     main = "Elbow method for finding optimal clusters",
     xlab = "Number of clusters",
     ylab = "WCSS",
     color = 'blue')

######################## Find category based on AQI ############################

get_cat_aqi <- function (clust, aqi, nclust, pol) {

  cat <- c("Very good","Good","Moderate","Unhealthy/Hazardous")

  if (pol=="CO") {
    codes <- c(2, 4)
    for (i in 1:nclust) {
      temp <- which(clust==i)
      aqi[temp] <- cat[codes[i]]
    }
  } else if (pol=="NO2") {
    codes <- c(2, 3)
    for (i in 1:nclust) {
      temp <- which(clust==i)
      aqi[temp] <- cat[codes[i]]
    }
  } else if (pol=="Ozone") {
    codes <- c(4, 3, 1, 2)
    for (i in 1:nclust) {
      temp <- which(clust==i)
      aqi[temp] <- cat[codes[i]]
    }
  } else if (pol=="SO2") {
    codes <- c(2, 4, 3)
    for (i in 1:nclust) {
      temp <- which(clust==i)
      aqi[temp] <- cat[codes[i]]
    }
  }

  return (aqi)
}

####################### K-means clustering ###########################

kmeans_compute <- function (df, cols, nclusters, pollutant) {

  data <- subset(df, select = cols)
  set.seed(100)
  clusters <- kmeans(x = data$AQI,
                        centers = nclusters,
                        iter.max = 500,
                        nstart = 20)

  cat_aqi <- get_cat_aqi(clusters$cluster,
                         data$AQI,
                         nclusters,
                         pollutant)

  fig <- plot_ly(data = data,
               x = ~Latitude,
               y = ~Longitude,
               z = ~AQI,
               color = as.factor(cat_aqi),
               colors = c("orange", "blue", "black","#c10dd1")) %>%
    layout(title = sprintf("USA, 2021 AQI Clustering - %s",
                           unique(data$'Parameter Name')))

  fig <- fig %>%
    add_trace(
      marker = list(
        opacity = 0.5,
        size = 5
      )
    )

  return (fig)

}
########################## PLotly 3d plot for k-means ###########################
df <- fread('./pollutant dataframes/Carbon_monoxide.csv')
kmeans_compute(df, consider_cols, 2, "CO")

df <- fread('./pollutant dataframes/Nitrogen_dioxide_(NO2).csv')
kmeans_compute(df, consider_cols, 2, "NO2")

df <- fread('./pollutant dataframes/Ozone.csv')
kmeans_compute(df, consider_cols, 4, "Ozone")

df <- fread('./pollutant dataframes/Sulfur_dioxide.csv')
kmeans_compute(df, consider_cols, 3, "SO2")
