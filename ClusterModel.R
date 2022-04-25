library(data.table)
library(dplyr)
library(cluster)
library(fastDummies)
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
library(tseries)

consider_cols <- c('Latitude','Longitude','AQI')

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

####################### K-means clustering ###########################
kmeans_compute <- function (df, cols, nclusters) {

  data <- subset(df, select = cols)
  set.seed(100)
  clusters <- kmeans(x = data$AQI,
                        centers = nclusters,
                        iter.max = 500,
                        nstart = 20)

  fig <- plot_ly(data = data,
               x = ~Latitude,
               y = ~Longitude,
               z = ~AQI,
               color = as.factor(clusters$cluster),
               colors = c("orange", "blue", "black","#c10dd1"))

  return (fig)

}
########################## PLotly 3d plot for k-means ###########################
df <- fread('./pollutant dataframes/Carbon_monoxide.csv')
kmeans_compute(df, consider_cols, 2)

df <- fread('./pollutant dataframes/Nitrogen_dioxide_(NO2).csv')
kmeans_compute(df, consider_cols, 2)

df <- fread('./pollutant dataframes/Ozone.csv')
kmeans_compute(df, consider_cols, 4)

df <- fread('./pollutant dataframes/Sulfur_dioxide.csv')
kmeans_compute(df, consider_cols, 3)

