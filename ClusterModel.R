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

df <- fread('./pollutant dataframes/Carbon_monoxide.csv')

consider_cols <- c('Latitude','Longitude','AQI', 'month','day')
data <- subset(df, select = consider_cols)
data$Latitude <- scale(data$Latitude, center = TRUE, scale = TRUE)
data$Longitude <- scale(data$Longitude, center = TRUE, scale = TRUE)
data$AQI <- scale(data$AQI, center = TRUE, scale = TRUE)

# Elbow method to find best number of clusters.
set.seed(100)
wcss <- vector()
for (i in 1:10) {wcss[i] <- sum(kmeans(x = data, centers = i)$withinss)}

plot(x = 1:10,
     y = wcss,
     type = 'b',
     main = "Elbow method for finding optimal clusters",
     xlab = "Number of clusters",
     ylab = "WCSS",
     color = 'blue')

