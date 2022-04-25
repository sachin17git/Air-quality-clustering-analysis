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

df <- fread('./pollutant dataframes/Sulfur_dioxide.csv')

#consider_cols <- c('Latitude','Longitude','AQI', 'month','day')
consider_cols <- c('AQI')
data <- subset(df, select = consider_cols)

#data$Latitude <- scale(data$Latitude, center = TRUE, scale = TRUE)
#data$Longitude <- scale(data$Longitude, center = TRUE, scale = TRUE)
#data$AQI <- scale(data$AQI, center = TRUE, scale = TRUE)
#data$month <- scale(data$month, center = TRUE, scale = TRUE)
#data$day <- scale(data$day, center = TRUE, scale = TRUE)

################## Elbow method to find best number of clusters.################
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

####################### K-means clustering ###########################
set.seed(100)
clusters <- kmeans(x = data,
                      centers = 3,
                      iter.max = 500,
                      nstart = 20)

########################## PLotly 3d plot ###########################
fig <- plot_ly(data = df,
               x = ~Latitude,
               y = ~Longitude,
               z = ~AQI,
               color = clusters$cluster)
               #colors = c('#BF382A', '#0C4B8E'))
fig

