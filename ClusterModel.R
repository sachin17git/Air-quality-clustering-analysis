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

######################### MapBox by filtering state/county for dataframes ###########################

filterPlot_maps <- function (state, df, category_aqi) {

  if (state!="All States") {
    states_indexes <- which(df$'State Name'==state)
    data <- df[states_indexes, ]
    new_cat_aqi <- category_aqi[states_indexes]
  } else {
    data <- copy(df)
    new_cat_aqi <- copy(category_aqi)
  }

  fig <- data %>% plot_mapbox(lat = ~Latitude, lon = ~Longitude,
                            split = as.factor(new_cat_aqi), size=2,
                            mode = 'scattermapbox', hoverinfo='name')

  fig <- fig %>% layout(title = sprintf("USA, 2021 %s - %s", state, unique(data$'Parameter Name')),
                       font = list(color='white'),
                       plot_bgcolor = '#191A1A', paper_bgcolor = '#191A1A',
                       mapbox = list(style = 'dark'),
                       legend = list(orientation = 'h',
                                     font = list(size = 8)),
                       margin = list(l = 25, r = 25,
                                     b = 25, t = 25,
                                     pad = 2, zoom = 20))

  fig <- fig %>% add_trace(marker = list(
                           size = 10))

  fig <- fig %>% config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"))

  return (fig)

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

  new_list <- list(fig, cat_aqi)
  return (new_list)

}
########################## PLotly 3d plot for k-means ###########################
df_co <- fread('./pollutant dataframes/Carbon_monoxide.csv')
fig_aqi_co <- kmeans_compute(df_co, consider_cols, 2, "CO")
fig_aqi_co[[1]] #This is the figure object. Unpacking two variables present in the list. fig_aqi[2] is another variable.

df_no <- fread('./pollutant dataframes/Nitrogen_dioxide_(NO2).csv')
fig_aqi_no <- kmeans_compute(df_no, consider_cols, 2, "NO2")
fig_aqi_no[[1]]

df_ozone <- fread('./pollutant dataframes/Ozone.csv')
fig_aqi_ozone <- kmeans_compute(df_ozone, consider_cols, 4, "Ozone")
fig_aqi_ozone[[1]]

df_so <- fread('./pollutant dataframes/Sulfur_dioxide.csv')
fig_aqi_so <- kmeans_compute(df_so, consider_cols, 3, "SO2")
fig_aqi_so[[1]]

########################## Visualization Maps ################################

mapbox_token <- "pk.eyJ1IjoiYmFsYXN1YjQiLCJhIjoiY2wyZ3EwbGdrMDNwdDNjbnFmbGtjaHozOCJ9.v3Hi5eF-UYkbpzmqz6_Y6g"
Sys.setenv("MAPBOX_TOKEN" = mapbox_token) # for Orca

filterPlot_maps(state = "California", df_co, fig_aqi_co[[2]]) # Maps for CO

filterPlot_maps(state = "California", df_no, fig_aqi_no[[2]]) # Maps for No2

filterPlot_maps(state = "California", df_ozone, fig_aqi_ozone[[2]]) # Maps for Ozone

filterPlot_maps(state = "Texas", df_so, fig_aqi_so[[2]]) # Maps for So2


############################ Maps for all the states combined #############################

filterPlot_maps(state = "All States", df_so, fig_aqi_so[[2]]) # Maps for all states.

filterPlot_maps(state = "All States", df_co, fig_aqi_co[[2]]) # Maps for all states.

filterPlot_maps(state = "All States", df_no, fig_aqi_no[[2]]) # Maps for all states.

filterPlot_maps(state = "All States", df_ozone, fig_aqi_ozone[[2]]) # Maps for all states.

########################### Visualizations #######################################
