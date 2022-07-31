#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(dplyr)
library(readr)
library(leaflet)
library(htmlwidgets)
library(data.table)
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
library(dplyr)
library(leaflet)
library(timetk)
library(dplyr)
library(ggplot2)
library(datasets)
library(zoo)
library(gganimate)
library(repr)
library(extrafont)
library(hrbrthemes)
library(ggthemes)
library(plotly)
library(gifski)

#Load data

df_co <- fread('Carbon_monoxide.csv')
df_ozone <- fread('Ozone.csv')
df_so2 <- fread('Sulfur_dioxide.csv')
df_no2 <- fread('Nitrogen_dioxide_(NO2).csv')
consider_cols <- c('Latitude','Longitude','AQI', 'Parameter Name')



######################## Time Series for CO ############################

Carbon <- fread('co_new_state.csv')
No2 <- fread('no2_new_state.csv')
So2 <- fread('so2_new_state.csv')
Ozone <- fread('ozone_new_state.csv')

list_state = as.list(Carbon)
list_state = unique(list_state$'State Name')
list_state

time_series_co <- function(filename,state){
  
  data_co = read.csv(filename)
  data_co = data_co[ , -which(names(data_co) %in% c("X"))]
  data_co = data_co[data_co$State.Name == state,]
  data_co$Date.Local = as.Date(data_co$Date.Local, format = "%Y-%m-%d")
  
  p <- data_co %>%
    ggplot(aes(x=Date.Local, y=AQI)) +
    geom_line( color="red") + 
    geom_point() +
    scale_x_date(date_breaks = "30 day")+
    theme_light()+
    labs(title = 'AQI for the year 2021',x='Date',y='AQI')+
    theme(plot.title = element_text(size = 25,face="bold"),axis.text=element_text(size=14,face="bold"),axis.title=element_text(size=14,face="bold"))+
    transition_reveal(Date.Local)

  animation <- animate(p,nframes = 110, fps = 10, height = 700, width = 1000, renderer = gifski_renderer())
  anim_save("co.gif", animation)
  
}

############################### Time Series for CO #################################

#NO2

time_series_no2 <- function(filename,state){
  
  data_no2 = read.csv(filename)
  data_no2 = data_no2[ , -which(names(data_no2) %in% c("X"))]
  data_no2 = data_no2[data_no2$State.Name == state,]
  data_no2$Date.Local = as.Date(data_no2$Date.Local, format = "%Y-%m-%d")
  
  p <- data_no2 %>%
    ggplot(aes(x=Date.Local, y=AQI)) +
    geom_line( color="blue") + 
    geom_point() +
    scale_x_date(date_breaks = "30 day")+
    theme_light()+
    labs(title = 'AQI for the year 2021',x='Date',y='AQI')+
    theme(plot.title = element_text(size = 25,face="bold"),axis.text=element_text(size=14,face="bold"),axis.title=element_text(size=14,face="bold"))+
    transition_reveal(Date.Local)
  
  #animation = animate(p,nframes=181,fps = 10, height = 700, width = 1200)
  animation <- animate(p,nframes=110, fps = 10, height = 700, width = 1000, renderer = gifski_renderer())
  anim_save("no2.gif", animation)
  
}

####################################################### SO2 #########################################################################################

#SO2

time_series_so2 <- function(filename,state){
  
  data_so2 = read.csv(filename)
  data_so2 = data_so2[ , -which(names(data_so2) %in% c("X"))]
  data_so2 = data_so2[data_so2$State.Name == state,]
  data_so2$Date.Local = as.Date(data_so2$Date.Local, format = "%Y-%m-%d")
  
  p <- data_so2 %>%
    ggplot(aes(x=Date.Local, y=AQI)) +
    geom_line( color="green") + 
    geom_point() +
    scale_x_date(date_breaks = "30 day")+
    theme_light()+
    labs(title = 'AQI for the year 2021',x='Date',y='AQI')+
    theme(plot.title = element_text(size = 25,face="bold"),axis.text=element_text(size=14,face="bold"),axis.title=element_text(size=14,face="bold"))+
    transition_reveal(Date.Local)
  
  
  animation = animate(p,nframes=110, fps = 10, height = 700, width = 1000, renderer = gifski_renderer())
  anim_save("so2.gif", animation)
  
}


##################################################### Ozone ###################################################################################

#Ozone

time_series_ozone <- function(filename,state){
  
  data_ozone = read.csv(filename)
  data_ozone = data_ozone[ , -which(names(data_ozone) %in% c("X"))]
  data_ozone = data_ozone[data_ozone$State.Name == state,]
  data_ozone$Date.Local = as.Date(data_ozone$Date.Local, format = "%Y-%m-%d")
  
  p <- data_ozone %>%
    ggplot(aes(x=Date.Local, y=AQI)) +
    geom_line( color="orange") + 
    geom_point() +
    scale_x_date(date_breaks = "30 day")+
    theme_light()+
    labs(title = 'AQI for the year 2021',x='Date',y='AQI')+
    theme(plot.title = element_text(size = 25,face="bold"),axis.text=element_text(size=14,face="bold"),axis.title=element_text(size=14,face="bold"))+
    transition_reveal(Date.Local)
  
  
  animation = animate(p,nframes=110, fps = 10, height = 700, width = 1000, renderer = gifski_renderer())
  anim_save("ozone.gif", animation)
  
}

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
                           unique(data$'Parameter Name')),plot_bgcolor='rgb(250,247,237)',paper_bgcolor='rgb(250,247,237)')
  
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

fig_aqi_co <- kmeans_compute(df_co, consider_cols, 2, "CO")
fig_aqi_no <- kmeans_compute(df_no2, consider_cols, 2, "NO2")
fig_aqi_ozone <- kmeans_compute(df_ozone, consider_cols, 4, "Ozone")
fig_aqi_so <- kmeans_compute(df_so2, consider_cols, 3, "SO2")


################################## Function for Pie Chart ############################

pie_chart <- function(categories, df) {
  
  new_d <- data.frame(category=categories,
                      AQI=df$AQI)
  
  new_d <- new_d %>% count(~category)
  
  fig <- plot_ly(data = new_d,
                 labels = ~category,
                 values = ~freq,
                 type = 'pie')
  
  fig <- fig %>% layout(title = sprintf('USA, 2021 Air Quality - %s',unique(df$`Parameter Name`)),
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        plot_bgcolor='rgba(250,247,237,1)',paper_bgcolor='rgba(250,247,237,1)')
  
  return (fig)
}

############################## Function for Funnel chart  ###########################

funnel_chart <- function(categories, df) {
  
  new_d <- data.frame(category=categories,
                      AQI=df$AQI)
  
  new_d <- new_d %>% count(~category)
  
  fig <- plot_ly()
  
  fig <- fig %>% add_trace(type = "funnel",
                           y = new_d$category,
                           x = new_d$freq)
  
  fig <- fig %>% layout(yaxis = list(categoryarray = new_d$category),
                        title = sprintf('USA, 2021 AQI Funnel Chart - %s',unique(df$`Parameter Name`)),plot_bgcolor='rgb(250,247,237)',paper_bgcolor='rgb(250,247,237)')
  
  return (fig)
}

################################################################ BOX PLOTS #######################################################################

box_plots_gas <- function(filename){
  
  data = read.csv(filename)
  data = data[ , -which(names(data) %in% c("X"))]
  abc = aggregate(AQI ~ State.Name, data, mean)
  abc = abc[order(abc$AQI),]
  abc_max = tail(abc,n=5)
  list_max = as.list(abc_max)
  list_state = list_max$State.Name
  
  data_5 = data %>%
    filter(State.Name %in% list_state)
  
  fig = plot_ly(data_5,y=~AQI,x=~State.Name,color=~State.Name,type="box")
  fig <- fig %>% layout(title = "Box Plot of States with Highest AQI values",plot_bgcolor='rgb(250,247,237)',paper_bgcolor='rgb(250,247,237)')
  fig
  
}

############################## Define UI for application  ########################### 

ui <- fluidPage(setBackgroundImage(src = 'https://assets.new.siemens.com/siemens/assets/api/uuid:5e8832d7-b28d-4cf0-9046-bfcf05ac3c28/width:2732/crop:0:0,04464:0,999:0,83705/quality:high/city-air-quality-management--cyam--software-from-siemens-uses-ar.jpg',shinydashboard = FALSE),
                theme = shinytheme("superhero"),
    # Application title
    titlePanel(div("Clustering Model of Air Quality Index of the United States", style="color : #253243; font-family : sans-serif; font-weight : bold; text-align : center")),
    # Sidebar with a slider input for number of bins 
   # sidebarLayout(
      # sidebarPanel(
       #   selectInput(inputId = "type", label = strong("Air Quality Index"),
     #                 choices = unique(trend_data_CO$State.Name),
     #                 selected = "Alabama"),
     #     dateRangeInput("date", strong("Date range"), start = min(trend_data_CO$Date.Local), 
     #                    end = max(trend_data_CO$Date.Local)),
     #   ),

        # Main Panel
        mainPanel(
          br(),
          br(),
          navlistPanel(well = TRUE,
            tabPanel("About",
                     tags$h3(div("Objective",style="color : #253243; font-weight : bold")),
                     p("The objective of the project is to help the general public have more accessible information and understanding about the Air Quality with the help of Machine learning techniques like K-Means Clustering using the RShiny app. The presence of pollutants in the air can be determined by the presence of harmful gases like Nitrogen Dioxide, Ozone, Carbon Monoxide and Sulfur Dioxide which have impacts ranging from shortness of breath to reduced lung function. 
",style="color : #253243; font-weight : bold"),
                     br(),
                     tags$h3(div("About the Project",style="color : #253243; font-weight : bold")),
                     p("This project takes the individual Air Quality Index (AQI) of these gases into account and performs a K-Means clustering analysis that clusters the AQI into the group of Good, Very-Good, Harmful and presents it in the visualizable manner on the interactive RShiny app. Along with the Clustering Analysis, the results also show a time series plot of the AQI of different states with respect to these harmful gases. The main goal of this project is to provide the general public with an interactive and easy to comprehend platform that helps them have more awareness about the presence of pollutants in their surroundings which was achieved by performing a clustering analysis that segregates the presence of all the harmful gases into different clusters. The Rshiny app also provides the time series analysis of the Air Quality Index of these particular gases for all 50 states in the United States. 
",style="color : #253243; font-weight : bold"),
                     br(),
                     tags$h3(div("References",style="color : #253243; font-weight : bold")),
                     p("The data for this project has been taken from United States Environmental Protection Agency.",style="color : #253243; font-weight : bold"),
                     tags$a(href="https://aqs.epa.gov/aqsweb/airdata/download_files.html#Annual","Link to the Dataset", style="color : #253243; font-weight : bold"),
                     br()
            ),
            tabPanel("Carbon Monoxide",
                     tags$h3(div("Carbon Monoxide Concentration for all the States",style="color : #253243; font-weight : bold")),
                     plotlyOutput("co_plot_2",height="600px",width = "1000px"),
                     tags$h3(div("Carbon Monoxide Concentration in each State",style="color : #253243; font-weight : bold")),
                     selectInput(inputId = "conc_co", label = strong("States"),choices = list_state, selected = "Alabama"),
                     plotlyOutput("co_plot_1",height="600px",width = "1000px"),
                     tags$h3(div("K-Means Clustering",style="color : #253243; font-weight : bold")),
                     plotlyOutput("co",height="600px",width = "1000px"),
                     tags$h3(div("Pie-Graph",style="color : #253243; font-weight : bold")),
                     plotlyOutput("co_pie",height="600px",width = "1000px"),
                     tags$h3(div("Funnel-Graph",style="color : #253243; font-weight : bold")),
                     plotlyOutput("co_funnel",height="600px",width = "1000px"),
                     tags$h3(div("Box plot of States with Highest AQI values",style="color : #253243; font-weight : bold")),
                     plotlyOutput("co_boxplot",height="600px",width = "1000px"),
                     tags$h3(div("AQI for the Year 2021 for each State",style="color : #253243; font-weight : bold")),
                     selectInput(inputId = "type_co", label = strong("States"),choices = list_state, selected = "Alabama"),
                     imageOutput("co_timeseries",height="800px",width = "800px"),
                     br()),
            tabPanel("Nitrogen Dioxide",
                     tags$h3(div("Nitrogen Dioxide Concentration for all the States",style="color : #253243; font-weight : bold")),
                     plotlyOutput("no2_plot_2",height="600px",width = "1000px"),
                     tags$h3(div("Nitrogen Dioxide Concentration in each State",style="color : #253243; font-weight : bold")),
                     selectInput(inputId = "conc_no", label = strong("States"),choices = list_state, selected = "Alabama"),
                     plotlyOutput("no2_plot_1",height="600px",width = "1000px"),
                     tags$h3(div("K-Means Clustering",style="color : #253243; font-weight : bold")),
                     plotlyOutput("no2", height="600px",width = "1000px"),
                     tags$h3(div("Pie-Graph",style="color : #253243; font-weight : bold")),
                     plotlyOutput("no2_pie",height="600px",width = "1000px"),
                     tags$h3(div("Funnel-Graph",style="color : #253243; font-weight : bold")),
                     plotlyOutput("no2_funnel",height="600px",width = "1000px"),
                     tags$h3(div("Box plot of States with Highest AQI values",style="color : #253243; font-weight : bold")),
                     plotlyOutput("no2_boxplot",height="600px",width = "1000px"),
                     tags$h3(div("AQI for the Year 2021 for each State",style="color : #253243; font-weight : bold")),
                     selectInput(inputId = "type_no2", label = strong("States"),choices = list_state, selected = "Alabama"),
                     imageOutput("no2_timeseries",height="800px",width = "800px"),
                     br()),
            tabPanel("Sulfer Dioxide",
                     tags$h3(div("Sulfer Dioxide Concentration for all the States",style="color : #253243; font-weight : bold")),
                     plotlyOutput("so2_plot_2",height="600px",width = "1000px"),
                     tags$h3(div("Sulfur Dioxide Concentration in each State",style="color : #253243; font-weight : bold")),
                     selectInput(inputId = "conc_so", label = strong("States"),choices = list_state, selected = "Alabama"),
                     plotlyOutput("so2_plot_1",height="600px",width = "1000px"),
                     tags$h3(div("K-Means Clustering",style="color : #253243; font-weight : bold")),
                     plotlyOutput("so2", height="600px",width = "1000px"),
                     tags$h3(div("Pie-Graph",style="color : #253243; font-weight : bold")),
                     plotlyOutput("so2_pie",height="600px",width = "1000px"),
                     tags$h3(div("Funnel-Graph",style="color : #253243; font-weight : bold")),
                     plotlyOutput("so2_funnel",height="600px",width = "1000px"),
                     tags$h3(div("Box plot of States with Highest AQI values",style="color : #253243; font-weight : bold")),
                     plotlyOutput("so2_boxplot",height="600px",width = "1000px"),
                    tags$h3(div("AQI for the Year 2021 for each State",style="color : #253243; font-weight : bold")),
                    selectInput(inputId = "type_so2", label = strong("States"),choices = list_state, selected = "Alabama"),
                    imageOutput("so2_timeseries",height="800px",width = "800px"),
                     br()),
            tabPanel("Ozone",
                     tags$h3(div("Ozone Concentration for all the States",style="color : #253243; font-weight : bold")),
                     plotlyOutput("ozone_plot_2",height="600px",width = "1000px"),
                     tags$h3(div("Ozone Concentration in each State",style="color : #253243; font-weight : bold")),
                     selectInput(inputId = "conc_ozone", label = strong("States"),choices = list_state, selected = "Alabama"),
                     plotlyOutput("ozone_plot_1",height="600px",width = "1000px"),
                     tags$h3(div("K-Means Clustering",style="color : #253243; font-weight : bold")),
                     plotlyOutput("ozone", height="600px",width = "1000px"),
                     tags$h3(div("Pie-Graph",style="color : #253243; font-weight : bold")),
                     plotlyOutput("ozone_pie",height="600px",width = "1000px"),
                     tags$h3(div("Funnel-Graph",style="color : #253243; font-weight : bold")),
                     plotlyOutput("ozone_funnel",height="600px",width = "1000px"),
                     tags$h3(div("Box plot of States with Highest AQI values",style="color : #253243; font-weight : bold")),
                     plotlyOutput("ozone_boxplot",height="600px",width = "1000px"),
                     tags$h3(div("AQI for the Year 2021 for each State",style="color : #253243; font-weight : bold")),
                     selectInput(inputId = "type_ozone", label = strong("Air Quality Index"),choices = list_state, selected = "Alabama"),
                     imageOutput("ozone_timeseries",height="800px",width = "800px"),
                     br()),
          )
        )
)

############################## Define server logic  ###########################  

server <- function(input, output) {
  output$co <- renderPlotly({
    fig_aqi_co <- kmeans_compute(df_co, consider_cols, 2, "CO")
    fig_aqi_co[[1]]
  })
  
  output$no2 <- renderPlotly({
    fig_aqi_no <- kmeans_compute(df_no2, consider_cols, 2, "NO2")
    fig_aqi_no[[1]]
    
  })
  
  output$so2 <- renderPlotly({
    fig_aqi_so <- kmeans_compute(df_so2, consider_cols, 3, "SO2")
    fig_aqi_so[[1]]
  })
  
  output$ozone <- renderPlotly({
    fig_aqi_ozone <- kmeans_compute(df_ozone, consider_cols, 4, "Ozone")
    fig_aqi_ozone[[1]]
  })
  
  output$co_pie <- renderPlotly({
    pie_chart(fig_aqi_co[[2]], df_co)
  })
  
  output$no2_pie <- renderPlotly({
    pie_chart(fig_aqi_no[[2]], df_no2)
  })
  
  output$so2_pie <- renderPlotly({
    pie_chart(fig_aqi_so[[2]], df_so2)
  })
  
  output$ozone_pie <- renderPlotly({
    pie_chart(fig_aqi_ozone[[2]], df_ozone)
  })
  
  output$co_funnel <- renderPlotly({
    funnel_chart(fig_aqi_co[[2]], df_co)
  })
  
  output$no2_funnel <- renderPlotly({
    funnel_chart(fig_aqi_no[[2]], df_no2)
  })
  
  output$so2_funnel <- renderPlotly({
    funnel_chart(fig_aqi_so[[2]], df_so2)
  })
  
  output$ozone_funnel <- renderPlotly({
    funnel_chart(fig_aqi_ozone[[2]], df_ozone)
  })
  
  output$co_plot_2 <- renderPlotly({
    mapbox_token <- "pk.eyJ1IjoiYmFsYXN1YjQiLCJhIjoiY2wyZ3EwbGdrMDNwdDNjbnFmbGtjaHozOCJ9.v3Hi5eF-UYkbpzmqz6_Y6g"
    Sys.setenv("MAPBOX_TOKEN" = mapbox_token)
    filterPlot_maps(state = "All States", df_co, fig_aqi_co[[2]])
  })
  
  output$no2_plot_2 <- renderPlotly({
    mapbox_token <- "pk.eyJ1IjoiYmFsYXN1YjQiLCJhIjoiY2wyZ3EwbGdrMDNwdDNjbnFmbGtjaHozOCJ9.v3Hi5eF-UYkbpzmqz6_Y6g"
    Sys.setenv("MAPBOX_TOKEN" = mapbox_token)
    filterPlot_maps(state = "All States", df_no2, fig_aqi_no[[2]])
  })
  
  output$so2_plot_2 <- renderPlotly({
    mapbox_token <- "pk.eyJ1IjoiYmFsYXN1YjQiLCJhIjoiY2wyZ3EwbGdrMDNwdDNjbnFmbGtjaHozOCJ9.v3Hi5eF-UYkbpzmqz6_Y6g"
    Sys.setenv("MAPBOX_TOKEN" = mapbox_token)
    filterPlot_maps(state = "All States", df_so2, fig_aqi_so[[2]])
  })
  
  output$ozone_plot_2 <- renderPlotly({
    mapbox_token <- "pk.eyJ1IjoiYmFsYXN1YjQiLCJhIjoiY2wyZ3EwbGdrMDNwdDNjbnFmbGtjaHozOCJ9.v3Hi5eF-UYkbpzmqz6_Y6g"
    Sys.setenv("MAPBOX_TOKEN" = mapbox_token)
    filterPlot_maps(state = "All States", df_ozone, fig_aqi_ozone[[2]])
  })
  
  output$co_plot_1 <- renderPlotly({
    mapbox_token <- "pk.eyJ1IjoiYmFsYXN1YjQiLCJhIjoiY2wyZ3EwbGdrMDNwdDNjbnFmbGtjaHozOCJ9.v3Hi5eF-UYkbpzmqz6_Y6g"
    Sys.setenv("MAPBOX_TOKEN" = mapbox_token)
    filterPlot_maps(state = input$conc_co, df_co, fig_aqi_co[[2]])
  })
  
  output$no2_plot_1 <- renderPlotly({
    mapbox_token <- "pk.eyJ1IjoiYmFsYXN1YjQiLCJhIjoiY2wyZ3EwbGdrMDNwdDNjbnFmbGtjaHozOCJ9.v3Hi5eF-UYkbpzmqz6_Y6g"
    Sys.setenv("MAPBOX_TOKEN" = mapbox_token)
    filterPlot_maps(state = input$conc_no, df_no2, fig_aqi_no[[2]])
  })
  
  output$so2_plot_1 <- renderPlotly({
    mapbox_token <- "pk.eyJ1IjoiYmFsYXN1YjQiLCJhIjoiY2wyZ3EwbGdrMDNwdDNjbnFmbGtjaHozOCJ9.v3Hi5eF-UYkbpzmqz6_Y6g"
    Sys.setenv("MAPBOX_TOKEN" = mapbox_token)
    filterPlot_maps(state = input$conc_so, df_so2, fig_aqi_so[[2]])
  })
  
  output$ozone_plot_1 <- renderPlotly({
    mapbox_token <- "pk.eyJ1IjoiYmFsYXN1YjQiLCJhIjoiY2wyZ3EwbGdrMDNwdDNjbnFmbGtjaHozOCJ9.v3Hi5eF-UYkbpzmqz6_Y6g"
    Sys.setenv("MAPBOX_TOKEN" = mapbox_token)
    filterPlot_maps(state = input$conc_ozone, df_ozone, fig_aqi_ozone[[2]])
  })
  
  output$co_timeseries <- renderImage({
    time_series_co('co_new_state.csv',input$type_co)
    list(src = "co.gif",
         contentType = 'image/gif'
         # width = 400,
         # height = 300,
         # alt = "This is alternate text"
    )}, deleteFile = TRUE)
  
  output$no2_timeseries <- renderImage({
    time_series_no2('no2_new_state.csv',input$type_no2)
    list(src = "no2.gif",
         contentType = 'image/gif'
         # width = 400,
         # height = 300,
         # alt = "This is alternate text"
    )}, deleteFile = TRUE)
  
  output$so2_timeseries <- renderImage({
    time_series_so2('so2_new_state.csv',input$type_so2)
    list(src = "so2.gif",
         contentType = 'image/gif'
         # width = 400,
         # height = 300,
         # alt = "This is alternate text"
    )}, deleteFile = TRUE)
  
  output$ozone_timeseries <- renderImage({
    time_series_ozone('ozone_new_state.csv',input$type_ozone)
    list(src = "ozone.gif",
         contentType = 'image/gif'
         # width = 400,
         # height = 300,
         # alt = "This is alternate text"
    )}, deleteFile = TRUE)
  
  output$co_boxplot <- renderPlotly({
    box_plots_gas('co_new_state.csv')
  })
  
  output$no2_boxplot <- renderPlotly({
    box_plots_gas('no2_new_state.csv')
  })
  
  output$so2_boxplot <- renderPlotly({
    box_plots_gas('so2_new_state.csv')
  })
  
  output$ozone_boxplot <- renderPlotly({
    box_plots_gas('ozone_new_state.csv')
  })
  
  
  # output$country <- renderLeaflet({
  #   map_interative <- coordinates %>%
  #     leaflet() %>%
  #     addProviderTiles(provider = "CartoDB.Positron") %>%
  #     addTiles() %>%
  #     addCircleMarkers(data = coordinates, lat = ~ Latitude, lng = ~Longitude, radius = 3)
  #   
  #   })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
