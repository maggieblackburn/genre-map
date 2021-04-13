library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(readxl)

coordinates <- read_excel("genre.xlsx") 


df <- data.frame(lng = c(coordinates$lon),
                 lat = c(coordinates$lat),
                 year = c(coordinates$year), 
                 colour = c(coordinates$colour), 
                 play = c(coordinates$play),
                 genre = c(coordinates$genre),
                 town = c(coordinates$town),
                 theatre = c(coordinates$location),
                 stringsAsFactors = FALSE)



#Shiny UI
ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("map", width = "100%", height = "100%"),
    absolutePanel(top = 10, right = 10,
                  style="z-index:500;", # legend over my map (map z = 400)
                  tags$h3("Popularity of Theatre Genres"), 
                  sliderInput("yearrange", "Select Date Range",
                              min(df$year),
                              max(df$year),
                              value = range(df$year),
                              step = 1,
                              sep = ""
                  )
    )
)

#Shiny Server
server <- function(input, output, session) {
    
    # reactive filtering data from UI
    
    reactive_data_chrono <- reactive({
        df %>%
            filter(year >= input$yearrange[1] & year <= input$yearrange[2])
    })
    
    
    # static backround map
    output$map <- renderLeaflet({
        leaflet(df) %>%
            addProviderTiles(providers$Stamen.Terrain) %>%
            addLegend("bottomright", colors = c("deepskyblue", "skyblue", "lightsteelblue", 
                                                "lightpink", "red", "darkorange", "darkgreen", "magenta", "violet", "mistyrose", "darkgrey"), labels = c("Comedy", "Farce", "Comedietta", 
                                                                                                                                                       "Comic Drama", "Drama", "Melodrama", "Adaptation", "Burlesque", "Other", "Musical Other", "Unknown"), title = "Genre")%>%
            fitBounds(~min(lng), ~min(lat), ~max(lng), ~max(lat))
    })  
    
    # reactive circles map
    observe({
        leafletProxy("map", data = reactive_data_chrono()) %>%
            clearMarkerClusters() %>% 
            clearPopups() %>% 
            clearMarkers()%>%
            addCircleMarkers(radius = 12, stroke = TRUE, color = "black", weight = 1, opacity = 0.8, fill = TRUE, 
                             fillColor = ~colour, fillOpacity = 1,
                             popup = ~paste("<b>Play: </b>", play, 
                                            "<br>", 
                                            "<b>Genre: </b>", genre,
                                            "<br>",
                                            "<b>Theatre: </b>", theatre,
                                            "<br>",
                                            "<b>Year: </b>", year, 
                                            "<br>",
                                            "<b>Location: </b>", town
                             ), 
                             clusterOptions = markerClusterOptions())
    })
}

shinyApp(ui, server)
