library(shiny)
library(leaflet)
library(leaflet)
library(ggplot2)
library(maps)
library(mapproj)
library(dplyr)
library(shiny)

files <- list.files(path="./cleaned_well_data/", pattern="*.csv", full.names=TRUE, recursive=FALSE)


list_of_data <- list()

for (file in files){
  file_name <- substr(file, 21, (nchar(file) - 4))
  print(file_name)
  list_of_data[[file_name]] <- read.csv(file)
}

states <- map_data("state")
florida <- filter(states, region == "florida")

ggplot() + 
  geom_map( data=florida, 
            map=florida, 
            aes( x=long, y=lat, map_id=region ), 
            fill="white", colour="black" ) + 
  coord_map( "albers", lat0=29.5, lat1=49.5 )


well_location <- data.frame(
  wells = c("G-3549",
            "G-860",
            "G-580A",
            "F-319",
            "F-179",
            "F-45",
            "G-852",
            "G-561_T",
            "G-1220_T",
            "G-2147_T",
            "G-1260_T",
            "G-2866_T",
            "PB-1680_T"
  ),
  longitude = c(
    -80.34908333,
    -80.32286111,
    -80.30256389,
    -80.28833333,
    -80.2464,
    -80.20415556,
    -80.17555556,
    -80.13888889,
    -80.14625,
    -80.10125,
    -80.11313889,
    -80.11533333,
    -80.09480556
  ),
  latitude = c(
    25.49294444,
    25.62188889,
    25.66709444,
    25.70472222,
    25.74564722,
    25.82883889,
    25.91027778,
    26.09583333,
    26.13106111,
    26.25044444,
    26.27805556,
    26.31780556,
    26.36677778
  )
  
)


r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  titlePanel("my_title"),
  fluidRow(
    column(5,
           leafletOutput("mymap"),
           p(),
           actionButton("recalc", "New points")
           
    ),
    column(5,
      plotOutput("my_plot")
    )
  )
)

server <- function(input, output, session) {
  
  output$mymap <- renderLeaflet({
    
    leafIcons <- makeIcon(
      iconUrl = "well_pic.png",
      iconWidth = 35, iconHeight = 25  )
    
    
    
    leaflet() %>%
      addTiles() %>%
      addMarkers(lng = well_location$longitude, 
                 lat = well_location$latitude, 
                 popup = well_location$wells,
                 layerId = well_location$wells,
                 icon = leafIcons)
  })
  
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)
  
  
  ts_plot <- reactive({
    
    print(input$mymap_marker_click$id)
    
    if(is.null(input$mymap_marker_click$id)){
      plot(0)
    } else {
      well_ts <- ts(list_of_data[[input$mymap_marker_click$id]][, "well_ft"], frequency = 12)
      plot(well_ts)  
    }
    
    
  })

  output$my_plot <- renderPlot(
      ts_plot()
  )
  
}

shinyApp(ui, server)