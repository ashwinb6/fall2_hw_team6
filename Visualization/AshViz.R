library(shiny)
library(leaflet)
library(leaflet)
library(ggplot2)
library(maps)
library(mapproj)
library(dplyr)
library(shiny)
library(shinydashboard)
library(fma)
library(expsmooth)
library(lmtest)
library(zoo)
library(seasonal)
library(tseries)
library(forecast)
library(lubridate)


files <- list.files(path="./final_data/", pattern="*.csv", full.names=TRUE, recursive=FALSE)


list_of_data <- list()

for (file in files){
  print(file)
  file_name <- substr(file, 19, (nchar(file) - 4))
  print(file_name)
  list_of_data[[file_name]] <- read.csv(file, stringsAsFactors = F)
}

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

well_location <- arrange(well_location, latitude)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- dashboardPage(
  dashboardHeader(title = "Well Data Exploration Dashboard"),
  
  #-------------------------------------------------------------------------------------    
  # dashboard sidebar
  #-------------------------------------------------------------------------------------    
  
  
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Introduction", 
        tabName = "intro", 
        icon = icon("globe")
        #menuSubItem("Watersheds", tabName = "m_water", icon = icon("map"))
      ),
      menuItem(
        "Explore Individual Wells", 
        tabName = "with_map", 
        icon = icon("globe")
        #menuSubItem("Watersheds", tabName = "m_water", icon = icon("map"))
      ),
      menuItem(
        "Compare Multiple Wells", 
        tabName = "compare", 
        icon = icon("globe")
        #menuSubItem("Watersheds", tabName = "m_water", icon = icon("map"))
      ),
      menuItem(
        "Conclusion", 
        tabName = "conclude", 
        icon = icon("globe")
        #menuSubItem("Watersheds", tabName = "m_water", icon = icon("map"))
      )
    )
  ),
  
  #-------------------------------------------------------------------------------------    
  # the dashboard body
  #-------------------------------------------------------------------------------------    
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "intro",
        box(collapsible = TRUE, width = "100%", height = "100%",
            img(src = "intro_pic1.png", height = "90%", width = "90%")
            
        )
      ),
      tabItem(
        tabName = "with_map",
        # Map in Dashboard
        box(collapsible = TRUE, width = "100%", height = "100%",
            fluidPage(
              titlePanel(
                h3("Click on a well to view more \n information about it")
              ),
              fluidRow(
                column(width = 3,
                       leafletOutput("mymap", width = "100%", height = "700px")
                       #p(),
                       #actionButton("recalc", "New points")
                       
                ),
                column(width = 9,
                       sidebarLayout(
                         mainPanel(
                           plotOutput("my_plot", width = "100%", height = "550px"),
                           tableOutput("table")
                         ),
                         sidebarPanel(
                           #selectInput('x', 'X', c("some name", "other name")),
                           #selectInput('y', 'Y', c("some name", "other name")),
                           #selectInput('color', 'Color', c("some name", "other name")),
                           h4("Select the time span to view"),
                           sliderInput('sampleSize', 'Sample Size', 
                                       min=1, 
                                       max=50, 
                                       value=25, 
                                       step=5, 
                                       round=0),
                           br(),
                           checkboxInput('forecast', 'Show Forecasted Values'),
                           checkboxInput('season', 'Show Seasonally Adjusted Values'),
                           checkboxInput('trend', 'Show Trend Line Overlaid')
                         )
                       )
                )
              )
            )
            
            
            
        )
      ),
      tabItem(
        tabName = "compare",
        box(
          collapsible = TRUE, height = "100%", width = "100%"
        )
      )
      
    )
  )
)


server <- function(input, output, session) {
  
  #-------------------------------------------------------------------------------------    
  # table
  #-------------------------------------------------------------------------------------    
  
  
  output$mymap <- renderLeaflet({
    
    leafIcons <- makeIcon(
      iconUrl = "./well_pic1.png",
      iconWidth = 60, iconHeight = 35  )
    
    
    initial_lat = well_location$latitude[7]
    initial_lng = well_location$longitude[7]
    initial_zoom = 9
    
    #-------------------------------------------------------------------------------------    
    # the map
    #-------------------------------------------------------------------------------------    
    
    
    leaflet() %>%
      setView(lat = initial_lat, lng = initial_lng, zoom = initial_zoom) %>%
      addTiles() %>%
      addMarkers(lng = well_location$longitude[-c(1,3,5,7,9,11)], 
                 lat = well_location$latitude[-c(1,3,5,7,9,11)], 
                 #popup = well_location$wells,
                 layerId = well_location$wells[-c(1,3,5,7,9,11)],
                 icon = leafIcons,
                 label = well_location$wells[-c(1,3,5,7,9,11)],
                 labelOptions = labelOptions(noHide = T, direction = 'right',
                                             offset=c(25,0),  
                                             style=list(
                                               'color'='black',
                                               'box-shadow' = '0px 0px rgba(0,0,0,0)',
                                               'font-size' = '14px',
                                               'border-color' = 'rgba(0,0,0,0)'
                                             ))) %>%
      addMarkers(lng = well_location$longitude[c(1,3,5,7,9,11)], 
                 lat = well_location$latitude[c(1,3,5,7,9,11)], 
                 #popup = well_location$wells,
                 layerId = well_location$wells[c(1,3,5,7,9,11)],
                 icon = leafIcons,
                 label = well_location$wells[c(1,3,5,7,9,11)],
                 labelOptions = labelOptions(noHide = T, direction = 'left',
                                             offset=c(25,0),  
                                             style=list(
                                               'color'='black',
                                               'box-shadow' = '0px 0px rgba(0,0,0,0)',
                                               'font-size' = '14px',
                                               'border-color' = 'rgba(0,0,0,0)'
                                             )))
  })
  
  #points <- eventReactive(input$recalc, {
  #  cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  #}, ignoreNULL = FALSE)
  
  
  ts_plot <- reactive({
    
    print(input$mymap_marker_click$id)
    
    #-------------------------------------------------------------------------------------    
    # the graph
    #-------------------------------------------------------------------------------------    
    
    if(is.null(input$mymap_marker_click$id)){
      selected_data <- list_of_data[["PB-1680_T"]]
      
      print(head(selected_data))
      
      selected_data$date_time <- as.POSIXct(selected_data$time,  '%Y-%m-%d %H:%M:%S', tz = "")
      
      ggplot(selected_data) +
        geom_line(mapping = aes(x = as.numeric(date_time), y = well_impute)) +
        ggtitle(paste0("Well Elevation (Feet) for ", "PB-1680_T"))+
        xlab("Time") + 
        ylab("Well depth(feet)") +
        theme(plot.title = element_text(hjust = 0.5,size=22),
              axis.title=element_text(size=18),
              axis.text = element_text(size=18,lineheight = 5),
              legend.position = c(0.7, 0.8),
              legend.text = element_text( size=18),
              legend.title = element_blank(),
              plot.margin=unit(c(0.5,0.5,0.8,0.8),"cm"),
              panel.background = element_rect(fill = "aliceblue",colour = "aliceblue",
                                              size = 0.5, linetype = "solid"),
              panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                              colour = "white")
        )
    } else {
      selected_data <- list_of_data[[input$mymap_marker_click$id]]
      
      selected_data$date_time <- as.POSIXct(selected_data$time,  '%Y-%m-%d %H:%M:%S', tz = "")
      
      #plot(well_ts)
      ggplot(selected_data) +
        geom_line(mapping = aes(x = as.numeric(date_time), y = well_impute)) +
        ggtitle(paste0("Well Elevation (Feet) for ", as.character(input$mymap_marker_click$id)))+
        xlab("Time") + 
        ylab("Well depth(feet)") +
        theme(plot.title = element_text(hjust = 0.5,size=22),
              axis.title=element_text(size=18),
              axis.text = element_text(size=18,lineheight = 5),
              legend.position = c(0.7, 0.8),
              legend.text = element_text( size=18),
              legend.title = element_blank(),
              plot.margin=unit(c(0.5,0.5,0.8,0.8),"cm"),
              panel.background = element_rect(fill = "aliceblue",colour = "aliceblue",
                                              size = 0.5, linetype = "solid"),
              panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                              colour = "white")
        )
    }
    
    
  })
  
  output$my_plot <- renderPlot(
    ts_plot()
  )
  
  ts_table <- reactive({
    
    well <- input$mymap_marker_click$id
    
    if(is.null(well)){
      well <- "select well"
      long <- "select well"
      lat <- "select well"
    } else {
      selected_well <- filter(well_location, wells == well)
      long <- selected_well$longitude
      lat <- selected_well$latitude 
    }
    
    well_df <- data.frame(
      "Well" = c(as.character(well)),
      "Lat" = c(lat),
      "Long" = c(long),
      "Start Date" = c("some date"),
      "End Date" = c("some date"),
      "Mean" = c(0.0),
      "St Dev" = c(0.0),
      "Arima Model" = c("Arima(2, 1, 2)"),
      "MAPE" = c(0.0),
      "AIC" = c(0.0)
    )
    
    well_df
  })
  
  output$table <- renderTable(
    ts_table()
  )
  
}

shinyApp(ui, server)
