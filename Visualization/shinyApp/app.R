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
  temp_df <- read.csv(file, stringsAsFactors = F)
  
  if(file_name %in% c("F-179", "F-319", "G-1260_T", "G-2866_T")){
      temp_df$date_time <- as.POSIXct(temp_df$time, "%m/%d/%y %H:%M", tz = "")
      temp_df$year <- as.numeric( substr( temp_df$date_time, 1, 4) )
  } else {
      temp_df$date_time <- as.POSIXct(temp_df$time,  '%Y-%m-%d %H:%M:%S', tz = "")
      temp_df$year <- as.numeric( substr( temp_df$time, 1, 4) )
  }
  
  #temp_ts <- ts(temp_df$well_impute, frequency = 365.25)
  #temp_decomp <- stl(temp_ts, s.window = 7)
  #temp_season <- temp_decomp$time.series[,1]
  #temp_trend <- temp_decomp$time.series[,2]
  #temp_df$trend <- temp_trend
  #temp_df$seas_adj <- temp_df$well_impute - temp_season
  ncol(temp_df)
  list_of_data[[file_name]] <- temp_df
}

vis_stats <- read.csv("./Visual_stat.csv", stringsAsFactors = F)

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
        "Explore Wells", 
        tabName = "with_map", 
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
                           sliderInput('years', 'Years of Data', 
                                       min=2007, 
                                       max=2018, 
                                       value=c(2007,2018), 
                                       step=1, 
                                       round=0),
                           br(),
                           checkboxInput('forecast', 'Show Forecasted Values'),
                           checkboxInput('rain', 'Show Hourly Rain Data')
                           #checkboxInput('trend', 'Show Trend Line Overlaid')
                         )
                       )
                )
              )
            )
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
    
    start_year <- input$years[1]
    end_year <- input$years[2]
    
    well <- input$mymap_marker_click$id
    
    if(is.null(well)){
      well <- "PB-1680_T"
    }
    
    selected_data <- list_of_data[[well]]
    
    if(input$forecast){
      
      selected_data <- filter(selected_data, !is.na(forecast))
      
      main_plot <- ggplot(selected_data) +
        geom_line(mapping = aes(x = date_time, y = well_impute, colour = "well_impute")) +
        geom_line(mapping = aes(x = date_time, y = forecast, colour = "forecast")) +
        ggtitle(paste0("Well Elevation (Feet) for ", well))+
        xlab("Time (years)") + 
        ylab("Well Elevation (feet)") +
        theme(plot.title = element_text(hjust = 0.5,size=26),
              axis.title=element_text(size=22),
              axis.text = element_text(size=16,lineheight = 5),
              legend.position = "top",
              legend.text = element_text( size=16),
              legend.title = element_blank(),
              plot.margin=unit(c(0.5,0.5,0.8,0.8),"cm"),
              panel.background = element_rect(fill = "aliceblue",colour = "grey",
                                              size = 0.5, linetype = "solid"),
              panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                              colour = "white"),
              axis.title.x = element_text(margin = margin(t = 24)),
              axis.title.y = element_text(margin = margin(r = 24))
        ) + 
        scale_x_datetime(date_labels = "%b %Y") +
        scale_colour_manual(name="Time Series Label",
                            values=c("red", "navy"), labels = c("Predicted", "Actual"))
    } else if (input$rain & ("rain_impute" %in% colnames(selected_data))){
      
      selected_data <- filter(selected_data, year >= start_year, year <= end_year)
      
      main_plot <- ggplot(selected_data) +
        geom_line(mapping = aes(x = date_time, y = well_impute, colour = "well_impute")) +
        geom_line(mapping = aes(x = date_time, y = rain_impute * 5, colour = "rain_impute")) +
        ggtitle(paste0("Well Elevation (Feet) for ", well))+
        xlab("Time (years)") + 
        ylab("Well Elevation (ft)") +
        theme(plot.title = element_text(hjust = 0.5,size=26),
              axis.title=element_text(size=22),
              axis.text = element_text(size=16,lineheight = 5),
              legend.position = "top",
              legend.text = element_text( size=16),
              legend.title = element_blank(),
              plot.margin=unit(c(0.5,0.5,0.8,0.8),"cm"),
              panel.background = element_rect(fill = "aliceblue",colour = "grey",
                                              size = 0.5, linetype = "solid"),
              panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                              colour = "white"),
              axis.title.x = element_text(margin = margin(t = 24)),
              axis.title.y = element_text(margin = margin(r = 24)),
              axis.title.y.right = element_text(margin = margin(l = 24))
        ) + 
        scale_x_datetime(date_labels = "%b %Y") +
        scale_colour_manual(name="Time Series Label",
                            values=c("red", "navy"), labels = c("Rain", "Well Elevation")) +
        scale_y_continuous(sec.axis = sec_axis(~./5, name = "Rain Level (ft)"))
      
      
    } else {
      
      selected_data <- filter(selected_data, year >= start_year, year <= end_year)
      
      main_plot <- ggplot(selected_data) +
        geom_line(mapping = aes(x = date_time, y = well_impute)) +
        ggtitle(paste0("Well Elevation (Feet) for ", well))+
        xlab("Time (years)") + 
        ylab("Well Elevation (feet)") +
        theme(plot.title = element_text(hjust = 0.5,size=26),
              axis.title=element_text(size=22),
              axis.text = element_text(size=16,lineheight = 5),
              legend.position = "top",
              legend.text = element_text( size=16),
              legend.title = element_blank(),
              plot.margin=unit(c(0.5,0.5,0.8,0.8),"cm"),
              panel.background = element_rect(fill = "aliceblue",colour = "grey",
                                              size = 0.5, linetype = "solid"),
              panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                              colour = "white"),
              axis.title.x = element_text(margin = margin(t = 24)),
              axis.title.y = element_text(margin = margin(r = 24))
        ) + 
        scale_x_datetime(date_labels = "%b %Y")
    }
    
    main_plot
    
    
    
  })
  
  output$my_plot <- renderPlot(
    ts_plot()
  )
  
  ts_table <- reactive({
    
    well_name <- input$mymap_marker_click$id
    
    if(is.null(well_name)){
      well_name <- "PB-1680_T"
    }
    
    selected_stats <- filter(vis_stats, well == well_name)
    print(selected_stats)
    selected_well <- filter(well_location, wells == well_name)
    long <- selected_well$longitude
    lat <- selected_well$latitude 
    #selected_well_df <- list_of_data[[well]]
    #start_date <- strsplit(as.character(selected_well_df$date_time[1]), " ")[[1]][1]
    #end_date <- strsplit(as.character(selected_well_df$date_time[nrow(selected_well_df)]), " ")[[1]][1]
    start_date <- selected_stats$start
    end_date <- selected_stats$end
    num_missing <- selected_stats$missing
    
    if(input$forecast){
      
      model <- selected_stats$model
      mape <- selected_stats$mape
      aic <- selected_stats$aic
      
      well_df <- data.frame(
        "Well" = c(as.character(well_name)),
        "Lat" = c(lat),
        "Long" = c(long),
        "Start Date" = c(start_date),
        "End Date" = c(end_date),
        "Missing" = c(num_missing),
        "Model" = c(model),
        "MAPE" = c(mape),
        "AIC" = c(aic)
      )
      
      
    } else {
      mean <- selected_stats$mean
      stdev <- selected_stats$stdev
      
      well_df <- data.frame(
        "Well" = c(as.character(well_name)),
        "Lat" = c(lat),
        "Long" = c(long),
        "Start Date" = c(start_date),
        "End Date" = c(end_date),
        "Mean" = c(mean),
        "St Dev" = c(stdev),
        "Missing" = c(num_missing)
      )
    }
    well_df
  })
  
  output$table <- renderTable(
    ts_table()
    )
  
}

shinyApp(ui, server)
