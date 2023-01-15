#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

library(shiny) # Build interactive web apps
library(tidyverse) # Collection of R packages for data science
library(ggplot2) # Data visualization package
library(dplyr) # Tools for easier df manipulation (e.g. pipes)
library(plotly) # Interactive plots
library(forcats) # Categorical values handling
library(sp) # Spatial data
library(sf) # Spatial data
library(rworldmap) # Mapping global data
library(maps) # Display of maps
library(ggmap) # Visualize spatial data from Stamen maps
source("helper.R") # Map coordinates to continents

library(leaflet)
library(RColorBrewer)
library(shinydashboard)
library(lubridate)
library(xts)
library(DT)
library(maptools)
library(viridisLite)
library(highcharter)
library(treemap)


earthquakes<-read.csv('database.csv')
earthquakes$Date <- as.Date(earthquakes$Date, "%m/%d/%Y")

min_date <- min(earthquakes$Date, na.rm = TRUE)
max_date <- max(earthquakes$Date, na.rm = TRUE)
min_magnitude <-  min(earthquakes$Magnitude, na.rm = TRUE)
max_magnitude <- max(earthquakes$Magnitude, na.rm = TRUE)

coords.df <- data.frame(Lng=earthquakes$Longitude, Lat=earthquakes$Latitude)
#https://github.com/fraxen/tectonicplates/edit/master/GeoJSON/PB2002_plates.json
plates <- fortify(st_read("plates.json"))

earthquakes$Continent <- as.factor(coords2continent(coords.df))
CONTINENTS = factor(c("Africa", "Antartica", "Asia", "Australia", "Europe", "North America", "South America", "Ocean"))
earthquakes$Country <- as.factor(coords2continent(coords.df, 'country'))

earthquakes$DiscreteMagnitude <- floor(earthquakes$Magnitude)

SizeLabels <- c("5.5 to 5.9", "6.0 to 6.9", "7.0 to 7.9", "8.0 to 9.1")
earthquakes$Size <- cut(earthquakes$Magnitude, c(5.4, 5.9, 6.9, 7.9, 9.1), labels=SizeLabels)
# colour pallet Magnitude
pallet <- colorFactor(c("green", "yellow", "orange", "red"), domain = SizeLabels, ordered = TRUE)

DepthLabels <- c("<50km", "<100km", ">100km")
earthquakes$DepthType <- cut(earthquakes$Depth, c(-1.1, 50, 100, 700), labels=DepthLabels)
palletDepth <- colorFactor(c("red",  "yellow", "green"), domain=DepthLabels, ordered = TRUE)



# Define UI for application that draws a histogram
ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
    style = "position:fixed",
    dateRangeInput("dateRange",
                   "Range of dates for the graph",
                   start = min_date,
                   end = max_date,
                   min = min_date,
                   max = max_date,
                   startview = "decade"
    ),
    sliderInput("magnitudeRange",
                "Earthquake magnitude range",
                min = min_magnitude,
                max = max_magnitude,
                value = c(min_magnitude,max_magnitude),
                step = 0.2),
    radioButtons("groupingUnit",
                 "Group data by month or year",
                 c("Month", "Year")),
    checkboxGroupInput("continents",
                       "Selected continents",
                       choices = CONTINENTS,
                       selected = CONTINENTS),
    checkboxInput("seasonality",
                  "Visualize seasonality",
                  value = FALSE),
    checkboxInput("groupPoints",
                  "Group points in location map",
                  value = FALSE)
  ),
  mainPanel(
    navbarPage("Earthquakes from 1965 to 2016", position = c("fixed-top"),
               tags$style(type="text/css", "body {padding-top: 70px;}", 
                          '.navbar { background-color: #2010A0;
                           font-family: Arial;
                           font-size: 18px;
                           color: #ABCB0C; }',
                          
                          '.navbar-default .navbar-brand {
                             color: #ABCB0C;
                             font-size: 25px;
                           }'
               ),
               tabPanel("Evolution of earthquakes",
                          mainPanel(
                            width = 12,
                            h2("Number of earthquakes per time period"),
                            plotOutput("tsFreqPlot"),
                            conditionalPanel(condition = 'input.seasonality && input.groupingUnit == "Month"', plotOutput('decompositionFrec')),
                            h2("Min, mean and max magnitude of earthquakes per time period"),
                            plotOutput("tsMagnitudePlot"),
                            conditionalPanel(condition = 'input.seasonality && input.groupingUnit == "Month"', plotOutput('decompositionAvg')),
                            h2('Top 10 time periods with Highest Earthquake Frequency'),
                            plotOutput("Top10QuakeFreq")
                            
                          )
               ),
               tabPanel("Location of earthquakes",
                          mainPanel(
                            width = 12,
                            h2("Map of Earthquakes by magnitude"),
                            leafletOutput("quakemap"),
                            h2("Earthquakes per Country"),
                            highchartOutput("ChartCountry")
                          )
               ),
               tabPanel("Magnitude Analysis",
                          mainPanel(
                            width = 12,
                            h2('Number of earthquakes per continent and magnitude range'),
                            plotOutput("continentsMagnitudeDistribution"),
                            h2('Total number of earthquakes per magnitude range'),
                            plotOutput("QuakesMag"),
                            h2('Magnitude Vs. Depth in Km'),
                            plotOutput("MagnitudeDepth")
                          )
                ),
               tabPanel("Depth Analysis",
                        mainPanel(
                          width = 12,
                          h2('Map of Earthquakes by depth'),
                          leafletOutput("quakemap_Depth"),
                          h2('Distribution of Earthquakes per depth'),
                          plotOutput("QuakesDepth")
                        )
               )
              )
          ),
  position = "left",
  fluid = TRUE
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
  
    # REACTIVE VARIABLES
    get.used.earthquakes <- reactive({ 
      filter(earthquakes, 
             Date >= input$dateRange[1], 
             Date <= input$dateRange[2], 
             Magnitude >= input$magnitudeRange[1], 
             Magnitude <= input$magnitudeRange[2],
             Continent %in% input$continents
    )})
    get.date.format <- reactive({ ifelse(input$groupingUnit == "Month", "%Y-%m", "%Y") })
    get.used.frec <- reactive({ ifelse(input$groupingUnit == "Month", 12, 1) })
    start.year <- reactive({ strtoi(format(input$dateRange[1], "%Y")) })
    start.month <- reactive({ strtoi(format(input$dateRange[1], "%m")) })
    end.year <- reactive({ strtoi(format(input$dateRange[2], "%Y")) })
    end.month <- reactive({ strtoi(format(input$dateRange[2], "%m")) })
    ts.start <-  reactive({ ifelse(input$groupingUnit == "Month", c(start.year(), start.month()), start.year()) })
    ts.end <-  reactive({ ifelse(input$groupingUnit == "Month", c(end.year(), end.month()), end.year()) })
    
    # Frequency (per date)
    n.per.date <- reactive({
      get.used.earthquakes() %>% count(date = format(Date, get.date.format())) %>% na.omit
    })
    # Magnitude (per date)
    magn.per.date <- reactive({
      get.used.earthquakes() %>% group_by(date = format(Date, get.date.format())) %>% summarise(min = min(Magnitude), mean = mean(Magnitude), max = max(Magnitude)) %>% na.omit
    })
    
    mapPointPopup <- reactive({
      paste("<b>Mag:</b>", as.character(get.used.earthquakes()$Magnitude), "<br>",
            "<b>Depth:</b>", as.character(get.used.earthquakes()$Depth), "km<br>",
            "<b>Time:</b>", as.character(get.used.earthquakes()$Date),"<br>",
            "<b>Lat:</b>", as.character(get.used.earthquakes()$Latitud),"<br>",
            "<b>Long:</b>", as.character(get.used.earthquakes()$Longitud),"<br>",
            "<b>ID:</b>", get.used.earthquakes()$ID,"<br>")
    })
    
    
    ### PLOTS ###############################################################
    
    ### EVOLUTION PLOTS ###
    output$tsFreqPlot <- renderPlot({
      x <- n.per.date()
      ts.earthquakes <- ts(x$n, start = ts.start(), end = ts.end(), frequency = get.used.frec())
      plot.ts(ts.earthquakes, xlab = "Date", ylab = "Frequency")
    })
    
    output$decompositionFrec <- renderPlot({
      x <- n.per.date()
      ts.earthquakes <- ts(x$n, start = ts.start(), end = ts.end(), frequency = get.used.frec())
      ts.dec <- decompose(ts.earthquakes)
      plot(ts.dec)
    })
    
    output$tsMagnitudePlot <- renderPlot({
      #ts.min <- ts(magn.per.date()$min, start = ts.start(), end = ts.end(), frequency = get.used.frec())
      #ts.mean <- ts(magn.per.date()$mean, start = ts.start(), end = ts.end(), frequency = get.used.frec())
      #ts.max <- ts(magn.per.date()$max, start = ts.start(), end = ts.end(), frequency = get.used.frec())
      #ts.plot(ts.min, ts.mean, ts.max, ylab = "Magnitude")
      
      df <- magn.per.date()
      df$date <- as.Date(paste(df$date, ifelse(get.date.format()=='Month', '01', '01-01'), sep='-'))
      ggplot(df, aes(x=date, y=mean)) + 
        geom_line() +
        geom_ribbon(aes(x=date, ymax=max, ymin=min), alpha=0.1) +
        theme_bw() +
        labs(x = "Date", y = "Magnitude")
    })
    
    output$decompositionAvg <- renderPlot({
      ts.earthquakes <- ts(magn.per.date()$mean, start = ts.start(), end = ts.end(), frequency = get.used.frec())
      ts.dec <- decompose(ts.earthquakes)
      plot(ts.dec)
    })
    
    #Top 10 Dates with Highest Earthquake Frequency in selected Range of Dates
    output$Top10QuakeFreq <- renderPlot({
      ggplot(n.per.date() %>% arrange(desc(n)) %>% head(10), aes(x=reorder(date,n), y=n)) + 
        geom_bar(stat='identity',colour="white", fill = c("#66a3da")) +
        labs(x = 'Date', y = 'Count') +
        coord_flip() + 
        theme_bw()
    })
    ### END EVOLUTION PLOTS ###
    
    
    ### LOCATION PLOTS #############################################
    # Map with earthquakes by depth
    output$quakemap <- renderLeaflet({
      leaflet(get.used.earthquakes()) %>% 
        addTiles() %>%
        addPolygons(data = plates) %>%
        setView(0.000, 0.000, zoom = 2) %>%
        addCircleMarkers(popup = mapPointPopup(),
                         radius = ~ifelse(Magnitude < 5.9, 2, 4),
                         color = ~pallet(Size),
                         stroke = FALSE, fillOpacity = 0.6,
                         clusterOptions = {if (input$groupPoints) markerClusterOptions()}) %>%
        leaflet::addLegend("bottomleft", pal = pallet, values=~Size, title = "Magnitude")
    })
    
    #TreeMap Earthquakes Country
    output$ChartCountry <- renderHighchart({
      hchart(get.used.earthquakes() %>% count(Country),"treemap", hcaes(x = Country, value = n, color = "n"))%>%
        hc_credits(enabled = TRUE, style = list(fontSize = "10px"))
    })
    ### END LOCATION PLOTS #########################################
    
    
    ### MAGNITUDE PLOTS ############################################
    output$continentsMagnitudeDistribution <- renderPlot({
      ggplot(get.used.earthquakes() %>% count(Continent, DiscreteMagnitude), 
        aes(fill=Continent, y=n, x=DiscreteMagnitude)) + 
        geom_bar(position = "stack", stat = "identity") +
        labs(x = 'Magnitude', y = 'Frequency') +
        theme_bw()
    })
    
    #Earthquakes per Magnitude:
    output$QuakesMag <- renderPlot({
      ggplot(get.used.earthquakes(), aes(x=Magnitude))+geom_histogram(fill="purple", bins = 10)+
        labs(x="Magnitude", y="Frequency")+theme_bw()
    })
    
    #Magnitude vs Depth
    output$MagnitudeDepth <- renderPlot({
      ggplot(get.used.earthquakes(), aes(Depth,Magnitude,color = Size))+
        geom_point()+
        theme_bw()+ xlab('Depth')+ ylab('Magnitude')+ 
        theme(plot.title = element_text(hjust = 0.5))
      
    })
    ### END MAGNITUDE PLOTS ###########################################
    
    
    ### DEPTH PLOTS ########################################
    # Map with earthquakes by depth
    output$quakemap_Depth <- renderLeaflet({
      leaflet(get.used.earthquakes()) %>% addTiles() %>%
        addPolygons(data = plates) %>%
        setView(0.000, 0.000, zoom = 2) %>%
        addCircleMarkers(popup = mapPointPopup(),
                         radius = 2,
                         color = ~palletDepth(DepthType),
                         stroke = FALSE, fillOpacity = 0.6) %>%
        leaflet::addLegend("bottomleft", pal = palletDepth, values=~DepthType, title = "Depth")
    })
    
    #Depth Analysis 
    output$QuakesDepth <- renderPlot({
      ggplot(get.used.earthquakes(),aes(Depth))+ xlim(0, 700) +
        stat_density(fill="lightblue")+
        labs(y="Density")+
        theme_bw()
    })
    ### END DEPTH PLOTS ####################################
}

# Run the application 
shinyApp(ui = ui, server = server)
