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
earthquakes$Year <- format(earthquakes$Date, "%Y")
earthquakes$Month <- format(earthquakes$Date, "%m")
earthquakes$Day <- format(earthquakes$Date, "%d")

min_date <- min(earthquakes$Date, na.rm = TRUE)
max_date <- max(earthquakes$Date, na.rm = TRUE)
min_magnitude <-  min(earthquakes$Magnitude, na.rm = TRUE)
max_magnitude <- max(earthquakes$Magnitude, na.rm = TRUE)
min_long <-  min(earthquakes$Longitude, na.rm = TRUE)
max_long <- max(earthquakes$Longitude, na.rm = TRUE)
min_lat <-  min(earthquakes$Latitude, na.rm = TRUE)
max_lat <- max(earthquakes$Latitude, na.rm = TRUE)

coords.df <- data.frame(Lng=earthquakes$Longitude, Lat=earthquakes$Latitude)
map.bbox <- c(bottom=-85, top=85, right=185, left=-185)
map <- get_stamenmap(bbox = map.bbox, zoom=3, maptype='watercolor')
#https://github.com/fraxen/tectonicplates/edit/master/GeoJSON/PB2002_plates.json
plates <- st_transform(st_read("plates.json"), 3857)

earthquakes$Continent <- as.factor(coords2continent(coords.df))
CONTINENTS = factor(c("Africa", "Antartica", "Asia", "Australia", "Europe", "North America", "South America", "Ocean"))
earthquakes$Country <- as.factor(coords2continent(coords.df, 'country'))

earthquakes$DiscreteMagnitude <- floor(earthquakes$Magnitude)

earthquakes$Size <- cut(earthquakes$Magnitude,
                        c(5.4, 5.9, 6.9, 7.9, 9.1),
                        labels=c("5.5 to 5.9", "6.0 to 6.9", "7.0 to 7.9", "8.0 to 9.1"))

# colour pallet Magnitude
pallet <- colorFactor(c("yellow",  "purple", "green", "red"),
                      domain = c("5.5 to 5.9", "6.0 to 6.9", "7.0 to 7.9", "8.0 to 9.1"))


earthquakes$DepthType <- cut(earthquakes$Depth,
                        c(-1.1, 99, 200, 700),
                        labels=c("<50km", "<100km", ">100km"))

palletDepth <- colorFactor(c("yellow",  "orange", "red"),
                          domain = c("<50km", "<100km", ">100km"))

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
                            h1("Evolution of Earthquakes over time"),
                            p("p creates a paragraph of text."),
                            plotOutput("tsFreqPlot"),
                            conditionalPanel(condition = 'input.seasonality && input.groupingUnit == "Month"', plotOutput('decompositionFrec')),
                            h1("Evolution of Average Magnitude over time"),
                            plotOutput("tsMagnitudePlot"),
                            conditionalPanel(condition = 'input.seasonality && input.groupingUnit == "Month"', plotOutput('decompositionAvg')),
                            # plotOutput("MagnitudeYearPlot"),
                            #plotOutput("mapRepresentation"),
                            h1("Analysis of Frequency"),
                            h2("Histograms"),
                            plotOutput("QuakesYear"),
                            plotOutput("QuakesYear2"),
                            h2("Top 10"),
                            plotOutput("Top10QuakeFreq")
                            
                          )
               ),
               tabPanel("Location of earthquakes",
                          mainPanel(
                            width = 12,
                            h1("Map of Earthquakes by magnitude"),
                            leafletOutput("quakemap"),
                            h1("Map of Earthquakes grouped by location"),
                            leafletOutput("quakemap2"),
                            h1("Treemap - Earthquakes per Country"),
                            highchartOutput("ChartCountry")
                          )
               ),
               tabPanel("Magnitude Analysis",
                          mainPanel(
                            width = 12,
                            plotOutput("continentsMagnitudeDistribution"),
                            plotOutput("continentsPiePlot"),
                            plotOutput("QuakesMag"),
                            plotOutput("MagnitudeDepth"),
                          )
                ),
               tabPanel("Depth Analysis",
                        mainPanel(
                          width = 12,
                          plotOutput("QuakesDepth"),
                          leafletOutput("quakemap_Depth"),
                          plotOutput("QuakesDepth2")
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
      n.per.date <- get.used.earthquakes() %>% count(date = format(Date, get.date.format())) %>% na.omit
      #n.per.date$date <- as.Date(n.per.date$date, "%Y")
      n.per.date
    })
    # Magnitude (per date)
    magn.per.date <- reactive({
      get.used.earthquakes() %>% group_by(date = format(Date, get.date.format())) %>% summarise(min = min(Magnitude), mean = mean(Magnitude), max = max(Magnitude)) %>% na.omit
    })
    
    
    # Frequency (per continent)
    n.per.continent <- reactive({
      get.used.earthquakes() %>% count(Continent)
    })
    # Frequency (per continent and magnitude)
    n.per.continent.per.magnitude <- reactive({
      get.used.earthquakes() %>% count(Continent, DiscreteMagnitude)
    })
    n.per.depth <- reactive({
      get.used.earthquakes() %>% count(Depth)
    })
    
    per_year <- reactive({
      get.used.earthquakes() %>% filter(Type=="Earthquake") %>% 
        group_by(Year) %>% summarise(Observations=n())
    })
    per_magnitude <- reactive({
      get.used.earthquakes() %>% filter(Type=="Earthquake") %>% 
        group_by(Magnitude) %>% summarise(Observations=n())
    })
    per_Depth <- reactive({
      get.used.earthquakes() %>% filter(Type=="Earthquake") %>% group_by(Depth) %>% summarise(Observations=n())
    })
    sum_country <- reactive({
      get.used.earthquakes() %>% group_by(Country) %>% summarise(Observations=n())
    })
    Quake_Freq <- reactive({
      get.used.earthquakes() %>% group_by(Year) %>% summarise(n=n()) %>% arrange(desc(n)) %>% head(10)
    })
    
    # PLOTS
    output$tsFreqPlot <- renderPlot({
      x <- n.per.date()
      ts.earthquakes <- ts(x$n, start = ts.start(), end = ts.end(), frequency = get.used.frec())
      plot.ts(ts.earthquakes, main = paste("Number of earthquakes per ", input$groupingUnit))
    })
    
    output$decompositionFrec <- renderPlot({
      x <- n.per.date()
      ts.earthquakes <- ts(x$n, start = ts.start(), end = ts.end(), frequency = get.used.frec())
      ts.dec <- decompose(ts.earthquakes)
      plot(ts.dec)
    })
    
    output$tsMagnitudePlot <- renderPlot({
      ts.min <- ts(magn.per.date()$min, start = ts.start(), end = ts.end(), frequency = get.used.frec())
      ts.mean <- ts(magn.per.date()$mean, start = ts.start(), end = ts.end(), frequency = get.used.frec())
      ts.max <- ts(magn.per.date()$max, start = ts.start(), end = ts.end(), frequency = get.used.frec())
      ts.plot(ts.min, ts.mean, ts.max, main = paste("Min, mean and max magnitude of earthquakes per ", input$groupingUnit))
      
      #df <- magn.per.date()
      #ggplot(df, aes(1:nrow(df), mean)) + 
      #  geom_line() +
      #  geom_ribbon(aes(1:nrow(df), ymax=max, ymin=min), alpha=0.1) +
      #  theme_bw() +
      #  labs(x = "Date", y = "Min, Mean and Max magnitude")
    })
    
    output$decompositionAvg <- renderPlot({
      ts.earthquakes <- ts(magn.per.date()$mean, start = ts.start(), end = ts.end(), frequency = get.used.frec())
      ts.dec <- decompose(ts.earthquakes)
      plot(ts.dec)
    })
    
    output$distributionPlot <- renderPlot({
      hist(get.used.earthquakes()$Magnitude, xlab = "Magnitude", main = "Distribution of earthquake magnitudes")  
    })
    
    # output$MagnitudeYearPlot <- renderPlot({
    #   x <- magn.per.date()$mean
    # ggplot(x, aes(Year, AvgMagnitude)) + 
    #   geom_line(color = 'steelblue')+ theme_bw()+ 
    #   ggtitle("Earthquake Magnitude")+
    #   xlab('Date')+
    #   ylab('Magnitude')+
    #   theme(plot.title = element_text(hjust = 0.5))
    # })
    
    output$continentsPiePlot <- renderPlot({
      pie(n.per.continent()$n, labels = n.per.continent()$Continent)
    })
    
    output$continentsMagnitudeDistribution <- renderPlot({
      ggplot(n.per.continent.per.magnitude(), aes(fill=Continent, y=n, x=DiscreteMagnitude)) + geom_bar(position = "stack", stat = "identity")
    })
    # output$continentsMagnitudeDistribution <- renderPlot({
    #   ggplot(n.per.continent.per.magnitude(), aes(fill=DiscreteMagnitude, y=n, x=Continent)) + geom_bar(position = "stack", stat = "identity")
    # })
    
    output$mapRepresentation <- renderPlot({
      ggmap(map) +
        geom_point(data = get.used.earthquakes(), aes(x=longitude, y=latitude, fill="red", alpha=0.8), size=2, shape=21) +
        guides(fill=FALSE, alpha=FALSE)  +
        geom_sf(data = plates, colour = "red", fill = NA, inherit.aes = FALSE)
    })
    
    # Map with earthquakes by depth
    qm <- function() {
      # earthquake.get <- get.used.earthquakes()
      pu <- paste("<b>Mag:</b>", as.character(get.used.earthquakes()$Magnitude), "<br>",
                  "<b>Depth:</b>", as.character(get.used.earthquakes()$Depth), "km<br>",
                  "<b>Time:</b>", as.character(get.used.earthquakes()$Date),"<br>",
                  "<b>Lat:</b>", as.character(get.used.earthquakes()$Latitud),"<br>",
                  "<b>Long:</b>", as.character(get.used.earthquakes()$Longitud),"<br>",
                  "<b>ID:</b>", get.used.earthquakes()$ID,"<br>"
      )
      tempmap <- leaflet(get.used.earthquakes()) %>% addTiles() %>%
        setView(0.000, 0.000, zoom = 2) %>%
        addCircleMarkers(popup = pu,
                         radius = ~ifelse(Magnitude < 5.9, 4, 6),
                         color = ~pallet(Size),
                         stroke = FALSE, fillOpacity = 0.6) #%>%
      # addLegend("bottomleft", colors = c("black", "green",  "blue", "purple", "red", "yellow"),
      # labels=c("3.3 to 3.9", ">3.9 to 4.9", ">4.9 to 5.9", ">5.9 to 6.9", ">6.9 to 7.9", ">7.9 to 9.1"),
      # title = "Magnitude")
    }
    output$quakemap <- renderLeaflet(qm())
    
    # Map with grouped earthquakes in numbers
    output$quakemap2 <- renderLeaflet(
      leaflet(get.used.earthquakes()) %>%
      addTiles() %>%
      setView(0.000, 0.000, zoom = 2) %>%
      addMarkers(lat=get.used.earthquakes()$Latitude, lng=get.used.earthquakes()$Longitude, clusterOptions = markerClusterOptions(),
                 popup= paste("<b>Mag:</b>", as.character(get.used.earthquakes()$Magnitude), "<br>",
                              "<b>Depth:</b>", as.character(get.used.earthquakes()$Depth), "km<br>",
                              "<b>Time:</b>", as.character(get.used.earthquakes()$Date),"<br>",
                              "<b>Lat:</b>", as.character(get.used.earthquakes()$Latitud),"<br>",
                              "<b>Long:</b>", as.character(get.used.earthquakes()$Longitud),"<br>",
                              "<b>ID:</b>", get.used.earthquakes()$ID,"<br>"
    )))

    # output$map <- renderLeaflet({
    #   leaflet(get.used.earthquakes())%>%
    #     addTiles() %>%
    #     setView(0.000, 0.000, zoom = 2) %>%
    #     addCircles(radius = 5, color = "red", fillOpacity = 0.7, popup=paste("Mag=", earthquakes$Magnitude))
    # })

    
    # Map with earthquakes by depth
    qd <- function() {
      # earthquake.get <- get.used.earthquakes()
      pu2 <- paste("<b>Mag:</b>", as.character(get.used.earthquakes()$Magnitude), "<br>",
                  "<b>Depth:</b>", as.character(get.used.earthquakes()$Depth), "km<br>",
                  "<b>Time:</b>", as.character(get.used.earthquakes()$Date),"<br>",
                  "<b>Lat:</b>", as.character(get.used.earthquakes()$Latitud),"<br>",
                  "<b>Long:</b>", as.character(get.used.earthquakes()$Longitud),"<br>",
                  "<b>ID:</b>", get.used.earthquakes()$ID,"<br>"
      )
      tempmap <- leaflet(get.used.earthquakes()) %>% addTiles() %>%
        setView(0.000, 0.000, zoom = 2) %>%
        addCircleMarkers(popup = pu2,
                         radius = 5,
                         color = ~palletDepth(DepthType),
                         stroke = FALSE, fillOpacity = 0.6) #%>%
      # addLegend("bottomleft", colors = c("black", "green",  "blue", "purple", "red", "yellow"),
      # labels=c("3.3 to 3.9", ">3.9 to 4.9", ">4.9 to 5.9", ">5.9 to 6.9", ">6.9 to 7.9", ">7.9 to 9.1"),
      # title = "Magnitude")
    }
    output$quakemap_Depth <- renderLeaflet(qd())
    
    
    #TreeMap Earthquakes Country
    output$ChartCountry <- renderHighchart({
      hchart(sum_country(),"treemap", hcaes(x = Country, value = Observations, color = "Observations"))%>%
      hc_credits(enabled = TRUE, style = list(fontSize = "10px")) %>%
      hc_title(text = "Earthquakes per Country")
    })
    
    #Earthquakes per Year:
    output$QuakesYear <- renderPlot({
    ggplot(per_year(), aes(x=Year,y=Observations))+geom_bar(stat = "identity",fill="pink")+
      labs(y="Observations", x="Year", title="Earthquakes per Year",
           caption="Source: Significant Earthquakes, 1965-2016")+theme_bw()
    })

    output$QuakesYear2 <- renderPlot({
    ggplot(per_year(), aes(x =Year, y =Observations, colour = "orange"))  + 
      geom_point()  + geom_line()
    })
    
    #Earthquakes per Magnitude:
    output$QuakesMag <- renderPlot({
      ggplot(per_magnitude(), aes(x=Magnitude,y=Observations))+geom_bar(stat = "identity",fill="purple")+
        labs(y="Observations", x="Magnitude", title="Magnitude Analysis",
             caption="Source: Significant Earthquakes, 1965-2016")+theme_bw()
    })
    
    #Magnitude vs Depth
    output$MagnitudeDepth <- renderPlot({
    ggplot(get.used.earthquakes(), aes(Depth,Magnitude,color = Size))+
      geom_jitter(alpha = 0.5)+
      theme_bw()+ xlab('Magnitude')+ ylab('Depth')+ ggtitle('Magnitude Vs. Depth in Km')+ 
        theme(plot.title = element_text(hjust = 0.5))

      })
    
    
    #Depth Analysis 
    output$QuakesDepth <- renderPlot({
      ggplot(get.used.earthquakes(),aes(Depth))+ xlim(0, 700) +
        stat_density(fill="lightblue")+
        labs(title="Earthquakes",subtitle="Depth")+
        scale_x_log10(breaks = seq.int(from = 0,to = 700,by = 50))
    })
    
    output$QuakesDepth2 <- renderPlot({
      ggplot(per_Depth(), aes(x =Depth, y =Observations, colour = "red"))  + 
        geom_point()  + geom_line()
    })
    

    #Top 10 Years with Highest Earthquake Frequency in selected Range of Dates
    output$Top10QuakeFreq <- renderPlot({
      ggplot(Quake_Freq(), aes(x =reorder(Year,n), y =  n )) + geom_bar(stat='identity',colour="white", fill = c("#66a3da")) +
        labs(x = 'Year', y = 'Count', title = 'Top 10 Years with Highest Earthquake Frequency in selected Range of Dates') +
        coord_flip() + theme_bw()
    })

    
}

# Run the application 
shinyApp(ui = ui, server = server)
