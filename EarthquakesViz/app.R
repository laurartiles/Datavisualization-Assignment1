#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(plotly)
library(forcats)
library(sp)
library(sf) #https://github.com/fraxen/tectonicplates/edit/master/GeoJSON/PB2002_plates.json
library(maps)
library(rworldmap)
library(ggmap)
source("helper.R")

earthquakes<-read.csv('database.csv')
earthquakes$Date <- as.Date(earthquakes$Date, "%m/%d/%Y")
min_date <- min(earthquakes$Date, na.rm = TRUE)
max_date <- max(earthquakes$Date, na.rm = TRUE)
min_magnitude <-  min(earthquakes$Magnitude, na.rm = TRUE)
max_magnitude <- max(earthquakes$Magnitude, na.rm = TRUE)

coords.df <- data.frame(Lng=earthquakes$Longitude, Lat=earthquakes$Latitude)
map.bbox <- c(bottom=-85, top=85, right=185, left=-185)
map <- get_stamenmap(bbox = map.bbox, zoom=3, maptype='watercolor')
plates <- st_transform(st_read("plates.json"), 3857)

earthquakes$Continent <- as.factor(coords2continent(coords.df))
CONTINENTS = factor(c("Africa", "Antartica", "Asia", "Australia", "Europe", "North America", "South America", "Ocean"))

earthquakes$DiscreteMagnitude <- floor(earthquakes$Magnitude)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Earthquakes from 1965 to 2016"),

    # Sidebar with a slider input for number of bins 
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

        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("mapRepresentation"),
          plotOutput("tsFreqPlot"),
          conditionalPanel(condition = 'input.seasonality && input.groupingUnit == "Month"', plotOutput('decompositionFrec')),
          plotOutput("tsMagnitudePlot"),
          conditionalPanel(condition = 'input.seasonality && input.groupingUnit == "Month"', plotOutput('decompositionAvg')),
          plotOutput("continentsMagnitudeDistribution"),
          #plotOutput("distributionPlot"),
          plotOutput("continentsPiePlot")
        )
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
    
    n.per.date <- reactive({
      n.per.date <- get.used.earthquakes() %>% count(date = format(Date, get.date.format())) %>% na.omit
      #n.per.date$date <- as.Date(n.per.date$date, "%Y")
      n.per.date
    })
    avg.per.date <- reactive({
      avg.per.date <- get.used.earthquakes() %>% group_by(date = format(Date, get.date.format())) %>% summarise(AvgMagnitude = mean(Magnitude)) %>% na.omit
      #avg.per.date$date <- as.Date(avg.per.date$date, "%Y")
      avg.per.date
    })
    
    n.per.continent <- reactive({
      get.used.earthquakes() %>% count(Continent)
    })
    n.per.continent.per.magnitude <- reactive({
      get.used.earthquakes() %>% count(Continent, DiscreteMagnitude)
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
      x <- avg.per.date()
      ts.earthquakes <- ts(x$AvgMagnitude, start = ts.start(), end = ts.end(), frequency = get.used.frec())
      plot.ts(ts.earthquakes, main = paste("Average magnitude of earthquakes per ", input$groupingUnit))
    })
    
    output$decompositionAvg <- renderPlot({
      x <- avg.per.date()
      ts.earthquakes <- ts(x$AvgMagnitude, start = ts.start(), end = ts.end(), frequency = get.used.frec())
      ts.dec <- decompose(ts.earthquakes)
      plot(ts.dec)
    })
    
    output$distributionPlot <- renderPlot({
      hist(get.used.earthquakes()$Magnitude, xlab = "Magnitude", main = "Distribution of earthquake magnitudes")  
    })
    
    output$continentsPiePlot <- renderPlot({
      pie(n.per.continent()$n, labels = n.per.continent()$Continent)
    })
    
    output$continentsMagnitudeDistribution <- renderPlot({
      ggplot(n.per.continent.per.magnitude(), aes(fill=Continent, y=n, x=DiscreteMagnitude)) + geom_bar(position = "stack", stat = "identity")
    })
    
    output$mapRepresentation <- renderPlot({
      ggmap(map) +
        geom_point(data = get.used.earthquakes(), aes(x=Longitude, y=Latitude, fill="red", alpha=0.8), size=2, shape=21) +
        guides(fill=FALSE, alpha=FALSE)  +
        geom_sf(data = plates, colour = "red", fill = NA, inherit.aes = FALSE)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)