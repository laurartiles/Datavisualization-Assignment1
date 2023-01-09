### APP LESSON 5 ###

#install.packages(c("maps", "mapproj"))

library(shiny)

library(maps)
library(mapproj)
source("helpers.R")
counties <- readRDS("data/counties.rds")
percent_map(counties$white, "darkgreen", "% White")

# Define UI ----
ui <- fluidPage(
  
  titlePanel("censusVis"),
  
  sidebarLayout(
    sidebarPanel(
      
      helpText("Create demographic maps with information from the 2010 US Census."),
      
      selectInput("displayVariable",
                  label = "Choose a variable to display",
                  choices = list(
                    "Percent White",
                    "Percent Black",
                    "Percent Hispanic",
                    "Percent Asian"
                  ),
                  selected = "Percent White"),
      
      sliderInput("rangeInterest",
                  label = "Range of interest",
                  min = 0, max = 100, value = c(0,100))
      
      
    ),
    
    mainPanel(
      textOutput("selected_var"),
      textOutput("selected_range"),
      plotOutput("map")
    )
    
  )
)

# Define server logic ----
server <- function(input, output) {
  
  output$selected_var <- renderText({
    paste("You have selected", input$displayVariable)
  })
  
  output$selected_range <- renderText({
    paste("You have chosen a range that goes from", input$rangeInterest[1], "to", input$rangeInterest[2])
  })
  
  output$map <- renderPlot({
    data <- switch(input$displayVariable, 
                   "Percent White" = counties$white,
                   "Percent Black" = counties$black,
                   "Percent Hispanic" = counties$hispanic,
                   "Percent Asian" = counties$asian)
    
    color <- switch(input$displayVariable, 
                    "Percent White" = "darkgreen",
                    "Percent Black" = "coral",
                    "Percent Hispanic" = "blue",
                    "Percent Asian" = "deeppink")
    
    legend <- switch(input$displayVariable, 
                     "Percent White" = "% White",
                     "Percent Black" = "% Black",
                     "Percent Hispanic" = "% Hispanic",
                     "Percent Asian" = "% Asian")
    
    percent_map(data, color, legend, input$rangeInterest[1], input$rangeInterest[2])
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)

