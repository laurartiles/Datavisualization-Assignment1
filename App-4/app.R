### APP LESSON 4 ###

library(shiny)

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
      textOutput("selected_range")
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
}

# Run the app ----
shinyApp(ui = ui, server = server)

