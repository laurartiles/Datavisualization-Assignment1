### APP LESSON 2 ###

library(shiny)

# Define UI ----
ui <- fluidPage(
  titlePanel("My Shiny App"),
  sidebarLayout(
    sidebarPanel(
      h2("Installation"),
      p("Shiny is available on CRAN, so you can install it in the usual way from your R console:"),
      code("install.packages(\"shiny\")"),
      br(), br(), br(), br(),
      img(src = "rstudio.png", height = 70, width = 200),
      br(),
      p("Shiny is a product of", a("Rstudio"))
    ),
    mainPanel(
      h1("Introducing Shiny"),
      p("Shiny is a new package from RStudio that makes it ", em("incredibly easy"), " to build interactive web applications in R."),
      br(),
      p("For an introduction and live examples, visit the ", a("Shiny homepage.")),
      br(),
      h1("Features"),
      p("- Build useful web application with only a few lines of code - no JavaScript required."),
      p("- Shiny applications are automatically 'live' in the same way that ", 
        strong("spreadsheets"),
        " are live. Outputs change instantly as users modify inputs, without requiring a reload of the browser.")
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
}

# Run the app ----
shinyApp(ui = ui, server = server)

