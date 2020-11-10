library(shiny)
#install.packages("shinythemes")
library(shinythemes)
library(sf)

#global variables
#setwd("SouthBend") #for console only
abandoned.properties <- st_read("Abandoned_Property_Parcels.shp")
street.lights <- read.csv("Street_Lights.csv")
parks <- read.csv("Public_Facilities.csv")
code.enforcement <- read.csv("Code_Enforcement_Cases.csv")
schools <- st_read("School_Boundaries.shp")


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("flatly"), 
    
    titlePanel("Navlist panel example"),
    
    navlistPanel(
        "Header",
        tabPanel("Tyler's Page",
                 h3("This is the first panel")
        ),
        tabPanel("Avisek's Page",
                 h3("This is the second panel")
        ),
        tabPanel("Ben's Page",
                 h3("This is the third panel")
        ),
        tabPanel("Cindy's Page",
                 h3("This is the fourth panel"),
                 # Sidebar with a slider input for number of bins 
                 sidebarLayout(
                     sidebarPanel(
                         sliderInput("bins",
                                     "Number of bins:",
                                     min = 1,
                                     max = 50,
                                     value = 30)
                     ),
                     
                     # Show a plot of the generated distribution
                     mainPanel(
                         plotOutput("distPlot")
                     )
                 )
        )
    )
)



# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
