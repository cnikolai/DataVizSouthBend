library(shiny)
#install.packages("shinythemes")
library(shinythemes)
library(sf)
library(leaflet)
library(tidyverse)

#global variables
#setwd("/Users/Cindy/Documents/ND\ MS\ Data\ Science/Data\ Viz/DataVizSouthBend/SouthBend") #for console only
#setwd("SouthBend") #for console only
abandoned.properties <- st_read("Abandoned_Property_Parcels.shp", stringsAsFactors = FALSE)
street.lights <- read.csv("Street_Lights.csv")
parks <- read.csv("Public_Facilities.csv")
code.enforcement <- read.csv("Code_Enforcement_Cases.csv")
schools <- st_read("School_Boundaries.shp", stringsAsFactors = FALSE)

#for Cindy's code
abandoned.properties.no.nas <- abandoned.properties
code.outcome.names <- unique(abandoned.properties.no.nas$Outcome_St)
code.outcome.names
schools.no.nas <- schools
schools.types <- unique(schools.no.nas$SchoolType)
schools.types
pal <- colorFactor(topo.colors(5), code.outcome.names)
school.pal <- colorFactor(topo.colors(2), schools.types)


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
                     #checkboxInput(inputId = "code.outcome",
                      #             label = "Code Outcome 1", value = FALSE, width = NULL)
                     checkboxGroupInput(inputId = "code.outcome", 
                                        label = "Select a Code Outcome", 
                                        choices = code.outcome.names, 
                                        selected = NULL, 
                                        inline = FALSE,
                                        width = NULL, 
                                        choiceNames = NULL, 
                                        choiceValues = NULL),
                     checkboxGroupInput(inputId = "schools.types",
                                        label = "Select a School Type",
                                        choices = c("Public", "Private"))
                     ), #end sidebarPanel
                     # checkboxGroupInput(inputId = "schools.types", label = "Select a School Type", choices = schools.types, selected = NULL,
                     #                  inline = FALSE, width = NULL, choiceNames = NULL, choiceValues = NULL)

                 
                     
                     # Show a plot of the generated distribution
                     mainPanel(
                         leafletOutput(outputId = "code.outcome.map")
                     )
                 )
        )
    )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
    code.subset <- reactive({
        return(abandoned.properties[abandoned.properties$Outcome_St %in% input$code.outcome,])
    })
    output$code.outcome.map <- renderLeaflet({
        leaflet()%>%
            addTiles()%>%
            addPolygons(data = abandoned.properties, color = ~pal(code.subset()$Outcome_St)) 
            #addPolygons(data = schools, color = ~school.pal(code.subset()))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
