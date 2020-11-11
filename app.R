
library(shiny)
library(shinythemes)
library(leaflet)
library(sf)
library(tidyverse)



##AVISEK'S CODE - START
staticMapDf <- st_read("Abandoned_Property_Parcels/Abandoned_Property_Parcels.shp",
                       stringsAsFactors = FALSE)
#Replace the missing values
staticMapDf$Outcome_St <- ifelse(is.na(staticMapDf$Outcome_St), 
                                 "No Category Yet", 
                                 staticMapDf$Outcome_St)

#Import the Prk Locations File
parkLocDf.points <- read.csv("Parks_Locations_and_Features.csv")
#Generate the Coordinates
parkLocDf.spatial <- parkLocDf.points %>% #projecting the table as an sf and setting the coordinate system
    st_as_sf(coords = c("Lon","Lat")) %>% 
    st_set_crs(value = 4326)

#Generate the Icon List
parkIcons <- iconList(
    Neighborhood = makeIcon(iconUrl = "Icons/Neighborhood Park.png",  iconWidth = 25, iconHeight = 25),
    Zoo = makeIcon(iconUrl = "Icons/Zoo.png", iconWidth = 25, iconHeight = 25),
    Community = makeIcon(iconUrl = "Icons/Community Park.png", iconWidth = 25, iconHeight = 25),
    Special = makeIcon(iconUrl = "Icons/Special.png", iconWidth = 25, iconHeight = 25),
    Memorial = makeIcon(iconUrl = "Icons/Memorial.png", iconWidth = 25, iconHeight = 25),
    Block = makeIcon(iconUrl = "Icons/Block Park.png", iconWidth = 25, iconHeight = 25),
    Cemetery = makeIcon(iconUrl = "Icons/Cemetery.png", iconWidth = 25, iconHeight = 25),
    Golf = makeIcon(iconUrl = "Icons/Golf Course.png", iconWidth = 25, iconHeight = 25)
)

#Assign the Icon
parkLocDf.spatial['icon'] <- sapply(strsplit(parkLocDf.spatial$Park_Type," "), `[`, 1)

html_legend <- "<img src = 'https://drive.google.com/thumbnail?id=1RLYlQxoFoAzrdLcId-wZdDFqsZ9751KM' height='20' width='20'>Block <br/> <img src = 'https://drive.google.com/thumbnail?id=1FrKZx2CBM_ejsFRYXpg39-dWGVjd8TfG' height='20' width='20'>Cemetery <br/> <img src = 'https://drive.google.com/thumbnail?id=1nkbGqW-Hth9EuK-W0qknYRcfsH65JKKc' height='20' width='20'>Community <br/> <img src = 'https://drive.google.com/thumbnail?id=1-Vcwbd2EmNGm_oBu488hZg7cpUbSCbSb' height='20' width='20'>Golf Course <br/> <img src = 'https://drive.google.com/thumbnail?id=1SlEI6UZ2X9jKNvfxhmUSBEQgxPZNRNwv' height='20' width='20'>Memorial <br/> <img src='https://drive.google.com/thumbnail?id=1Masz0fderVFlcLN_bjS7RHDDDebkerd6' height='20' width='20'>Neighboorhood <br/> <img src = 'https://drive.google.com/thumbnail?id=1-UGYwT3g5XN0GhZGutCIndfQ0XbQ_ZLg' height='20' width='20'>Special <br/> <img src = 'https://drive.google.com/thumbnail?id=1IP8ieSyNkVQdsHwH3UTkOHokPzar61lb' height='20' width='20'>Zoo "

#Generate the Popup text
parkLocDf.spatial$Popup_Text <- paste("Park Name: ", parkLocDf.spatial$Park_Name, "<br>",
                                      "Park Type: ", parkLocDf.spatial$Park_Type, sep=" ")

##AVISEK'S CODE - END


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("flatly"), 
                
                titlePanel("Project - Data Visualisation"),
                
                navbarPage(
                    "Header",
                    tabPanel("Tyler's Page",
                             h3("This is the first panel")
                    ),
                    tabPanel("Abandoned Property & Parks",
                             # Application title
                             titlePanel("Abandoned Property in Southbend, IN"),
                             
                             sidebarLayout(
                                 sidebarPanel(
                                     selectInput(inputId = "abanType", 
                                                 label = "Type of Abandoned Property", 
                                                 choices = unique(staticMapDf$Outcome_St),
                                                 selected = "Demolished"
                                     ),
                                     checkboxInput("park", "Overlay Parks")
                                 ), # end sidebar panel
                                 
                                 # Show a plot of the generated distribution
                                 mainPanel(
                                     leafletOutput("leafLetPlot", width = "100%", height = 600)
                                 )
                             ) #end of sidebar layout
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
    
    output$leafLetPlot <- renderLeaflet({
        
        if(input$park){
            #Add tiles and Add Markers
            leaflet()  %>% 
                addTiles()  %>% 
                # addMarkers(data = staticMapDf %>% st_centroid(), popup = ~Outcome_St )
                addPolygons(data = staticMapDf %>% 
                                filter(Outcome_St == input$abanType), 
                            popup = ~Outcome_St)%>% 
                addMarkers(data = parkLocDf.spatial, 
                           popup = ~Popup_Text,
                           icon = ~parkIcons[icon])  %>%
                addControl(html = html_legend, position = "bottomleft")
            
        } else{
            #Add tiles and Add Markers
            leaflet()  %>% 
                addTiles()  %>% 
                addPolygons(data = staticMapDf %>% 
                                filter(Outcome_St == input$abanType), 
                            popup = ~Outcome_St)
        }
    })
    
    # observe({
    #     map <- leafletProxy("leafLetPlot") %>% 
    #         clearControls()
    #     
    #     if (input$park) {
    #         map <- map %>%
    #             addControl(html = html_legend, 
    #                        position = "bottomleft") }
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)