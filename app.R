library(shiny)
library(shinythemes)
library(leaflet)
library(sf)
library(tidyverse)

## GLOBAL VARIABLES/DATA
#Import the Abandoned Property File
abandoned.properties <- st_read("Abandoned_Property_Parcels/Abandoned_Property_Parcels.shp", stringsAsFactors = FALSE)
#Import the Street Lights File
street.lights <- read.csv("Street_Lights.csv")
#Import the Park Locations File
parkLocDf.points <- read.csv("Parks_Locations_and_Features.csv")
#Import the Code Enforcement File
code.enforcement <- read.csv("Code_Enforcement_Cases.csv")
#Import the School Data
schools <- st_read("School_Boundaries/School_Boundaries.shp", stringsAsFactors = FALSE)

## ===========================================================================================================

## CINDY'S CODE - START
code.outcome.names <- unique(abandoned.properties$Outcome_St) 
code.outcome.names <- code.outcome.names[-5]

schools.types <- unique(schools$SchoolType)
pal <- colorFactor(topo.colors(5), code.outcome.names)
school.pal <- colorFactor(topo.colors(2), schools.types)

#Generate the Popup text for abandoned properties
abandoned.properties$Popup_Text <- paste("<b>Property Name: ", paste(abandoned.properties$Direction, abandoned.properties$Street_Nam, abandoned.properties$Suffix, sep = " "), "</b><br>",
                                         "Code Enforcement: ", abandoned.properties$Code_Enfor, sep=" ")
#Generate the Popup text for schools
schools$Popup_Text <- paste("<b>School Name: ", schools$School, "</b><br>",
                            "School Type: ", schools$SchoolType, sep=" ")
## CINDY's CODE END

## ===========================================================================================================

##AVISEK'S CODE - START

#Replace the missing values
abandoned.properties$Outcome_St <- ifelse(is.na(abandoned.properties$Outcome_St), 
                                 "No Category Yet", 
                                 abandoned.properties$Outcome_St)

#Import the Park Locations File
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
parkLocDf.spatial['icon'] <- sapply(strsplit(as.character(parkLocDf.spatial$Park_Type)," "), `[`, 1)

html_legend <- "<img src = 'https://drive.google.com/thumbnail?id=1RLYlQxoFoAzrdLcId-wZdDFqsZ9751KM' height='20' width='20'>Block <br/> <img src = 'https://drive.google.com/thumbnail?id=1FrKZx2CBM_ejsFRYXpg39-dWGVjd8TfG' height='20' width='20'>Cemetery <br/> <img src = 'https://drive.google.com/thumbnail?id=1nkbGqW-Hth9EuK-W0qknYRcfsH65JKKc' height='20' width='20'>Community <br/> <img src = 'https://drive.google.com/thumbnail?id=1-Vcwbd2EmNGm_oBu488hZg7cpUbSCbSb' height='20' width='20'>Golf Course <br/> <img src = 'https://drive.google.com/thumbnail?id=1SlEI6UZ2X9jKNvfxhmUSBEQgxPZNRNwv' height='20' width='20'>Memorial <br/> <img src='https://drive.google.com/thumbnail?id=1Masz0fderVFlcLN_bjS7RHDDDebkerd6' height='20' width='20'>Neighboorhood <br/> <img src = 'https://drive.google.com/thumbnail?id=1-UGYwT3g5XN0GhZGutCIndfQ0XbQ_ZLg' height='20' width='20'>Special <br/> <img src = 'https://drive.google.com/thumbnail?id=1IP8ieSyNkVQdsHwH3UTkOHokPzar61lb' height='20' width='20'>Zoo "

#Generate the Popup text
parkLocDf.spatial$Popup_Text <- paste("Park Name: ", parkLocDf.spatial$Park_Name, "<br>",
                                      "Park Type: ", parkLocDf.spatial$Park_Type, sep=" ")

#Generate the distance Dataframe
distanceDf<- as.data.frame(st_distance(parkLocDf.spatial, st_centroid(abandoned.properties)))
distanceDf <-  as.data.frame(sapply(distanceDf, as.numeric))
distanceDf <- round(distanceDf * 0.006, 2) # Convert Meters to Miles
distanceDf['Park_Name'] <- parkLocDf.spatial$Park_Name
distanceDf['Park_Type'] <- parkLocDf.spatial$Park_Type
names(distanceDf) <- c(c(1:1511), 'Park_Name', 'Park_Type')

##AVISEK'S CODE - END

## ===========================================================================================================

##########  UI 

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("flatly"), 
                
                titlePanel("Project - Data Visualization"),
                
                navbarPage(
                  "Header",
                  tabPanel("Tyler's Page",
                           h3("This is the first panel")
                  ),
                  tabPanel("Abandoned Property & Parks",
                           # Application title
                           titlePanel("Abandoned Property in Southbend, IN"),
                           fluidRow(
                             column(3,
                                    selectInput(inputId = "abanType", 
                                                label = "Type of Abandoned Property", 
                                                choices = unique(abandoned.properties$Outcome_St),
                                                selected = "Demolished"),
                                    checkboxInput("park", "Overlay Parks"),
                                    conditionalPanel(condition = "input.park == 1", 
                                                     selectInput("overlayType", "Overlay Type",
                                                                 choices = c("All Parks", "Selected Park"),
                                                                 selected = "All Parks"),
                                                     conditionalPanel(condition = "input.park == 1 & 
                                                           input.overlayType == 'Selected Park'",
                                                            shinydashboard::box(width = 12, 
                                                                title = "Select Park Name & Distance",
                                                                  selectInput("parkName", "Park Name", 
                                                                              choices = parkLocDf.spatial$Park_Name,
                                                                              selected = NULL),
                                                                  sliderInput(inputId = "distance", 
                                                                               label = "Distance in Miles", 
                                                                               min = 1, max = 40, 
                                                                               value = 10),
                                                                               htmlOutput("parkDetails")
                                                                      )
                                                     )# End of Condition panel
                                    )# End of Condition panel
                             ), # #End of First Column
                             column(9,
                                    leafletOutput("leafLetPlot", width = "100%", height = 600),
                                    dataTableOutput("propertydata")
                             ) #End of Second Column
                           ) #end of Row
                  ),
                  tabPanel("Ben's Page",
                           h3("This is the third panel")
                  ),
                  tabPanel("Abandoned Properties & Schools",
                           sidebarLayout(
                             sidebarPanel(
                               checkboxGroupInput(inputId = "code.outcome", 
                                                  label = "Select a Code Outcome", 
                                                  choices = code.outcome.names, 
                                                  selected = code.outcome.names, 
                                                  inline = FALSE,
                                                  width = NULL, 
                                                  choiceNames = NULL, 
                                                  choiceValues = NULL),
                               checkboxGroupInput(inputId = "schools.types",
                                                  label = "Select a School Type",
                                                  choices = schools.types, 
                                                  selected = schools.types)
                             ), #end sidebarPanel
                             # Show a leaflet map
                             mainPanel(
                               leafletOutput(outputId = "code.outcome.map")
                             )
                           )
                  )
                )
)

##########  SERVER 

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #CINDY'S CODE
  properties.subset <- reactive({
    return(abandoned.properties[abandoned.properties$Outcome_St %in% input$code.outcome,])
  })
  schools.subset <- reactive({
    return(schools[schools$SchoolType %in% input$schools.types,])
  })
  output$code.outcome.map <- renderLeaflet({
    leaflet() %>%
      addTiles()%>%
      addPolygons(data = abandoned.properties, popup = ~Popup_Text,color = ~pal(properties.subset()$Outcome_St)) %>%
      addPolygons(data = schools, popup = ~Popup_Text, color = ~school.pal(schools.subset()$SchoolType))# %>%
    # addLegend("bottomright", pal = ~pal(properties.subset()$Outcome_St), values = code.outcome.names,
    #           title = "Code Enforcement Legend",
    #           opacity = 1
    # )
  })
  
  #AVISEK'S CODE - Start
  observe({
    
    #Capture the Inputs
    aban_type <- input$abanType  
    overlay_park <- input$park
    overlay_type <- input$overlayType
    park_name <- input$parkName
    distance <- input$distance
    
    #Filter the Distance Data Frame
    modDistanceDf <- distanceDf[which(parkLocDf.spatial$Park_Name == park_name), ] %>% 
      select(- c("Park_Name", "Park_Type"))
    distanceVec <- as.numeric(modDistanceDf)
    names(distanceVec) <- c(1:1511)
    
    selectedProp <- abandoned.properties[names(distanceVec[distanceVec <= distance]), ]
    selectedProp$Distance <- distanceVec[distanceVec <= distance]
    
    output$leafLetPlot <- renderLeaflet({
      
      if(overlay_park){
        if(overlay_type == 'All Parks') {
          #print("You selected All Parks")
          leaflet()  %>%
            addTiles()  %>%
            # addMarkers(data = abandoned.properties %>% st_centroid(), popup = ~Outcome_St )
            addPolygons(data = abandoned.properties %>%
                          filter(Outcome_St == input$abanType),
                        popup = ~Outcome_St) %>%
            addMarkers(data = parkLocDf.spatial,
                       popup = ~Popup_Text,
                       icon = ~parkIcons[icon])  %>%
            addControl(html = html_legend, position = "bottomleft")
        }else{
          leaflet()  %>%
            addTiles()  %>%
            addPolygons(data = selectedProp %>%
                          filter(Outcome_St == input$abanType),
                        popup = ~Outcome_St) %>%
            addMarkers(data = parkLocDf.spatial %>%
                         filter(Park_Name == park_name),
                       popup = ~Popup_Text,
                       icon = ~parkIcons[icon])  %>%
            addControl(html = html_legend, position = "bottomleft")
        }
        
      } else{
        #Add tiles and Add Markers
        leaflet()  %>% 
          addTiles()  %>% 
          addPolygons(data = abandoned.properties %>% 
                        filter(Outcome_St == aban_type), 
                      popup = ~Outcome_St)
      }
    })
    
    
    if(overlay_park){
      if(overlay_type == 'All Parks') {
        data <- abandoned.properties %>%
          filter(Outcome_St == aban_type) %>% 
          select(Outcome_St, Address_Nu, Street_Nam, Zip_Code) %>% 
          rename(Property_Type = Outcome_St,
                 Street_Num = Address_Nu,
                 Street_Name = Street_Nam) %>% 
          st_set_geometry(NULL)
      }else{
        data<- selectedProp %>%
          filter(Outcome_St == aban_type) %>% 
          select(Outcome_St, Address_Nu, Street_Nam, Zip_Code, Distance) %>% 
          rename(Property_Type = Outcome_St,
                 Street_Num = Address_Nu,
                 Street_Name = Street_Nam) %>% 
          st_set_geometry(NULL)
      }
    }else{
      data <- abandoned.properties %>%
        filter(Outcome_St == aban_type) %>% 
        select(Outcome_St, Address_Nu, Street_Nam, Zip_Code) %>% 
        rename(Property_Type = Outcome_St,
               Street_Num = Address_Nu,
               Street_Name = Street_Nam) %>% 
        st_set_geometry(NULL)
    }
    
    #Datatable
    output$propertydata <- renderDataTable (
      data, rownames = NULL, width = 200, height = 200,
      options = list(scrollX = TRUE, scrollY = TRUE)
    )
    
    park.data <-  parkLocDf.spatial %>%
      filter(Park_Name == park_name) %>% 
      select(Park_Name, Park_Type, Address)
    
    output$parkDetails <- renderUI({ 
      str1 <- paste0("Park Name: ", park.data$Park_Name)
      str2 <- paste0("Park Type: ", park.data$Park_Type)
      str3 <- paste0("Address: ", park.data$Address)
      HTML(paste(str1, str2, str3, sep = '<br/>'))
    })
    
  })
  
  #AVISEK'S CODE - End
  

}

##########  APP

# Run the application 
shinyApp(ui = ui, server = server)