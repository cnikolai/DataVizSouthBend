library(shiny)
library(shinythemes)
library(leaflet)
library(sf)
library(tidyverse)
library(RColorBrewer)
library(DT)
library(scales)

## GLOBAL VARIABLES/DATA
#setwd("/Users/cindy/Documents/ND MS Data Science/Data Viz/DataVizSouthBend")
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
#Import City Council Data
council <- st_read("City_Council_Districts/City_Council_Districts.shp", stringsAsFactors = FALSE)

## ===========================================================================================================

## CINDY'S CODE - START
code.outcome.names <- unique(abandoned.properties$Outcome_St) 
code.outcome.names <- code.outcome.names[-5]

schools.types <- unique(schools$SchoolType)
school.pal <- colorFactor(c("#fac125","#7d5f0b"), schools.types)
council.pal <- colorFactor("#828b99",council$Dist)


#Generate the Popup text for abandoned properties
abandoned.properties$Popup_Text <- paste("<b>Property Name: ", ifelse(is.na(abandoned.properties$Direction),paste(abandoned.properties$Street_Nam, abandoned.properties$Suffix, sep = " "),paste(abandoned.properties$Direction, abandoned.properties$Street_Nam, abandoned.properties$Suffix, sep = " ")), "</b><br>",
                                         "Code Enforcement: ", abandoned.properties$Code_Enfor, sep=" ")
#Generate the Popup text for schools
schools$Popup_Text <- paste("<b>School Name: ", schools$School, "</b><br>",
                            "School Type: ", schools$SchoolType, sep=" ")

#Generate the Popup text for districts
council$Popup_Text <- paste("<b>District Name: ", council$Dist, "</b><br>")

#Dropdown menu
selectedSchool <- sort(schools$School, decreasing = FALSE)
  #arrange(toString(schools$School))
  #sort(schools$School)

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

## TYLER's CODE - START

## ===========================================================================================================

# Clean up street.lights

#Fix Bulb Type
street.lights$Bulb_Type <- street.lights$Bulb_Type %>% str_replace("^(Yellow)?[-\\s]*[hH][\\s\\.]*[pP][\\s\\.]*S[op]dium[\\s]*$","Yellow - H.P. Sodium")
street.lights$Bulb_Type <- street.lights$Bulb_Type %>% str_replace("HP[sS]","Yellow - H.P. Sodium")
street.lights$Bulb_Type <- street.lights$Bulb_Type %>% str_replace("^$","Unknown")

#Fix Service Type
street.lights$Service <- street.lights$Service %>% str_replace("^[\\s]*$","Unknown")

#Fix Ownership
street.lights$Ownership <- street.lights$Ownership %>% str_replace("^[\\s]*$","Unknown")

#Fix Lumens
street.lights$LumensClass <- street.lights$Lumens
street.lights$LumensClass <- street.lights$LumensClass %>% str_replace("^9500$","K")
street.lights$LumensClass <- street.lights$LumensClass %>% str_replace("^50000$","D")
street.lights$LumensClass <- street.lights$LumensClass %>% str_replace("[\\s]?-.*$","")
street.lights$LumensClass <- street.lights$LumensClass %>% str_replace("^$","Unknown")
street.lights$LumensClass <- street.lights$LumensClass %>% str_to_title()


street.lights$Lumens <- street.lights$Lumens %>% str_replace("^[a-zA-Z\\-\\s]*","")
street.lights$Lumens <- street.lights$Lumens %>% str_replace("[\\s]?L.*$","")
street.lights$Lumens <- street.lights$Lumens %>% str_replace(",","")
street.lights$Lumens <- street.lights$Lumens %>% str_replace("^$","50000")
street.lights$Lumens <- street.lights$Lumens %>% str_replace("^500000$","50000")
street.lights$Lumens <- street.lights$Lumens %>% as.integer()

street.lights$LumensClass <- paste(street.lights$LumensClass," (",street.lights$Lumens,")",sep="")

street.lights.lumens.name <- unique(street.lights$LumensClass)


street.lights.spatial <- street.lights %>%
  st_as_sf(coords = c("Lon","Lat")) %>%
  st_set_crs(value = 4326)


## ===========================================================================================================

## TYLER's CODE - END

## ===========================================================================================================

## BEN'S CODE - START

# Limit data to no older than 2013, to match abandoned property data.
# Select the Code violations most closely related to homes
small_code <- code.enforcement %>% 
  filter(Case_Year>=13) %>% 
  filter(Case_Type_Code_Description=='HOUSING REPAIR' | 
           Case_Type_Code_Description=='ENVIRONMENTAL MOWING' | 
           Case_Type_Code_Description=='ZONING VIOLATIONS')

# Create spacial data and assign coordinates
code.enforcement.spatial <- small_code %>%
  st_as_sf(coords  = c("Lon","Lat"))%>%
  st_set_crs(value = 4326)

# Create color palettes
pal1 <- colorFactor(palette = 'Set1', domain =code.enforcement.spatial$Case_Type_Code_Description)
pal2 <- colorFactor(palette = 'Dark2', domain =abandoned.properties$Outcome_St)
pal3 <- colorFactor(palette = 'Accent', domain=council$Name)

colors <- c('goldenrod', 'darkorchid', 'blue', 'orangered', 'deeppink','slategray')

# Create City Council name popup
council$popup <- paste("<b>",council$Council_Me,"</b><br>",
                       "District: ",council$Dist,"<br>")

# Concatenate District number with Councilmen
council$Name <- paste(council$Dist,council$Council_Me,sep="-")

# Join abandoned properties with city council districts
council.properties <- st_join(x = abandoned.properties, y = council %>% select(Name))

# Create lists of unique values for filtering
code.violation.type <- unique(code.enforcement.spatial$Case_Type_Code_Description)
abandoned.outcome <- unique(abandoned.properties$Outcome_St)
council.dist <- unique(council.properties$Name)

## BEN'S CODE - END

## ===========================================================================================================
##########  UI 

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("flatly"), 
                
                titlePanel("South Bend Abandoned Properties"),
                
                navbarPage(
                  "",
                  tabPanel("Abandoned Properties & Streetlights", #Tyler's Page
                           titlePanel("Streetlights of South Bend, IN"),
                           sidebarLayout(
                             sidebarPanel(
                               checkboxGroupInput(
                                 inputId = "streetlights.lumens",
                                 label = "Select Streetlight Brightness",
                                 choices = street.lights.lumens.name,
                                 selected = street.lights.lumens.name #select all by default,
                               ), # End checkboxGroupInput for streetlights.lumens
                               checkboxGroupInput(inputId = "code.outcome", 
                                                  label = "Select a Code Outcome", 
                                                  choices = code.outcome.names, 
                                                  selected = code.outcome.names, 
                                                  inline = FALSE,
                                                  width = NULL, 
                                                  choiceNames = NULL, 
                                                  choiceValues = NULL
                               ) # End checkboxGroupInput for code outcome
                             ), # End SidebarPanel
                             mainPanel(
                               leafletOutput(outputId = "streetsLeaflet")
                             )
                           ) # End SidebarLayout
                           
                  ), # End Tyler's Page
                  tabPanel("Abandoned Property & Parks",
                           # Application title
                           titlePanel("Abandoned Property in South Bend, IN"),
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
                  tabPanel("City Council Districts", # Ben's Page - Start
                           fluidRow(
                             column(3,
                               checkboxGroupInput(inputId = "abandoned.outcome", 
                                                  label = "Select an Abandonment Outcome", 
                                                  choices = abandoned.outcome, 
                                                  selected = abandoned.outcome, 
                                                  inline = FALSE,
                                                  width = NULL, 
                                                  choiceNames = NULL, 
                                                  choiceValues = NULL
                               ),# End checkboxGroupInput for abandoned outcome
                               checkboxGroupInput(inputId = "council.dist",
                                                  label = "Select a City Council District",
                                                  choices = council.dist, 
                                                  selected = council.dist) 
                             ), # End SidebarPanel
                             column(9,
                               leafletOutput(outputId = "codesLeaflet", width = "100%", height = 600),
                               dataTableOutput('councilTable')
                             )
                           ) # End SidebarLayout
                  ), # Ben's Page - End
                  tabPanel("Abandoned Properties & Schools",
                           sidebarLayout(
                             sidebarPanel(
                               # fluidRow(
                               #   column(3,
                               checkboxGroupInput(inputId = "code.outcome2", 
                                                  label = "Type of Abandoned Property", 
                                                  choices = code.outcome.names, 
                                                  selected = code.outcome.names, 
                                                  ),
                               checkboxGroupInput(inputId = "schools.types",
                                                  label = "School Type",
                                                  choices = schools.types),
                               checkboxInput(inputId = "citydistricts", "Overlay City Districts"),
                               selectInput(inputId = "school.names", 
                                           label = "School Name", 
                                           choices = c("Select One",selectedSchool))
                               #uiOutput("schoolnames")
                             ), #end sidebarPanel
                             # Show a leaflet map
                               # )),
                             # fluidRow(
                             #   column(9,
                             mainPanel(
                               leafletOutput(outputId = "code.outcome.map", width = "100%", height = 600)
                             )
                               # ))
                           )
                  )
                )
)

##########  SERVER 

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  ########################################     CINDY'S CODE
  properties.subset <- reactive({
    return(abandoned.properties[abandoned.properties$Outcome_St %in% input$code.outcome2,])
  })
  schools.subset <- reactive({
    return(schools[schools$SchoolType %in% input$schools.types,])
  })
  schools.subset.single <- reactive({
    return(schools[schools$School %in% input$school.names,])
  })
  # council.subset <- reactive({
  #   return(council[council$Dist %in% input$council,])
  # })

  observe({
    
    #Capture the Inputs
    city_districts <- input$citydistricts
    
  output$code.outcome.map <- renderLeaflet({
   if(city_districts){
       leaflet() %>%
       setView(zoom = 12, lat = 41.6764, lng = -86.2520) %>%
       addTiles()%>%
       addPolygons(data = abandoned.properties,
                   popup = ~Popup_Text,
                   color = ~pal2(properties.subset()$Outcome_St)) %>%
       addPolygons(data = schools,
                   popup = ~Popup_Text,
                   color = ~school.pal(schools.subset()$SchoolType)) %>%
       addPolygons(data = council,
                   fillOpacity = 0.2,
                   popup = ~Popup_Text,
                   color = ~council.pal(council.dist) 
       ) %>%
       addPolygons(data = council,
                   fillOpacity = 0.2,
                   popup = ~Popup_Text,
                   color = ~council.pal(council.dist)) %>%
       addLegend("bottomright", pal = pal2, values = code.outcome.names,
                 title = "Abandoned Property Legend",
                 opacity = 1
       ) %>%
       addLegend("bottomright", pal = school.pal, values = schools.types,
                 title = "School Type Legend",
                 opacity = 1
       )
   }else{
     if(input$school.names != "Select One") {
       leaflet() %>%
         setView(zoom = 12, lat = 41.6764, lng = -86.2520) %>%
         addTiles()%>%
         addPolygons(data = abandoned.properties, 
                     popup = ~Popup_Text,
                     color = ~pal2(properties.subset()$Outcome_St)) %>%
         addPolygons(data = schools, 
                     popup = ~Popup_Text, 
                     color = ~school.pal(schools.subset()$SchoolType)) %>%
         addLegend("bottomright", pal = pal2, values = code.outcome.names,
                   title = "Abandoned Property Legend",
                   opacity = 1
         ) %>%
         addLegend("bottomright", pal = school.pal, values = schools.types,
                   title = "School Type Legend",
                   opacity = 1
         ) %>%
       addPolygons(data = schools %>% filter(schools$School == input$school.names), 
                   popup = ~Popup_Text, 
                   color = "#3693eb") 
     }#end if
     else {
    leaflet() %>%
      setView(zoom = 12, lat = 41.6764, lng = -86.2520) %>%
      addTiles()%>%
      addPolygons(data = abandoned.properties, 
                  popup = ~Popup_Text,
                  color = ~pal2(properties.subset()$Outcome_St)) %>%
      addPolygons(data = schools, 
                  popup = ~Popup_Text, 
                  color = ~school.pal(schools.subset()$SchoolType)) %>%
      addLegend("bottomright", pal = pal2, values = code.outcome.names,
                title = "Abandoned Property Legend",
                opacity = 1
      ) %>%
      addLegend("bottomright", pal = school.pal, values = schools.types,
                title = "School Type Legend",
                opacity = 1
      )
     
     }#end else
    }# end else
  })#end render leaflet
  }) #end observe
  
  observeEvent(input$school.names, {
    #print("observe")
    updateCheckboxGroupInput(session,"schools.types", selected = c("",""))
    proxy <- leafletProxy("code.outcome.map")
    proxy %>% clearShapes() 
    proxy %>% addPolygons(data = abandoned.properties, 
                          popup = ~Popup_Text,
                          color = ~pal2(properties.subset()$Outcome_St))
    proxy %>% addPolygons(data = schools %>% filter(schools$School == input$school.names), 
                          popup = ~Popup_Text, 
                          color = "#3693eb") 
    if(input$citydistricts) {
        proxy %>% addPolygons(data = council,
                          fillOpacity = 0.2,
                          popup = ~Popup_Text,
                          color = ~council.pal(council.dist)
        )#end add polygons
    }#end if statement
  }) #end observe event 
  
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
      dplyr::select(- c("Park_Name", "Park_Type"))
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
                        color = ~pal2(Outcome_St),
                        popup = ~Outcome_St) %>%
            addMarkers(data = parkLocDf.spatial,
                       popup = ~Popup_Text,
                       icon = ~parkIcons[icon])  %>%
            addControl(html = html_legend, position = "bottomleft") %>% 
            addLegend("bottomright", pal = pal2, 
                      values = abandoned.properties$Outcome_St,
                      title = "Abandoned Property Legend",
                      opacity = 1)
        }else{
          leaflet()  %>%
            addTiles()  %>%
            addPolygons(data = selectedProp %>%
                        filter(Outcome_St == input$abanType),
                        color = ~pal2(Outcome_St),
                        popup = ~Outcome_St) %>%
            addMarkers(data = parkLocDf.spatial %>%
                       filter(Park_Name == park_name),
                       popup = ~Popup_Text,
                       icon = ~parkIcons[icon])  %>%
            addControl(html = html_legend, position = "bottomleft") %>% 
            addLegend("bottomright", pal = pal2, 
                      values = abandoned.properties$Outcome_St,
                      title = "Abandoned Property Legend",
                      opacity = 1)
        }
        
      } else{
        #Add tiles and Add Markers
        leaflet()  %>% 
          addTiles()  %>% 
          addPolygons(data = abandoned.properties %>% 
                      filter(Outcome_St == aban_type), 
                      color = ~pal2(Outcome_St),
                      popup = ~Outcome_St) %>% 
          addLegend("bottomright", pal = pal2, 
                  values = abandoned.properties$Outcome_St,
                  title = "Abandoned Property Legend",
                  opacity = 1)
      }
    })
    
    
    if(overlay_park){
      if(overlay_type == 'All Parks') {
        data <- abandoned.properties %>%
          filter(Outcome_St == aban_type) %>% 
          dplyr::select(Outcome_St, Address_Nu, Street_Nam, Zip_Code) %>% 
          rename(Property_Type = Outcome_St,
                 Street_Num = Address_Nu,
                 Street_Name = Street_Nam) %>% 
          st_set_geometry(NULL)
      }else{
        data<- selectedProp %>%
          filter(Outcome_St == aban_type) %>% 
          dplyr::select(Outcome_St, Address_Nu, Street_Nam, Zip_Code, Distance) %>% 
          rename(Property_Type = Outcome_St,
                 Street_Num = Address_Nu,
                 Street_Name = Street_Nam) %>% 
          st_set_geometry(NULL)
      }
    }else{
      data <- abandoned.properties %>%
        filter(Outcome_St == aban_type) %>% 
        dplyr::select(Outcome_St, Address_Nu, Street_Nam, Zip_Code) %>% 
        rename(Property_Type = Outcome_St,
               Street_Num = Address_Nu,
               Street_Name = Street_Nam) %>% 
        st_set_geometry(NULL)
    }
    
    #Datatable
    output$propertydata <- DT::renderDataTable (
      data, rownames = NULL, width = 200, height = 200,
      options = list(scrollX = TRUE, scrollY = TRUE)
    )
    
    park.data <-  parkLocDf.spatial %>%
      filter(Park_Name == park_name) %>% 
      dplyr::select(Park_Name, Park_Type, Address)
    
    output$parkDetails <- renderUI({ 
      str1 <- paste0("Park Name: ", park.data$Park_Name)
      str2 <- paste0("Park Type: ", park.data$Park_Type)
      str3 <- paste0("Address: ", park.data$Address)
      HTML(paste(str1, str2, str3, sep = '<br/>'))
    })
    
  })
  
  #AVISEK'S CODE - End
  
  #TYLER'S CODE - START
  observe({
    streetlights.lumens <- input$streetlights.lumens
    code.outcome <- input$code.outcome
    
    output$streetsLeaflet <- renderLeaflet({
      leaflet() %>%
        setView(zoom = 12, lat = 41.6764, lng = -86.2520) %>%
        addProviderTiles(providers$Stamen.TonerLite) %>%
        addPolygons(
          weight=1,
          # layerId = abandoned.properties$geometry,
          # group = abandoned.properties$Code_Enfor,
          data = abandoned.properties %>% 
            filter(Outcome_St %in% code.outcome)
        ) %>% addCircles(
          stroke = 0,
          fillOpacity = 0.2,
          radius = ~ sqrt(Lumens/20),
          color = "green",
          group = street.lights.spatial$LumensClass,
          data = street.lights.spatial %>% 
            filter(LumensClass %in% streetlights.lumens) 
        )# %>%
      # addLayersControl(
      #   overlayGroups = c(
      #     street.lights.spatial$LumensClass
      #   ),
      #   options = layersControlOptions(collapsed = F)
      # )
    })
  })
  
  # BEN'S CODE - START
  observe({
    abandon <- input$abandoned.outcome
    council_in <- input$council.dist
    council_len <- dim(abandoned.properties %>% 
                      filter(Outcome_St %in% abandon))[1]
    
    # Start Leaflet
    output$codesLeaflet <- renderLeaflet({
      leaflet()  %>%
        setView(zoom = 12, lat = 41.6764, lng = -86.2520) %>%
        addProviderTiles(providers$Stamen.TonerLite) %>%
        addPolygons(data = abandoned.properties %>% 
                      filter(Outcome_St %in% abandon), 
                    color = ~pal2(Outcome_St)) %>% 
        addPolygons(data = council %>% 
                    filter(Name %in% council_in), 
                    color = colors,
                    fillOpacity = 0.04,
                    popup = ~popup) %>% 
        addLegend("bottomright", pal = pal2, values = abandoned.properties$Outcome_St,
                  title = "Abandoned Property Legend",
                  opacity = 1)
    }) # End Leaflet
    
    # Start Table
    council_table <- council.properties %>%
      filter(Outcome_St %in% abandon & Name %in% council_in) %>%
      st_set_geometry(NULL) %>%
      group_by(Outcome = Outcome_St, Name) %>%
      summarize(`Abandonded Property Count` = n()) %>% 
      mutate(`Percent of All Abandonded Properties` = percent(`Abandonded Property Count`/council_len)) %>% 
      arrange(desc(`Abandonded Property Count`))
    
    output$councilTable <- DT::renderDataTable (
      council_table, rownames = NULL, width = 200, height = 200,
      options = list(scrollX = TRUE, scrollY = TRUE, 
                     columnDefs = list(list(className = "dt-center", targets = 2:3)))
    ) # End table
  })
  # BEN'S CODE - END
  
}

##########  APP

# Run the application 
shinyApp(ui = ui, server = server)