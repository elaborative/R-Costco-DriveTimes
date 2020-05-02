library(shiny)
library(leaflet)
library(osrm)
library(rgdal)
library(sp)
library(sf)
library(viridis)
library(shinythemes)

install.packages("lwgeom")
library("lwgeom")
server <- function(input, output, session) {
  
  isoCoords <- reactive({
    coords <- c(lat = input$lat,
                lon = input$lon)
    coords
  })
  isochrone <- eventReactive(input$submit, {
    withProgress(message = 'Sending Request',
                 isochrone <- osrmIsochrone(loc = c(isoCoords()[['lon']],
                                                    isoCoords()[['lat']]),
                                            breaks = c(15,30,45,60),
                                            res = 30) %>%
                   st_as_sf()
    )
    isochrone
  })
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers[['OpenStreetMap']])
  })
  observeEvent(input$costco, {
    if(input$costco == "Medford"){
      updateTextInput(session, "lat", value = 42.3740)
      updateTextInput(session, "lon", value = -122.8879)
    }
    
    if(input$costco == "Roseburg"){
      updateTextInput(session, "lat", value = 43.2602)
      updateTextInput(session, "lon", value = -123.3486)
    }
    if(input$costco == "Eugene"){
      updateTextInput(session, "lat", value = 44.0890)
      updateTextInput(session, "lon", value = -123.0670)
    }
    if(input$costco == "Albany"){
      updateTextInput(session, "lat", value = 44.6387)
      updateTextInput(session, "lon", value = -123.0672)
    }
    if(input$costco == "Bend"){
      updateTextInput(session, "lat", value = 44.0570)
      updateTextInput(session, "lon", value = -121.2674)
    }
    if(input$costco == "Salem"){
      updateTextInput(session, "lat", value = 44.918813)
      updateTextInput(session, "lon", value = -122.995855)
    }
    if(input$costco == "Wilsonville"){
      updateTextInput(session, "lat", value = 45.332358)
      updateTextInput(session, "lon", value = -122.763590)
    }
    if(input$costco == "Hillsboro"){
      updateTextInput(session, "lat", value = 45.535738)
      updateTextInput(session, "lon", value = -122.935688)
    }
    if(input$costco == "Warrenton"){
      updateTextInput(session, "lat", value = 46.146220)
      updateTextInput(session, "lon", value = -123.914532)
    }
  })
  
  observeEvent(input$submit , {
    steps <- sort(as.numeric(c(15,30,45,60)))
    isochrone <- cbind(steps = steps[isochrone()[['id']]], isochrone())
    pal <- colorFactor(viridis::plasma(nrow(isochrone), direction = -1), 
                       isochrone$steps)
    leafletProxy("map") %>%
      clearShapes() %>% 
      clearMarkers() %>%
      clearControls() %>%
      addPolygons(data = isochrone,
                  weight = .5, 
                  color = ~pal(steps)) %>%
      addLegend(data = isochrone,
                pal = pal, 
                values = ~steps,
                title = 'Drive Time (min.)',
                opacity = 1) %>%
      addMarkers(lng = input$lon, input$lat) %>%
      setView(isoCoords()[['lon']], isoCoords()[['lat']], zoom = 9)
  })
  observe({
    leafletProxy("map") %>%
      clearTiles() %>%
      addProviderTiles(providers[["OpenStreetMap"]])
  })
  output$dlIsochrone <- downloadHandler(
    filename = function() {
      paste0("drivetime_isochrone", input$shapeOutput)
    },
    content = function(file) {
      st_write(isochrone(), file)
    }
  )
  
}

ui <- fluidPage(theme = shinytheme('paper'),
                  headerPanel(title = "Costco Drive Time Isochrone Map"),
                  sidebarLayout(
                    sidebarPanel(
                      
                      selectInput("costco",
                                  "Choose a Costco",
                                  c("Medford","Roseburg","Eugene","Albany","Bend","Salem","Wilsonville","Hillsboro","Warrenton")
                                  ),
                      numericInput("lat",
                                   "Latitude",
                                   41.2524,
                                   min = -90,
                                   max = 90),
                      numericInput("lon",
                                   "Longitude",
                                   -95.9980,
                                   min = -180,
                                   max = 180),
                      
                     
                      actionButton("submit", "Submit", 
                                   icon = icon("ok",
                                               lib = "glyphicon")),
                      br(),
                      br(),
                      tags$p("Powered by:"),
                      tags$a(href = 'http://project-osrm.org/', 
                             "Open Source Routing Machine")
                      
                     
                    ),
                    mainPanel(
                      leafletOutput("map", width="100%",height = 1200),
                      tags$p("Choose an Oregon City with a Costco location from the dropdown and click submit to start generating an isochrone with 15 minute intervals. Please BE PATIENT! The isochrone map overlay takes about 60-90 seconds to load. Original Programming for this UI is by by Thomas Roh. https://www.r-bloggers.com/shiny-app-drive-time-isochrones/ - Powered by Open Source Routing Machine - http://project-osrm.org/ - Adapted by Ryan J Cooper - I added the Costco lat/lng dropdowns and simplified the interface. ")
                      
                    )
                  )
)

shinyApp(ui = ui, server = server)
