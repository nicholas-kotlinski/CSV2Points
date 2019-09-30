# Code Stolen from: https://stackoverflow.com/questions/47937427/leaflet-map-responsive-to-csv-fileinput-in-shiny
# Add Bing Map: https://bhaskarvk.github.io/leaflet.extras/reference/addBingTiles.html#see-also
# Maybe add this someday?: https://gist.github.com/psychemedia/9737637

list.of.packages <- c("shiny", "dplyr","raster", "rgdal", "sp", "leaflet", "leaflet.extras", "leaflet.opacity", "htmltools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny); library(dplyr); library(raster); library(rgdal); library(sp); library(leaflet); library(leaflet.extras);library(leaflet.opacity); library(htmltools)

#aoi <- readOGR("//clark/SE/Action/Code_Lunch/RShiny_Projects/NK/Pupuna/shapes/putumayo_atacuari_aoi_riverclip_5819.shp")
#map <- readRDS("//clark/SE/Action/Code_Lunch/RShiny_Projects/NK/Pupuna/shapes/gadm36_PER_1_sp.rds")

ui <- fluidPage(
  titlePanel("CSV to Points on Bing Image Service"),
    
    p(h5(strong("Format:"),("CSV file must be formated with column headers named 'ID', 'NAME', 'LAT', 'LONG'"))),
  
    fileInput("myData_in", "Upload a .csv", buttonLabel = "Choose file", 
              placeholder = "No File Selected...", width = "255px", 
              accept = c(".csv","text/csv")),
  
    leafletOutput("basemap", height = 1000)
  )

server <- function(input, output) {
  
  my_table <- reactive({
    
    inFile <- input$myData_in
    if (is.null(inFile))
      return(NULL)
    
    myData = read.csv(inFile$datapath)
    
    df0 = data.frame(myData$ID, myData$NAME, myData$LAT, myData$LONG)
    df = unique(df0)
    names(df)[1] = 'ID'
    names(df)[2] = 'NAME'
    names(df)[3] = 'LAT'
    names(df)[4] = 'LONG'
    print(df)
    return(df)
  })
  
  output$basemap = renderLeaflet({
    if(is.null(my_table()))
    {
      return(leaflet()  %>% addBingTiles(apikey = "Ar2zhWwAE7IwTl1fYHkk5CK9WwjghmlHFz67Dk8kS_f0U_0bcP8z7qFGh52jN-dh",
                                         imagerySet = c("AerialWithLabels"), group = "Bing"))
    }
    else
    {
      leaflet(data = my_table()) %>%
        addBingTiles(apikey = "Ar2zhWwAE7IwTl1fYHkk5CK9WwjghmlHFz67Dk8kS_f0U_0bcP8z7qFGh52jN-dh",
                              imagerySet = c("AerialWithLabels"),
                              group = "Bing",
                              options = providerTileOptions(maxZoom = 4, maxNativeZoom = 1)) %>% 
        addTiles(group = "OSM") %>%
        addProviderTiles(providers$OpenTopoMap, group = "Topography") %>%
        addMarkers(~LONG, ~LAT, label = ~htmlEscape(NAME), group = "CSV Points") %>%
      
      addLayersControl(
        baseGroups = c("Bing", "OSM", "Topography"),
        overlayGroups = ("CSV Points"),
        options = layersControlOptions(collapsed = FALSE))
      
    }
  })
}

shinyApp(ui = ui, server = server)