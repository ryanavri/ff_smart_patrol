library(shiny)
library(sf)
library(mapview)
library(leaflet)

# Define UI for the app
ui <- fluidPage(
  titlePanel("Upload and View Shapefile"),
  sidebarLayout(
    sidebarPanel(
      fileInput(
        inputId = "filemap",
        label = "Upload Shapefile (.shp, .dbf, .shx, .prj)",
        multiple = TRUE,
        accept = c(".shp", ".dbf", ".shx", ".prj")
      ),
      actionButton("show_map", "Show Map")
    ),
    mainPanel(
      leafletOutput(outputId = "map", height = "600px")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Reactive function to read the uploaded shapefile
  map_data <- eventReactive(input$show_map, {
    req(input$filemap)
    
    # Get the uploaded files and extract the directory name
    shpdf <- input$filemap
    tempdirname <- dirname(shpdf$datapath[1])
    
    # Rename uploaded files to their original names
    for (i in 1:nrow(shpdf)) {
      file.rename(
        shpdf$datapath[i],
        file.path(tempdirname, shpdf$name[i])
      )
    }
    
    # Find the .shp file among the uploaded files
    shp_file <- shpdf$name[grep("\\.shp$", shpdf$name)]
    
    # Read the shapefile using sf
    st_read(file.path(tempdirname, shp_file))
  })
  
  # Render the map using leaflet
  output$map <- renderLeaflet({
    req(map_data())
    map <- map_data()
    
    # Create a mapview object and extract the leaflet map
    m <- mapview(map, 
                 layer.name = "Landcover changes",
                 map.types = c("OpenStreetMap", "Esri.WorldImagery"),
                 alpha.regions = 0.35)
    m@map
  })
}

# Run the app
shinyApp(ui = ui, server = server)
