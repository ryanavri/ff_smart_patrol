# Increase the maximum file upload size to 200 MB
options(shiny.maxRequestSize = 200 * 1024^2)

# Load necessary libraries
library(shiny)
library(sf)
library(tidyverse)
library(anytime)
library(raster)
library(dplyr)
library(exactextractr)
library(corrplot)
library(ggplot2)
library(mapview)
library(leaflet)

# Define UI
ui <- fluidPage(
  titlePanel("Spatial Data Processing App"),
  tabsetPanel(
    # Tab 1: CSV Data Processing
    tabPanel(
      "CSV Data Processing",
      sidebarLayout(
        sidebarPanel(
          fileInput("csv_data", "Upload CSV Data (e.g., Aktivitas_2016.csv)", accept = ".csv"),
          actionButton("process_csv", "Process CSV Data")
        ),
        mainPanel(
          h3("Findings per Quarter by Kategori Temuan"),
          tableOutput("findings_per_quarter")
        )
      )
    ),
    
    # Tab 2: Raster and Shapefile Processing
    tabPanel(
      "Raster and Shapefile Processing",
      sidebarLayout(
        sidebarPanel(
          fileInput("raster_layers", "Upload Raster Layers (.tif)", multiple = TRUE, accept = ".tif"),
          fileInput("shapefile", "Upload Shapefile for Hex Grid (.shp, .dbf, .shx, .prj)", multiple = TRUE, accept = c(".shp", ".dbf", ".shx", ".prj")),
          actionButton("process", "Process Data")
        ),
        mainPanel(
          h3("Raster Summary"),
          verbatimTextOutput("raster_summary"),
          h3("Correlation Matrix"),
          plotOutput("correlation_matrix"),
          h3("Distribution of Values by Raster Layer"),
          plotOutput("distribution_plot"),
          h3("Map of Values by Raster Layer"),
          leafletOutput("value_map", height = 600)
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Part 1: CSV Data Processing
  data <- eventReactive(input$process_csv, {
    req(input$csv_data)
    
    # Read CSV and process into a spatial object
    csv_data <- read.csv(input$csv_data$datapath)
    
    csv_data %>%
      st_as_sf(coords = c("X", "Y"), crs = 32747) %>%
      rename('Patrol_ID' = 'Patrol.ID',
             'Kategori_temuan' = 'Observation.Category.1',
             'Tanggal' = 'Waypoint.Date') %>%
      mutate(Tanggal = anytime(Tanggal)) %>%
      dplyr::select(everything()) %>%
      mutate(Quarter = quarters(Tanggal), 
             Year = year(Tanggal),
             YearQuarter = paste(Year, Quarter, sep = "-"))
  })
  
  # Calculate the number of findings per quarter by Kategori Temuan and render it as a table
  output$findings_per_quarter <- renderTable({
    req(data())
    data() %>%
      st_set_geometry(NULL) %>%  # Remove the geometry to avoid list columns
      filter(`Observation.Category.0` == "Aktivitas Manusia") %>%
      group_by(YearQuarter, Kategori_temuan) %>%
      summarize(Findings = n(), .groups = 'drop') %>%
      filter(Kategori_temuan != "") %>% 
      pivot_wider(names_from = Kategori_temuan, values_from = Findings, values_fill = 0) %>%
      arrange(YearQuarter)
  })
  
  # Part 2: Raster and Shapefile Processing
  # Reactive expression to read and process shapefile
  hex_grid <- reactive({
    req(input$shapefile)
    shpdf <- input$shapefile
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
  
  # Reactive expression to process raster layers
  raster_stack <- reactive({
    req(input$raster_layers)
    raster_list <- lapply(input$raster_layers$datapath, raster)
    names(raster_list) <- gsub("\\.tif$", "", input$raster_layers$name)
    stack(raster_list)
  })
  
  # Display a summary of the raster layers
  output$raster_summary <- renderPrint({
    req(raster_stack())
    print(raster_stack())
  })
  
  # Reactive expression to extract mean values and standardize covariates
  processed_data <- eventReactive(input$process, {
    req(raster_stack(), hex_grid())
    
    # Extract mean values of raster layers for each hexagonal grid
    hex_3k <- hex_grid()
    rasters <- raster_stack()
    
    for (layer in names(rasters)) {
      hex_3k[[layer]] <- exact_extract(rasters[[layer]], hex_3k, fun = 'mean')
    }
    
    # Extract the covariate data before standardization
    raw_preds <- hex_3k %>%
      st_set_geometry(NULL) %>%  # Remove geometry to work with a data frame
      dplyr::select(-grid_id)  # Adjust this line to match the actual structure of `hex_3k`
    
    # Standardize function
    var.stdz <- function(x) {
      m.x <- mean(x, na.rm = TRUE)
      sd.x <- sd(x, na.rm = TRUE)
      (x - m.x) / sd.x
    }
    
    # Apply standardization to each predictor column
    stdz.preds <- as.data.frame(lapply(raw_preds, var.stdz))
    colnames(stdz.preds) <- names(raw_preds)
    
    list(stdz_preds = stdz.preds, raw_preds = raw_preds, hex_grid = hex_3k)
  })
  
  # Render the correlation matrix plot
  output$correlation_matrix <- renderPlot({
    req(processed_data())
    stdz_preds <- processed_data()$stdz_preds
    
    # Calculate and plot the correlation matrix
    corMat <- cor(stdz_preds, use = "complete.obs")
    corrplot(corMat, method = "number", type = "upper", tl.cex = 0.8)
  })
  
  # Render the distribution plot using ggplot2 as density plots
  output$distribution_plot <- renderPlot({
    req(processed_data())
    raw_preds <- processed_data()$raw_preds
    
    # Convert raw_preds to long format for easier plotting
    raw_preds_long <- raw_preds %>%
      pivot_longer(cols = everything(), names_to = "Layer", values_to = "Value")
    
    ggplot(raw_preds_long, aes(x = Value, fill = Layer)) +
      geom_density(alpha = 0.6) +
      facet_wrap(~ Layer, scales = "free") +
      theme_minimal() +
      labs(title = "Density of Values by Raster Layer", x = "Mean Value", y = "Density")
  })
  
  # Render the map of values using leaflet and mapview
  output$value_map <- renderLeaflet({
    req(processed_data())
    hex_3k <- processed_data()$hex_grid
    raw_preds <- processed_data()$raw_preds
    
    # Merge raw_preds back to hex_3k for visualization
    hex_3k <- cbind(hex_3k, raw_preds)
    
    # Create a mapview object with the hex grid and raster layer values
    m <- mapview(hex_3k,
                 zcol = names(raw_preds),  # Display all layers using their names
                 layer.name = names(raw_preds),
                 map.types = c("OpenStreetMap", "Esri.WorldImagery"),
                 alpha.regions = 0.35)
    
    # Extract the leaflet map from the mapview object
    m@map
  })
}

# Run the app
shinyApp(ui = ui, server = server)
