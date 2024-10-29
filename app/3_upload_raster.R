# Increase the maximum file upload size to 200 MB
options(shiny.maxRequestSize = 200 * 1024^2)

# Load necessary libraries
library(shiny)
library(raster)
library(sf)
library(dplyr)
library(exactextractr)
library(corrplot)
library(ggplot2)
library(mapview)

# Define UI
ui <- fluidPage(
  titlePanel("Upload and Process Raster Layers"),
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
      plotOutput("distribution_plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
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
  
}

# Run the app
shinyApp(ui = ui, server = server)
