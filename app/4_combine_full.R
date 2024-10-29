# Increase the maximum file upload size to 200 MB
options(shiny.maxRequestSize = 200 * 1024^2)

# Load necessary libraries
library(shiny)
library(sf)
library(tidyverse)
library(anytime)
library(raster)
library(exactextractr)
library(corrplot)
library(ggplot2)
library(mapview)
library(leaflet)
library(unmarked)
library(ubms)
library(patchwork)
library(lubridate)

# Define UI
ui <- fluidPage(
  titlePanel("Spatial Data Processing and Occupancy Modeling App"),
  tabsetPanel(
    # Tab 1: CSV Data Processing
    tabPanel(
      "CSV Data Processing",
      sidebarLayout(
        sidebarPanel(
          fileInput("csv_data", "Upload CSV Data", accept = ".csv"),
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
          fileInput("shapefile", "Upload Shapefile for Hex Grid (.shp, .dbf, .shx, .prj)",
                    multiple = TRUE, accept = c(".shp", ".dbf", ".shx", ".prj")),
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
    ),
    
    # Tab 3: Occupancy Modeling
    tabPanel(
      "Occupancy Modeling",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "kategori_temuan",
            "Select Kategori Temuan for Analysis:",
            choices = NULL  # Populated dynamically after CSV data upload
          ),
          textInput(
            "model_formula",
            "Specify the Model Formula:",
            placeholder = "Example: elev*elev + TRI + biomass + road + cost"
          ),
          numericInput(
            "iterations",
            "Number of Iterations:",
            value = 500,
            min = 500,
            step = 500
          ),
          helpText("Higher iterations can slow down the process. Use 500 for testing, 50000 for the final model."),
          actionButton("run_model", "Run Occupancy Model")
        ),
        mainPanel(
          h3("Model Summary"),
          verbatimTextOutput("model_summary"),
          h3("Predicted and Naive Occupancy Plots"),
          plotOutput("occupancy_plots"),
          h3("Interactive Maps"),
          leafletOutput("predicted_map", height = 400),
          leafletOutput("naive_map", height = 400)
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Part 1: CSV Data Processing----
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
             YearQuarter = paste(Year, Quarter, sep = "-"),
             Month = month(Tanggal))
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
  
  # Update selectInput for available Kategori_temuan after CSV data is processed
  observeEvent(data(), {
    req(data())
    
    # Extract unique Kategori_temuan values from the processed data
    unique_kategori <- unique(data()$Kategori_temuan)
    
    # Update the selectInput for Kategori_temuan
    updateSelectInput(
      session = session,
      inputId = "kategori_temuan",
      label = "Select Kategori Temuan for Analysis:",
      choices = unique_kategori,
      selected = unique_kategori[1]  # Set default to the first option
    )
  })
  
  # Part 2: Raster and Shapefile Processing----
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
  
  # Part 3: Occupancy Modeling----
  # Reactive expression for filtering data based on the selected Kategori_temuan
  perburuan_satwa <- reactive({
    req(data(), input$kategori_temuan)
    
    # Filter data for the selected category
    data() %>%
      filter(`Observation.Category.0` == "Aktivitas Manusia") %>%
      filter(Kategori_temuan == input$kategori_temuan)
  })
  
  # Reactive expression for performing spatial intersection with hex grid
  intersected_findings <- reactive({
    req(data(), processed_data())
    
    # Perform spatial intersection between all findings and the hex grid
    st_intersection(data(), processed_data()$hex_grid)
  })
  
  # Reactive expression for creating visitation reference data
  visitation_reference <- reactive({
    req(intersected_findings())
    
    intersected_findings() %>%
      st_set_geometry(NULL) %>%
      group_by(grid_id, Month) %>%
      summarize(visitation = 1) %>%
      ungroup()
  })
  
  # Reactive expression for creating binary detection data
  binary_detection <- reactive({
    req(perburuan_satwa(), processed_data())
    
    st_intersection(perburuan_satwa(), processed_data()$hex_grid) %>%
      st_set_geometry(NULL) %>%
      group_by(grid_id, Month) %>%
      summarize(detection_binary = 1) %>%
      ungroup()
  })
  
  # Reactive expression for merging visitation and detection data
  monthly_replication <- reactive({
    req(visitation_reference(), binary_detection())
    
    visitation_reference() %>%
      left_join(binary_detection(), by = c("grid_id", "Month")) %>%
      mutate(detection_binary = ifelse(is.na(detection_binary), 0, detection_binary)) %>%  # Assign 0 for visits without poaching
      complete(grid_id, Month = 1:12, fill = list(visitation = NA, detection_binary = NA)) %>%  
      dplyr::select(-visitation)
  })
  
  # Reactive expression to convert to wide format for occupancy modeling
  monthly_replication_wide <- reactive({
    req(monthly_replication())
    
    monthly_replication() %>%
      arrange(grid_id, Month) %>%
      group_by(grid_id) %>%
      spread(Month, detection_binary)
  })
  
  # Prepare the data for occupancy modeling
  occu_dat <- reactive({
    req(monthly_replication_wide(), processed_data())
    
    # Left join to ensure all grid_ids are included
    processed_data()$hex_grid %>%
      as.data.frame() %>%
      left_join(monthly_replication_wide(), by="grid_id")
  })
  
  # Run the occupancy model
  model_results <- eventReactive(input$run_model, {
    req(occu_dat(), input$model_formula, input$iterations, processed_data())
    
    # Ensure that the number of sites matches between y and siteCovs
    num_sites_y <- nrow(occu_dat())
    num_sites_covs <- nrow(processed_data()$stdz_preds)
    
    if (num_sites_y != num_sites_covs) {
      stop("The number of sites in the detection history does not match the number of sites in the site covariates.")
    }
    
    det.mat <- occu_dat() %>% dplyr::select(matches("^[0-9]+$"))
    stdz.preds <- processed_data()$stdz_preds
    
    umf <- unmarkedFrameOccu(
      y = det.mat,
      siteCovs = stdz.preds)
    
    # Use the user-defined formula
    user_formula <- as.formula(paste("~ 1 ~", input$model_formula))
    
    # Set model parameters
    chains <- 3
    iterations <- input$iterations
    thin <- 1
    save_warmup <- FALSE
    
    # Run Bayesian occupancy model using stan_occu()
    model_basic <- stan_occu(
      formula = user_formula,
      data = umf,
      chains = chains,
      iter = iterations,
      thin = thin,
      save_warmup = save_warmup
    )
    
    # Predict occupancy for each site
    psi_pred <- predict(model_basic, submodel = "state")
    
    # Calculate naive occupancy
    naive <- monthly_replication_wide() %>%
      mutate(naive = rowSums(select(., matches("^[0-9]+$")), na.rm = TRUE)) %>%
      select(grid_id, naive)
    
    list(model = model_basic, psi_pred = psi_pred, naive = naive)
  })
  
  # Display model summary
  output$model_summary <- renderPrint({
    req(model_results())
    summary(model_results()$model)
  })
  
  # Display predicted and naive occupancy plots
  output$occupancy_plots <- renderPlot({
    req(model_results(), processed_data())
    psi_pred <- model_results()$psi_pred
    naive <- model_results()$naive
    hex_grid <- processed_data()$hex_grid
    
    psi_pred$grid_id <- hex_grid$grid_id
    psi <- hex_grid %>%
      left_join(naive, by = "grid_id") %>%
      left_join(psi_pred, by = "grid_id")
    
    # Adjust plotting code as needed
    plot_predicted <- ggplot(psi) +
      geom_sf(aes(fill = Predicted)) +
      scale_fill_gradient(low = "green", high = "red", name = "Predicted") +
      theme_bw() +
      labs(title = "Predicted Occupancy", fill = "Predicted")
    
    # Create a new column for simplified naive occupancy (categorical)
    psi <- psi %>%
      mutate(naive_cat = case_when(
        is.na(naive) ~ "NA",
        naive == 0 ~ "0",
        naive >= 1 ~ "1+"
      ))
    
    # Plot naive occupancy
    plot_naive <- ggplot(psi) +
      geom_sf(aes(fill = naive_cat)) +
      scale_fill_manual(values = c("NA" = "grey", "0" = "blue", "1+" = "red"), 
                        name = "Naive",
                        na.translate = FALSE) +
      theme_bw() +
      labs(title = "Naive Occupancy", fill = "Naive")
    
    # Combine the plots
    combined_plot <- plot_predicted + plot_naive
    
    print(combined_plot)
  })
  
  # Display interactive map of predicted occupancy
  output$predicted_map <- renderLeaflet({
    req(model_results(), processed_data())
    psi_pred <- model_results()$psi_pred
    hex_grid <- processed_data()$hex_grid
    
    psi_pred$grid_id <- hex_grid$grid_id
    psi <- hex_grid %>%
      left_join(psi_pred, by = "grid_id")
    
    mapview(psi, zcol = "Predicted", 
            col.regions = colorRampPalette(c("green", "yellow", "red")),  # Color gradient
            alpha.regions = 0.5,
            layer.name = "Predicted Occupancy")@map
  })
  
  # Display interactive map of naive occupancy
  output$naive_map <- renderLeaflet({
    req(model_results(), processed_data())
    naive <- model_results()$naive
    hex_grid <- processed_data()$hex_grid
    
    psi <- hex_grid %>%
      left_join(naive, by = "grid_id")
    
    mapview(psi, zcol = "naive", 
            col.regions = colorRampPalette(c("blue", "red")),  # Custom color gradient
            alpha.regions = 0.5,
            layer.name = "Naive Occupancy")@map
  })
}

# Run the app
shinyApp(ui = ui, server = server)
