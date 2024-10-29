# Increase the maximum file upload size to 200 MB
options(shiny.maxRequestSize = 200 * 1024^2)

# Load necessary libraries
library(shiny)
library(shinythemes)  # For UI themes
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
library(reactable)

# Define UI
ui <- navbarPage(
  title = "SmarteR Patrol Analysis App",
  theme = shinytheme("cosmo"),  # Applied a Bootstrap theme for a modern look
  
  # Step 1: Covariate Processing
  tabPanel(
    "1. Covariate Processing",
    sidebarLayout(
      sidebarPanel(
        h4("Step 1: Upload Raster and Shapefile"),
        p("Upload your raster layers and shapefile for the patrol sector grid."),
        fileInput("raster_layers", "Raster Layers (.tif)", multiple = TRUE, accept = ".tif"),
        fileInput("shapefile", "Shapefile (should contain .shp, .dbf, .shx, .prj)", multiple = TRUE,
                  accept = c(".shp", ".dbf", ".shx", ".prj")),
        actionButton("process", "Process Data", icon = icon("play")),
        width = 3
      ),
      mainPanel(
        h4("Raster Summary"),
        verbatimTextOutput("raster_summary"),
        tags$hr(),
        h4("Correlation Matrix"),
        plotOutput("correlation_matrix"),
        tags$hr(),
        h4("Distribution of Covariate Values"),
        plotOutput("distribution_plot"),
        tags$hr(),
        h4("Map of Covariate Values"),
        leafletOutput("value_map", height = 600)
      )
    )
  ),
  
  # Step 2: CSV Data Processing
  tabPanel(
    "2. CSV Data Processing",
    sidebarLayout(
      sidebarPanel(
        h4("Step 2: Upload CSV Data"),
        p("Please upload your CSV data file containing query from SMART"),
        fileInput("csv_data", "Select CSV File", accept = ".csv"),
        actionButton("process_csv", "Process Data", icon = icon("play")),
        width = 3
      ),
      mainPanel(
        h4("Findings per Quarter"),
        reactableOutput("findings_per_quarter")
      )
    )
  ),
  
  # Step 3: CPUE Plot
  tabPanel(
    "3. CPUE Plot",
    sidebarLayout(
      sidebarPanel(
        h4("Step 3: CPUE Plot Parameters"),
        p("Upload your patrol track shapefile and configure parameters for the CPUE plot."),
        # Added fileInput for patrol track shapefile
        fileInput("patrol_shapefile", "(should contain .shp, .dbf, .shx, .prj)", multiple = TRUE,
                  accept = c(".shp", ".dbf", ".shx", ".prj")),
        # Moved selectInput here
        selectInput(
          "kategori_temuan",
          "Select Kategori Temuan for Analysis:",
          choices = NULL  # Populated dynamically after CSV data upload
        ),
        # Added actionButton to process patrol track data
        actionButton("process_patrol", "Process Data", icon = icon("play")),
        # You can add more input controls here for the CPUE plot
        width = 3
      ),
      mainPanel(
        h4("CPUE Plot"),
        # Placeholder for CPUE plot output
        tableOutput("effort_table")
      )
    )
  ),
  
  # Step 4: Occupancy Modeling
  tabPanel(
    "4. Occupancy Modeling",
    sidebarLayout(
      sidebarPanel(
        h4("Step 4: Model Parameters"),
        p("Set up your occupancy model parameters."),
        textInput(
          "model_formula",
          "Specify the Model Formula:",
          placeholder = "e.g., elev*elev + TRI + biomass + road + cost"
        ),
        numericInput(
          "iterations",
          "Number of Iterations:",
          value = 500,
          min = 500,
          step = 500
        ),
        helpText("Higher iterations can slow down the process. Use 500 for testing, 50000 for the final model."),
        actionButton("run_model", "Run Model", icon = icon("play")),
        width = 3
      ),
      mainPanel(
        h4("Model Summary"),
        verbatimTextOutput("model_summary"),
        tags$hr(),
        h4("Predicted Map"),
        plotOutput("occupancy_plots"),
        tags$hr(),
        h4("Response Effect"),
        plotOutput("plot_effects")
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
  ## Part 1.1: Generate table of quarterly findings----
  output$findings_per_quarter <- renderReactable({
    req(data())
    
    # Process the data
    findings_data <- data() %>%
      st_set_geometry(NULL) %>%  # Remove the geometry to avoid list columns
      filter(`Observation.Category.0` == "Aktivitas Manusia") %>%
      group_by(YearQuarter, Kategori_temuan) %>%
      summarize(Findings = n(), .groups = 'drop') %>%
      filter(Kategori_temuan != "") %>% 
      pivot_wider(names_from = Kategori_temuan, values_from = Findings, values_fill = 0) %>%
      arrange(YearQuarter)
    
    # Calculate row totals
    findings_data <- findings_data %>%
      mutate(Total = rowSums(dplyr::select(., -YearQuarter)))
    
    # Calculate column totals
    total_row <- findings_data %>%
      dplyr::select(-YearQuarter) %>%
      summarise(across(everything(), sum)) %>%
      mutate(YearQuarter = "Total", .before = 1)
    
    # Combine the data with the total row
    findings_data <- bind_rows(findings_data, total_row)
    
    # Render the table using reactable
    reactable(
      findings_data,
      pagination = FALSE,
      highlight = TRUE,
      bordered = TRUE,
      columns = list(
        YearQuarter = colDef(name = "Year-Quarter")
      ),
      defaultSorted = "YearQuarter"
    )
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
  ## Part 2.1: Read shapefile of patrol sector----
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
  
  ## Part 2.2: Reactive expression to extract mean values and standardize covariates----
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
  
  ## Part 2.3: Render the correlation matrix plot----
  output$correlation_matrix <- renderPlot({
    req(processed_data())
    stdz_preds <- processed_data()$stdz_preds
    
    # Calculate and plot the correlation matrix
    corMat <- cor(stdz_preds, use = "complete.obs")
    corrplot(corMat, method = "number", type = "upper", tl.cex = 0.8)
  })
  
  ## Part 2.4: Render the density plots plot----
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
      labs(title = "", x = "Mean Value", y = "Density")
  })
  
  ## Part 2.5: Render the map of values using mapview----
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

  # Part 3: CPUE----
  
  # Reactive expression to read and process patrol track shapefile
  patrol_tracks <- eventReactive(input$process_patrol, {
    req(input$patrol_shapefile)
    shpdf <- input$patrol_shapefile
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
  
  # Reactive expression to process the patrol tracks
  CRP <- reactive({
    req(patrol_tracks())
    
    patrol_tracks() %>% 
    mutate(Jarak = st_length(.)) %>%
    dplyr::select(-Patrol_L_1, -Patrol_L_2, -Armed, -Patrol_Leg) %>%
    mutate(Quarter = quarters(Patrol_Sta), 
           Year = year(Patrol_Sta),
           YearQuarter = paste(Year, Quarter, sep = "-"))
  })
  
  # Reactive expression to calculate effort per pseudogrid
  qr_effort <- reactive({
    req(CRP(), hex_grid())
    
    # Select and rename necessary columns
    crp_data <- CRP() %>%
      dplyr::select(geometry, YearQuarter_crp = YearQuarter)
    
    hex_data <- hex_grid() %>%
      dplyr::select(geometry, grid_id_hex = grid_id)
    
    # Perform spatial intersection
    intersected_data <- st_intersection(crp_data, hex_data)
    
    # Calculate lengths
    intersected_data <- intersected_data %>%
      mutate(intersect_length = st_length(geometry))
    
    # Group and summarize
    effort_data <- intersected_data %>%
      group_by(grid_id, YearQuarter) %>%
      summarize(effort_km = sum(as.numeric(intersect_length) / 1000, na.rm = TRUE)) %>%
      as.data.frame() %>%
      dplyr::select(-geometry)
    
    # Ensure all columns are atomic vectors
    effort_data$grid_id <- as.character(effort_data$grid_id)
    effort_data$YearQuarter <- as.character(effort_data$YearQuarter)
    effort_data$effort_km <- as.numeric(effort_data$effort_km)
    
    # Return the data frame
    effort_data
  })
  
  
  # Render the effort table
  output$effort_table <- renderTable({
    req(qr_effort())
    
    qr_ef <- qr_effort()
    
    # Check the structure
    print(str(qr_ef))
    
    head(qr_ef)
  })
  
  
  
  
    
  # Part 4: Occupancy Modeling----
  
  # Reactive expression for filtering data based on the selected Kategori_temuan
  perburuan_satwa <- reactive({
    req(data(), input$kategori_temuan)
    
    # Filter data for the selected category
    data() %>%
      filter(`Observation.Category.0` == "Aktivitas Manusia") %>%
      filter(Kategori_temuan == input$kategori_temuan)
  })
  
  ## Part 4.1 : Create occupancy detection----
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
  
  ## Part 4.2: Run the occupancy model----
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
    chains <- 4
    iterations <- input$iterations
    thin <- 1
    save_warmup <- FALSE
    seed <- 212
    
    # Run Bayesian occupancy model using stan_occu()
    model_basic <- stan_occu(
      formula = user_formula,
      data = umf,
      cores =3,
      chains = chains,
      iter = iterations,
      thin = thin,
      seed = seed
    )
    
    # Predict occupancy for each site
    psi_pred <- predict(model_basic, submodel = "state")
    
    # Calculate naive occupancy
    naive <- monthly_replication_wide() %>%
      mutate(across(starts_with("1"):starts_with("12"), as.numeric)) %>%
      mutate(naive = rowSums(across(starts_with("1"):starts_with("12")), na.rm = TRUE)) %>%
      dplyr::select(grid_id, naive) %>%
      mutate(naive = ifelse(naive > 1, 1, naive))
    
    list(model = model_basic, psi_pred = psi_pred, naive = naive)
    
  })
  
  # Display model summary
  output$model_summary <- renderPrint({
    req(model_results())
    (model_results()$model)
  })
  
  ## Part 4.3: Display predicted and naive occupancy plots----
  output$occupancy_plots <- renderPlot({
    req(model_results(), processed_data())
    
    psi_pred <- model_results()$psi_pred
    naive <- model_results()$naive
    hex_grid <- processed_data()$hex_grid
    
    psi_pred$grid_id <- hex_grid$grid_id
    
    psi <- hex_grid %>%
      left_join(naive, by = "grid_id") %>%
      left_join(psi_pred, by = "grid_id") %>%
      mutate(naive = as.factor(naive))
    
    # Plot predicted occupancy
    plot_predicted <- ggplot(psi) +
      geom_sf(aes(fill = Predicted)) +
      scale_fill_gradient(low = "green", high = "red", name = "Predicted Occupancy") +
      theme_bw() +
      labs(title = "", fill = "Predicted")
    
    # Calculate occupancy stats from posterior predictions
    zpost <- posterior_predict(model_results()$model, "z")
    psi_dat <- rowMeans(zpost, na.rm = TRUE)
    psi_df <- data.frame(
      occ = mean(psi_dat),
      lower = quantile(psi_dat, 0.025),
      upper = quantile(psi_dat, 0.975)
    )
    
    # Format the mean and confidence interval as text
    occ_text <- paste0(
      "Mean Occupancy: ", round(psi_df$occ, 2),
      " (", round(psi_df$lower, 2), " - ", round(psi_df$upper, 2), ")"
    )
    
    # Plot naive occupancy
    plot_combined <- plot_predicted +
      geom_point(
        data = psi %>% filter(!is.na(naive)),  # Exclude NA values
        aes(color = naive, geometry = geometry),
        stat = "sf_coordinates",  # Plot using sf coordinates
        size = 2
      ) +
      scale_color_manual(values = c("0" = "grey", "1" = "black"), 
                         name = "Naive Occupancy", 
                         labels = c("0" = "Absent", "1" = "Present")) +
      annotate(
        "text",
        x = Inf, y = Inf, hjust = 1.1, vjust = 1.5,  # Adjust position
        label = occ_text, color = "black", size = 5, fontface = "bold"
      ) +
      theme_bw() +
      labs(title = "")
    
    print(plot_combined)
  })
  
  ## Part 4.4: Display plot effects----
  output$plot_effects <- renderPlot({
    req(model_results())
    
    pe <- plot_effects(model_results()$model, "state")
    
    print(pe)
    
  })
  
  
}

# Run the app
shinyApp(ui = ui, server = server)
