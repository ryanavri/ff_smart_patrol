# Part 1: Upload and Process CSV Data with Debugging and Geometry Exclusion
library(shiny)
library(sf)
library(tidyverse)
library(anytime)

# Define UI
ui <- fluidPage(
  titlePanel("Upload and Process CSV Data"),
  sidebarLayout(
    sidebarPanel(
      fileInput("csv_data", "Upload CSV Data (e.g., Aktivitas_2016.csv)", accept = ".csv")
    ),
    mainPanel(
      h3("Findings per Quarter by Kategori Temuan"),
      tableOutput("findings_per_quarter")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Reactive expression to process CSV data
  data <- reactive({
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
}

# Run the app
shinyApp(ui = ui, server = server)

