# Covariate Preparation -------------------------------------------------------

# Create a list of required packages
package.list <- c("dplyr", "purrr", "terra", "sf", "leaflet", "raster", "fasterize", 
                  "ggplot2", "exactextractr","movecost", "corrplot", "readxl")

# Install and load packages
for (package in package.list) {
  if (!require(package, character.only = TRUE, quietly = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}

# Step 1: Load Raster and Spatial Data -----------------------------------------

# Load elevation raster (reference)
elevation <- terra::rast("spatial/elevation.tif")

# Load and resample biomass raster to match elevation's resolution and extent
biomass_raw <- terra::rast("spatial/biomass.tif")
biomass <- resample(biomass_raw, elevation)

# Step 2: Calculate Distance from Roads ----------------------------------------

# Load road shapefile and clean geometry (remove Z and M coordinates if present)
road <- st_read("spatial/road.shp")
road <- st_zm(road)  # Drop Z and M dimensions
road_vect <- vect(road)  # Convert to terra vector object

# Rasterize road shapefile to match elevation raster
road_raster <- rasterize(road_vect, elevation, field = 1)  # Set all road cells to 1

# Calculate distance from roads (Euclidean distance from each raster cell)
road_distance <- distance(road_raster)

# Step 3: Calculate Accumulated Cost Surface -----------------------------------

# Load road endpoints (access points) shapefile
road_points <- st_read("spatial/ujung_jalan.shp")

# Convert to SpatialPointsDataFrame
spatial_points <- as(road_points, "Spatial")
xy_coords <- coordinates(spatial_points)[,1:2]
final_spatial_df <- SpatialPointsDataFrame(coords = xy_coords, data = as.data.frame(spatial_points))

# Calculate accumulated cost raster using hiking function (move cost analysis)
ele_raster <- raster(elevation)  # Convert elevation to raster format for dtm model
costrast <- movecost(dtm = ele_raster, origin = final_spatial_df, funct = "r",
                     move = 8, breaks = 0.05, cogn.slp = TRUE, N = 2) #see documentation for parameter

# Step 4: Load Pseudo Grid and Assign Grid IDs ---------------------------------

# Load_polygon of patrol sector----
hex <- st_read("spatial/hex.shp")

# Step 5: Extract Covariate Values for Each Grid --------------------------------

# Create a list of raster layers to extract (elevation, TRI, biomass, road distance, cost)
raster_layers <- list(
  elev = elevation,
  TRI = terrain(elevation, "TRI"),  # Terrain Roughness Index (TRI)
  biomass = biomass,
  road = road_distance,
  cost = costrast$accumulated.cost.raster  # Cost surface raster
)

# Extract mean values of raster layers for each hexagonal grid
for (layer in names(raster_layers)) {
  hex[[layer]] <- exact_extract(raster_layers[[layer]], hex, fun = 'mean')
}

# Step 6: Standardize Covariates ------------------------------------------------

# Function to standardize variables (mean = 0, SD = 1)
var.stdz <- function(x) {
  m.x <- mean(x, na.rm = TRUE)
  sd.x <- sd(x, na.rm = TRUE)
  (x - m.x) / sd.x
}

# Extract the covariate data (numeric predictors) from the hex dataframe
preds <- hex %>%
  dplyr::select(c(3:7)) %>%  # Select covariate columns (excluding geometry and grid_id)
  as.data.frame() %>%
  dplyr::select(-geometry)

# Initialize a new dataframe to store standardized predictors
stdz.preds <- as.data.frame(matrix(NA, nrow = nrow(preds), ncol = length(names(preds))))

# Apply standardization function to each predictor
for (i in 1:length(colnames(preds))) {
  colnames(stdz.preds)[i] <- names(preds)[i]  # Preserve column names
  stdz.preds[,i] <- var.stdz(preds[,i])  # Standardize each column
}

# Step 7: Check for Multicollinearity (Correlation Matrix) ---------------------

# Calculate correlation matrix for the standardized predictors
corMat <- cor(stdz.preds, use = "complete.obs")

# Round the correlation matrix to 3 decimal places for readability
round(corMat, 3)

# Plot correlation matrix (consider |r| >= 0.7 as problematic)
corrplot(corMat, method="number")

# Save the objects to an RData file ---------------------
save(raster_layers, preds, hex, stdz.preds, file = "covariate.RData")
