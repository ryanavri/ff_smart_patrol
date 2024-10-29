# Load necessary libraries
# Create a list of required packages
package.list <- c("tidyverse", "lubridate", "chron", "anytime", "patchwork",
                  "sf", "nlme", "ggpubr", "ubms", "reshape2", "ggplot2", "mapview")

# Install and load packages
for (package in package.list) {
  if (!require(package, character.only = TRUE, quietly = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}

# Step 1: Data Preparation -------------------------------------------------------

# Load covariate from previous code
load("covariate.RData")

# Load "Aktivitas_2019.csv" data
aktivitas_manusia <- read.csv("query/Aktivitas_2019.csv")

# Convert to spatial object (sf), adjust column names, and create date-related variables
CAM <- aktivitas_manusia %>%
  st_as_sf(coords = c("X", "Y"), crs = 32747) %>%  # Use UTM Zone 47S CRS
  rename(Patrol_ID = 'Patrol.ID',
         Kategori_temuan = 'Observation.Category.1',
         Tanggal = 'Waypoint.Date') %>%
  mutate(Tanggal = anytime(Tanggal)) %>%  # Convert date to proper format
  dplyr::select(-`Observation.Category.0`) %>%  # Remove unnecessary column
  mutate(Quarter = quarters(Tanggal), 
         Year = year(Tanggal),
         YearQuarter = paste(Year, Quarter, sep = "-"),
         Month = month(Tanggal))

# Step 2: Filter for Poaching-Related Threats ------------------------------------

# Filter for "Perburuan Satwa" (Poaching) in Kategori_temuan
perburuan_satwa <- CAM %>%
  filter(Kategori_temuan == "Perburuan Satwa")

# Step 3: Spatial Intersection with Hex Grid ------------------------------------

# Perform spatial intersection with the hex grid (hex_3k)
intersected_findings <- st_intersection(CAM, hex_3k)

# Step 4: Create Visitation and Detection Data ----------------------------------

# Create visitation reference (0 for visit without "Perburuan Satwa")
visitation_reference <- intersected_findings %>%
  group_by(grid_id, Month) %>%
  summarize(visitation = 1) %>%
  ungroup() %>%
  as.data.frame()

# Create binary detection for "Perburuan Satwa" (1 for detection)
binary_detection <- st_intersection(perburuan_satwa, hex_3k) %>%
  group_by(grid_id, Month) %>%
  summarize(detection_binary = 1) %>%
  ungroup()%>%
  as.data.frame()

# Step 5: Merge Visitation and Detection Data -----------------------------------

# Merge visitation and detection data and handle missing values (0 for non-detections)
monthly_replication <- visitation_reference %>%
  left_join(binary_detection, by = c("grid_id", "Month")) %>%
  mutate(detection_binary = ifelse(is.na(detection_binary), 0, detection_binary)) %>%  # Assign 0 for visits without poaching
  complete(grid_id, Month = 1:12, fill = list(visitation = NA, detection_binary = NA)) %>%  
  dplyr::select(-geometry.x, -geometry.y, -visitation)

# Convert to wide format for occupancy modeling
monthly_replication_wide <- monthly_replication %>%
  arrange(grid_id, Month) %>%
  group_by(grid_id) %>%
  spread(Month, detection_binary)

# Step 6: Occupancy Modeling ----------------------------------------------------

# Prepare data for unmarkedFrameOccu (occupancy model)
occu.dat <- hex %>%
  left_join(monthly_replication_wide, by = "grid_id") %>%
  as.data.frame() %>%
  dplyr::select(-geometry)

# Create unmarkedFrame object for occupancy modeling
umf <- unmarkedFrameOccu(y = occu.dat[, 7:18], siteCovs = stdz.preds)

# Define the model formula
model_basic <- "~ 1 ~1" #null model
model_additive <- "~ 1 ~elev + TRI + biomass + road + cost" #additive model with all covariate 
model_quadrat <- "~ 1 ~elev + I(elev^2) + TRI + biomass + road + cost" #quadrat for elevation and additive for all

# Set model parameters
chains <- 4           # Number of Markov chains
cores <- 3            # Number of cores for parallel processing
iterations <- 500     # Number of iterations (500 for demonstration, increase to 5000 full run)
thin <- 1             # Thinning interval
save_warmup <- FALSE  # save draws during warmup phase 
draws <- NULL         # number of posterior draws for predictions. Set NULL for all posterior samples (can be memory intensive for large number of iterations)

options(mc.cores=cores) # parallel processing

# Step 7: Run Bayesian Occupancy Model ------------------------------------------

# Run Bayesian occupancy model using stan_occu()
model_basic <- stan_occu(
  formula = formula(model_basic),
  data = umf,
  chains = chains,
  iter = iterations,
  thin = thin,
  save_warmup = FALSE)

# Display model summary and diagnostic plots
model_basic

# run another models
model_additive <- stan_occu(
  formula = formula(model_additive),
  data = umf,
  chains = chains,
  iter = iterations,
  thin = thin,
  save_warmup = FALSE)

model_quadrat <- stan_occu(
  formula = formula(model_quadrat),
  data = umf,
  chains = chains,
  iter = iterations,
  thin = thin,
  save_warmup = FALSE)

# Compare the models
mods <- fitList(model_basic, model_additive, model_quadrat)
round(modSel(mods), 3)


plot_effects(model_basic, "state")
#plot_effects(model_basic, "det")

# Predict the occupancy for each site (state predictions)
psi_pred <- predict(model_basic, submodel = "state")
psi_pred <- predict(model_additive, submodel = "state")
psi_pred <- predict(model_quadrat, submodel = "state")

# Evaluate the model (posterior predictive should close to 0.5 for perfect model)
# Consider re analysis if p <0 or >1
#(fit_top_gof <- gof(model_basic, draws=1000, quiet=TRUE))


# Step 8: Calculate Naive Occupancy ---------------------------------------------

# Calculate naive occupancy as the row sum of detection columns (ignoring NA)
naive <- monthly_replication_wide %>%
  mutate(across(starts_with("1"):starts_with("12"), as.numeric)) %>%
  mutate(naive = rowSums(across(starts_with("1"):starts_with("12")), na.rm = TRUE)) %>%
  dplyr::select(grid_id, naive) %>%
  mutate(naive = ifelse(naive > 1, 1, naive))

# Merge naive occupancy with hex grid
psi.naive <- hex %>%
  left_join(naive, by = "grid_id")

# Add grid_id to predicted data
n <- nrow(psi_pred)
psi_pred$grid_id <- sprintf("hx3k%02d", 1:n)

# Merge naive occupancy and predicted values
psi <- psi.naive %>%
  left_join(psi_pred, by = "grid_id") %>%
  mutate(naive = as.factor(naive))

# Step 9: Visualize Predicted and Naive Values ----------------------------------

# Using ggplot2 for static plotting
# Plot predicted occupancy
plot_predicted <- ggplot(psi) +
  geom_sf(aes(fill = Predicted)) +
  scale_fill_gradient(low = "green", high = "red", name = "Predicted Occupancy") +
  theme_bw() +
  labs(title = "", fill = "Predicted")

# Calculate occupancy stats from posterior predictions
zpost <- posterior_predict(model_additive, "z")
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

plot_combined


# Using mapview for interactive mapping
mapview(psi, zcol = "Predicted", 
        col.regions = colorRampPalette(c("green", "yellow", "red")),  # Color gradient
        alpha = 0.5,  # Set polygon transparency to 0.5
        layer.name = "Predicted Occupancy")

# Plot naive occupancy using mapview
mapview(psi, zcol = "naive", 
        col.regions = colorRampPalette(c("green", "red", "red", "red")),  # Custom color gradient
        alpha = 0.5,  # Set opacity to 0.5
        layer.name = "Naive Occupancy")