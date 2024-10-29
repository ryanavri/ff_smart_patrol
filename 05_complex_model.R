# Load necessary libraries
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

# Load data for multiple years into a list
aktivitas_list <- list(
  aktivitas_2016 = read.csv("Aktivitas_2016.csv"),
  aktivitas_2017 = read.csv("Aktivitas_2017.csv"),
  aktivitas_2018 = read.csv("Aktivitas_2018.csv"),
  aktivitas_2019 = read.csv("Aktivitas_2019.csv")
)

# Initialize lists to store the results for each year
monthly_replication_list <- list()
occu_data_list <- list()

# Loop through each year's data
for (year_name in names(aktivitas_list)) {
  
  # Step 2: Data Preparation for Each Year
  CAM <- aktivitas_list[[year_name]] %>%
    st_as_sf(coords = c("X", "Y"), crs = 32747) %>%  # Use UTM Zone 47S CRS
    rename(Patrol_ID = 'Patrol.ID',
           Kategori_temuan = 'Observation.Category.1',
           Tanggal = 'Waypoint.Date') %>%
    mutate(Tanggal = anytime(Tanggal)) %>%  # Convert date to proper format
    dplyr::select(-`Observation.Category.0`) %>%
    mutate(Quarter = quarters(Tanggal), 
           Year = year(Tanggal),
           YearQuarter = paste(Year, Quarter, sep = "-"),
           Month = month(Tanggal))
  
  # Step 3: Filter for Poaching-Related Threats ---------------------------------
  perburuan_satwa <- CAM %>%
    filter(Kategori_temuan == "Perburuan Satwa")
  
  # Step 4: Spatial Intersection with Hex Grid ----------------------------------
  intersected_findings <- st_intersection(CAM, hex_3k)
  
  # Step 5: Create Visitation and Detection Data --------------------------------
  visitation_reference <- intersected_findings %>%
    group_by(grid_id, Month) %>%
    summarize(visitation = 1) %>%
    ungroup() %>%
    as.data.frame()
  
  binary_detection <- st_intersection(perburuan_satwa, hex_3k) %>%
    group_by(grid_id, Month) %>%
    summarize(detection_binary = 1) %>%
    ungroup() %>%
    as.data.frame()
  
  # Step 6: Merge Visitation and Detection Data ---------------------------------
  monthly_replication <- visitation_reference %>%
    left_join(binary_detection, by = c("grid_id", "Month")) %>%
    mutate(detection_binary = ifelse(is.na(detection_binary), 0, detection_binary)) %>%
    complete(grid_id, Month = 1:12, fill = list(visitation = NA, detection_binary = NA)) %>%
    dplyr::select(-geometry.x, -geometry.y, -visitation)
  
  # Convert to wide format for occupancy modeling
  monthly_replication_wide <- monthly_replication %>%
    arrange(grid_id, Month) %>%
    group_by(grid_id) %>%
    spread(Month, detection_binary)
  
  # Store the result for each year in the list
  monthly_replication_list[[year_name]] <- monthly_replication_wide
  
  # Step 7: Create occu.dat for Each Year ---------------------------------------
  
  occu.dat <- hex_3k %>%
    select(grid_id) %>%
    left_join(monthly_replication_wide, by = "grid_id") %>%
    as.data.frame() %>%
    dplyr::select(-geometry)
  
  # Store occu.dat for each year in occu_data_list
  occu_data_list[[year_name]] <- occu.dat
}

# Combine all the data frames in the occu_data_list into a single data frame
combined_occu_data <- do.call(rbind, occu_data_list)

preds <- as.data.frame(scale(combined_occu_data[,2:6]))
preds <- preds %>%
  mutate(year = as.factor(rep(2016:2019, each = 93)))

# Create unmarkedFrame object for occupancy modeling
umf <- unmarkedFrameOccu(y = combined_occu_data[, 7:18], siteCovs = preds)

model_formula <- "~ (1 | year) ~ (elev + TRI + biomass + road + cost  + (elev + TRI + biomass + road + cost  || year))"


# Set model parameters
chains <- 3           # Number of Markov chains
cores <- 3            # Number of cores for parallel processing
iterations <- 1000     # Number of iterations (500 for demonstration, increase to 5000 full run)
thin <- 1             # Thinning interval
save_warmup <- FALSE  # save draws during warmup phase 
draws <- NULL         # number of posterior draws for predictions. Set NULL for all posterior samples (can be memory intensive for large number of iterations)



options(mc.cores=cores) # parallel processing

# Step 7: Run Bayesian Occupancy Model ------------------------------------------

# Run Bayesian occupancy model using stan_occu()
model_complex <- stan_occu(
  formula = formula(model_formula),
  data = umf,
  chains = chains,
  iter = iterations,
  thin = thin,
  save_warmup = FALSE)

# Display model summary and diagnostic plots
model_complex
plot_effects(model_complex, "state")
plot_effects(model_complex, "det")

# goodness-of-fit test test   ####
gof_results <- gof(model, draws = 500)
gof_results



# calculate PAO (percentage of area occupied)  ####

years <- sort(unique(umf@siteCovs[["year"]]))
covariates <- preds[1:93, 1:5]

pao_year <- list()

# loop over year-semester
for(yr in 1:length(years)){
  
  df_pred <- cbind(covariates, year = factor(years[yr], levels = years))  
  
  # occupancy probability for each cell / posterior sample / year 
  # rows = grid cells, columns = posterior draws
  pred_psi <- t(posterior_linpred(object = model_complex, 
                                  submodel = "state",
                                  newdata = as.data.frame(df_pred),
                                  transform = T,
                                  draws = draws))
  
  # random binomial trial for each posterior sample at each cell
  z_vect <- sapply(as.vector(pred_psi), rbinom, n = 1, size = 1)
  
  # matrix with occupancy status for each posterior sample at each cell
  z <- matrix(z_vect, 
              ncol = ncol(pred_psi), 
              nrow = nrow(pred_psi))
  
  # PAO estimate for each posterior sample (summarizing all cells)
  pao_year[[yr]] <- data.frame(PAO = colMeans(z))
}

names(pao_year) <- years

df_pao_year <- reshape2::melt(pao_year, measure.vars = "PAO")
colnames(df_pao_year)[ncol(df_pao_year)] <- "year"


# summarize PAO estimate distributions (by year)
df_pao_year2 <- data.frame(year = years,
                           do.call("rbind", tapply(df_pao_year$value, 
                                                   INDEX = list(Year = df_pao_year$year),
                                                   FUN = summary)))

colnames(df_pao_year2) <- gsub("X1st.Qu.", "lower25", colnames(df_pao_year2), fixed = T)
colnames(df_pao_year2) <- gsub("X3rd.Qu.", "upper25", colnames(df_pao_year2), fixed = T)
rownames(df_pao_year2) <- NULL

# table with PAO estimates by year
df_pao_year2


# plot PAO estimates by year
p <- ggplot(data = df_pao_year, 
            aes(x = year, y = value)) +
  geom_violin() +
  geom_pointrange(data = df_pao_year2, 
                  mapping = aes(ymin = lower25,
                                ymax = upper25,
                                x = years,
                                y = Median),
                  inherit.aes = F, fatten = 2, size = 0.5) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(c(0,1)) +
  ylab ("Percentage of area occupied (PAO)") +
  xlab("Year")

p

# Step 7: Extract delta occupancy across years ------------------------------------------

# Ensure both dataframes are aligned by grid_id before calculating delta
delta_data <- psi2016 %>%
  dplyr::select(grid_id, Predicted_2016 = Predicted, Naive_2016 = naive) %>%
  as.data.frame() %>% # Select grid_id and Predicted for 2016
  left_join(psi2017 %>% dplyr::select(grid_id, Predicted_2017 = Predicted, Naive_2017 = naive), by = "grid_id")%>%
  left_join(psi2018 %>% dplyr::select(grid_id, Predicted_2018 = Predicted, Naive_2018 = naive), by = "grid_id")%>%
  left_join(psi20019 %>% dplyr::select(grid_id, Predicted_2019 = Predicted, Naive_2019 = naive), by = "grid_id") 


# Calculate year-to-year deltas
delta_data <- delta_data %>%
  mutate(delta = Predicted_2019 - Predicted_2016)  

write_sf(delta_data, "delta_psi.shp")
