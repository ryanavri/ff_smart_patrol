library(tidyverse)
library(lubridate)
library(chron)
library(anytime)

library(sf)

# Load_data----
ringkasan_patroli <- st_read("Jalur_Patroli_2019.shp")
CRP <- ringkasan_patroli %>% 
  mutate(Jarak = st_length(ringkasan_patroli)) %>%
  select(-Patrol_L_1, -Patrol_L_2, -Armed, -Patrol_Leg) %>%
  mutate(Quarter = quarters(Patrol_Sta), 
         Year = year(Patrol_Sta)) %>%
  mutate(YearQuarter = paste(Year, Quarter, sep = "-"))
  
  
aktivitas_manusia <- read.csv("Aktivitas_Manusia_2019.csv")
CAM <- aktivitas_manusia %>%
  st_as_sf(coords = c("X", "Y"), crs = 32747) %>%
  rename('Patrol_ID' ='Patrol.ID') %>%
  rename('Kategori_temuan' ='Observation.Category.1') %>%
  rename('Tanggal' = 'Waypoint.Date') %>%
  mutate(Tanggal = anytime(Tanggal)) %>%
  select(-`Observation.Category.0`) %>%
  mutate(Quarter = quarters(Tanggal), 
         Year = year(Tanggal)) %>%
  mutate(YearQuarter = paste(Year, Quarter, sep = "-"))

# Load_polygon----
core <- st_read("Core_area.shp")
hex_3k <- st_read("hex3000.shp")
hex_5k <- st_read("hex5000.shp")

# Create a sequence for grid_id----
n <- nrow(hex_5k)  # Get the number of rows
hex_5k$grid_id <- sprintf("hx5k%02d", 1:n)  # Create the series 
hex_5k <- hex_5k[, c("geometry", "grid_id")] # Optimizing attribute 
glimpse(hex_5k)

# Calculate effort per pseudogrid----
# Ensure both datasets use the same CRS (coordinate reference system)
st_crs(CRP) <- st_crs(hex_5k)

# Perform the spatial intersection to get the patrol segments within each grid
intersected_data <- st_intersection(CRP, hex_5k)

intersected_data <- intersected_data %>%
  mutate(intersect_length = st_length(geometry))

# Summarize the total patrol length within each grid and each YearQuarter
qr_effort <- intersected_data %>%
  group_by(grid_id, YearQuarter) %>%
  summarize(effort_km = sum(as.numeric(intersect_length) / 1000, na.rm = TRUE)) %>%
  as.data.frame() %>%
  select(-geometry)

# Calculate cpue per pseudogrid----
# Ensure both datasets have the same CRS
st_crs(CAM) <- st_crs(hex_5k)

# Step 3: Perform spatial intersection with hex_3k to count findings in each grid
intersected_findings <- st_intersection(CAM, hex_5k)

# Step 4: Count the number of findings within each grid_id
qr_cpue <- intersected_findings %>%
  group_by(grid_id, YearQuarter) %>%
  summarize(findings = n())   %>% 
  as.data.frame() %>%
  select(-geometry) %>%
  inner_join(qr_effort) %>%
  mutate(cpue = findings/effort_km) %>%
  mutate(diff_cpue = ifelse(grepl("Q1", YearQuarter), NA, cpue - lag(cpue)),
         diff_effort = ifelse(grepl("Q1", YearQuarter), NA, effort_km - lag(effort_km))) 

# Remove rows with NA values resulting from the lag
qr_cpue <- na.omit(qr_cpue)

# plot 
# Plot the relationship between the difference in CPUE and the difference in patrol effort
ggplot(qr_cpue, aes(x = diff_effort, y = diff_cpue)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_bw() +
  labs(title = "CPUE vs Patrol Effort",
       x = "Patrol Effort (km) (t-1)",
       y = "CPUE (t-1)")

# inspect statistically
# Fit a GLS model accounting for ruggedness and proximity to boundary, assuming within-grid correlation
gls_model <- gls(diff_cpue ~ diff_effort, 
                 data = qr_cpue)

# Summary of the GLS model
summary(gls_model)
