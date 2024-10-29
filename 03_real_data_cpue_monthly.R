# loaf libraries
suppressMessages(require(tidyverse))
suppressMessages(require(lubridate))
suppressMessages(require(chron))
suppressMessages(require(anytime))
suppressMessages(require(sf))
suppressMessages(require(nlme))
suppressMessages(require(ggpubr))

args = commandArgs(trailingOnly=TRUE)
myquery1 = args[1]
myquery2 = args[2]

# Load_data----
setwd("D:/11_Coding_test/ffip_smart_patrol")

# READ IN ILLEGAL ACTIVITY AND ASSOCIATED EFFORT DATA

aktivitas_manusia <- read.csv("myquery1")
CAM <- aktivitas_manusia %>%
  st_as_sf(coords = c("X", "Y"), crs = 32747) %>%
  rename('Patrol_ID' ='Patrol.ID') %>%
  rename('Kategori_temuan' ='Observation.Category.1') %>%
  rename('Tanggal' = 'Waypoint.Date') %>%
  mutate(Tanggal = anytime(Tanggal)) %>%
  select(-`Observation.Category.0`) %>%
  mutate(Quarter = quarters(Tanggal), 
         Year = year(Tanggal)) %>%
  mutate(YearQuarter = paste(Year, Quarter, sep = "-")) %>%
  filter(Kategori_temuan == "Perburuan Satwa") # poaching-related threats

ringkasan_patroli <- st_read("myquery2")
CRP <- ringkasan_patroli %>% 
  mutate(Jarak = st_length(ringkasan_patroli)) %>%
  select(-Patrol_L_1, -Patrol_L_2, -Armed, -Patrol_Leg) %>%
  mutate(Quarter = quarters(Patrol_Sta), 
         Year = year(Patrol_Sta)) %>%
  mutate(YearQuarter = paste(Year, Quarter, sep = "-"))

# Load_polygon----
hex <- st_read("hex.shp")

# Calculate effort per pseudogrid----
# Ensure both datasets use the same CRS (coordinate reference system)
st_crs(CRP) <- st_crs(hex)

# Perform the spatial intersection to get the patrol segments within each grid
intersected_data <- st_intersection(CRP, hex)

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
st_crs(CAM) <- st_crs(hex)

# Step 3: Perform spatial intersection with hex_3k to count findings in each grid
intersected_findings <- st_intersection(CAM, hex)

# Step 4: Count the number of findings within each grid_id
qr_find <- intersected_findings %>%
  group_by(grid_id, YearQuarter) %>%
  summarize(findings = n())   %>% 
  as.data.frame() %>%
  select(-geometry)

qr_cpue <- qr_effort %>% 
  left_join(qr_find, by = c("grid_id", "YearQuarter")) %>%
  mutate(cpue = findings / effort_km) %>%
  replace_na(list(findings = 0,  cpue = 0)) %>%
  mutate(diff_cpue = ifelse(grepl("Q1", YearQuarter), NA, cpue - lag(cpue)),
         diff_effort = ifelse(grepl("Q1", YearQuarter), NA, effort_km - lag(effort_km))) 


# Remove rows with NA values resulting from the lag
qr_cpue1 <- na.omit(qr_cpue)

# plot 
# Plot the relationship between the difference in CPUE and the difference in patrol effort
ggplot(qr_cpue1, aes(x = diff_effort, y = diff_cpue)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_bw() +
  labs(title = "CPUE vs Patrol Effort",
       x = "Patrol Effort (km) (t-1)",
       y = "CPUE (t-1)") +
  # Add regression equation and R² to the top-right corner
  stat_regline_equation(aes(label = ..eq.label..), formula = y ~ x, 
                        label.x = Inf, label.y = Inf, hjust = 1.1, vjust = 1.2) +
  stat_cor(aes(label = ..rr.label..), label.x = Inf, label.y = Inf, 
           hjust = 1.1, vjust = 2.5)  # Adjust hjust and vjust for precise positioning

# inspect statistically
# Fit a GLS model accounting for ruggedness and proximity to boundary, assuming within-grid correlation
gls_model <- gls(diff_cpue ~ diff_effort, 
                 data = qr_cpue1)

# Summary of the GLS model
summary(gls_model)

qr_cpue2 <- qr_cpue %>%
  group_by(YearQuarter) %>%
  summarize(effort_kmtot = sum(effort_km),
            cpuetot=sum(cpue))

# Get the maximum values for scaling
max_effort <- max(qr_cpue2$effort_kmtot, na.rm = TRUE)
max_records <- max(qr_cpue2$cpuetot, na.rm = TRUE)

# Calculate the scaling factor based on max values
scaling_factor <- max_effort / max_records

# Create the combined plot
ggplot() +
  # Bar plot for the number of records
  geom_bar(data = qr_cpue2, aes(x = YearQuarter, y = cpuetot), 
           stat = "identity", fill = "lightblue", color = "black", alpha = 0.7) +
  
  # Line plot for the patrol effort (scaled down)
  geom_line(data = qr_cpue2, aes(x = YearQuarter, y = effort_kmtot / scaling_factor, group = 1), color = "blue") +
  geom_point(data = qr_cpue2, aes(x = YearQuarter, y = effort_kmtot / scaling_factor), color = "blue") +
  
  # Theme and labels
  theme_bw() +
  labs(title = "Patrol Effort and CPUE Over Time", x = "Year-Quarter") +
  
  # Left y-axis for number of records
  scale_y_continuous(name = "CPUE", 
                     sec.axis = sec_axis(~.*scaling_factor, name = "Patrol Effort (km)")) +
  
  # Customize x-axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
