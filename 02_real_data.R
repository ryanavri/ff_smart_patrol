library(tidyverse)
library(lubridate)
library(chron)
library(anytime)

library(sf)

ringkasan_patroli <- st_read("Jalur_Patroli_2019.shp")

CRP <- ringkasan_patroli %>%  #name of data stands for "Cleaned Ringkasan Patroli"
  mutate(Jarak = st_length(ringkasan_patroli)) %>%
  select(-Patrol_L_1, -Patrol_L_2, -Armed, -Patrol_Leg) %>%
  as.data.frame()

aktivitas_manusia <- read.csv("Aktivitas_Manusia_2019.csv")

CAM <- aktivitas_manusia %>%
  rename('Patrol_ID' ='Patrol.ID') %>%
  rename('Kategori_temuan' ='Observation.Category.1') %>%
  rename('Tanggal' = 'Waypoint.Date') %>%
  mutate(Tanggal = anytime(Tanggal)) %>%
  select(-`Observation.Category.0`)

# Add a quarter column based on the date
CRP <- CRP %>%
  mutate(Quarter = quarters(Patrol_Sta), 
         Year = year(Patrol_Sta))

# Combine Year and Quarter
CRP <- CRP %>%
  mutate(YearQuarter = paste(Year, Quarter, sep = "-"))

# Summarize effort by quarter
quarterly_effort <- CRP %>%
  group_by(YearQuarter) %>%
  summarise(Total_Effort = as.numeric(sum(Jarak))/1000)

# Plotting the quarterly effort
ggplot(quarterly_effort, aes(x = YearQuarter, y = Total_Effort)) +
  geom_line(group = 1, color = "blue") +
  geom_point(color = "blue") +
  theme_bw() + 
  labs(title = "Patrol Effort Over Time", x = "Year", y = "Patrol Effort (km)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Add a quarter and year column based on the date
CAM <- CAM %>%
  filter(Patrol_ID %in% CRP$Patrol_ID) %>%
  mutate(Quarter = quarters(Tanggal), 
         Year = year(Tanggal))
         
         # Combine Year and Quarter
         CAM <- CAM %>%
           mutate(YearQuarter = paste(Year, Quarter, sep = "-"))
         
         # Count the number of rows per quarter (which reflects the number of patrol records per quarter)
         quarterly_rows <- CAM %>%
           group_by(YearQuarter) %>%
           summarise(Num_Records = n()) %>%
           left_join(quarterly_effort, by="YearQuarter") %>%
           mutate(cpue = Num_Records/Total_Effort)
         
         # Plotting the number of rows (records) per quarter
         # Plotting the number of rows (records) per quarter as a bar plot
         ggplot(quarterly_rows, aes(x = YearQuarter, y = cpue)) +
           geom_bar(stat = "identity", fill = "blue", color = "black") +
           theme_bw() + 
           labs(title = "Number of Patrol Records Over Time", x = "Year-Quarter", y = "Number of Records") +
           theme(axis.text.x = element_text(angle = 45, hjust = 1))
         
         
# Join plot
         
# Get the maximum values for scaling
max_effort <- max(quarterly_rows$Total_Effort, na.rm = TRUE)
max_records <- max(quarterly_rows$cpue, na.rm = TRUE)

# Calculate the scaling factor based on max values
scaling_factor <- max_effort / max_records

# Create the combined plot
ggplot() +
  # Bar plot for the number of records
  geom_bar(data = quarterly_rows, aes(x = YearQuarter, y = cpue), 
  stat = "identity", fill = "lightblue", color = "black", alpha = 0.7) +
  
  # Line plot for the patrol effort (scaled down)
  geom_line(data = quarterly_rows, aes(x = YearQuarter, y = Total_Effort / scaling_factor, group = 1), color = "blue") +
  geom_point(data = quarterly_rows, aes(x = YearQuarter, y = Total_Effort / scaling_factor), color = "blue") +
  
  # Theme and labels
  theme_bw() +
  labs(title = "Patrol Effort and and CPUE Over Time", x = "Year-Quarter") +
  
  # Left y-axis for number of records
  scale_y_continuous(name = "CPUE", 
  sec.axis = sec_axis(~.*scaling_factor, name = "Patrol Effort (km)")) +
  
  # Customize x-axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))