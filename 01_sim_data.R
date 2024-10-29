library(dplyr)
library(ggplot2)
library(nlme)
library(ggpubr)

# Set seed for reproducibility
set.seed(212)

# Number of grids and years
n_grids <- 50
n_years <- 8

# Create a data frame representing grid IDs and years
grid_ids <- rep(1:n_grids, each = n_years)
years <- rep(1:n_years, times = n_grids)

# Simulate patrol effort (e.g., distance patrolled in km) for each grid and year
patrol_effort <- runif(n_grids * n_years, min = 5, max = 25)

# Simulate trail ruggedness (lower values mean easier trails) - one value per grid
ruggedness <- rep(runif(n_grids, min = 1, max = 5), each = n_years)

# Simulate proximity to national park boundary (smaller distance means closer to boundary) - one value per grid
proximity_to_boundary <- rep(runif(n_grids, min = 0, max = 10), each = n_years)

# Scenario where detterent effect is in place----
# Simulate poaching threats, incorporating ruggedness and proximity to boundary
poaching_threats <- rpois(n_grids * n_years, lambda = exp(3 - 0.1 * patrol_effort - 0.05 * ruggedness + 0.02 * proximity_to_boundary))

# Calculate Catch Per Unit Effort (CPUE)
CPUE <- poaching_threats / patrol_effort

# Create a data frame with patrol data
sim_data <- data.frame(
  grid_id = grid_ids,
  year = years,
  patrol_effort = patrol_effort,
  ruggedness = ruggedness,
  proximity_to_boundary = proximity_to_boundary,
  poaching_threats = poaching_threats,
  CPUE = CPUE
)

# Calculate the difference in CPUE and patrol effort between consecutive years within each grid
sim_data <- sim_data %>%
  group_by(year) %>%
  arrange(grid_id) %>%
  ungroup() %>%
  mutate(diff_CPUE = CPUE - lag(CPUE, n = 1),
         diff_patrol_effort = patrol_effort - lag(patrol_effort, n = 1)) 

# Remove rows with NA values resulting from the lag
sim_data <- na.omit(sim_data)


# Fit a GLS model accounting for ruggedness and proximity to boundary, assuming within-grid correlation
gls_model <- gls(diff_CPUE ~ diff_patrol_effort + ruggedness + proximity_to_boundary, 
                 correlation = corAR1(form = ~ year | grid_id),  # AR(1) correlation within grids over years
                 data = sim_data)

# Summary of the GLS model
summary(gls_model)

# Plot the relationship between the difference in CPUE and the difference in patrol effort
ggplot(sim_data, aes(x = diff_patrol_effort, y = diff_CPUE)) +
  geom_point() +
  geom_smooth(method="lm", color="#CD001A", se=F, 
              size=1.3, linetype="dashed") +
  theme_bw() +
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "CPUE vs Patrol Effort",
       x = "Patrol Effort (km) (t-1)",
       y = "CPUE (t-1)") +
  # Add regression equation and R² to the top-right corner
  stat_regline_equation(aes(label = ..eq.label..), formula = y ~ x, 
                        label.x = Inf, label.y = Inf, hjust = 1.1, vjust = 1.2) +
  stat_cor(aes(label = ..rr.label..), label.x = Inf, label.y = Inf, 
           hjust = 1.1, vjust = 2.5)  # Adjust hjust and vjust for precise positioning


# Simulate poaching threats proportional to patrol effort (no deterrent effect)----
# The expected number of threats increases with patrol effort
poaching_threats <- rpois(n_grids * n_years, 
                          lambda = patrol_effort * exp(0.1 - 0.5 * ruggedness + 0.02 * proximity_to_boundary))

# Calculate Catch Per Unit Effort (CPUE)
CPUE <- poaching_threats / patrol_effort

# Create a data frame with patrol data
sim_data <- data.frame(
  grid_id = grid_ids,
  year = years,
  patrol_effort = patrol_effort,
  ruggedness = ruggedness,
  proximity_to_boundary = proximity_to_boundary,
  poaching_threats = poaching_threats,
  CPUE = CPUE
)

# Calculate the difference in CPUE and patrol effort between consecutive years within each grid
sim_data <- sim_data %>%
  group_by(grid_id) %>%
  arrange(year) %>%
  mutate(
    diff_CPUE = CPUE - lag(CPUE),
    diff_patrol_effort = patrol_effort - lag(patrol_effort)
  ) %>%
  ungroup()

# Remove rows with NA values resulting from the lag
sim_data <- na.omit(sim_data)

# Fit a GLS model accounting for ruggedness and proximity to boundary, assuming within-grid correlation
gls_model <- gls(
  diff_CPUE ~ diff_patrol_effort + ruggedness + proximity_to_boundary, 
  correlation = corAR1(form = ~ year | grid_id),  # AR(1) correlation within grids over years
  data = sim_data
)

# Summary of the GLS model
summary(gls_model)

# Plot the relationship between the difference in CPUE and the difference in patrol effort
ggplot(sim_data, aes(x = diff_patrol_effort, y = diff_CPUE)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#CD001A", se = FALSE, 
              size = 1.3, linetype = "dashed") +
  geom_smooth(method = "loess", se = TRUE) +
  theme_bw() +
  labs(
    title = "CPUE vs Patrol Effort (No Deterrent Effect)",
    x = "Change in Patrol Effort (km)",
    y = "Change in CPUE"
  ) +
  # Add regression equation and R² to the top-right corner
  stat_regline_equation(
    aes(label = ..eq.label..), formula = y ~ x, 
    label.x = Inf, label.y = Inf, hjust = 1.1, vjust = 1.2
  ) +
  stat_cor(
    aes(label = ..rr.label..), label.x = Inf, label.y = Inf, 
    hjust = 1.1, vjust = 2.5
  )

