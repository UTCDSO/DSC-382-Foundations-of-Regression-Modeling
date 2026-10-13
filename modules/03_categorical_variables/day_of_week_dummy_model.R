library(tidyverse)
library(broom)

data_path = if (file.exists("texas_grid_hourly.csv")) {
  "texas_grid_hourly.csv"
} else {
  file.path("modules", "03_categorical_variables", "texas_power_grid_hourly.csv")
}

power_grid = read_csv(data_path, show_col_types = FALSE) 

head(power_grid)

# Day of week is a raw character vector -- let's make sure
# R understands it is an ordered factor
power_grid = power_grid %>%
  mutate(
    DayOfWeek = factor(DayOfWeek,
      levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    ),
    DemandGW = DemandMW / 1000
  )

head(power_grid)

# One categorical predictor with K=7 levels:
levels(power_grid$DayOfWeek)

# Visualize the seven groups.
ggplot(power_grid, aes(DayOfWeek, DemandGW)) +
  geom_boxplot(fill = "#d9e2f2", color = "#1f2430", outlier.alpha = 0.15) +
  labs(
    x = "Day of week",
    y = "Demand (GW)",
    title = "Texas Grid demand by day of week"
  ) +
  theme_minimal(base_size = 14) + 
  coord_flip()

# The raw group means are what the dummy-variable model will reproduce.
day_means = power_grid %>%
  group_by(DayOfWeek) %>%
  summarize(
    n = n(),
    mean_demand_gw = mean(DemandGW)
  )

day_means

# Fit the dummy-variable model.
# Because Monday is the first factor level, Monday is the baseline.
monday_baseline_model = lm(DemandGW ~ DayOfWeek, data = power_grid)

# Interpretation:
#   intercept = Monday mean
#   each DayOfWeek coefficient = that day's mean minus Monday's mean
coef(monday_baseline_model) %>%
  round(2)


# Change the baseline.
# fct_relevel() comes from forcats, which is loaded by tidyverse.
power_grid_sunday = power_grid %>%
  mutate(DayOfWeek = fct_relevel(DayOfWeek, "Sunday"))

levels(power_grid_sunday$DayOfWeek)

sunday_baseline_model = lm(DemandGW ~ DayOfWeek, data = power_grid_sunday)

# Now:
#   intercept = Sunday mean
#   each DayOfWeek coefficient = that day's mean minus Sunday mean
coef(sunday_baseline_model) %>%
  round(2)

coef(monday_baseline_model) %>%
  round(2)

# spot check for Wednesday under both models: looks good up to rounding error
41.36 + 2.90  # sunday baseline + wednesday offset
44.77 + (-0.50) # monday baseline + wednesday offset
day_means
