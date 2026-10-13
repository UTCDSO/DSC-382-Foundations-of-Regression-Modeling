library(tidyverse)
library(broom)
library(splines)

if (!interactive()) {
  pdf(NULL)
}

data_path <- if (file.exists("texas_grid_hourly.csv")) {
  "texas_grid_hourly.csv"
} else {
  file.path("modules", "08_curve_fitting_and_additive_models", "texas_grid_hourly.csv")
}

grid <- read_csv(
  data_path,
  col_types = cols(Timestamp = col_character()),
  show_col_types = FALSE
) %>%
  mutate(
    Timestamp = lubridate::ymd_hms(Timestamp, tz = "America/Chicago"),
    Month = factor(Month, levels = month.abb),
    DayOfWeek = factor(
      DayOfWeek,
      levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    ),
    Hour = factor(Hour, levels = 0:23),
    DemandGW = DemandMW / 1000,
    TempCentered = TempF - 65
  )

set.seed(38216)
grid %>% sample_n(8)

# 1. First look: demand versus temperature.
grid %>%
  sample_n(5500) %>%
  ggplot(aes(TempF, DemandGW)) +
  geom_point(alpha = 0.20) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    x = "Temperature (F)",
    y = "Demand (GW)",
    title = "Texas grid demand versus temperature"
  ) +
  theme_minimal(base_size = 14)

# 2. Calendar structure: demand is not a temperature-only story.
grid %>%
  group_by(Month, Hour) %>%
  summarize(DemandGW = mean(DemandGW), .groups = "drop") %>%
  ggplot(aes(Hour, Month, fill = DemandGW)) +
  geom_tile() +
  labs(
    x = "Hour of day",
    y = "Month",
    fill = "Demand (GW)",
    title = "Average grid demand by month and hour"
  ) +
  theme_minimal(base_size = 14)

# 3. Strip out the calendar so we can study the one-variable
#    temperature relationship first.
nuisance_model <- lm(
  DemandGW ~ Hour + DayOfWeek + Month + TrendYears,
  data = grid
)

grid_adjusted <- grid %>%
  mutate(DemandAdjusted = mean(DemandGW) + resid(nuisance_model))

# 4. K-nearest-neighbor smoothing: the by-hand instinct made precise.
#    To estimate the curve at temperature t, average the k nearest hours.
knn_curve <- function(targets, x, y, k) {
  vapply(targets, function(t) {
    mean(y[order(abs(x - t))[seq_len(k)]])
  }, numeric(1))
}

temp_targets <- seq(14, 108, length.out = 220)

knn_fits <- bind_rows(
  tibble(
    TempF = temp_targets,
    fit = knn_curve(temp_targets, grid_adjusted$TempF, grid_adjusted$DemandAdjusted, k = 100),
    k = "k = 100"
  ),
  tibble(
    TempF = temp_targets,
    fit = knn_curve(temp_targets, grid_adjusted$TempF, grid_adjusted$DemandAdjusted, k = 8000),
    k = "k = 8,000"
  )
)

grid_adjusted %>%
  sample_n(5500) %>%
  ggplot(aes(TempF, DemandAdjusted)) +
  geom_point(alpha = 0.08) +
  geom_line(data = knn_fits, aes(y = fit), color = "firebrick", linewidth = 1) +
  facet_wrap(~k) +
  labs(
    x = "Temperature (F)",
    y = "Demand, adjusted for calendar and trend (GW)",
    title = "KNN smoothing: small k is jittery, huge k blurs the curve"
  ) +
  theme_minimal(base_size = 13)

# Notice the boundary problem: above ~100F every neighbor is cooler,
# so the KNN curve flattens exactly where the grid operator cares most.

# 5. Local linear smoothing: fit a small weighted LINE near each target.
#    The local slope lets a rising edge stay rising at the boundary.
#    loess() with degree = 1 is a local linear smoother; span is the bandwidth.
grid_adjusted %>%
  sample_n(5500) %>%
  ggplot(aes(TempF, DemandAdjusted)) +
  geom_point(alpha = 0.12) +
  geom_smooth(method = "loess", se = FALSE, span = 0.45,
              method.args = list(degree = 1)) +
  labs(
    x = "Temperature (F)",
    y = "Demand, adjusted for calendar and trend (GW)",
    title = "Local linear smooth of adjusted demand"
  ) +
  theme_minimal(base_size = 14)

# Smoothers answer the one-variable question. But our question has a
# calendar in it, and adjustment is regression's home turf. We need a
# curve that lives INSIDE lm(): represent it with basis functions.

# 6. The obvious basis: polynomials. A straight term first...
linear_model <- lm(
  DemandGW ~ TempCentered + Hour + DayOfWeek + Month + TrendYears,
  data = grid
)

tidy(linear_model)

# ...then the quadratic basis (1, x, x^2), still ordinary least squares,
# and the calendar adjustment comes along for free.
quadratic_model <- lm(
  DemandGW ~ TempCentered + I(TempCentered^2) +
    Hour + DayOfWeek + Month + TrendYears,
  data = grid
)

tidy(quadratic_model)

# With a curve, the slope depends on where you stand:
quad_b1 <- coef(quadratic_model)[["TempCentered"]]
quad_b2 <- coef(quadratic_model)[["I(TempCentered^2)"]]

tibble(
  temperature = c(50, 90),
  estimated_slope_gw_per_degree = quad_b1 + 2 * quad_b2 * (temperature - 65)
)

# 7. The cubic adds asymmetry -- but polynomial basis functions are GLOBAL:
#    every coefficient touches every temperature, and high degrees
#    misbehave at the edges.
cubic_model <- lm(
  DemandGW ~ TempCentered + I(TempCentered^2) + I(TempCentered^3) +
    Hour + DayOfWeek + Month + TrendYears,
  data = grid
)

tidy(cubic_model)

# 8. Splines: piecewise polynomials glued together smoothly at knots.
#    ns() manufactures LOCAL basis functions -- look at the columns:
head(ns(grid$TempF, df = 6))

spline_model <- lm(
  DemandGW ~ ns(TempF, df = 6) +
    Hour + DayOfWeek + Month + TrendYears,
  data = grid
)

tidy(spline_model)

# 9. Build adjusted curves by averaging over the same mix of calendar values.
set.seed(38217)
reference_rows <- grid %>%
  sample_n(2600) %>%
  select(Hour, DayOfWeek, Month, TrendYears)

make_curve <- function(model, model_name) {
  temp_grid <- crossing(
    TempF = seq(18, 108, length.out = 160),
    ref_id = seq_len(nrow(reference_rows))
  ) %>%
    bind_cols(reference_rows[.$ref_id, ]) %>%
    mutate(TempCentered = TempF - 65)

  temp_grid$fit <- predict(model, newdata = temp_grid)

  temp_grid %>%
    group_by(TempF) %>%
    summarize(DemandGW = mean(fit), .groups = "drop") %>%
    mutate(model = model_name)
}

curves <- bind_rows(
  make_curve(linear_model, "Linear"),
  make_curve(quadratic_model, "Quadratic"),
  make_curve(cubic_model, "Cubic"),
  make_curve(spline_model, "Spline")
)

grid_adjusted %>%
  sample_n(5500) %>%
  ggplot(aes(TempF, DemandAdjusted)) +
  geom_point(alpha = 0.12) +
  geom_line(
    data = curves %>% filter(model %in% c("Quadratic", "Spline")),
    aes(y = DemandGW, color = model),
    linewidth = 1.2
  ) +
  labs(
    x = "Temperature (F)",
    y = "Demand, adjusted for calendar and trend (GW)",
    color = "Model",
    title = "Adjusted temperature curves"
  ) +
  theme_minimal(base_size = 14)

# 10. Model-based comparisons from the spline.
comparison_temps <- crossing(
  TempF = c(45, 65, 95),
  ref_id = seq_len(nrow(reference_rows))
) %>%
  bind_cols(reference_rows[.$ref_id, ]) %>%
  mutate(TempCentered = TempF - 65)

comparison_temps$fit <- predict(spline_model, newdata = comparison_temps)

comparison_temps %>%
  group_by(TempF) %>%
  summarize(adjusted_demand_gw = mean(fit), .groups = "drop")

# 11. Case 2: courier dispatch -- the spline composes with dummies.
courier_path <- if (file.exists("courier_deliveries.csv")) {
  "courier_deliveries.csv"
} else {
  file.path("modules", "08_curve_fitting_and_additive_models", "courier_deliveries.csv")
}

couriers <- read_csv(courier_path, show_col_types = FALSE) %>%
  mutate(vehicle = factor(vehicle, levels = c("bike", "car")))

couriers %>% sample_n(8)

# EDA first. The simplest comparison says the fleet is interchangeable:
couriers %>%
  group_by(vehicle) %>%
  summarize(deliveries = n(), mean_min = mean(delivery_min))

# ...but the same comparison within distance bands tells another story:
couriers %>%
  mutate(band = cut(distance_km, c(0, 2, 4, 8),
                    labels = c("under 2 km", "2-4 km", "over 4 km"))) %>%
  group_by(band, vehicle) %>%
  summarize(mean_min = mean(delivery_min), .groups = "drop") %>%
  pivot_wider(names_from = vehicle, values_from = mean_min)

# The one-minute overall gap averages two big differences in opposite
# directions. Somewhere in the middle, the vehicles trade places.

# The raw picture:
couriers %>%
  slice_sample(n = 3000) %>%
  ggplot(aes(distance_km, delivery_min, color = vehicle)) +
  geom_point(alpha = 0.25) +
  labs(
    x = "Trip distance (km)",
    y = "Delivery time (minutes)",
    title = "Every delivery, colored by vehicle"
  ) +
  theme_minimal(base_size = 14)

# Design check: nearest-courier dispatch keeps vehicle roughly unrelated
# to distance, so both curves are observed across the whole range.
couriers %>%
  group_by(vehicle) %>%
  summarize(mean_distance = mean(distance_km), share_peak = mean(peak))

ggplot(couriers, aes(distance_km, fill = vehicle)) +
  geom_density(alpha = 0.4, color = NA) +
  labs(
    x = "Trip distance (km)",
    y = "Density",
    title = "Distance distributions by vehicle overlap"
  ) +
  theme_minimal(base_size = 14)

# Binned means by vehicle: the two relationships cross.
couriers %>%
  mutate(bin = cut(distance_km, seq(0, 8, by = 0.5), include.lowest = TRUE)) %>%
  group_by(bin, vehicle) %>%
  summarize(mid = mean(distance_km), mean_min = mean(delivery_min),
            n = n(), .groups = "drop") %>%
  filter(n >= 15) %>%
  ggplot(aes(mid, mean_min, color = vehicle)) +
  geom_point(size = 2.5) +
  labs(
    x = "Trip distance (km)",
    y = "Mean delivery time (minutes)",
    title = "Bikes and cars cross near 3 km"
  ) +
  theme_minimal(base_size = 14)

# The additive model allows one shape, shifted. Parallel curves cannot
# cross -- and averaging over the crossover hides the vehicle effect.
courier_additive <- lm(
  delivery_min ~ ns(distance_km, 4) + vehicle + peak,
  data = couriers
)
tidy(courier_additive) %>% filter(term == "vehiclecar")

# One asterisk per claim: each vehicle its own curve, and rush hour
# allowed to cost cars more than bikes.
courier_interaction <- lm(
  delivery_min ~ ns(distance_km, 4) * vehicle + vehicle * peak,
  data = couriers
)

# Fitted curves by vehicle and regime.
courier_grid <- crossing(
  distance_km = seq(0.3, 8, by = 0.02),
  vehicle = factor(c("bike", "car"), levels = levels(couriers$vehicle)),
  peak = c(0, 1)
)
courier_grid$fit <- predict(courier_interaction, newdata = courier_grid)

ggplot(courier_grid, aes(distance_km, fit, color = vehicle)) +
  geom_line(linewidth = 1.1) +
  facet_wrap(~peak, labeller = labeller(peak = c(`0` = "Off-peak", `1` = "Peak"))) +
  labs(
    x = "Trip distance (km)",
    y = "Fitted delivery time (minutes)",
    title = "Each vehicle its own curve; the crossover moves with peak"
  ) +
  theme_minimal(base_size = 14)

# The crossover distances: where the fitted car and bike curves intersect.
courier_grid %>%
  pivot_wider(names_from = vehicle, values_from = fit) %>%
  mutate(gap = car - bike) %>%
  group_by(peak) %>%
  summarize(crossover_km = distance_km[which.min(abs(gap))])

# The dispatch rule is one cutoff PER REGIME, read straight off the model.

# 12. The same question, asked of the grid: does the temperature curve
#     change on weekends? Here the answer is no -- the curves come back
#     parallel, and the additive model was the right model all along.
grid_flagged <- grid %>%
  mutate(Weekend = factor(
    if_else(DayOfWeek %in% c("Saturday", "Sunday"), "weekend", "weekday"),
    levels = c("weekday", "weekend")
  ))

weekend_fit <- lm(
  DemandGW ~ ns(TempF, 6) * Weekend + Hour + Month + TrendYears,
  data = grid_flagged
)

weekend_grid <- crossing(
  TempF = seq(18, 108, by = 1),
  ref_id = seq_len(500)
) %>%
  bind_cols(reference_rows[.$ref_id, ] %>% select(-DayOfWeek)) %>%
  crossing(Weekend = factor(c("weekday", "weekend"),
                            levels = c("weekday", "weekend")))

weekend_grid$fit <- predict(weekend_fit, newdata = weekend_grid)

weekend_grid %>%
  group_by(TempF, Weekend) %>%
  summarize(DemandGW = mean(fit), .groups = "drop") %>%
  ggplot(aes(TempF, DemandGW, color = Weekend)) +
  geom_line(linewidth = 1.1) +
  labs(
    x = "Temperature (F)",
    y = "Adjusted demand (GW)",
    title = "Weekday and weekend temperature curves: parallel, not reshaped"
  ) +
  theme_minimal(base_size = 14)

# An interaction is a question you ask of the data, not a default you assume.
