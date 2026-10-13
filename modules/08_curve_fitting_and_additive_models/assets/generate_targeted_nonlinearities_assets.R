library(dplyr)
library(ggplot2)
library(readr)
library(scales)
library(splines)
library(tidyr)

set.seed(38216)

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- sub("^--file=", "", script_arg[1] %||% "assets/generate_targeted_nonlinearities_assets.R")
out_dir <- dirname(normalizePath(script_path))
module_dir <- normalizePath(file.path(out_dir, ".."))

save_plot <- function(plot, name, width = 9.6, height = 6) {
  ggsave(
    filename = file.path(out_dir, name),
    plot = plot,
    width = width,
    height = height,
    dpi = 150,
    bg = "#fcfbf7"
  )
}

theme_classroom <- function(base_size = 18) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background = element_rect(fill = "#fcfbf7", color = NA),
      panel.background = element_rect(fill = "#fcfbf7", color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#d9e2f2", linewidth = 0.35),
      axis.title = element_text(color = "#1f2430", face = "bold"),
      axis.text = element_text(color = "#1f2430"),
      plot.title = element_text(color = "#163a5f", face = "bold", size = 24),
      plot.subtitle = element_text(color = "#1f2430", size = 18),
      strip.text = element_text(color = "#163a5f", face = "bold", size = 15),
      strip.background = element_rect(fill = "#eef3f8", color = NA),
      legend.position = "bottom",
      legend.title = element_blank()
    )
}

blue <- "#2b6cb0"
red <- "#8b2e2e"
gold <- "#e2b84b"
green <- "#007d62"
purple <- "#2f2544"
gray <- "#6b7280"

timestamps <- seq(
  as.POSIXct("2023-01-01 00:00:00", tz = "America/Chicago"),
  as.POSIXct("2025-12-31 23:00:00", tz = "America/Chicago"),
  by = "hour"
)

day_levels <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

grid <- tibble(timestamp = timestamps) %>%
  mutate(
    Date = as.Date(timestamp, tz = "America/Chicago"),
    Hour = as.integer(format(timestamp, "%H")),
    DayOfWeek = factor(weekdays(Date), levels = day_levels),
    Month = factor(month.abb[as.integer(format(timestamp, "%m"))], levels = month.abb),
    Year = as.integer(format(timestamp, "%Y")),
    DayOfYear = as.integer(format(Date, "%j")),
    DayIndex = as.integer(Date - min(Date)),
    TrendYears = as.numeric(difftime(timestamp, min(timestamp), units = "days")) / 365.25
  )

daily_weather <- grid %>%
  distinct(Date, DayOfYear, DayIndex) %>%
  mutate(
    seasonal_mean = 68 + 20 * sin(2 * pi * (DayOfYear - 109) / 365.25),
    weather_anomaly = as.numeric(arima.sim(model = list(ar = 0.82), n = n(), sd = 6.2)) +
      rt(n(), df = 4) * 1.5,
    daily_mean_temp = seasonal_mean + weather_anomaly,
    diurnal_amp = 7.0 + 2.5 * sin(2 * pi * (DayOfYear - 115) / 365.25)
  ) %>%
  select(Date, daily_mean_temp, diurnal_amp)

grid <- grid %>%
  left_join(daily_weather, by = "Date") %>%
  mutate(
    TempF = daily_mean_temp +
      diurnal_amp * sin(2 * pi * (Hour - 15) / 24) +
      rnorm(n(), 0, 1.35),
    TempF = pmin(pmax(TempF, 10), 112),
    cooling = pmax(TempF - 72, 0),
    heating = pmax(58 - TempF, 0),
    hour_effect = 3100 * sin(2 * pi * (Hour - 14) / 24) +
      1500 * sin(4 * pi * (Hour - 7) / 24),
    day_effect = case_when(
      DayOfWeek == "Saturday" ~ -2100,
      DayOfWeek == "Sunday" ~ -2600,
      DayOfWeek == "Monday" ~ 450,
      TRUE ~ 0
    ),
    lighting_effect = 2100 * (1 - sin(2 * pi * (DayOfYear - 80) / 365.25)) / 2,
    trend_effect = 3100 * TrendYears / 3,
    temp_effect = 175 * cooling + 17 * cooling^2 +
      115 * heating + 6.5 * heating^2,
    event_noise = if_else(runif(n()) < 0.006, rnorm(n(), 0, 5200), 0),
    DemandMW = 38500 + temp_effect + hour_effect + day_effect +
      lighting_effect + trend_effect + rnorm(n(), 0, 1550) + event_noise,
    DemandMW = round(pmax(DemandMW, 26000)),
    DemandGW = DemandMW / 1000,
    TempCentered = TempF - 65
  ) %>%
  select(timestamp, Date, Year, Month, DayOfWeek, Hour, TempF, DemandMW, DemandGW, TrendYears, TempCentered)

write_csv(
  grid %>%
    transmute(
      Timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S"),
      Date,
      Year,
      Month,
      DayOfWeek,
      Hour,
      TempF = round(TempF, 1),
      DemandMW,
      TrendYears = round(TrendYears, 4)
    ),
  file.path(module_dir, "texas_grid_hourly.csv")
)

grid <- grid %>%
  mutate(
    Hour = factor(Hour, levels = 0:23),
    Month = factor(Month, levels = month.abb),
    DayOfWeek = factor(DayOfWeek, levels = day_levels)
  )

raw_fit <- lm(DemandGW ~ TempF, data = grid)
nuisance_fit <- lm(DemandGW ~ Hour + DayOfWeek + Month + TrendYears, data = grid)
linear_fit <- lm(DemandGW ~ TempCentered + Hour + DayOfWeek + Month + TrendYears, data = grid)
quadratic_fit <- lm(DemandGW ~ TempCentered + I(TempCentered^2) + Hour + DayOfWeek + Month + TrendYears, data = grid)
cubic_fit <- lm(DemandGW ~ TempCentered + I(TempCentered^2) + I(TempCentered^3) + Hour + DayOfWeek + Month + TrendYears, data = grid)
spline_fit <- lm(DemandGW ~ ns(TempF, df = 6) + Hour + DayOfWeek + Month + TrendYears, data = grid)

grid_adjusted <- grid %>%
  mutate(
    DemandAdjusted = mean(DemandGW) + resid(nuisance_fit),
    temp_bin = cut_width(TempF, width = 4, boundary = 0)
  )

set.seed(38217)
plot_sample <- grid %>% slice_sample(n = 5500)
adjusted_sample <- grid_adjusted %>% slice_sample(n = 5500)

ref_rows <- grid %>%
  slice_sample(n = 2600) %>%
  select(Hour, DayOfWeek, Month, TrendYears)

temp_seq <- seq(18, 108, length.out = 160)

standardized_predictions <- function(model, model_name) {
  newdata <- expand_grid(TempF = temp_seq, ref_id = seq_len(nrow(ref_rows))) %>%
    bind_cols(ref_rows[.$ref_id, ]) %>%
    mutate(TempCentered = TempF - 65)

  newdata$fit <- as.numeric(predict(model, newdata = newdata))

  newdata %>%
    group_by(TempF) %>%
    summarize(DemandGW = mean(fit), Model = model_name, .groups = "drop")
}

curve_quadratic <- standardized_predictions(quadratic_fit, "Quadratic")
curve_linear <- standardized_predictions(linear_fit, "Linear")
curve_cubic <- standardized_predictions(cubic_fit, "Cubic")
curve_spline <- standardized_predictions(spline_fit, "Spline")

curve_all <- bind_rows(curve_linear, curve_quadratic, curve_cubic, curve_spline)

p_raw <- ggplot(plot_sample, aes(TempF, DemandGW)) +
  geom_point(color = purple, alpha = 0.18, size = 1.3) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = red, linewidth = 1.25) +
  scale_y_continuous(labels = label_number(suffix = " GW")) +
  labs(
    title = "Texas grid demand versus temperature",
    subtitle = "Hourly observations, 2023-2025",
    x = "Temperature (F)",
    y = "Demand"
  ) +
  theme_classroom()
save_plot(p_raw, "grid-demand-temperature-raw.png")

hour_month <- grid %>%
  group_by(Month, Hour) %>%
  summarize(DemandGW = mean(DemandGW), .groups = "drop")

p_heatmap <- ggplot(hour_month, aes(Hour, Month, fill = DemandGW)) +
  geom_tile(color = "#fcfbf7", linewidth = 0.25) +
  scale_fill_gradientn(
    colors = c("#eef3f8", "#bdd7e7", "#6baed6", "#2b6cb0", "#163a5f"),
    labels = label_number(suffix = " GW")
  ) +
  labs(
    title = "Demand also has calendar structure",
    subtitle = "Average demand by month and hour of day",
    x = "Hour of day",
    y = "Month",
    fill = "Demand"
  ) +
  theme_classroom(base_size = 16) +
  theme(panel.grid = element_blank(), axis.text.x = element_text(size = 10))
save_plot(p_heatmap, "grid-hour-month-heatmap.png", width = 9.8, height = 6.2)

monthly <- grid %>%
  group_by(Month) %>%
  summarize(
    average_temp = mean(TempF),
    average_demand = mean(DemandGW),
    .groups = "drop"
  ) %>%
  mutate(
    z_temp = as.numeric(scale(average_temp)),
    z_demand = as.numeric(scale(average_demand))
  ) %>%
  pivot_longer(c(z_temp, z_demand), names_to = "quantity", values_to = "z") %>%
  mutate(
    quantity = recode(quantity, z_temp = "Average temperature", z_demand = "Average demand")
  )

p_month <- ggplot(monthly, aes(Month, z, fill = quantity)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.64) +
  geom_hline(yintercept = 0, color = "#1f2430", linewidth = 0.5) +
  scale_fill_manual(values = c("Average temperature" = gold, "Average demand" = blue)) +
  labs(
    title = "Temperature and calendar are tangled",
    subtitle = "Monthly z-scores for temperature and demand",
    x = "Month",
    y = "Z-score"
  ) +
  theme_classroom(base_size = 16)
save_plot(p_month, "grid-month-zbars.png")

p_adjusted_quad <- ggplot(adjusted_sample, aes(TempF, DemandAdjusted)) +
  geom_point(color = purple, alpha = 0.14, size = 1.2) +
  geom_line(data = curve_quadratic, aes(y = DemandGW), color = blue, linewidth = 1.35) +
  scale_y_continuous(labels = label_number(suffix = " GW")) +
  labs(
    title = "Quadratic temperature curve after adjustment",
    subtitle = "Adjusted for hour, day of week, month, and trend",
    x = "Temperature (F)",
    y = "Demand, adjusted for calendar and trend"
  ) +
  theme_classroom()
save_plot(p_adjusted_quad, "grid-adjusted-quadratic.png")

p_poly <- ggplot(adjusted_sample, aes(TempF, DemandAdjusted)) +
  geom_point(color = purple, alpha = 0.10, size = 1.1) +
  geom_line(
    data = curve_all %>% filter(Model %in% c("Linear", "Quadratic", "Cubic")),
    aes(y = DemandGW, color = Model),
    linewidth = 1.2
  ) +
  scale_color_manual(values = c("Linear" = gray, "Quadratic" = blue, "Cubic" = red)) +
  scale_y_continuous(labels = label_number(suffix = " GW")) +
  labs(
    title = "Linear, quadratic, and cubic temperature terms",
    subtitle = "Same calendar adjustment in all three models",
    x = "Temperature (F)",
    y = "Demand, adjusted for calendar and trend"
  ) +
  theme_classroom()
save_plot(p_poly, "grid-polynomial-comparison.png")

p_spline <- ggplot(adjusted_sample, aes(TempF, DemandAdjusted)) +
  geom_point(color = purple, alpha = 0.10, size = 1.1) +
  geom_line(
    data = curve_all %>% filter(Model %in% c("Quadratic", "Spline")),
    aes(y = DemandGW, color = Model),
    linewidth = 1.25
  ) +
  scale_color_manual(values = c("Quadratic" = blue, "Spline" = green)) +
  scale_y_continuous(labels = label_number(suffix = " GW")) +
  labs(
    title = "Spline temperature curve after adjustment",
    subtitle = "A targeted nonlinear term for temperature",
    x = "Temperature (F)",
    y = "Demand, adjusted for calendar and trend"
  ) +
  theme_classroom()
save_plot(p_spline, "grid-spline-comparison.png")

bin_summary <- grid_adjusted %>%
  mutate(temp_bin_mid = round(TempF / 4) * 4) %>%
  group_by(temp_bin_mid) %>%
  summarize(
    DemandAdjusted = mean(DemandAdjusted),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= 20)

local_window <- adjusted_sample %>%
  filter(between(TempF, 85, 95))

local_fit <- lm(DemandAdjusted ~ TempF, data = local_window)
local_line <- tibble(TempF = seq(85, 95, length.out = 40))
local_line$DemandAdjusted <- predict(local_fit, newdata = local_line)

p_local <- ggplot(adjusted_sample, aes(TempF, DemandAdjusted)) +
  geom_point(color = purple, alpha = 0.08, size = 1.0) +
  geom_point(data = local_window, color = gold, alpha = 0.28, size = 1.3) +
  geom_line(
    data = curve_spline,
    aes(y = DemandGW),
    color = green,
    linewidth = 1.2
  ) +
  geom_line(data = local_line, color = red, linewidth = 1.6) +
  annotate("rect", xmin = 85, xmax = 95, ymin = -Inf, ymax = Inf, alpha = 0.06, fill = red) +
  scale_y_continuous(labels = label_number(suffix = " GW")) +
  labs(
    title = "Local linear smoothing idea",
    subtitle = "Fit a small line using nearby temperatures, then slide the window",
    x = "Temperature (F)",
    y = "Demand, adjusted for calendar and trend"
  ) +
  theme_classroom()
save_plot(p_local, "grid-local-linear-window.png")

prediction_newdata <- tibble(TempF = c(45, 65, 95)) %>%
  expand_grid(ref_id = seq_len(nrow(ref_rows))) %>%
  bind_cols(ref_rows[.$ref_id, ]) %>%
  mutate(TempCentered = TempF - 65)

prediction_newdata$fit <- as.numeric(predict(spline_fit, newdata = prediction_newdata))

prediction_cases <- prediction_newdata %>%
  group_by(TempF) %>%
  summarize(adjusted_demand_gw = mean(fit), .groups = "drop")

model_summary <- tibble(
  quantity = c(
    "n_rows",
    "mean_demand_gw",
    "mean_temp_f",
    "raw_temp_slope_gw_per_f",
    "linear_adjusted_temp_slope_gw_per_f",
    "quadratic_b1_centered_65",
    "quadratic_b2_centered_65",
    "quadratic_slope_at_50_gw_per_f",
    "quadratic_slope_at_90_gw_per_f",
    "spline_adjusted_demand_45f",
    "spline_adjusted_demand_65f",
    "spline_adjusted_demand_95f",
    "spline_95_minus_65_gw",
    "spline_45_minus_65_gw"
  ),
  value = c(
    nrow(grid),
    mean(grid$DemandGW),
    mean(grid$TempF),
    coef(raw_fit)[["TempF"]],
    coef(linear_fit)[["TempCentered"]],
    coef(quadratic_fit)[["TempCentered"]],
    coef(quadratic_fit)[["I(TempCentered^2)"]],
    coef(quadratic_fit)[["TempCentered"]] + 2 * coef(quadratic_fit)[["I(TempCentered^2)"]] * (50 - 65),
    coef(quadratic_fit)[["TempCentered"]] + 2 * coef(quadratic_fit)[["I(TempCentered^2)"]] * (90 - 65),
    prediction_cases$adjusted_demand_gw[prediction_cases$TempF == 45],
    prediction_cases$adjusted_demand_gw[prediction_cases$TempF == 65],
    prediction_cases$adjusted_demand_gw[prediction_cases$TempF == 95],
    prediction_cases$adjusted_demand_gw[prediction_cases$TempF == 95] -
      prediction_cases$adjusted_demand_gw[prediction_cases$TempF == 65],
    prediction_cases$adjusted_demand_gw[prediction_cases$TempF == 45] -
      prediction_cases$adjusted_demand_gw[prediction_cases$TempF == 65]
  )
)

write_csv(model_summary, file.path(out_dir, "grid_model_summary.csv"))

write_csv(
  tibble(TempF = temp_seq) %>%
    left_join(curve_quadratic %>% select(TempF, quadratic_demand_gw = DemandGW), by = "TempF") %>%
    left_join(curve_spline %>% select(TempF, spline_demand_gw = DemandGW), by = "TempF"),
  file.path(out_dir, "grid_temperature_curves.csv")
)

# Smoothing-first assets: KNN, local linear, and the spline basis ------------

set.seed(38218)

# KNN smoother: mean demand among the k nearest hours by temperature
knn_curve <- function(targets, x, y, k) {
  vapply(targets, function(t) {
    mean(y[order(abs(x - t))[seq_len(k)]])
  }, numeric(1))
}

knn_targets <- seq(14, 108, length.out = 220)
knn_small <- knn_curve(knn_targets, grid_adjusted$TempF, grid_adjusted$DemandAdjusted, k = 100)
knn_large <- knn_curve(knn_targets, grid_adjusted$TempF, grid_adjusted$DemandAdjusted, k = 8000)

knn_fits <- tibble(
  TempF = rep(knn_targets, 2),
  fit = c(knn_small, knn_large),
  k = rep(c("k = 100 neighbors", "k = 8,000 neighbors"), each = length(knn_targets))
) %>%
  mutate(k = factor(k, levels = c("k = 100 neighbors", "k = 8,000 neighbors")))

p_knn <- ggplot(adjusted_sample, aes(TempF, DemandAdjusted)) +
  geom_point(alpha = 0.06, color = gray) +
  geom_line(data = knn_fits, aes(TempF, fit), color = red, linewidth = 1.1) +
  facet_wrap(~k) +
  labs(
    title = "The same idea, two settings of k",
    subtitle = "Average the k nearest hours at each temperature.",
    x = "Temperature, F",
    y = "Demand, adjusted for calendar and trend"
  ) +
  theme_classroom(base_size = 16)
save_plot(p_knn, "grid-knn-fits.png", width = 10.6, height = 5.6)

# KNN versus local linear at the hot boundary
knn_mid <- knn_curve(knn_targets, grid_adjusted$TempF, grid_adjusted$DemandAdjusted, k = 1500)
loess_fit <- loess(
  DemandAdjusted ~ TempF,
  data = grid_adjusted,
  degree = 1,
  span = 0.15
)
local_curve <- tibble(
  TempF = knn_targets,
  fit = predict(loess_fit, newdata = tibble(TempF = knn_targets))
)

boundary_fits <- bind_rows(
  tibble(TempF = knn_targets, fit = knn_mid, method = "KNN, k = 1,500"),
  local_curve %>% mutate(method = "Local linear")
)

p_boundary <- ggplot(
  adjusted_sample %>% filter(TempF >= 85),
  aes(TempF, DemandAdjusted)
) +
  geom_point(alpha = 0.15, color = gray) +
  geom_line(
    data = boundary_fits %>% filter(TempF >= 85),
    aes(TempF, fit, color = method),
    linewidth = 1.3
  ) +
  scale_color_manual(values = c("KNN, k = 1,500" = gold, "Local linear" = green)) +
  labs(
    title = "The hot edge of the data",
    subtitle = "Past 100F every neighbor is cooler, so KNN flattens.",
    x = "Temperature, F",
    y = "Demand, adjusted for calendar and trend"
  ) +
  theme_classroom(base_size = 16)
save_plot(p_boundary, "grid-knn-vs-local.png", width = 9.6, height = 5.8)

# the natural spline basis over temperature
basis <- as.data.frame(ns(knn_targets, df = 6))
names(basis) <- paste0("b", seq_len(ncol(basis)))
basis_long <- basis %>%
  mutate(TempF = knn_targets) %>%
  pivot_longer(-TempF, names_to = "basis_fn", values_to = "value")

p_basis <- ggplot(basis_long, aes(TempF, value, color = basis_fn)) +
  geom_line(linewidth = 1.15) +
  scale_color_manual(values = c(blue, red, gold, green, purple, "#c05299")) +
  guides(color = "none") +
  labs(
    title = "The six building blocks of ns(TempF, df = 6)",
    subtitle = "Each curve is one basis function of temperature.",
    x = "Temperature, F",
    y = "Basis function value"
  ) +
  theme_classroom(base_size = 16)
save_plot(p_basis, "grid-spline-basis.png", width = 9.6, height = 5.6)

# Case 2: courier dispatch -- spline-by-dummy interaction --------------------

set.seed(38219)

n_orders <- 6000

couriers <- tibble(
  order_id = sprintf("D%05d", sample(10000:99999, n_orders)),
  distance_km = round(pmin(8, pmax(0.3, rlnorm(n_orders, log(2.2), 0.6))), 2),
  vehicle = sample(c("bike", "car"), n_orders, replace = TRUE, prob = c(0.55, 0.45)),
  peak = rbinom(n_orders, 1, 0.35)
) %>%
  mutate(
    mu = if_else(
      vehicle == "bike",
      6.0 + 4.2 * distance_km + 0.10 * distance_km^2 + 2.0 * peak,
      12.5 + 1.8 * distance_km + 7.0 * peak
    ),
    delivery_min = round(pmax(3, mu + rnorm(n_orders, 0, if_else(vehicle == "bike", 2.8, 3.2))), 1)
  ) %>%
  select(order_id, distance_km, vehicle, peak, delivery_min)

write_csv(couriers, file.path(module_dir, "courier_deliveries.csv"))

couriers <- couriers %>%
  mutate(vehicle = factor(vehicle, levels = c("bike", "car")))

# additive model: one shape, shifted
courier_additive <- lm(
  delivery_min ~ ns(distance_km, df = 4) + vehicle + peak,
  data = couriers
)

# interaction model: each vehicle its own curve, each vehicle its own peak shift
courier_interaction <- lm(
  delivery_min ~ ns(distance_km, df = 4) * vehicle + vehicle * peak,
  data = couriers
)

readr::write_csv(
  broom::tidy(courier_interaction, conf.int = TRUE),
  file.path(out_dir, "courier_interaction_summary.csv")
)

# fitted curves on a grid, by vehicle and regime
courier_grid <- tidyr::expand_grid(
  distance_km = seq(0.3, 8, by = 0.02),
  vehicle = factor(c("bike", "car"), levels = c("bike", "car")),
  peak = c(0, 1)
)
courier_grid$fit_additive <- predict(courier_additive, newdata = courier_grid)
courier_grid$fit_interaction <- predict(courier_interaction, newdata = courier_grid)

# crossover distances: where the fitted car and bike curves intersect
crossover <- courier_grid %>%
  select(distance_km, vehicle, peak, fit_interaction) %>%
  tidyr::pivot_wider(names_from = vehicle, values_from = fit_interaction) %>%
  mutate(gap = car - bike) %>%
  group_by(peak) %>%
  summarize(crossover_km = distance_km[which.min(abs(gap))], .groups = "drop")
write_csv(crossover, file.path(out_dir, "courier_crossover.csv"))

# comparison table at chosen distances
courier_compare <- courier_grid %>%
  filter(distance_km %in% c(1, 3, 6)) %>%
  select(distance_km, vehicle, peak, fit_interaction) %>%
  arrange(peak, distance_km, vehicle)
write_csv(courier_compare, file.path(out_dir, "courier_compare_table.csv"))

# binned means by vehicle (both regimes pooled for the first look)
courier_bins <- couriers %>%
  mutate(bin = cut(distance_km, breaks = seq(0, 8, by = 0.5), include.lowest = TRUE)) %>%
  group_by(bin, vehicle) %>%
  summarize(
    mid = mean(distance_km),
    mean_min = mean(delivery_min),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= 15)

p_courier_bins <- ggplot(courier_bins, aes(mid, mean_min, color = vehicle)) +
  geom_point(aes(size = n), alpha = 0.85) +
  scale_color_manual(values = c(bike = green, car = purple), labels = c("Bike", "Car")) +
  scale_size_continuous(range = c(2, 5), guide = "none") +
  labs(
    title = "Mean delivery time by distance, bikes versus cars",
    subtitle = "Binned means. The two relationships cross.",
    x = "Trip distance, km",
    y = "Mean delivery time, minutes"
  ) +
  theme_classroom(base_size = 16)
save_plot(p_courier_bins, "courier-binned-means.png", width = 9.6, height = 5.8)

# the additive model's structural failure: parallel curves cannot cross
p_courier_additive <- ggplot() +
  geom_point(data = courier_bins, aes(mid, mean_min, color = vehicle), alpha = 0.55, size = 2.6) +
  geom_line(
    data = courier_grid %>% filter(peak == 0),
    aes(distance_km, fit_additive, color = vehicle),
    linewidth = 1.2
  ) +
  scale_color_manual(values = c(bike = green, car = purple), labels = c("Bike", "Car")) +
  labs(
    title = "The additive model: one shape, shifted",
    subtitle = "Parallel curves can never cross. The data do.",
    x = "Trip distance, km",
    y = "Delivery time, minutes"
  ) +
  theme_classroom(base_size = 16)
save_plot(p_courier_additive, "courier-additive-fail.png", width = 9.6, height = 5.8)

# the interaction model: two shapes, and a crossover that moves with peak
regime_labels <- c(`0` = "Off-peak", `1` = "Peak hours")
cross_lines <- crossover %>% mutate(peak_lab = regime_labels[as.character(peak)])

p_courier_interaction <- ggplot(
  courier_grid %>% mutate(peak_lab = regime_labels[as.character(peak)]),
  aes(distance_km, fit_interaction, color = vehicle)
) +
  geom_line(linewidth = 1.25) +
  geom_vline(
    data = cross_lines,
    aes(xintercept = crossover_km),
    linetype = "dashed",
    color = gray
  ) +
  geom_text(
    data = cross_lines,
    aes(x = crossover_km + 0.15, y = 8.5, label = sprintf("%.1f km", crossover_km)),
    inherit.aes = FALSE,
    hjust = 0, size = 4.6, color = "#1f2430"
  ) +
  facet_wrap(~peak_lab) +
  scale_color_manual(values = c(bike = green, car = purple), labels = c("Bike", "Car")) +
  labs(
    title = "Each vehicle its own curve -- and the crossover moves",
    subtitle = "Fitted curves from ns(distance, 4) * vehicle + vehicle * peak.",
    x = "Trip distance, km",
    y = "Fitted delivery time, minutes"
  ) +
  theme_classroom(base_size = 16)
save_plot(p_courier_interaction, "courier-interaction-curves.png", width = 10.6, height = 5.6)

# Coda: the same question on the grid data, where the answer is "no" ---------

weekend_fit <- lm(
  DemandGW ~ ns(TempF, df = 6) * WeekendFlag + Hour + Month + TrendYears,
  data = grid %>% mutate(WeekendFlag = factor(if_else(DayOfWeek %in% c("Saturday", "Sunday"), "weekend", "weekday"), levels = c("weekday", "weekend")))
)

weekend_grid <- tidyr::expand_grid(
  TempF = seq(18, 108, by = 0.5),
  WeekendFlag = factor(c("weekday", "weekend"), levels = c("weekday", "weekend"))
) %>%
  tidyr::expand_grid(ref_rows %>% slice_sample(n = 500)) %>%
  mutate(TempCentered = TempF - 65)
weekend_grid$fit <- predict(weekend_fit, newdata = weekend_grid)

weekend_curves <- weekend_grid %>%
  group_by(TempF, WeekendFlag) %>%
  summarize(DemandGW = mean(fit), .groups = "drop")
write_csv(weekend_curves, file.path(out_dir, "grid_weekend_curves.csv"))

p_weekend <- ggplot(weekend_curves, aes(TempF, DemandGW, color = WeekendFlag)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = c(weekday = blue, weekend = gold), labels = c("Weekday", "Weekend")) +
  labs(
    title = "The same question, asked of the grid",
    subtitle = "Two fitted temperature curves from ns(TempF, 6) * weekend.",
    x = "Temperature, F",
    y = "Adjusted demand, GW"
  ) +
  theme_classroom(base_size = 16)
save_plot(p_weekend, "grid-weekend-curves.png", width = 9.6, height = 5.6)

# EDA assets for the courier case --------------------------------------------

# overall means by vehicle: the tempting first comparison
courier_overall <- couriers %>%
  group_by(vehicle) %>%
  summarize(
    deliveries = n(),
    mean_min = mean(delivery_min),
    mean_distance = mean(distance_km),
    share_peak = mean(peak),
    .groups = "drop"
  )
write_csv(courier_overall, file.path(out_dir, "courier_overall_means.csv"))

# the same comparison within distance bands
courier_bands <- couriers %>%
  mutate(band = cut(distance_km, c(0, 2, 4, 8),
                    labels = c("under 2 km", "2 to 4 km", "over 4 km"))) %>%
  group_by(band, vehicle) %>%
  summarize(deliveries = n(), mean_min = mean(delivery_min), .groups = "drop")
write_csv(courier_bands, file.path(out_dir, "courier_band_means.csv"))

# raw scatter, colored by vehicle
p_courier_raw <- ggplot(
  couriers %>% slice_sample(n = 3000),
  aes(distance_km, delivery_min, color = vehicle)
) +
  geom_point(alpha = 0.25, size = 1.4) +
  scale_color_manual(values = c(bike = green, car = purple), labels = c("Bike", "Car")) +
  labs(
    title = "Every delivery: time versus distance",
    subtitle = "3,000 of the 6,000 deliveries, colored by vehicle.",
    x = "Trip distance, km",
    y = "Delivery time, minutes"
  ) +
  theme_classroom(base_size = 16)
save_plot(p_courier_raw, "courier-raw-scatter.png", width = 9.6, height = 5.8)

# design check: distance distributions by vehicle overlap
p_courier_design <- ggplot(couriers, aes(distance_km, fill = vehicle)) +
  geom_density(alpha = 0.45, color = NA) +
  scale_fill_manual(values = c(bike = green, car = purple), labels = c("Bike", "Car")) +
  labs(
    title = "Both vehicles serve the whole distance range",
    subtitle = "Nearest-courier dispatch keeps vehicle roughly unrelated to distance.",
    x = "Trip distance, km",
    y = "Density"
  ) +
  theme_classroom(base_size = 16)
save_plot(p_courier_design, "courier-design-check.png", width = 9.6, height = 5.2)
