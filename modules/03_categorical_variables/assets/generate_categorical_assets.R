library(ggplot2)
library(scales)
library(dplyr)
library(forcats)
library(readr)
library(tidyr)

set.seed(38204)

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- sub("^--file=", "", script_arg[1] %||% "assets/generate_categorical_assets.R")
out_dir <- dirname(normalizePath(script_path))

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
      strip.text = element_text(color = "#163a5f", face = "bold", size = 16),
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

# Toy numerical-by-group example
set.seed(38248)
toy <- bind_rows(
  tibble(Group = "Red", x = runif(30, -4.8, 1.4)),
  tibble(Group = "Blue", x = runif(30, -1.4, 4.8))
) %>%
  mutate(
    Group = factor(Group, levels = c("Red", "Blue")),
    y = case_when(
      Group == "Red" ~ 5.2 + 0.70 * x + rnorm(n(), 0, 0.95),
      TRUE ~ 6.4 + 1.55 * x + rnorm(n(), 0, 1.15)
    )
  )

write_csv(toy, file.path(dirname(out_dir), "toy_grouped_lines.csv"))

toy_single <- lm(y ~ x, data = toy)
toy_main <- lm(y ~ x + Group, data = toy)
toy_interaction <- lm(y ~ x * Group, data = toy)

toy_grid <- expand_grid(
  x = seq(min(toy$x), max(toy$x), length.out = 160),
  Group = levels(toy$Group)
) %>%
  mutate(Group = factor(Group, levels = levels(toy$Group)))
toy_grid$single_fit <- predict(toy_single, newdata = toy_grid)
toy_grid$main_fit <- predict(toy_main, newdata = toy_grid)
toy_grid$interaction_fit <- predict(toy_interaction, newdata = toy_grid)

toy_colors <- c("Red" = red, "Blue" = blue)

p_toy_raw <- ggplot(toy, aes(x, y, color = Group)) +
  geom_point(size = 3.1, alpha = 0.82) +
  scale_color_manual(values = toy_colors) +
  labs(
    title = "Toy grouped data",
    subtitle = "Two groups overlap, but the blue group is shifted up and to the right",
    x = "x",
    y = "y"
  ) +
  theme_classroom()
save_plot(p_toy_raw, "toy-group-raw.png")

p_toy_single <- ggplot(toy, aes(x, y, color = Group)) +
  geom_point(size = 3.1, alpha = 0.82) +
  geom_line(
    data = distinct(toy_grid, x, single_fit),
    aes(x, single_fit),
    inherit.aes = FALSE,
    color = "#111111",
    linewidth = 1.35
  ) +
  scale_color_manual(values = toy_colors) +
  labs(
    title = "One line for all points",
    subtitle = "Model: y ~ x",
    x = "x",
    y = "y"
  ) +
  theme_classroom()
save_plot(p_toy_single, "toy-single-line.png")

p_toy_main <- ggplot(toy, aes(x, y, color = Group)) +
  geom_point(size = 3.1, alpha = 0.82) +
  geom_line(data = toy_grid, aes(y = main_fit), linewidth = 1.35) +
  scale_color_manual(values = toy_colors) +
  labs(
    title = "Different intercepts, same slope",
    subtitle = "Model: y ~ x + Group",
    x = "x",
    y = "y"
  ) +
  theme_classroom()
save_plot(p_toy_main, "toy-main-effects.png")

p_toy_interaction <- ggplot(toy, aes(x, y, color = Group)) +
  geom_point(size = 3.1, alpha = 0.82) +
  geom_line(data = toy_grid, aes(y = interaction_fit), linewidth = 1.35) +
  scale_color_manual(values = toy_colors) +
  labs(
    title = "Different intercepts and different slopes",
    subtitle = "Model: y ~ x * Group",
    x = "x",
    y = "y"
  ) +
  theme_classroom()
save_plot(p_toy_interaction, "toy-interaction.png")

# Cheese display example
cheese <- data.frame(
  disp = rep(c("No display", "Display"), c(34, 27))
)
cheese$vol <- c(
  round(rnorm(34, mean = 2341, sd = 580)),
  round(rnorm(27, mean = 5577, sd = 980))
)
cheese$vol <- pmax(cheese$vol, 700)
cheese$disp <- factor(cheese$disp, levels = c("No display", "Display"))
cheese_means <- aggregate(vol ~ disp, data = cheese, mean)

p_cheese <- ggplot(cheese, aes(disp, vol, color = disp)) +
  geom_jitter(width = 0.14, height = 0, alpha = 0.55, size = 2.6) +
  geom_point(data = cheese_means, aes(y = vol), color = blue, size = 5) +
  geom_segment(
    data = data.frame(x = 1, xend = 2, y = cheese_means$vol[1], yend = cheese_means$vol[2]),
    aes(x = x, xend = xend, y = y, yend = yend),
    color = gold,
    linewidth = 1.2
  ) +
  annotate(
    "text",
    x = 1.5,
    y = mean(cheese_means$vol) + 400,
    label = "difference = 3236",
    color = purple,
    fontface = "bold",
    size = 5
  ) +
  scale_color_manual(values = c(purple, red)) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Weekly cheese sales by display status",
    subtitle = "Dallas-area Kroger, 61 weeks",
    x = "In-store display?",
    y = "Sales volume"
  ) +
  theme_classroom()
save_plot(p_cheese, "cheese-display.png")

# Delivery operations observational example
inv_logit <- plogis

restaurant_profiles <- tibble(
  Restaurant = factor(sprintf("R%02d", 1:12)),
  popularity = c(1.35, 1.20, 1.10, 1.00, 0.95, 0.90, 0.85, 0.80, 0.76, 0.72, 0.68, 0.64),
  peak_tilt = c(0.95, 0.72, 0.55, 0.40, 0.20, 0.05, -0.10, -0.25, -0.35, -0.48, -0.62, -0.78),
  priority_tilt = c(0.95, 0.80, 0.58, 0.35, 0.18, 0.02, -0.12, -0.24, -0.36, -0.52, -0.68, -0.84),
  baseline_late = c(8.5, 7.4, 6.7, 5.9, 5.3, 4.7, 4.0, 3.4, 2.9, 2.3, 1.8, 1.2),
  priority_bonus = c(-0.5, -1.0, -1.8, -2.7, -3.6, -4.5, -5.4, -6.2, -7.0, -7.8, -8.4, -9.0)
)

delivery <- tibble(order_id = 1:1800) %>%
  mutate(
    Restaurant = sample(
      restaurant_profiles$Restaurant,
      n(),
      replace = TRUE,
      prob = restaurant_profiles$popularity
    )
  ) %>%
  left_join(restaurant_profiles, by = "Restaurant") %>%
  mutate(
    Peak = rbinom(n(), 1, inv_logit(-0.45 + peak_tilt)),
    Priority = rbinom(n(), 1, inv_logit(-1.55 + 1.35 * Peak + priority_tilt)),
    late_min = 4.2 + baseline_late + 7.6 * Peak + priority_bonus * Priority -
      3.0 * Peak * Priority + rnorm(n(), 0, 3.0),
    late_min = pmax(late_min, -3),
    Restaurant = factor(Restaurant, levels = levels(restaurant_profiles$Restaurant)),
    PeakF = factor(Peak, levels = c(0, 1), labels = c("Off-peak", "Peak")),
    PriorityF = factor(Priority, levels = c(0, 1), labels = c("Standard", "Priority")),
    order_type = case_when(
      Peak == 0 & Priority == 0 ~ "Off-peak, standard",
      Peak == 0 & Priority == 1 ~ "Off-peak, priority",
      Peak == 1 & Priority == 0 ~ "Peak, standard",
      TRUE ~ "Peak, priority"
    ),
    order_type = factor(
      order_type,
      levels = c("Off-peak, standard", "Off-peak, priority", "Peak, standard", "Peak, priority")
    )
  ) %>%
  select(order_id, Restaurant, Peak, Priority, PeakF, PriorityF, order_type, late_min)

write_csv(
  delivery %>% select(order_id, Restaurant, Peak, Priority, late_min),
  file.path(dirname(out_dir), "delivery_dispatch.csv")
)

p_priority_only <- ggplot(delivery, aes(PriorityF, late_min, fill = PriorityF)) +
  geom_boxplot(width = 0.55, alpha = 0.78, outlier.alpha = 0.25) +
  geom_jitter(width = 0.08, alpha = 0.18, size = 1.2, color = purple) +
  scale_fill_manual(values = c("#c9b35d", green)) +
  labs(
    title = "Delivery lateness by dispatch rule",
    subtitle = "All orders pooled across peak status and restaurant",
    x = "Dispatch rule",
    y = "Minutes late"
  ) +
  theme_classroom()
save_plot(p_priority_only, "delivery-priority-only-boxplot.png")

priority_by_peak <- delivery %>%
  count(PeakF, PriorityF) %>%
  group_by(PeakF) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

p_priority_rate <- ggplot(priority_by_peak, aes(PeakF, prop, fill = PriorityF)) +
  geom_col(width = 0.62, color = "#fcfbf7", linewidth = 1) +
  geom_text(
    aes(label = percent(prop, accuracy = 1)),
    position = position_stack(vjust = 0.5),
    color = "#1f2430",
    fontface = "bold",
    size = 5
  ) +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.04))) +
  scale_fill_manual(values = c("#c9b35d", green)) +
  labs(
    title = "Dispatch rule by peak status",
    subtitle = "Share of orders in each dispatch category",
    x = "Time of order",
    y = "Share of orders"
  ) +
  theme_classroom()
save_plot(p_priority_rate, "delivery-priority-rate-by-peak.png")

p_delivery <- ggplot(delivery, aes(order_type, late_min, fill = PriorityF)) +
  geom_boxplot(width = 0.58, alpha = 0.78, outlier.alpha = 0.25) +
  geom_jitter(width = 0.08, alpha = 0.16, size = 1.1, color = purple) +
  scale_fill_manual(values = c("#c9b35d", green)) +
  labs(
    title = "Delivery lateness by dispatch rule and peak status",
    subtitle = "Four groups formed by crossing dispatch rule and peak status",
    x = NULL,
    y = "Minutes late"
  ) +
  theme_classroom() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))
save_plot(p_delivery, "delivery-priority-peak-boxplots.png")

restaurant_summary <- delivery %>%
  group_by(Restaurant) %>%
  summarize(
    priority_rate = mean(Priority),
    peak_rate = mean(Peak),
    avg_late = mean(late_min),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(RestaurantEDA = fct_reorder(Restaurant, avg_late))

p_restaurant <- ggplot(delivery %>% left_join(select(restaurant_summary, Restaurant, RestaurantEDA), by = "Restaurant"),
                       aes(RestaurantEDA, late_min)) +
  geom_boxplot(width = 0.62, fill = "#c9b35d", color = purple, outlier.alpha = 0.25) +
  geom_hline(yintercept = mean(delivery$late_min), color = green, linewidth = 1.1) +
  labs(
    title = "Delivery lateness by restaurant",
    subtitle = "Orders grouped by restaurant",
    x = "Restaurant",
    y = "Minutes late"
  ) +
  theme_classroom() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
save_plot(p_restaurant, "delivery-restaurant-boxplots.png")

p_restaurant_priority <- restaurant_summary %>%
  mutate(RestaurantRate = fct_reorder(Restaurant, priority_rate)) %>%
  ggplot(aes(RestaurantRate, priority_rate, fill = peak_rate)) +
  geom_col(width = 0.68, color = "#fcfbf7") +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.05))) +
  scale_fill_gradient(low = "#c9b35d", high = red, labels = percent) +
  labs(
    title = "Priority dispatch share by restaurant",
    subtitle = "Fill color shows each restaurant's peak-hour share",
    x = "Restaurant",
    y = "Priority share"
  ) +
  theme_classroom() +
  theme(legend.title = element_text(color = "#1f2430", face = "bold"))
save_plot(p_restaurant_priority, "delivery-restaurant-priority-rates.png")

full_fit <- lm(late_min ~ Priority * Peak + Restaurant + Priority:Restaurant, data = delivery)
effect_grid <- expand_grid(
  Restaurant = levels(delivery$Restaurant),
  Peak = c(0, 1),
  Priority = c(0, 1)
) %>%
  mutate(
    Restaurant = factor(Restaurant, levels = levels(delivery$Restaurant)),
    PeakF = factor(Peak, levels = c(0, 1), labels = c("Off-peak", "Peak"))
  )
effect_grid$fit <- predict(full_fit, newdata = effect_grid)

priority_effects <- effect_grid %>%
  select(Restaurant, PeakF, Priority, fit) %>%
  pivot_wider(names_from = Priority, values_from = fit, names_prefix = "priority_") %>%
  mutate(priority_effect = priority_1 - priority_0)

p_restaurant_effects <- effect_grid %>%
  mutate(PriorityF = factor(Priority, levels = c(0, 1), labels = c("Standard", "Priority"))) %>%
  ggplot(aes(Restaurant, fit, fill = PriorityF)) +
  geom_col(
    width = 0.72,
    position = position_dodge(width = 0.74)
  ) +
  facet_grid(PeakF ~ .) +
  scale_fill_manual(values = c("#c9b35d", green)) +
  labs(
    title = "Fitted delivery lateness by restaurant and dispatch rule",
    subtitle = "Facets show peak status",
    x = "Restaurant",
    y = "Fitted minutes late"
  ) +
  theme_classroom()
save_plot(p_restaurant_effects, "delivery-priority-effects-by-restaurant.png", height = 7.2)

# Austin house price example
set.seed(38245)
n_house <- 1300
neighborhood_levels <- c("Hyde Park", "Clarksville", "Barton Hills")

house <- tibble(
  sale_id = 1:n_house,
  Neighborhood = sample(
    neighborhood_levels,
    n_house,
    replace = TRUE,
    prob = c(0.35, 0.30, 0.35)
  )
) %>%
  mutate(
    reno_lp = -0.4 +
      case_when(
        Neighborhood == "Hyde Park" ~ -0.25,
        Neighborhood == "Clarksville" ~ 0.35,
        TRUE ~ 0.15
      ),
    MajorReno10 = rbinom(n(), 1, plogis(reno_lp)),
    broad_sqft = runif(n(), 950, 3600),
    small_peak_sqft = rnorm(n(), 1400, 170),
    large_peak_sqft = rnorm(n(), 2750, 380),
    from_peak = rbinom(n(), 1, if_else(MajorReno10 == 1, 0.62, 0.50)),
    Sqft = if_else(
      from_peak == 1,
      if_else(MajorReno10 == 1, large_peak_sqft, small_peak_sqft),
      broad_sqft
    ),
    Sqft = Sqft +
      case_when(
        Neighborhood == "Hyde Park" ~ 0,
        Neighborhood == "Clarksville" ~ -180,
        TRUE ~ 500
      ) +
      rnorm(n(), 0, 60),
    Sqft = pmin(pmax(round(Sqft), 800), 4000),
    Sqft100 = (Sqft - 1800) / 100,
    NeighborhoodOffset = case_when(
      Neighborhood == "Hyde Park" ~ 0,
      Neighborhood == "Clarksville" ~ 90,
      TRUE ~ 460
    ),
    heavy_noise = rt(n(), df = 4) * 145 / sqrt(2),
    outlier = rbinom(n(), 1, 0.035),
    outlier_noise = outlier * sample(c(-1, 1), n(), replace = TRUE) * runif(n(), 300, 800),
    PriceK = 1050 + 32 * Sqft100 + NeighborhoodOffset +
      90 * MajorReno10 + 10 * Sqft100 * MajorReno10 +
      heavy_noise + outlier_noise,
    PriceK = pmax(PriceK, 450),
    Price = round(PriceK * 1000)
  ) %>%
  transmute(
    sale_id,
    Neighborhood = factor(Neighborhood, levels = neighborhood_levels),
    Sqft,
    MajorReno10 = factor(MajorReno10, levels = c(0, 1), labels = c("No", "Yes")),
    PriceK,
    Price
  ) %>%
  mutate(
    Sqft100 = (Sqft - 1800) / 100
  )

write_csv(
  house %>%
    transmute(sale_id, Neighborhood, Sqft, MajorReno10, PriceK = round(PriceK, 1), Price),
  file.path(dirname(out_dir), "austin_house_sales.csv")
)

lm_house_naive <- lm(PriceK ~ Sqft100, data = house)
lm_house_uncentered <- lm(PriceK ~ Sqft, data = house)
lm_house_nbhd <- lm(PriceK ~ Sqft100 + Neighborhood, data = house)
lm_house_main <- lm(PriceK ~ Sqft100 + MajorReno10 + Neighborhood, data = house)
lm_house_reno <- lm(PriceK ~ Sqft100 * MajorReno10 + Neighborhood, data = house)
lm_house_final <- lm_house_reno

house_grid <- tibble(Sqft = seq(min(house$Sqft), max(house$Sqft), length.out = 140)) %>%
  mutate(Sqft100 = (Sqft - 1800) / 100)
house_grid$regression_fit <- predict(lm_house_naive, newdata = house_grid)
house_grid$ppsf_fit <- mean(house$Price / house$Sqft) * house_grid$Sqft / 1000

house_grid_uncentered <- tibble(Sqft = seq(0, max(house$Sqft), length.out = 180)) %>%
  mutate(
    Sqft100 = (Sqft - 1800) / 100,
    fit_uncentered = predict(lm_house_uncentered, newdata = tibble(Sqft = Sqft)),
    fit_centered = predict(lm_house_naive, newdata = tibble(Sqft100 = Sqft100))
  )

uncentered_intercept <- coef(lm_house_uncentered)[["(Intercept)"]]
centered_intercept <- coef(lm_house_naive)[["(Intercept)"]]

house_grid_nbhd <- expand_grid(
  Sqft = seq(min(house$Sqft), max(house$Sqft), length.out = 140),
  Neighborhood = levels(house$Neighborhood)
) %>%
  mutate(Sqft100 = (Sqft - 1800) / 100)
house_grid_nbhd$fit <- predict(lm_house_nbhd, newdata = house_grid_nbhd)

house_grid_reno <- expand_grid(
  Sqft = seq(min(house$Sqft), max(house$Sqft), length.out = 140),
  Neighborhood = levels(house$Neighborhood),
  MajorReno10 = levels(house$MajorReno10)
) %>%
  mutate(
    Sqft100 = (Sqft - 1800) / 100,
    Neighborhood = factor(Neighborhood, levels = levels(house$Neighborhood)),
    MajorReno10 = factor(MajorReno10, levels = levels(house$MajorReno10))
)
house_grid_reno$fit <- predict(lm_house_reno, newdata = house_grid_reno)
house_grid_reno$main_fit <- predict(lm_house_main, newdata = house_grid_reno)

house_grid_zero <- expand_grid(
  Sqft = seq(0, max(house$Sqft), length.out = 180),
  MajorReno10 = levels(house$MajorReno10)
) %>%
  mutate(
    Sqft100 = (Sqft - 1800) / 100,
    Neighborhood = factor("Hyde Park", levels = levels(house$Neighborhood)),
    MajorReno10 = factor(MajorReno10, levels = levels(house$MajorReno10))
  )
house_grid_zero$final_fit <- predict(lm_house_final, newdata = house_grid_zero)
house_grid_zero$ppsf_fit <- mean(house$Price / house$Sqft) * house_grid_zero$Sqft / 1000
house_grid_zero$naive_fit <- predict(lm_house_naive, newdata = house_grid_zero)

house_grid_final <- expand_grid(
  Sqft = seq(min(house$Sqft), max(house$Sqft), length.out = 140),
  MajorReno10 = levels(house$MajorReno10)
) %>%
  mutate(
    Sqft100 = (Sqft - 1800) / 100,
    Neighborhood = factor("Hyde Park", levels = levels(house$Neighborhood)),
    MajorReno10 = factor(MajorReno10, levels = levels(house$MajorReno10))
  )
house_grid_final$fit <- predict(lm_house_final, newdata = house_grid_final)

p_house_size <- ggplot(house, aes(Sqft, fill = MajorReno10)) +
  geom_histogram(binwidth = 100, alpha = 0.62, position = "identity", boundary = 0, color = "#fcfbf7") +
  scale_fill_manual(values = c(purple, green), labels = c("No major renovation", "Major renovation")) +
  labs(
    title = "Austin home square footage by renovation status",
    subtitle = "Histogram bin width: 100 square feet",
    x = "Square footage",
    y = "Number of sales"
  ) +
  theme_classroom()
save_plot(p_house_size, "house-sqft-distribution.png")

p_house_overall <- ggplot(house, aes(Sqft, PriceK)) +
  geom_point(aes(color = Neighborhood), size = 1.45, alpha = 0.45) +
  geom_line(data = house_grid, aes(Sqft, regression_fit), inherit.aes = FALSE, color = "#111111", linewidth = 1.25) +
  geom_line(data = house_grid, aes(Sqft, ppsf_fit), inherit.aes = FALSE, color = red, linewidth = 1.1, linetype = "dashed") +
  scale_y_continuous(labels = label_dollar(suffix = "K")) +
  scale_color_manual(values = c(green, gold, red)) +
  labs(
    title = "Austin home sale price versus square footage",
    subtitle = "Dashed line: average price per square foot; solid line: one regression fit",
    x = "Square footage",
    y = "Sale price"
  ) +
  theme_classroom()
save_plot(p_house_overall, "house-overall.png")

p_house_ppsf_ratio <- ggplot(house, aes(Sqft, PriceK)) +
  geom_point(color = "#2f2544", alpha = 0.18, size = 1.15) +
  geom_line(
    data = distinct(house_grid_zero, Sqft, ppsf_fit),
    aes(Sqft, ppsf_fit),
    inherit.aes = FALSE,
    color = red,
    linewidth = 1.25,
    linetype = "dashed"
  ) +
  geom_line(
    data = distinct(house_grid_zero, Sqft, naive_fit),
    aes(Sqft, naive_fit),
    inherit.aes = FALSE,
    color = "#111111",
    linewidth = 1.2
  ) +
  scale_x_continuous(limits = c(0, max(house$Sqft)), labels = comma) +
  scale_y_continuous(labels = label_dollar(suffix = "K")) +
  labs(
    title = "Price per square foot is a ratio",
    subtitle = "Dashed line: average price per square foot; solid line: fitted regression",
    x = "Square footage",
    y = "Sale price"
  ) +
  theme_classroom()
save_plot(p_house_ppsf_ratio, "house-price-per-sqft-ratio.png")

p_house_origin <- ggplot(house, aes(Sqft, PriceK)) +
  geom_point(color = "#2f2544", alpha = 0.18, size = 1.15) +
  geom_line(
    data = bind_rows(
      distinct(house_grid_zero, Sqft, ppsf_fit) %>%
        transmute(Sqft, fit = ppsf_fit, model = "Price/sqft ratio"),
      distinct(house_grid_zero, Sqft, naive_fit) %>%
        transmute(Sqft, fit = naive_fit, model = "Naive regression"),
      house_grid_zero %>%
        transmute(
          Sqft,
          fit = final_fit,
          model = if_else(
            MajorReno10 == "Yes",
            "Final: renovated",
            "Final: not renovated"
          )
        )
    ),
    aes(Sqft, fit, color = model, linetype = model),
    inherit.aes = FALSE,
    linewidth = 1.25
  ) +
  scale_x_continuous(limits = c(0, max(house$Sqft)), labels = comma) +
  scale_y_continuous(labels = label_dollar(suffix = "K")) +
  scale_color_manual(values = c(
    "Price/sqft ratio" = red,
    "Naive regression" = "#111111",
    "Final: not renovated" = blue,
    "Final: renovated" = green
  )) +
  scale_linetype_manual(values = c(
    "Price/sqft ratio" = "dashed",
    "Naive regression" = "solid",
    "Final: not renovated" = "solid",
    "Final: renovated" = "solid"
  )) +
  labs(
    title = "Austin home sale price versus square footage",
    subtitle = "Zoomed out to show the no-intercept price-per-square-foot line",
    x = "Square footage",
    y = "Sale price"
  ) +
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  ) +
  theme_classroom()
save_plot(p_house_origin, "house-origin-comparison.png")

p_house_uncentered_intercept <- ggplot(house, aes(Sqft, PriceK)) +
  geom_point(color = "#2f2544", alpha = 0.18, size = 1.15) +
  geom_line(data = house_grid_uncentered, aes(Sqft, fit_uncentered), inherit.aes = FALSE, color = blue, linewidth = 1.25) +
  geom_vline(xintercept = 0, color = red, linewidth = 1.0) +
  annotate("point", x = 0, y = uncentered_intercept, color = red, size = 3.2) +
  annotate(
    "text",
    x = 170,
    y = uncentered_intercept + 180,
    label = "intercept at 0 sqft",
    hjust = 0,
    color = red,
    size = 5.2
  ) +
  scale_x_continuous(limits = c(0, max(house$Sqft)), labels = comma) +
  scale_y_continuous(labels = label_dollar(suffix = "K")) +
  labs(
    title = "Uncentered square footage",
    subtitle = "The intercept sits at a zero-square-foot house",
    x = "Square footage",
    y = "Sale price"
  ) +
  theme_classroom()
save_plot(p_house_uncentered_intercept, "house-uncentered-intercept.png")

p_house_centered_intercept <- ggplot(house, aes(Sqft100, PriceK)) +
  geom_point(color = "#2f2544", alpha = 0.22, size = 1.2) +
  geom_line(data = house_grid_uncentered, aes(Sqft100, fit_centered), inherit.aes = FALSE, color = blue, linewidth = 1.25) +
  geom_vline(xintercept = 0, color = green, linewidth = 1.0) +
  annotate("point", x = 0, y = centered_intercept, color = green, size = 3.2) +
  annotate(
    "text",
    x = 0.4,
    y = centered_intercept + 180,
    label = "intercept at Sqft100 = 0",
    hjust = 0,
    color = green,
    size = 5.2
  ) +
  coord_cartesian(xlim = range(house$Sqft100), ylim = range(house$PriceK)) +
  scale_y_continuous(labels = label_dollar(suffix = "K")) +
  labs(
    title = "Centered square footage",
    subtitle = "Now the intercept is actually at x = 0",
    x = "Centered square footage (Sqft100)",
    y = "Sale price"
  ) +
  theme_classroom()
save_plot(p_house_centered_intercept, "house-centered-intercept.png")

p_house_neighborhood <- ggplot(house, aes(Sqft, PriceK, color = Neighborhood)) +
  geom_point(size = 1.45, alpha = 0.45) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.1) +
  scale_y_continuous(labels = label_dollar(suffix = "K")) +
  scale_color_manual(values = c(green, gold, red)) +
  labs(
    title = "Austin home sale price by neighborhood",
    subtitle = "Separate simple fitted lines by neighborhood",
    x = "Square footage",
    y = "Sale price"
  ) +
  theme_classroom()
save_plot(p_house_neighborhood, "house-neighborhood.png")

p_house_partial <- ggplot(house, aes(Sqft, PriceK, color = Neighborhood)) +
  geom_point(size = 1.45, alpha = 0.42) +
  geom_line(data = house_grid_nbhd, aes(y = fit), linewidth = 1.15) +
  scale_y_continuous(labels = label_dollar(suffix = "K")) +
  scale_color_manual(values = c(green, gold, red)) +
  labs(
    title = "Austin home sale price by neighborhood",
    subtitle = "Parallel fitted lines from a model with neighborhood offsets",
    x = "Square footage",
    y = "Sale price"
  ) +
  theme_classroom()
save_plot(p_house_partial, "house-partial.png")

p_house_reno <- ggplot(house, aes(Sqft, PriceK, color = MajorReno10)) +
  geom_point(alpha = 0.22, size = 1.1) +
  geom_line(data = house_grid_reno, aes(y = fit), linewidth = 1.1) +
  facet_wrap(~ Neighborhood) +
  scale_y_continuous(labels = label_dollar(suffix = "K")) +
  scale_color_manual(values = c(purple, green), labels = c("No major renovation", "Major renovation")) +
  labs(
    title = "Austin home sale price by renovation status",
    subtitle = "Fitted lines from a neighborhood-adjusted interaction model",
    x = "Square footage",
    y = "Sale price"
  ) +
  theme_classroom(base_size = 15)
save_plot(p_house_reno, "house-reno-interaction.png", width = 10.5, height = 6.5)

p_house_reno_main <- ggplot(house, aes(Sqft, PriceK, color = MajorReno10)) +
  geom_point(alpha = 0.22, size = 1.1) +
  geom_line(data = house_grid_reno, aes(y = main_fit), linewidth = 1.1) +
  facet_wrap(~ Neighborhood) +
  scale_y_continuous(labels = label_dollar(suffix = "K")) +
  scale_color_manual(values = c(purple, green), labels = c("No major renovation", "Major renovation")) +
  labs(
    title = "Austin home sale price by renovation status",
    subtitle = "Main-effects model: same square-footage slope for both renovation groups",
    x = "Square footage",
    y = "Sale price"
  ) +
  theme_classroom(base_size = 15)
save_plot(p_house_reno_main, "house-reno-main-effects.png", width = 10.5, height = 6.5)

p_house_final <- ggplot(house_grid_final, aes(Sqft, fit, color = MajorReno10)) +
  geom_line(linewidth = 1.3) +
  scale_y_continuous(labels = label_dollar(suffix = "K")) +
  scale_color_manual(values = c(purple, green), labels = c("No major renovation", "Major renovation")) +
  labs(
    title = "Model-based fitted sale price by square footage",
    subtitle = "Hyde Park reference neighborhood",
    x = "Square footage",
    y = "Fitted sale price"
  ) +
  theme_classroom()
save_plot(p_house_final, "house-final-model.png")

# Kroger city and price elasticity
cities <- c("Atlanta", "Birmingham", "Cincinnati", "Columbus", "Dallas", "Detroit",
            "Houston", "Indianapolis", "Louisville", "Nashville", "Roanoke")
city_effect <- c(0.20, -0.95, 0.32, -0.20, -0.12, 0.03, 0.74, -0.28, -0.52, -0.35, 0.23)
mean_price <- c(2.83, 2.63, 2.75, 2.60, 2.95, 2.64, 2.38, 2.61, 2.74, 2.58, 2.52)
kroger <- do.call(rbind, lapply(seq_along(cities), function(i) {
  price <- rnorm(28, mean_price[i], 0.18)
  logvol <- 10.6 + city_effect[i] - 2.15 * log(price) + rnorm(28, 0, 0.13)
  data.frame(city = cities[i], price = price, vol = exp(logvol))
}))
kroger$city <- factor(kroger$city, levels = cities)

p_kroger <- ggplot(kroger, aes(log(price), log(vol))) +
  geom_point(alpha = 0.65, size = 1.6, color = purple) +
  geom_smooth(method = "lm", se = FALSE, color = red, linewidth = 0.9) +
  geom_smooth(aes(group = city), method = "lm", se = FALSE, color = blue, linewidth = 0.8) +
  facet_wrap(~ city, ncol = 4) +
  labs(
    title = "Log volume versus log price by city",
    subtitle = "Pooled fitted line and city-specific fitted lines",
    x = "log(price)",
    y = "log(volume)"
  ) +
  theme_classroom(base_size = 14)
save_plot(p_kroger, "kroger-city-elasticity.png", width = 10.5, height = 6.5)

# Mercedes S-Class trim example
sclass <- rbind(
  data.frame(trim = "S550", miles1K = runif(900, 8, 76)),
  data.frame(trim = "63 AMG", miles1K = runif(130, 6, 62))
)
sclass$price <- ifelse(
  sclass$trim == "S550",
  61123 - 311 * sclass$miles1K + rnorm(nrow(sclass), 0, 4300),
  90694 - 494 * sclass$miles1K + rnorm(nrow(sclass), 0, 7200)
)
sclass$trim <- factor(sclass$trim, levels = c("S550", "63 AMG"))

p_sclass_common <- ggplot(sclass, aes(miles1K * 1000, price)) +
  geom_point(alpha = 0.45, size = 1.3, color = purple) +
  geom_smooth(method = "lm", se = FALSE, color = blue, linewidth = 1.2) +
  facet_wrap(~ trim) +
  scale_y_continuous(labels = dollar) +
  scale_x_continuous(labels = comma) +
  labs(
    title = "Used S-Class price by mileage and trim",
    subtitle = "Same-slope fitted model by trim",
    x = "Mileage",
    y = "Price"
  ) +
  theme_classroom()
save_plot(p_sclass_common, "sclass-same-slope.png")

p_sclass_interaction <- ggplot(sclass, aes(miles1K * 1000, price)) +
  geom_point(alpha = 0.45, size = 1.3, color = purple) +
  geom_smooth(method = "lm", se = FALSE, color = blue, linewidth = 1.2) +
  facet_wrap(~ trim) +
  scale_y_continuous(labels = dollar) +
  scale_x_continuous(labels = comma) +
  labs(
    title = "Used S-Class price by mileage and trim",
    subtitle = "Separate-slope fitted model by trim",
    x = "Mileage",
    y = "Price"
  ) +
  theme_classroom()
save_plot(p_sclass_interaction, "sclass-interaction.png")

# Corn treatment by water
corn_grid <- expand.grid(water = seq(0, 0.3, length.out = 100), treat = c("Control", "Treatment"))
corn_grid$treat01 <- ifelse(corn_grid$treat == "Treatment", 1, 0)
corn_grid$bushels <- 2.0 + 0.9 * corn_grid$treat01 + 3 * corn_grid$water -
  1.8 * corn_grid$treat01 * corn_grid$water

p_corn <- ggplot(corn_grid, aes(water, bushels, color = treat)) +
  geom_line(linewidth = 1.4) +
  scale_color_manual(values = c(purple, green)) +
  labs(
    title = "Corn output by irrigation and treatment",
    subtitle = "Fitted lines for control and treatment",
    x = "Irrigation (inches per day)",
    y = "Output (bushels)"
  ) +
  theme_classroom()
save_plot(p_corn, "corn-treatment-water.png")

# Minimum legal drinking age RD
age <- seq(19, 23, length.out = 80)
over21 <- as.numeric(age >= 21)
years <- age - 21
rate <- 29.9 - 2.6 * years + 4.5 * over21 - 1.2 * over21 * years + rnorm(length(age), 0, 1.1)
mlda <- data.frame(age, years, over21 = factor(over21, labels = c("Under 21", "Over 21")), rate)

p_mlda <- ggplot(mlda, aes(years, rate, color = over21)) +
  geom_point(size = 2.1, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#444444") +
  annotate("segment", x = 0, xend = 0, y = 29.7, yend = 34.2, arrow = arrow(length = unit(0.18, "inches")), color = purple, linewidth = 1.1) +
  annotate("text", x = 0.35, y = 32.2, label = "discontinuity", color = purple, fontface = "bold", size = 5) +
  scale_color_manual(values = c(red, blue)) +
  labs(
    title = "Motor-vehicle death rate by age relative to 21",
    subtitle = "Separate fitted lines on either side of the threshold",
    x = "Years past 21",
    y = "Deaths per 100,000 people"
  ) +
  theme_classroom()
save_plot(p_mlda, "mlda-regression-discontinuity.png")
