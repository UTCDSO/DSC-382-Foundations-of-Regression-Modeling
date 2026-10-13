library(tidyverse)
library(broom)

set.seed(382050)

script_arg <- commandArgs(trailingOnly = FALSE)
script_file <- sub("--file=", "", script_arg[grepl("^--file=", script_arg)][1])
if (is.na(script_file)) {
  script_file <- "modules/05_model_building_inference_diagnostics/assets/generate_model_building_assets.R"
}
module_dir <- normalizePath(file.path(dirname(script_file), ".."), mustWork = FALSE)
asset_dir <- file.path(module_dir, "assets")
dir.create(asset_dir, showWarnings = FALSE, recursive = TRUE)

theme_course <- function(base_size = 15) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 2),
      plot.subtitle = element_text(color = "gray30"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

save_plot <- function(plot, filename, width = 8.2, height = 5.0) {
  ggsave(
    filename = file.path(asset_dir, filename),
    plot = plot,
    width = width,
    height = height,
    dpi = 220,
    bg = "white"
  )
}

# Case 1: nondestructive proxy test for gasoline octane --------------------

n_octane <- 80
cheap_index <- runif(n_octane, 84, 99)
octane_rating <- 5.8 + 0.91 * cheap_index + rnorm(n_octane, 0, 0.9)

octane <- tibble(
  sample_id = sprintf("G%03d", seq_len(n_octane)),
  cheap_test_index = round(cheap_index, 2),
  octane_rating = round(octane_rating, 2)
)

write_csv(octane, file.path(module_dir, "octane_calibration.csv"))

octane_model <- lm(octane_rating ~ cheap_test_index, data = octane)

set.seed(382051)
octane_boot <- replicate(4000, {
  boot_rows <- sample(seq_len(nrow(octane)), nrow(octane), replace = TRUE)
  coef(lm(octane_rating ~ cheap_test_index, data = octane[boot_rows, ]))[["cheap_test_index"]]
})

octane_summary <- tibble(
  quantity = c("intercept", "cheap_test_slope", "bootstrap_025", "bootstrap_975"),
  value = c(
    coef(octane_model)[["(Intercept)"]],
    coef(octane_model)[["cheap_test_index"]],
    quantile(octane_boot, 0.025),
    quantile(octane_boot, 0.975)
  )
)
write_csv(octane_summary, file.path(asset_dir, "octane_model_summary.csv"))

octane_aug <- augment(octane_model, data = octane)

save_plot(
  ggplot(octane, aes(cheap_test_index, octane_rating)) +
    geom_point(alpha = 0.78, size = 2.7, color = "#355C7D") +
    geom_smooth(method = "lm", se = FALSE, linewidth = 1.1, color = "#C0392B") +
    labs(
      title = "Gasoline samples: cheap test versus octane rating",
      x = "Cheap nondestructive test index",
      y = "Octane rating"
    ) +
    theme_course(),
  "octane-scatter.png"
)

save_plot(
  ggplot(tibble(slope = octane_boot), aes(slope)) +
    geom_histogram(bins = 42, fill = "#77A6B6", color = "white") +
    geom_vline(
      xintercept = quantile(octane_boot, c(0.025, 0.975)),
      linetype = "dashed",
      linewidth = 0.8
    ) +
    labs(
      title = "Bootstrap distribution for the cheap-test slope",
      x = "Slope estimate",
      y = "Bootstrap samples"
    ) +
    theme_course(),
  "octane-bootstrap-slope.png"
)

save_plot(
  ggplot(octane_aug, aes(cheap_test_index, .resid)) +
    geom_hline(yintercept = 0, color = "gray35") +
    geom_point(alpha = 0.75, size = 2.6, color = "#355C7D") +
    geom_smooth(se = FALSE, linewidth = 1.0, color = "#C0392B") +
    labs(
      title = "Residuals from the octane calibration model",
      x = "Cheap nondestructive test index",
      y = "Residual octane rating"
    ) +
    theme_course(),
  "octane-residuals.png"
)

# Case 2: Barcelona Airbnb prices -----------------------------------------

n_airbnb <- 900
distance <- pmax(0.05, pmin(5.5, rgamma(n_airbnb, shape = 2.2, scale = 1.05)))

eta0 <- 0.9 - 0.38 * distance
eta1 <- 1.4 - 0.12 * distance
eta2 <- 1.0 + 0.05 * distance
eta3 <- 0.10 + 0.18 * distance
eta4 <- -1.2 + 0.27 * distance
bedroom_prob <- exp(cbind(eta0, eta1, eta2, eta3, eta4))
bedroom_prob <- bedroom_prob / rowSums(bedroom_prob)

bedrooms <- apply(bedroom_prob, 1, function(p) sample(0:4, 1, prob = p))
bathrooms <- pmin(4, pmax(1, round(1 + 0.48 * bedrooms + rnorm(n_airbnb, 0, 0.45))))
superhost <- rbinom(n_airbnb, 1, plogis(-0.2 - 0.08 * distance + 0.08 * bedrooms))
num_reviews <- pmax(
  0,
  round(rlnorm(n_airbnb, meanlog = 2.4 - 0.08 * distance + 0.05 * superhost, sdlog = 1.05))
)

large_unit_premium <- ifelse(bedrooms >= 4, 0.08, 0) +
  ifelse(bedrooms == 3 & bathrooms >= 3, 0.03, 0)

log_price <- 4.72 -
  0.105 * distance +
  0.24 * bedrooms +
  0.12 * bathrooms +
  0.12 * superhost +
  0.035 * log1p(num_reviews) +
  large_unit_premium +
  rnorm(n_airbnb, 0, 0.23)

airbnb <- tibble(
  listing_id = sprintf("BCN%04d", seq_len(n_airbnb)),
  nightly_price_eur = round(pmax(45, exp(log_price)), 0),
  distance_to_rambla_km = round(distance, 2),
  bedrooms = bedrooms,
  bathrooms = bathrooms,
  superhost = superhost,
  num_reviews = num_reviews
)

write_csv(airbnb, file.path(module_dir, "barcelona_airbnb_prices.csv"))

overall_model <- lm(nightly_price_eur ~ distance_to_rambla_km, data = airbnb)
size_model <- lm(nightly_price_eur ~ distance_to_rambla_km + bedrooms + bathrooms, data = airbnb)
target_model <- lm(
  nightly_price_eur ~ distance_to_rambla_km + bedrooms + bathrooms + superhost + log1p(num_reviews),
  data = airbnb
)

set.seed(382052)
distance_boot <- replicate(4000, {
  boot_rows <- sample(seq_len(nrow(airbnb)), nrow(airbnb), replace = TRUE)
  coef(lm(
    nightly_price_eur ~ distance_to_rambla_km + bedrooms + bathrooms + superhost + log1p(num_reviews),
    data = airbnb[boot_rows, ]
  ))[["distance_to_rambla_km"]]
})

model_summary <- bind_rows(
  tidy(overall_model) %>% mutate(model = "overall"),
  tidy(size_model) %>% mutate(model = "size_adjusted"),
  tidy(target_model) %>% mutate(model = "target")
) %>%
  select(model, term, estimate, std.error, statistic, p.value)

write_csv(model_summary, file.path(asset_dir, "barcelona_model_summary.csv"))

distance_summary <- tibble(
  model = c("overall", "size_adjusted", "target", "target_bootstrap_025", "target_bootstrap_975"),
  distance_slope = c(
    coef(overall_model)[["distance_to_rambla_km"]],
    coef(size_model)[["distance_to_rambla_km"]],
    coef(target_model)[["distance_to_rambla_km"]],
    quantile(distance_boot, 0.025),
    quantile(distance_boot, 0.975)
  )
)
write_csv(distance_summary, file.path(asset_dir, "barcelona_distance_summary.csv"))

target_aug <- augment(target_model, data = airbnb)
residual_by_bedroom <- target_aug %>%
  group_by(bedrooms) %>%
  summarize(
    listings = n(),
    mean_residual_eur = mean(.resid),
    median_residual_eur = median(.resid),
    .groups = "drop"
  )
write_csv(residual_by_bedroom, file.path(asset_dir, "barcelona_residual_by_bedroom.csv"))

save_plot(
  ggplot(airbnb, aes(distance_to_rambla_km, nightly_price_eur)) +
    geom_point(alpha = 0.28, size = 1.8, color = "#355C7D") +
    geom_smooth(method = "lm", se = FALSE, linewidth = 1.1, color = "#C0392B") +
    scale_y_continuous(labels = scales::label_dollar(prefix = "EUR ")) +
    labs(
      title = "Barcelona Airbnb prices versus distance to La Rambla",
      x = "Distance to La Rambla, km",
      y = "Nightly price"
    ) +
    theme_course(),
  "barcelona-overall-distance.png"
)

save_plot(
  ggplot(airbnb, aes(distance_to_rambla_km, bedrooms)) +
    geom_jitter(height = 0.08, width = 0, alpha = 0.22, color = "#355C7D") +
    geom_smooth(method = "lm", se = FALSE, linewidth = 1.1, color = "#C0392B") +
    scale_y_continuous(breaks = 0:4) +
    labs(
      title = "Larger rentals tend to sit farther from La Rambla",
      x = "Distance to La Rambla, km",
      y = "Bedrooms"
    ) +
    theme_course(),
  "barcelona-distance-bedrooms.png"
)

save_plot(
  ggplot(airbnb, aes(bedrooms, bathrooms)) +
    geom_jitter(width = 0.08, height = 0.08, alpha = 0.20, color = "#355C7D") +
    geom_smooth(method = "lm", se = FALSE, linewidth = 1.1, color = "#C0392B") +
    scale_x_continuous(breaks = 0:4) +
    scale_y_continuous(breaks = 1:4) +
    labs(
      title = "Bedrooms and bathrooms move together",
      x = "Bedrooms",
      y = "Bathrooms"
    ) +
    theme_course(),
  "barcelona-bedrooms-bathrooms.png"
)

save_plot(
  ggplot(airbnb, aes(num_reviews)) +
    geom_histogram(bins = 45, fill = "#77A6B6", color = "white") +
    scale_x_continuous(trans = "log1p") +
    labs(
      title = "Review counts have a long right tail",
      x = "Number of reviews, log scale",
      y = "Listings"
    ) +
    theme_course(),
  "barcelona-reviews-distribution.png"
)

coef_plot_data <- distance_summary %>%
  filter(model %in% c("overall", "size_adjusted", "target")) %>%
  mutate(model = factor(
    model,
    levels = c("overall", "size_adjusted", "target"),
    labels = c("Overall", "Adjust for size", "Target model")
  ))

save_plot(
  ggplot(coef_plot_data, aes(model, distance_slope)) +
    geom_hline(yintercept = 0, color = "gray45") +
    geom_point(size = 3.6, color = "#355C7D") +
    geom_line(aes(group = 1), linewidth = 1.0, color = "#77A6B6") +
    labs(
      title = "The distance slope changes after adjustment",
      x = NULL,
      y = "Euros per km farther from La Rambla"
    ) +
    theme_course(),
  "barcelona-distance-coefficients.png",
  width = 7.6,
  height = 4.8
)

save_plot(
  ggplot(tibble(distance_slope = distance_boot), aes(distance_slope)) +
    geom_histogram(bins = 44, fill = "#77A6B6", color = "white") +
    geom_vline(
      xintercept = quantile(distance_boot, c(0.025, 0.975)),
      linetype = "dashed",
      linewidth = 0.8
    ) +
    labs(
      title = "Bootstrap distribution for the adjusted distance slope",
      x = "Euros per km farther from La Rambla",
      y = "Bootstrap samples"
    ) +
    theme_course(),
  "barcelona-bootstrap-distance.png"
)

save_plot(
  ggplot(target_aug, aes(factor(bedrooms), .resid)) +
    geom_hline(yintercept = 0, color = "gray45") +
    geom_boxplot(outlier.alpha = 0.2, fill = "#E8EEF2", color = "#355C7D") +
    geom_point(
      data = residual_by_bedroom,
      aes(factor(bedrooms), mean_residual_eur),
      inherit.aes = FALSE,
      size = 3.2,
      color = "#C0392B"
    ) +
    labs(
      title = "The target model underpredicts some large rentals",
      x = "Bedrooms",
      y = "Residual price, euros"
    ) +
    theme_course(),
  "barcelona-residuals-bedrooms.png"
)

save_plot(
  ggplot(target_aug, aes(.fitted, .resid, color = bedrooms >= 4)) +
    geom_hline(yintercept = 0, color = "gray45") +
    geom_point(alpha = 0.55, size = 1.9) +
    geom_smooth(se = FALSE, linewidth = 1.0, color = "#C0392B") +
    scale_color_manual(
      values = c("FALSE" = "#355C7D", "TRUE" = "#D17A22"),
      labels = c("0-3 bedrooms", "4 bedrooms"),
      name = NULL
    ) +
    labs(
      title = "Residuals versus fitted prices",
      x = "Fitted nightly price",
      y = "Residual price, euros"
    ) +
    theme_course(),
  "barcelona-residuals-fitted.png"
)

message("Generated Module 05 data and assets in: ", module_dir)
