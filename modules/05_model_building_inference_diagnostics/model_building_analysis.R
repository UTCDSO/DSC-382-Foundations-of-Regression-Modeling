library(tidyverse)
library(broom)

if (!interactive()) {
  pdf(NULL)
}

module_dir <- if (file.exists("octane_calibration.csv")) {
  "."
} else {
  file.path("modules", "05_model_building_inference_diagnostics")
}

# Case 1: octane calibration -----------------------------------------------

octane <- read_csv(file.path(module_dir, "octane_calibration.csv"), show_col_types = FALSE)

octane %>% sample_n(8)

octane_model <- lm(octane_rating ~ cheap_test_index, data = octane)
tidy(octane_model, conf.int = TRUE)
glance(octane_model)

set.seed(382051)
octane_boot <- replicate(4000, {
  boot_sample <- octane %>% slice_sample(n = nrow(octane), replace = TRUE)
  coef(lm(octane_rating ~ cheap_test_index, data = boot_sample))[["cheap_test_index"]]
})

quantile(octane_boot, c(0.025, 0.975))

ggplot(octane, aes(cheap_test_index, octane_rating)) +
  geom_point(alpha = 0.75) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Cheap nondestructive test index",
    y = "Octane rating",
    title = "Cheap test versus octane rating"
  ) +
  theme_minimal(base_size = 14)

ggplot(tibble(slope = octane_boot), aes(slope)) +
  geom_histogram(bins = 40) +
  geom_vline(xintercept = quantile(octane_boot, c(0.025, 0.975)), linetype = "dashed") +
  labs(
    x = "Bootstrap slope estimate",
    y = "Bootstrap samples",
    title = "Bootstrap uncertainty for the cheap-test slope"
  ) +
  theme_minimal(base_size = 14)

octane_aug <- augment(octane_model, data = octane)

ggplot(octane_aug, aes(cheap_test_index, .resid)) +
  geom_hline(yintercept = 0) +
  geom_point(alpha = 0.75) +
  geom_smooth(se = FALSE) +
  labs(
    x = "Cheap nondestructive test index",
    y = "Residual octane rating",
    title = "Residuals from the octane calibration model"
  ) +
  theme_minimal(base_size = 14)

# Case 2: Barcelona Airbnb prices ------------------------------------------

airbnb <- read_csv(file.path(module_dir, "barcelona_airbnb_prices.csv"), show_col_types = FALSE)

airbnb %>% sample_n(8)

airbnb %>%
  summarize(
    listings = n(),
    median_price = median(nightly_price_eur),
    median_distance_km = median(distance_to_rambla_km),
    median_reviews = median(num_reviews)
  )

ggplot(airbnb, aes(distance_to_rambla_km, nightly_price_eur)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Distance to La Rambla, km",
    y = "Nightly price, euros",
    title = "Overall price-distance relationship"
  ) +
  theme_minimal(base_size = 14)

overall_model <- lm(nightly_price_eur ~ distance_to_rambla_km, data = airbnb)

size_model <- lm(
  nightly_price_eur ~ distance_to_rambla_km + bedrooms + bathrooms,
  data = airbnb
)

target_model <- lm(
  nightly_price_eur ~ distance_to_rambla_km + bedrooms + bathrooms +
    superhost + log1p(num_reviews),
  data = airbnb
)

tidy(overall_model, conf.int = TRUE)
tidy(size_model, conf.int = TRUE)
tidy(target_model, conf.int = TRUE)
glance(target_model)

airbnb %>%
  summarize(
    cor_distance_bedrooms = cor(distance_to_rambla_km, bedrooms),
    cor_bedrooms_bathrooms = cor(bedrooms, bathrooms),
    cor_distance_price = cor(distance_to_rambla_km, nightly_price_eur),
    cor_bedrooms_price = cor(bedrooms, nightly_price_eur),
    cor_bathrooms_price = cor(bathrooms, nightly_price_eur)
  )

ggplot(airbnb, aes(distance_to_rambla_km, bedrooms)) +
  geom_jitter(height = 0.08, alpha = 0.25) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Distance to La Rambla, km",
    y = "Bedrooms",
    title = "A possible confounder: larger rentals sit farther away"
  ) +
  theme_minimal(base_size = 14)

set.seed(382052)
distance_boot <- replicate(4000, {
  boot_sample <- airbnb %>% slice_sample(n = nrow(airbnb), replace = TRUE)
  coef(lm(
    nightly_price_eur ~ distance_to_rambla_km + bedrooms + bathrooms +
      superhost + log1p(num_reviews),
    data = boot_sample
  ))[["distance_to_rambla_km"]]
})

quantile(distance_boot, c(0.025, 0.975))

ggplot(tibble(distance_slope = distance_boot), aes(distance_slope)) +
  geom_histogram(bins = 40) +
  geom_vline(xintercept = quantile(distance_boot, c(0.025, 0.975)), linetype = "dashed") +
  labs(
    x = "Adjusted distance slope, euros per km",
    y = "Bootstrap samples",
    title = "Bootstrap uncertainty for the adjusted distance slope"
  ) +
  theme_minimal(base_size = 14)

target_aug <- augment(target_model, data = airbnb)

ggplot(target_aug, aes(.fitted, .resid, color = bedrooms >= 4)) +
  geom_hline(yintercept = 0) +
  geom_point(alpha = 0.55) +
  geom_smooth(se = FALSE, color = "black") +
  labs(
    x = "Fitted nightly price, euros",
    y = "Residual price, euros",
    color = "4 bedrooms?",
    title = "Residuals from the target model"
  ) +
  theme_minimal(base_size = 14)

target_aug %>%
  group_by(bedrooms) %>%
  summarize(
    listings = n(),
    mean_residual = mean(.resid),
    median_residual = median(.resid),
    .groups = "drop"
  )
