library(tidyverse)
library(broom)

if (!interactive()) {
  pdf(NULL)
}

module_dir <- if (file.exists("austin_house_sales.csv")) {
  "."
} else {
  file.path("modules", "03_categorical_variables")
}

toy_path <- file.path(module_dir, "toy_grouped_lines.csv")
data_path <- if (file.exists("austin_house_sales.csv")) {
  "austin_house_sales.csv"
} else {
  file.path("modules", "03_categorical_variables", "austin_house_sales.csv")
}

toy <- read_csv(toy_path, show_col_types = FALSE) %>%
  mutate(Group = factor(Group, levels = c("Red", "Blue")))

# 1. Toy example: one numerical predictor and one group variable.
ggplot(toy, aes(x, y, color = Group)) +
  geom_point(size = 2.6, alpha = 0.8) +
  labs(
    x = "x",
    y = "y",
    title = "Toy grouped data"
  ) +
  theme_minimal(base_size = 14)

toy_single <- lm(y ~ x, data = toy)
tidy(toy_single)

ggplot(toy, aes(x, y, color = Group)) +
  geom_point(size = 2.6, alpha = 0.8) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "black") +
  labs(
    x = "x",
    y = "y",
    title = "One line for both groups"
  ) +
  theme_minimal(base_size = 14)

toy_main <- lm(y ~ x + Group, data = toy)
tidy(toy_main)

toy_grid <- expand_grid(
  x = seq(min(toy$x), max(toy$x), length.out = 100),
  Group = levels(toy$Group)
) %>%
  mutate(Group = factor(Group, levels = levels(toy$Group)))

toy_grid$main_fit <- predict(toy_main, newdata = toy_grid)

ggplot(toy, aes(x, y, color = Group)) +
  geom_point(size = 2.6, alpha = 0.8) +
  geom_line(data = toy_grid, aes(y = main_fit), linewidth = 1.1) +
  labs(
    x = "x",
    y = "y",
    title = "Different intercepts, same slope"
  ) +
  theme_minimal(base_size = 14)

toy_interaction <- lm(y ~ x * Group, data = toy)
tidy(toy_interaction)

coef(toy_interaction)

toy_grid$interaction_fit <- predict(toy_interaction, newdata = toy_grid)

ggplot(toy, aes(x, y, color = Group)) +
  geom_point(size = 2.6, alpha = 0.8) +
  geom_line(data = toy_grid, aes(y = interaction_fit), linewidth = 1.1) +
  labs(
    x = "x",
    y = "y",
    title = "Different intercepts, different slopes"
  ) +
  theme_minimal(base_size = 14)

# For the interaction model:
#   Red line:  intercept + x slope
#   Blue line: (intercept + GroupBlue) + (x slope + x:GroupBlue) * x
tibble(
  group = c("Red", "Blue"),
  intercept = c(
    coef(toy_interaction)[["(Intercept)"]],
    coef(toy_interaction)[["(Intercept)"]] + coef(toy_interaction)[["GroupBlue"]]
  ),
  slope = c(
    coef(toy_interaction)[["x"]],
    coef(toy_interaction)[["x"]] + coef(toy_interaction)[["x:GroupBlue"]]
  )
)

houses <- read_csv(data_path, show_col_types = FALSE) %>%
  mutate(
    Neighborhood = factor(Neighborhood, levels = c("Hyde Park", "Clarksville", "Barton Hills")),
    MajorReno10 = factor(MajorReno10, levels = c("No", "Yes")),
    # Center square footage at a typical home size.
    # Sqft100 = 0 means 1800 square feet; one unit is 100 square feet.
    Sqft100 = (Sqft - 1800) / 100,
    PricePerSqft = Price / Sqft
  )

set.seed(38204)
houses %>%
  sample_n(6) %>%
  select(sale_id, Neighborhood, Sqft, MajorReno10, PriceK, PricePerSqft)

# 2. Price per square foot is a ratio, not a regression slope.
simple_sqft_model <- lm(PriceK ~ Sqft, data = houses)
simple_centered_model <- lm(PriceK ~ Sqft100, data = houses)

sqft_grid <- tibble(Sqft = seq(0, max(houses$Sqft), length.out = 150)) %>%
  mutate(
    Sqft100 = (Sqft - 1800) / 100,
    price_per_sqft_line = mean(houses$PricePerSqft) * Sqft / 1000,
    uncentered_fit = predict(simple_sqft_model, newdata = tibble(Sqft = Sqft)),
    centered_fit = predict(simple_centered_model, newdata = tibble(Sqft100 = Sqft100))
  )

ggplot(houses, aes(Sqft, PriceK)) +
  geom_point(alpha = 0.18, size = 1.0) +
  geom_line(data = sqft_grid, aes(y = price_per_sqft_line), color = "firebrick", linetype = "dashed") +
  geom_line(data = sqft_grid, aes(y = uncentered_fit), color = "black") +
  labs(
    x = "Square footage",
    y = "Sale price (thousands)",
    title = "Price per square foot is a ratio"
  ) +
  theme_minimal(base_size = 14)

coef(simple_sqft_model)
coef(simple_centered_model)

ggplot(houses, aes(Sqft, PriceK)) +
  geom_point(alpha = 0.18, size = 1.0) +
  geom_line(data = sqft_grid, aes(y = uncentered_fit), color = "steelblue", linewidth = 1.0) +
  geom_vline(xintercept = 0, color = "firebrick") +
  labs(
    x = "Square footage",
    y = "Sale price (thousands)",
    title = "Without centering: the intercept is at 0 square feet"
  ) +
  theme_minimal(base_size = 14)

ggplot(houses, aes(Sqft, PriceK)) +
  geom_point(alpha = 0.18, size = 1.0) +
  geom_line(data = sqft_grid, aes(y = centered_fit), color = "steelblue", linewidth = 1.0) +
  geom_vline(xintercept = 1800, color = "darkgreen") +
  labs(
    x = "Square footage",
    y = "Sale price (thousands)",
    title = "With centering: the intercept is at 1800 square feet"
  ) +
  theme_minimal(base_size = 14)

# 3. Check whether square footage and price differ by neighborhood.
houses %>%
  group_by(Neighborhood) %>%
  summarize(
    n = n(),
    mean_sqft = mean(Sqft),
    mean_priceK = mean(PriceK),
    mean_price_per_sqft = mean(PricePerSqft),
    .groups = "drop"
  )

houses %>%
  group_by(MajorReno10) %>%
  summarize(
    n = n(),
    mean_sqft = mean(Sqft),
    mean_priceK = mean(PriceK),
    mean_price_per_sqft = mean(PricePerSqft),
    .groups = "drop"
  )

# 4. Fit a main-effects model: different intercepts, common square-footage slope.
main_model <- lm(PriceK ~ Sqft100 + MajorReno10 + Neighborhood, data = houses)
tidy(main_model)

fit_grid <- expand_grid(
  Sqft = seq(min(houses$Sqft), max(houses$Sqft), length.out = 100),
  MajorReno10 = levels(houses$MajorReno10),
  Neighborhood = levels(houses$Neighborhood)
) %>%
  mutate(
    Sqft100 = (Sqft - 1800) / 100,
    Neighborhood = factor(Neighborhood, levels = levels(houses$Neighborhood)),
    MajorReno10 = factor(MajorReno10, levels = levels(houses$MajorReno10))
  )

fit_grid$main_fit <- predict(main_model, newdata = fit_grid)

ggplot(houses, aes(Sqft, PriceK, color = MajorReno10)) +
  geom_point(alpha = 0.20, size = 1.0) +
  geom_line(data = fit_grid, aes(y = main_fit), linewidth = 1.1) +
  facet_wrap(~ Neighborhood) +
  scale_color_discrete(labels = c("No major renovation", "Major renovation")) +
  labs(
    x = "Square footage",
    y = "Sale price (thousands)",
    title = "Main-effects model"
  ) +
  theme_minimal(base_size = 14)

# 5. Add the square-footage by renovation interaction.
interaction_model <- lm(PriceK ~ Sqft100 * MajorReno10 + Neighborhood, data = houses)
tidy(interaction_model)

fit_grid$interaction_fit <- predict(interaction_model, newdata = fit_grid)

ggplot(houses, aes(Sqft, PriceK, color = MajorReno10)) +
  geom_point(alpha = 0.20, size = 1.0) +
  geom_line(data = fit_grid, aes(y = interaction_fit), linewidth = 1.1) +
  facet_wrap(~ Neighborhood) +
  scale_color_discrete(labels = c("No major renovation", "Major renovation")) +
  labs(
    x = "Square footage",
    y = "Sale price (thousands)",
    title = "Interaction model"
  ) +
  theme_minimal(base_size = 14)

# 6. Assemble intercepts and slopes from the interaction coefficients.
coef(interaction_model)

model_pieces <- tidy(interaction_model) %>%
  select(term, estimate)

model_pieces

sqft_slope_no_reno <- coef(interaction_model)[["Sqft100"]]
sqft_slope_reno <- coef(interaction_model)[["Sqft100"]] +
  coef(interaction_model)[["Sqft100:MajorReno10Yes"]]

tibble(
  MajorReno10 = c("No", "Yes"),
  slope_per_100_sqft = c(sqft_slope_no_reno, sqft_slope_reno)
)

neighborhood_offsets <- c(
  "Hyde Park" = 0,
  "Clarksville" = coef(interaction_model)[["NeighborhoodClarksville"]],
  "Barton Hills" = coef(interaction_model)[["NeighborhoodBarton Hills"]]
)

assembled_lines <- expand_grid(
  Neighborhood = names(neighborhood_offsets),
  MajorReno10 = c("No", "Yes")
) %>%
  mutate(
    intercept = coef(interaction_model)[["(Intercept)"]] +
      neighborhood_offsets[Neighborhood] +
      if_else(MajorReno10 == "Yes", coef(interaction_model)[["MajorReno10Yes"]], 0),
    slope = coef(interaction_model)[["Sqft100"]] +
      if_else(MajorReno10 == "Yes", coef(interaction_model)[["Sqft100:MajorReno10Yes"]], 0)
  )

assembled_lines

fit_grid_hyde <- fit_grid %>%
  filter(Neighborhood == "Hyde Park")

ggplot(fit_grid_hyde, aes(Sqft, interaction_fit, color = MajorReno10)) +
  geom_line(linewidth = 1.2) +
  scale_color_discrete(labels = c("No major renovation", "Major renovation")) +
  labs(
    x = "Square footage",
    y = "Fitted sale price (thousands)",
    title = "Interaction model: Hyde Park reference neighborhood"
  ) +
  theme_minimal(base_size = 14)
