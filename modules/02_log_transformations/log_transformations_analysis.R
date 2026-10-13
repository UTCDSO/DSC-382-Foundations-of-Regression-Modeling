library(tidyverse)
library(broom)

if (!interactive()) {
  pdf(NULL)
}

module_dir <- if (file.exists("ebola_cases.csv")) {
  "."
} else {
  file.path("modules", "02_log_transformations")
}

ebola <- read_csv(file.path(module_dir, "ebola_cases.csv"), show_col_types = FALSE)
mammals <- read_csv(file.path(module_dir, "mammal_brain_body.csv"), show_col_types = FALSE)
pricing <- read_csv(file.path(module_dir, "pricing_experiment.csv"), show_col_types = FALSE)

# 1. Exponential growth: fit a line after logging the response.
ggplot(ebola, aes(day, cases)) +
  geom_point(alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Days since WHO announced the outbreak",
    y = "Reported cases",
    title = "Ebola cases on the original scale"
  ) +
  theme_minimal(base_size = 14)

ebola_log_model <- lm(log(cases) ~ day, data = ebola)
tidy(ebola_log_model)

daily_growth_rate <- coef(ebola_log_model)[["day"]]
daily_growth_factor <- exp(daily_growth_rate)
doubling_time <- log(2) / daily_growth_rate

tibble(
  daily_growth_rate = daily_growth_rate,
  daily_growth_factor = daily_growth_factor,
  percent_growth_per_day = 100 * (daily_growth_factor - 1),
  doubling_time_days = doubling_time
)

ggplot(ebola, aes(day, log(cases))) +
  geom_point(alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Days since WHO announced the outbreak",
    y = "log(reported cases)",
    title = "Ebola cases on the log scale"
  ) +
  theme_minimal(base_size = 14)

# 2. Power laws: body weight and brain weight.
ggplot(mammals, aes(body_kg, brain_g)) +
  geom_point(alpha = 0.8) +
  labs(
    x = "Body weight (kg)",
    y = "Brain weight (g)",
    title = "Mammal brain weight and body weight"
  ) +
  theme_minimal(base_size = 14)

mammal_model <- lm(log(brain_g) ~ log(body_kg), data = mammals)
tidy(mammal_model)

ggplot(mammals, aes(body_kg, brain_g)) +
  geom_point(alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    x = "Body weight (kg), log scale",
    y = "Brain weight (g), log scale",
    title = "Mammal scaling on a log-log scale"
  ) +
  theme_minimal(base_size = 14)

body_weight_multiplier <- 2
brain_weight_multiplier <- body_weight_multiplier^coef(mammal_model)[["log(body_kg)"]]
brain_weight_multiplier

# 3. Price elasticity from a randomized pricing experiment.
ggplot(pricing, aes(price, quantity)) +
  geom_point(alpha = 0.8) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    x = "Price, log scale",
    y = "Units sold, log scale",
    title = "Pricing experiment on a log-log scale"
  ) +
  theme_minimal(base_size = 14)

pricing_model <- lm(log(quantity) ~ log(price), data = pricing)
tidy(pricing_model)

price_elasticity <- coef(pricing_model)[["log(price)"]]
revenue_elasticity <- 1 + price_elasticity

tibble(
  price_elasticity = price_elasticity,
  approximate_revenue_elasticity = revenue_elasticity
)
