library(tidyverse)
library(broom)

if (!interactive()) {
  pdf(NULL)
}

data_path <- if (file.exists("meal_kit_demand.csv")) {
  "meal_kit_demand.csv"
} else {
  file.path("modules", "04_multiple_regression", "meal_kit_demand.csv")
}

meal_kits <- read_csv(data_path, show_col_types = FALSE) %>%
  mutate(
    Marketing = factor(Marketing, levels = c("No push", "Push")),
    logPrice = log(Price),
    logEligibleViews = log(EligibleViews),
    logUnits = log(UnitsSold),
    logPriceCentered = logPrice - log(23)
  )

set.seed(38207)
meal_kits %>%
  sample_n(8)

# 1. The pooled relationship between demand and price.
ggplot(meal_kits, aes(logPrice, logUnits)) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "log(Price)",
    y = "log(Units sold)",
    title = "H-E-B meal kit units sold versus price"
  ) +
  theme_minimal(base_size = 14)

naive_model <- lm(logUnits ~ logPrice, data = meal_kits)
tidy(naive_model)

# 2. Adjust for whether the app ran a marketing push that week.
meal_kits %>%
  group_by(Marketing) %>%
  summarize(
    weeks = n(),
    average_price = mean(Price),
    average_eligible_views = mean(EligibleViews),
    average_units = mean(UnitsSold),
    .groups = "drop"
  )

marketing_adjusted_model <- lm(logUnits ~ logPrice + Marketing, data = meal_kits)
tidy(marketing_adjusted_model)

# 3. Adjust for how many shoppers reached the meal-kit page.
ggplot(meal_kits, aes(logEligibleViews, logUnits)) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "log(Eligible views)",
    y = "log(Units sold)",
    title = "Meal kit units sold versus eligible page views"
  ) +
  theme_minimal(base_size = 14)

exposure_adjusted_model <- lm(
  logUnits ~ logPrice + logEligibleViews + Marketing,
  data = meal_kits
)
tidy(exposure_adjusted_model)

# 4. Check whether markets differ in price, exposure, and demand.
market_means <- meal_kits %>%
  group_by(Market) %>%
  summarize(
    mean_log_price = mean(logPrice),
    mean_log_eligible_views = mean(logEligibleViews),
    mean_log_units = mean(logUnits),
    .groups = "drop"
  ) %>%
  mutate(
    z_price = as.numeric(scale(mean_log_price)),
    z_views = as.numeric(scale(mean_log_eligible_views)),
    z_units = as.numeric(scale(mean_log_units)),
    Market = fct_reorder(Market, z_price)
  )

market_means %>%
  pivot_longer(
    cols = c(z_price, z_views, z_units),
    names_to = "quantity",
    values_to = "z_score"
  ) %>%
  mutate(
    quantity = recode(
      quantity,
      z_price = "Average log price",
      z_views = "Average log eligible views",
      z_units = "Average log units"
    )
  ) %>%
  ggplot(aes(Market, z_score, fill = quantity)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_hline(yintercept = 0) +
  labs(
    x = "Market",
    y = "Z-score",
    title = "Market averages for price, eligible views, and units sold"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

# 5. Adjust for market as well as exposure and marketing status.
market_adjusted_model <- lm(
  logUnits ~ logPrice + logEligibleViews + Marketing + Market,
  data = meal_kits
)
tidy(market_adjusted_model)

# 6. Let the price slope differ in marketing weeks.
ggplot(meal_kits, aes(logPrice, logUnits, color = Marketing)) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ Market, ncol = 5) +
  labs(
    x = "log(Price)",
    y = "log(Units sold)",
    title = "H-E-B meal kit demand by market and marketing status"
  ) +
  theme_minimal(base_size = 12)

interaction_model <- lm(
  logUnits ~ logPriceCentered * Marketing + logEligibleViews + Market,
  data = meal_kits
)

tidy(interaction_model)

no_push_elasticity <- coef(interaction_model)[["logPriceCentered"]]
push_elasticity <- no_push_elasticity +
  coef(interaction_model)[["logPriceCentered:MarketingPush"]]

tibble(
  marketing_status = c("No push", "Push"),
  estimated_elasticity = c(no_push_elasticity, push_elasticity)
)
