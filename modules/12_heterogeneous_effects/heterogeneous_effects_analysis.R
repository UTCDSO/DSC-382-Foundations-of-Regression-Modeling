library(tidyverse)
library(broom)
library(splines)

if (!interactive()) {
  pdf(NULL)
}

module_dir <- if (file.exists("winback_offer.csv")) {
  "."
} else {
  file.path("modules", "12_heterogeneous_effects")
}

# Case 1: the win-back offer -------------------------------------------------

winback <- read_csv(file.path(module_dir, "winback_offer.csv"), show_col_types = FALSE) %>%
  mutate(segment = factor(segment, levels = c("active", "drifting", "lapsed")))

winback %>% sample_n(8)

# The average treatment effect: one number for everyone
overall_model <- lm(spend_60d ~ offer, data = winback)
tidy(overall_model, conf.int = TRUE)

# The segments were defined BEFORE the experiment ran:
# active (< 4 weeks), drifting (4-12 weeks), lapsed (> 12 weeks)
winback %>% count(segment, offer)

# Mean spend by segment and offer
winback %>%
  group_by(segment, offer) %>%
  summarize(mean_spend = mean(spend_60d), .groups = "drop")

# The treatment-by-segment interaction model
interaction_model <- lm(spend_60d ~ offer * segment, data = winback)
tidy(interaction_model, conf.int = TRUE)

# The offer coefficient is the effect for the BASELINE segment (active).
# Each interaction coefficient is the DIFFERENCE between that segment's
# effect and the active-segment effect.

# Effect within each segment, with intervals
winback %>%
  group_by(segment) %>%
  group_modify(\(d, g) {
    tidy(lm(spend_60d ~ offer, data = d), conf.int = TRUE) %>%
      filter(term == "offer")
  }) %>%
  select(segment, estimate, conf.low, conf.high)

ggplot(
  winback %>%
    group_by(segment) %>%
    group_modify(\(d, g) {
      tidy(lm(spend_60d ~ offer, data = d), conf.int = TRUE) %>%
        filter(term == "offer")
    }),
  aes(segment, estimate)
) +
  geom_hline(yintercept = 0) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  labs(
    x = NULL,
    y = "Offer effect on 60-day spend, dollars",
    title = "The offer effect by customer segment"
  ) +
  theme_minimal(base_size = 14)

# A numeric moderator: weeks since last order
weeks_model <- lm(spend_60d ~ offer * weeks_since_last_order, data = winback)
tidy(weeks_model, conf.int = TRUE)

# The linear interaction says the effect grows by about $0.18 per week.
# Check that against effects estimated within recency bins:
winback %>%
  mutate(bin = cut(weeks_since_last_order, c(0, 4, 8, 12, 20, 30))) %>%
  group_by(bin) %>%
  group_modify(\(d, g) {
    tidy(lm(spend_60d ~ offer, data = d), conf.int = TRUE) %>%
      filter(term == "offer")
  }) %>%
  select(bin, estimate, conf.low, conf.high)

# The effect rises and then falls. A single linear interaction cannot
# represent that shape.

# Subgroup fishing: what happens when you split by everything ---------------

# app_platform and signup_weekday have nothing to do with the offer.
# Test an interaction for ten junk splits and watch what turns up.

junk_splits <- list(
  ios = winback$app_platform == "ios",
  android = winback$app_platform == "android",
  web = winback$app_platform == "web",
  Mon = winback$signup_weekday == "Mon",
  Tue = winback$signup_weekday == "Tue",
  Wed = winback$signup_weekday == "Wed",
  Thu = winback$signup_weekday == "Thu",
  Fri = winback$signup_weekday == "Fri",
  Sat = winback$signup_weekday == "Sat",
  Sun = winback$signup_weekday == "Sun"
)

imap_dfr(junk_splits, \(flag, label) {
  d <- winback %>% mutate(in_group = as.integer(flag))
  lm(spend_60d ~ offer * in_group, data = d) %>%
    tidy() %>%
    filter(term == "offer:in_group") %>%
    transmute(subgroup = label, interaction_p = p.value)
}) %>%
  arrange(interaction_p)

# One of ten junk splits shows p < 0.05. With ten tests at the 5% level,
# the chance of at least one false hit is about 40%.

# Case 2: the ad frequency experiment ----------------------------------------

adcap <- read_csv(file.path(module_dir, "ad_frequency.csv"), show_col_types = FALSE)

adcap %>% sample_n(8)

# Mean spend at each cap
cap_means <- adcap %>%
  group_by(weekly_ad_cap) %>%
  summarize(
    n = n(),
    mean_spend = mean(spend_30d),
    se = sd(spend_30d) / sqrt(n())
  )

cap_means

ggplot(cap_means, aes(weekly_ad_cap, mean_spend)) +
  geom_pointrange(aes(
    ymin = mean_spend - 1.96 * se,
    ymax = mean_spend + 1.96 * se
  )) +
  labs(
    x = "Weekly ad impression cap",
    y = "Mean 30-day spend, dollars",
    title = "Spend at each ad frequency cap"
  ) +
  theme_minimal(base_size = 14)

# The straight-line dose model promises a constant return per impression
linear_dose <- lm(spend_30d ~ weekly_ad_cap, data = adcap)
tidy(linear_dose, conf.int = TRUE)

# The spline lets the response saturate (same machinery as the
# smoothing module)
spline_dose <- lm(spend_30d ~ ns(weekly_ad_cap, df = 4), data = adcap)

pred <- tibble(weekly_ad_cap = seq(0, 16, by = 0.5))
pred$linear <- predict(linear_dose, newdata = pred)
pred$spline <- predict(spline_dose, newdata = pred)

ggplot() +
  geom_pointrange(
    data = cap_means,
    aes(
      weekly_ad_cap, mean_spend,
      ymin = mean_spend - 1.96 * se,
      ymax = mean_spend + 1.96 * se
    )
  ) +
  geom_line(data = pred, aes(weekly_ad_cap, linear), linetype = "dashed") +
  geom_line(data = pred, aes(weekly_ad_cap, spline)) +
  labs(
    x = "Weekly ad impression cap",
    y = "Mean 30-day spend, dollars",
    title = "Linear versus spline dose-response"
  ) +
  theme_minimal(base_size = 14)

# Marginal value of more impressions, from the spline fit
tibble(weekly_ad_cap = c(0, 4, 8, 16)) %>%
  mutate(fitted_spend = predict(spline_dose, newdata = .)) %>%
  mutate(gain_from_previous = fitted_spend - lag(fitted_spend))

# Case 3: the effect curve and a targeting rule --------------------------------

# The binned effects traced a shape no linear interaction could follow.
# Let the treatment effect itself be a smooth curve in recency:
curve_fit <- lm(spend_60d ~ ns(weeks_since_last_order, df = 4) * offer, data = winback)

# The effect at recency w is the vertical gap between the two fitted curves.
# Each gap is a CONTRAST -- a linear combination of coefficients -- so it has
# a standard error, exactly as in the experiments module.
curve_grid <- seq(1, 30, by = 0.25)
nd1 <- tibble(weeks_since_last_order = curve_grid, offer = 1)
nd0 <- tibble(weeks_since_last_order = curve_grid, offer = 0)

tt <- delete.response(terms(curve_fit))
D <- model.matrix(tt, nd1) - model.matrix(tt, nd0)

effect_curve <- tibble(
  weeks = curve_grid,
  effect = drop(D %*% coef(curve_fit)),
  se = sqrt(rowSums((D %*% vcov(curve_fit)) * D))
) %>%
  mutate(conf.low = effect - 1.96 * se, conf.high = effect + 1.96 * se)

ggplot(effect_curve, aes(weeks, effect)) +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.25) +
  geom_line() +
  labs(
    x = "Weeks since last order",
    y = "Offer effect on 60-day spend, dollars",
    title = "The offer effect as a smooth curve, with a 95% band"
  ) +
  theme_minimal(base_size = 14)

# Where does the estimated effect turn positive?
effect_curve %>%
  filter(effect > 0) %>%
  slice_min(weeks)

# A targeting rule: send the offer wherever the estimated effect is positive.
nd1_all <- winback %>% mutate(offer = 1)
nd0_all <- winback %>% mutate(offer = 0)
tau_i <- drop((model.matrix(tt, nd1_all) - model.matrix(tt, nd0_all)) %*% coef(curve_fit))

winback_scored <- winback %>%
  mutate(tau_i = tau_i, contact = tau_i > 0)

# Price the two policies per 100,000 customers (model-estimated, same data --
# so treat this as a promise to validate on the next campaign, not a result).
winback_scored %>%
  summarize(
    blanket_per_100k = mean(tau_i) * 1e5,
    targeted_per_100k = mean(tau_i * contact) * 1e5,
    share_contacted = mean(contact)
  )
