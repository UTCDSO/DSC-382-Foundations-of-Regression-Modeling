library(ggplot2)
library(scales)
library(dplyr)
library(readr)
library(tidyr)

set.seed(38206)

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- sub("^--file=", "", script_arg[1] %||% "assets/generate_multiple_regression_assets.R")
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

n_pr <- 950

pr_data <- tibble(pr_id = 1:n_pr) %>%
  mutate(
    size_group = sample(
      c("small", "medium", "large"),
      n(),
      replace = TRUE,
      prob = c(0.42, 0.38, 0.20)
    ),
    LinesChanged = case_when(
      size_group == "small" ~ round(runif(n(), 15, 100)),
      size_group == "medium" ~ round(runif(n(), 85, 260)),
      TRUE ~ round(runif(n(), 240, 720))
    ),
    base_prompts = case_when(
      size_group == "small" ~ 2,
      size_group == "medium" ~ 7,
      TRUE ~ 17
    ),
    AIPrompts = pmax(0, round(base_prompts + rnorm(n(), 0, 4.0))),
    review_noise = rt(n(), df = 5) * 6 / sqrt(5 / 3),
    outlier_noise = if_else(runif(n()) < 0.025, runif(n(), 20, 55), 0),
    HoursToMerge = 16 + 0.09 * LinesChanged - 1.4 * AIPrompts +
      review_noise + outlier_noise,
    HoursToMerge = round(pmax(HoursToMerge, 2.5), 1),
    Lines100 = LinesChanged / 100
  ) %>%
  select(pr_id, AIPrompts, LinesChanged, HoursToMerge, Lines100)

write_csv(
  pr_data %>% select(pr_id, AIPrompts, LinesChanged, HoursToMerge),
  file.path(module_dir, "ai_pull_requests.csv")
)

fit_naive <- lm(HoursToMerge ~ AIPrompts, data = pr_data)
fit_adjusted <- lm(HoursToMerge ~ AIPrompts + Lines100, data = pr_data)

pr_plot_data <- pr_data %>%
  mutate(
    SizeQuartile = cut_number(
      LinesChanged,
      4,
      labels = c("Q1: smallest PRs", "Q2", "Q3", "Q4: largest PRs")
    )
  )

p_overall <- ggplot(pr_data, aes(AIPrompts, HoursToMerge)) +
  geom_point(color = purple, alpha = 0.32, size = 1.8) +
  geom_smooth(method = "lm", se = FALSE, color = red, linewidth = 1.3) +
  scale_y_continuous(labels = label_number(suffix = " h")) +
  labs(
    title = "Time to merge versus AI prompts",
    subtitle = "One-variable regression fit",
    x = "AI prompts used on the PR",
    y = "Hours to merge"
  ) +
  theme_classroom()
save_plot(p_overall, "ai-overall.png")

p_prompts_lines <- ggplot(pr_data, aes(LinesChanged, AIPrompts)) +
  geom_point(color = purple, alpha = 0.28, size = 1.8) +
  geom_smooth(method = "lm", se = FALSE, color = blue, linewidth = 1.3) +
  scale_x_continuous(labels = comma) +
  labs(
    title = "AI prompts versus PR size",
    subtitle = "PR size measured by lines changed",
    x = "Lines changed",
    y = "AI prompts used"
  ) +
  theme_classroom()
save_plot(p_prompts_lines, "ai-prompts-lines.png")

p_hours_lines <- ggplot(pr_data, aes(LinesChanged, HoursToMerge)) +
  geom_point(color = purple, alpha = 0.28, size = 1.8) +
  geom_smooth(method = "lm", se = FALSE, color = blue, linewidth = 1.3) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = label_number(suffix = " h")) +
  labs(
    title = "Time to merge versus PR size",
    subtitle = "PR size measured by lines changed",
    x = "Lines changed",
    y = "Hours to merge"
  ) +
  theme_classroom()
save_plot(p_hours_lines, "ai-hours-lines.png")

p_quartiles <- ggplot(pr_plot_data, aes(AIPrompts, HoursToMerge)) +
  geom_point(color = purple, alpha = 0.30, size = 1.5) +
  geom_smooth(data = pr_data, method = "lm", se = FALSE, color = red, linewidth = 1.0) +
  geom_smooth(method = "lm", se = FALSE, color = blue, linewidth = 1.25) +
  facet_wrap(~ SizeQuartile, ncol = 2) +
  scale_y_continuous(labels = label_number(suffix = " h")) +
  labs(
    title = "Overall fit and within-size fits",
    subtitle = "Panels split PRs by quartiles of lines changed",
    x = "AI prompts used on the PR",
    y = "Hours to merge"
  ) +
  theme_classroom() +
  theme(legend.position = "none")
save_plot(p_quartiles, "ai-quartile-slopes.png")

line_values <- quantile(pr_data$LinesChanged, c(0.15, 0.50, 0.85))

line_grid <- bind_rows(
  tibble(AIPrompts = seq(0, 9, length.out = 80), LinesChanged = line_values[[1]]),
  tibble(AIPrompts = seq(0, 15, length.out = 80), LinesChanged = line_values[[2]]),
  tibble(AIPrompts = seq(5, 25, length.out = 80), LinesChanged = line_values[[3]])
) %>%
  mutate(
    Lines100 = LinesChanged / 100,
    PRSize = factor(
      paste0(round(LinesChanged), " lines changed"),
      levels = paste0(round(quantile(pr_data$LinesChanged, c(0.15, 0.50, 0.85))), " lines changed")
    )
  )

line_grid$fit <- predict(fit_adjusted, newdata = line_grid)

p_adjusted_lines <- ggplot(pr_data, aes(AIPrompts, HoursToMerge)) +
  geom_point(color = purple, alpha = 0.16, size = 1.4) +
  geom_line(
    data = line_grid,
    aes(y = fit, color = PRSize),
    linewidth = 1.35
  ) +
  scale_color_manual(values = c(green, blue, red)) +
  scale_y_continuous(labels = label_number(suffix = " h")) +
  labs(
    title = "Fitted lines from the adjusted model",
    subtitle = "Holding PR size fixed at three values",
    x = "AI prompts used on the PR",
    y = "Fitted hours to merge"
  ) +
  theme_classroom()
save_plot(p_adjusted_lines, "ai-adjusted-lines.png")

resid_lines <- lm(HoursToMerge ~ Lines100, data = pr_data)
pr_resid <- pr_data %>%
  mutate(
    AdjustedHours = mean(HoursToMerge) + resid(resid_lines)
  )

p_adjusted_response <- ggplot(pr_resid, aes(AIPrompts, AdjustedHours)) +
  geom_point(color = purple, alpha = 0.30, size = 1.7) +
  geom_smooth(method = "lm", se = FALSE, color = blue, linewidth = 1.3) +
  scale_y_continuous(labels = label_number(suffix = " h")) +
  labs(
    title = "Hours to merge adjusted for PR size",
    subtitle = "Residualized response plotted against AI prompts",
    x = "AI prompts used on the PR",
    y = "Hours to merge, adjusted for PR size"
  ) +
  theme_classroom()
save_plot(p_adjusted_response, "ai-adjusted-response.png")

model_summary <- tibble(
  quantity = c(
    "naive_intercept",
    "naive_prompt_slope",
    "adjusted_intercept",
    "adjusted_prompt_slope",
    "adjusted_lines100_slope"
  ),
  value = c(
    coef(fit_naive)[["(Intercept)"]],
    coef(fit_naive)[["AIPrompts"]],
    coef(fit_adjusted)[["(Intercept)"]],
    coef(fit_adjusted)[["AIPrompts"]],
    coef(fit_adjusted)[["Lines100"]]
  )
)

write_csv(model_summary, file.path(out_dir, "ai_model_summary.csv"))

# Weeknight meal kit demand case -------------------------------------------

set.seed(70771)

markets <- tibble(
  Market = c(
    "Austin Central", "Austin North", "Austin South",
    "San Antonio North", "San Antonio South",
    "Houston Central", "Houston West", "Houston North",
    "Corpus Christi", "Rio Grande Valley",
    "Waco-Temple", "Bryan-College Stn", "East Texas",
    "Frisco-Plano", "Allen-McKinney"
  ),
  market_signal = c(
    0.9, 0.6, 0.2,
    0.4, -0.5,
    1.2, 0.8, 0.3,
    -0.6, -0.8,
    -0.2, 0.0, -0.4,
    1.3, 1.0
  )
) %>%
  mutate(
    base_price = 24 + 1.15 * market_signal + rnorm(n(), 0, 0.65),
    market_effect = 0.58 * market_signal + rnorm(n(), 0, 0.15),
    view_effect = 0.62 * market_signal + rnorm(n(), 0, 0.14),
    p_raw = runif(n(), 0.1, 0.9),
    p_market = 0.5 * p_raw + 0.5 * plogis(-0.7 * market_signal + rnorm(n(), 0, 0.8))
  )

meal_kits <- expand_grid(
  Market = markets$Market,
  Week = 1:104
) %>%
  left_join(markets, by = "Market") %>%
  mutate(
    price_season = 0.04 * sin(2 * pi * Week / 52) + 0.02 * cos(2 * pi * Week / 26),
    logPrice = log(base_price) + price_season + rnorm(n(), 0, 0.055),
    Price = exp(logPrice),
    view_season = 0.11 * sin(2 * pi * Week / 52 + 0.35) + 0.04 * cos(2 * pi * Week / 13),
    logEligibleViews = 9.45 + view_effect + view_season + rnorm(n(), 0, 0.18),
    EligibleViews = round(exp(logEligibleViews)),
    marketing_probability = plogis(qlogis(p_market) - 3.0 * (logPrice - log(base_price))),
    Marketing = rbinom(n(), 1, marketing_probability),
    logPriceCentered = logPrice - log(23),
    demand_season = 0.07 * sin(2 * pi * Week / 52 + 0.8),
    logUnits = -1.83 + market_effect +
      0.92 * logEligibleViews -
      2.45 * logPriceCentered +
      0.45 * Marketing -
      0.45 * logPriceCentered * Marketing +
      demand_season +
      rnorm(n(), 0, 0.20),
    UnitsSold = round(exp(logUnits)),
    Marketing = factor(
      if_else(Marketing == 1, "Push", "No push"),
      levels = c("No push", "Push")
    )
  ) %>%
  select(
    Market, Week, Price, EligibleViews, UnitsSold, Marketing,
    logPrice, logEligibleViews, logUnits, logPriceCentered
  )

write_csv(
  meal_kits %>%
    mutate(
      Price = round(Price, 2),
      logEligibleViews = round(logEligibleViews, 4),
      logPrice = round(logPrice, 4),
      logUnits = round(logUnits, 4),
      logPriceCentered = round(logPriceCentered, 4)
    ),
  file.path(module_dir, "meal_kit_demand.csv")
)

fit_meal_naive <- lm(logUnits ~ logPrice, data = meal_kits)
fit_meal_marketing <- lm(logUnits ~ logPrice + Marketing, data = meal_kits)
fit_meal_exposure <- lm(logUnits ~ logPrice + logEligibleViews + Marketing, data = meal_kits)
fit_meal_market <- lm(logUnits ~ logPrice + logEligibleViews + Marketing + Market, data = meal_kits)
fit_meal_interaction <- lm(
  logUnits ~ logPriceCentered * Marketing + logEligibleViews + Market,
  data = meal_kits
)

meal_summary <- tibble(
  quantity = c(
    "naive_log_price_slope",
    "marketing_adjusted_log_price_slope",
    "exposure_adjusted_log_price_slope",
    "log_eligible_views_slope",
    "market_adjusted_log_price_slope",
    "interaction_no_push_slope",
    "interaction_push_extra_slope",
    "interaction_push_slope",
    "push_lift_at_23_dollars",
    "mean_price",
    "median_eligible_views",
    "median_units",
    "push_week_rate"
  ),
  value = c(
    coef(fit_meal_naive)[["logPrice"]],
    coef(fit_meal_marketing)[["logPrice"]],
    coef(fit_meal_exposure)[["logPrice"]],
    coef(fit_meal_exposure)[["logEligibleViews"]],
    coef(fit_meal_market)[["logPrice"]],
    coef(fit_meal_interaction)[["logPriceCentered"]],
    coef(fit_meal_interaction)[["logPriceCentered:MarketingPush"]],
    coef(fit_meal_interaction)[["logPriceCentered"]] +
      coef(fit_meal_interaction)[["logPriceCentered:MarketingPush"]],
    coef(fit_meal_interaction)[["MarketingPush"]],
    mean(meal_kits$Price),
    median(meal_kits$EligibleViews),
    median(meal_kits$UnitsSold),
    mean(meal_kits$Marketing == "Push")
  )
)

write_csv(meal_summary, file.path(out_dir, "meal_kit_model_summary.csv"))

p_meal_naive <- ggplot(meal_kits, aes(logPrice, logUnits)) +
  geom_point(color = purple, alpha = 0.28, size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, color = red, linewidth = 1.25) +
  labs(
    title = "H-E-B meal kit units sold versus price",
    subtitle = "All Texas area-weeks pooled",
    x = "log(Price)",
    y = "log(Units sold)"
  ) +
  theme_classroom()
save_plot(p_meal_naive, "mealkit-naive.png")

market_z <- meal_kits %>%
  summarize(
    log_price_mean = mean(logPrice),
    log_price_sd = sd(logPrice),
    log_views_mean = mean(logEligibleViews),
    log_views_sd = sd(logEligibleViews),
    log_units_mean = mean(logUnits),
    log_units_sd = sd(logUnits)
  )

market_bars <- meal_kits %>%
  group_by(Market) %>%
  summarize(
    z_price = (mean(logPrice) - market_z$log_price_mean) / market_z$log_price_sd,
    z_views = (mean(logEligibleViews) - market_z$log_views_mean) / market_z$log_views_sd,
    z_units = (mean(logUnits) - market_z$log_units_mean) / market_z$log_units_sd,
    .groups = "drop"
  )

market_order <- market_bars %>%
  arrange(z_price) %>%
  pull(Market)

market_bars_long <- market_bars %>%
  mutate(Market = factor(Market, levels = market_order)) %>%
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
  )

p_market_bars <- ggplot(market_bars_long, aes(Market, z_score, fill = quantity)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.64) +
  geom_hline(yintercept = 0, color = "#1f2430", linewidth = 0.5) +
  scale_fill_manual(values = c(
    "Average log price" = blue,
    "Average log eligible views" = green,
    "Average log units" = gold
  )) +
  labs(
    title = "Market averages for price, eligible views, and units sold",
    subtitle = "Z-scores use global means and standard deviations",
    x = "Market",
    y = "Z-score"
  ) +
  theme_classroom(base_size = 16) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 12))
save_plot(p_market_bars, "mealkit-market-zbars.png", width = 10.6, height = 6.2)

p_meal_views <- ggplot(meal_kits, aes(logEligibleViews, logUnits)) +
  geom_point(color = purple, alpha = 0.28, size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, color = red, linewidth = 1.25) +
  labs(
    title = "Meal kit units sold versus eligible page views",
    subtitle = "Each point is one Texas area-week",
    x = "log(Eligible views)",
    y = "log(Units sold)"
  ) +
  theme_classroom()
save_plot(p_meal_views, "mealkit-eligible-views.png")

p_market_fits <- ggplot(meal_kits, aes(logPrice, logUnits, color = Marketing)) +
  geom_point(alpha = 0.34, size = 1.25) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.95) +
  facet_wrap(~ Market, ncol = 5) +
  scale_color_manual(values = c("No push" = purple, "Push" = green)) +
  labs(
    title = "H-E-B meal kit demand by market and marketing status",
    subtitle = "Separate fitted lines by marketing status",
    x = "log(Price)",
    y = "log(Units sold)"
  ) +
  theme_classroom(base_size = 14) +
  theme(
    strip.text = element_text(size = 11),
    axis.text = element_text(size = 9)
  )
save_plot(p_market_fits, "mealkit-market-fits.png", width = 11.2, height = 7.2)
