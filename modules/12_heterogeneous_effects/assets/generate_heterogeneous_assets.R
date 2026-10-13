library(tidyverse)
library(broom)
library(splines)

set.seed(382120)

script_arg <- commandArgs(trailingOnly = FALSE)
script_file <- sub("--file=", "", script_arg[grepl("^--file=", script_arg)][1])
if (is.na(script_file)) {
  script_file <- "modules/12_heterogeneous_effects/assets/generate_heterogeneous_assets.R"
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

# Case 1: the win-back offer ---------------------------------------------

n_customers <- 4000
weeks <- c(runif(1400, 1, 4), runif(1400, 4, 12), runif(1200, 12, 30))
weeks <- round(sample(weeks), 1)

segment <- cut(
  weeks,
  breaks = c(0, 4, 12, Inf),
  labels = c("active", "drifting", "lapsed")
)

segment_effect <- c(active = -4, drifting = 14, lapsed = 4)

winback <- tibble(
  customer_id = sprintf("W%05d", sample(10000:99999, n_customers)),
  weeks_since_last_order = weeks,
  segment = as.character(segment),
  offer = rbinom(n_customers, 1, 0.5),
  app_platform = sample(
    c("ios", "android", "web"),
    n_customers,
    replace = TRUE,
    prob = c(0.45, 0.35, 0.20)
  ),
  signup_weekday = sample(
    c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
    n_customers,
    replace = TRUE
  )
) |>
  mutate(
    spend_60d = round(pmax(
      0,
      70 * exp(-0.05 * weeks_since_last_order) +
        segment_effect[segment] * offer +
        rnorm(n_customers, 0, 20)
    ), 2)
  )

# the platform and signup-weekday columns are pure noise, drawn independently
# of everything else; they exist for the subgroup-fishing demonstration
set.seed(23)
winback <- winback |>
  mutate(
    app_platform = sample(
      c("ios", "android", "web"),
      n_customers,
      replace = TRUE,
      prob = c(0.45, 0.35, 0.20)
    ),
    signup_weekday = sample(
      c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
      n_customers,
      replace = TRUE
    )
  )

write_csv(winback, file.path(module_dir, "winback_offer.csv"))

winback <- winback |>
  mutate(segment = factor(segment, levels = c("active", "drifting", "lapsed")))

# overall (average) treatment effect
overall_model <- lm(spend_60d ~ offer, data = winback)
write_csv(
  tidy(overall_model, conf.int = TRUE),
  file.path(asset_dir, "winback_overall.csv")
)

# treatment-by-segment interaction model
interaction_model <- lm(spend_60d ~ offer * segment, data = winback)
write_csv(
  tidy(interaction_model, conf.int = TRUE),
  file.path(asset_dir, "winback_interaction.csv")
)

# per-segment effects with intervals
segment_effects <- winback |>
  group_by(segment) |>
  group_modify(\(d, g) {
    tidy(lm(spend_60d ~ offer, data = d), conf.int = TRUE) |>
      filter(term == "offer")
  }) |>
  ungroup() |>
  select(segment, estimate, std.error, conf.low, conf.high) |>
  mutate(n = as.integer(table(winback$segment)))
write_csv(segment_effects, file.path(asset_dir, "winback_segment_effects.csv"))

segment_labels <- c(
  active = "Active\n(< 4 weeks)",
  drifting = "Drifting\n(4-12 weeks)",
  lapsed = "Lapsed\n(> 12 weeks)"
)

save_plot(
  ggplot(segment_effects, aes(segment, estimate)) +
    geom_hline(yintercept = 0, color = "gray45") +
    geom_hline(
      yintercept = coef(overall_model)[["offer"]],
      linetype = "dashed",
      color = "#C0392B"
    ) +
    annotate(
      "text",
      x = 0.62, y = coef(overall_model)[["offer"]] + 1.1,
      label = "average effect", color = "#C0392B", size = 4.2, hjust = 0
    ) +
    geom_pointrange(
      aes(ymin = conf.low, ymax = conf.high),
      color = "#355C7D",
      linewidth = 1.1,
      size = 0.9
    ) +
    scale_x_discrete(labels = segment_labels) +
    scale_y_continuous(labels = scales::label_dollar()) +
    labs(
      title = "One offer, three very different effects",
      x = NULL,
      y = "Offer effect on 60-day spend"
    ) +
    theme_course(),
  "winback-segment-effects.png"
)

cellmeans <- winback |>
  group_by(segment, offer) |>
  summarize(
    mean_spend = mean(spend_60d),
    se = sd(spend_60d) / sqrt(n()),
    .groups = "drop"
  )
write_csv(cellmeans, file.path(asset_dir, "winback_cell_means.csv"))

save_plot(
  ggplot(
    cellmeans,
    aes(segment, mean_spend,
      color = factor(offer, labels = c("No offer", "30% offer")),
      group = offer
    )
  ) +
    geom_pointrange(
      aes(ymin = mean_spend - 1.96 * se, ymax = mean_spend + 1.96 * se),
      position = position_dodge(width = 0.35),
      size = 0.85,
      linewidth = 1.0
    ) +
    scale_color_manual(values = c("#355C7D", "#D17A22"), name = NULL) +
    scale_x_discrete(labels = segment_labels) +
    scale_y_continuous(labels = scales::label_dollar()) +
    labs(
      title = "Mean 60-day spend by segment and offer",
      x = NULL,
      y = "Mean spend per customer"
    ) +
    theme_course(),
  "winback-cell-means.png"
)

# treatment effect along the numeric moderator: binned effects
binned_effects <- winback |>
  mutate(bin = cut(
    weeks_since_last_order,
    breaks = c(0, 2, 4, 6, 8, 10, 12, 16, 20, 25, 30),
    include.lowest = TRUE
  )) |>
  group_by(bin) |>
  group_modify(\(d, g) {
    tidy(lm(spend_60d ~ offer, data = d), conf.int = TRUE) |>
      filter(term == "offer") |>
      mutate(mid = mean(d$weeks_since_last_order), n = nrow(d))
  }) |>
  ungroup()
write_csv(binned_effects, file.path(asset_dir, "winback_binned_effects.csv"))

# the linear treatment-by-weeks model, for contrast
weeks_linear <- lm(spend_60d ~ offer * weeks_since_last_order, data = winback)
write_csv(
  tidy(weeks_linear, conf.int = TRUE),
  file.path(asset_dir, "winback_weeks_linear.csv")
)

wl <- coef(weeks_linear)

save_plot(
  ggplot(binned_effects, aes(mid, estimate)) +
    geom_hline(yintercept = 0, color = "gray45") +
    geom_abline(
      intercept = wl[["offer"]],
      slope = wl[["offer:weeks_since_last_order"]],
      linetype = "dashed",
      color = "#C0392B",
      linewidth = 0.9
    ) +
    annotate(
      "text",
      x = 24, y = 8.4,
      label = "linear interaction model", color = "#C0392B", size = 4.0
    ) +
    geom_pointrange(
      aes(ymin = conf.low, ymax = conf.high),
      color = "#355C7D",
      linewidth = 0.9,
      size = 0.7
    ) +
    scale_y_continuous(labels = scales::label_dollar()) +
    labs(
      title = "The offer effect along the recency moderator",
      subtitle = "Points: effect estimated within bins of weeks since last order.",
      x = "Weeks since last order",
      y = "Offer effect on 60-day spend"
    ) +
    theme_course(),
  "winback-weeks-effect.png"
)

# subgroup fishing: junk moderators produce a spurious discovery
junk_splits <- list(
  "iOS users" = winback$app_platform == "ios",
  "Android users" = winback$app_platform == "android",
  "Web users" = winback$app_platform == "web",
  "Mon signups" = winback$signup_weekday == "Mon",
  "Tue signups" = winback$signup_weekday == "Tue",
  "Wed signups" = winback$signup_weekday == "Wed",
  "Thu signups" = winback$signup_weekday == "Thu",
  "Fri signups" = winback$signup_weekday == "Fri",
  "Sat signups" = winback$signup_weekday == "Sat",
  "Sun signups" = winback$signup_weekday == "Sun"
)

fishing <- imap_dfr(junk_splits, \(flag, label) {
  d <- winback |> mutate(in_group = as.integer(flag))
  fit <- lm(spend_60d ~ offer * in_group, data = d)
  # effect of the offer inside the subgroup, with interval
  sub <- lm(spend_60d ~ offer, data = filter(d, in_group == 1))
  bind_cols(
    tibble(subgroup = label),
    tidy(sub, conf.int = TRUE) |>
      filter(term == "offer") |>
      select(estimate, conf.low, conf.high),
    tidy(fit) |>
      filter(term == "offer:in_group") |>
      transmute(interaction_p = p.value)
  )
})
write_csv(fishing, file.path(asset_dir, "winback_fishing.csv"))

ate <- coef(overall_model)[["offer"]]

save_plot(
  ggplot(fishing, aes(reorder(subgroup, estimate), estimate)) +
    geom_hline(yintercept = ate, linetype = "dashed", color = "#C0392B") +
    geom_hline(yintercept = 0, color = "gray45") +
    geom_pointrange(
      aes(
        ymin = conf.low, ymax = conf.high,
        color = interaction_p < 0.05
      ),
      linewidth = 0.9,
      size = 0.6
    ) +
    scale_color_manual(
      values = c("FALSE" = "#355C7D", "TRUE" = "#D17A22"),
      labels = c("interaction p > 0.05", "interaction p < 0.05"),
      name = NULL
    ) +
    coord_flip() +
    scale_y_continuous(labels = scales::label_dollar()) +
    labs(
      title = "Ten junk subgroups: one 'discovery' by luck alone",
      subtitle = "Dashed line: the overall average effect. None of these splits means anything.",
      x = NULL,
      y = "Offer effect within subgroup"
    ) +
    theme_course(),
  "winback-fishing.png"
)

# Case 2: ad frequency dose-response ----------------------------------------

set.seed(382121)

caps <- c(0, 2, 4, 8, 12, 16)
n_per_cap <- 600

adcap <- tibble(
  customer_id = sprintf("F%05d", sample(10000:99999, n_per_cap * length(caps))),
  weekly_ad_cap = sample(rep(caps, each = n_per_cap))
) |>
  mutate(
    spend_30d = round(pmax(
      0,
      40 + 14 * (1 - exp(-weekly_ad_cap / 4)) - 0.15 * weekly_ad_cap +
        rnorm(n(), 0, 15)
    ), 2)
  )

write_csv(adcap, file.path(module_dir, "ad_frequency.csv"))

cap_means <- adcap |>
  group_by(weekly_ad_cap) |>
  summarize(
    n = n(),
    mean_spend = mean(spend_30d),
    se = sd(spend_30d) / sqrt(n()),
    .groups = "drop"
  )
write_csv(cap_means, file.path(asset_dir, "adcap_means.csv"))

linear_dose <- lm(spend_30d ~ weekly_ad_cap, data = adcap)
write_csv(
  tidy(linear_dose, conf.int = TRUE),
  file.path(asset_dir, "adcap_linear.csv")
)

spline_dose <- lm(spend_30d ~ ns(weekly_ad_cap, df = 4), data = adcap)

pred <- tibble(weekly_ad_cap = seq(0, 16, by = 0.25))
pred$linear <- predict(linear_dose, newdata = pred)
pred$spline <- predict(spline_dose, newdata = pred)
write_csv(pred, file.path(asset_dir, "adcap_curve_preds.csv"))

fitted_at_caps <- tibble(weekly_ad_cap = caps)
fitted_at_caps$spline_fit <- predict(spline_dose, newdata = fitted_at_caps)
write_csv(fitted_at_caps, file.path(asset_dir, "adcap_spline_at_caps.csv"))

save_plot(
  ggplot(cap_means, aes(weekly_ad_cap, mean_spend)) +
    geom_pointrange(
      aes(ymin = mean_spend - 1.96 * se, ymax = mean_spend + 1.96 * se),
      color = "#355C7D",
      linewidth = 1.0,
      size = 0.85
    ) +
    scale_x_continuous(breaks = caps) +
    scale_y_continuous(labels = scales::label_dollar()) +
    labs(
      title = "Mean 30-day spend at each ad frequency cap",
      x = "Weekly ad impression cap",
      y = "Mean spend per customer"
    ) +
    theme_course(),
  "adcap-means.png"
)

save_plot(
  ggplot() +
    geom_pointrange(
      data = cap_means,
      aes(
        weekly_ad_cap, mean_spend,
        ymin = mean_spend - 1.96 * se,
        ymax = mean_spend + 1.96 * se
      ),
      color = "#355C7D",
      linewidth = 1.0,
      size = 0.85
    ) +
    geom_line(
      data = pred,
      aes(weekly_ad_cap, linear, linetype = "Straight line"),
      color = "#C0392B",
      linewidth = 1.0
    ) +
    geom_line(
      data = pred,
      aes(weekly_ad_cap, spline, linetype = "Spline"),
      color = "#C0392B",
      linewidth = 1.0
    ) +
    scale_linetype_manual(
      values = c("Straight line" = "dashed", "Spline" = "solid"),
      name = NULL
    ) +
    scale_x_continuous(breaks = caps) +
    scale_y_continuous(labels = scales::label_dollar()) +
    labs(
      title = "Dose-response: the curve saturates, the line cannot",
      x = "Weekly ad impression cap",
      y = "Mean spend per customer"
    ) +
    theme_course(),
  "adcap-fits.png"
)

message("Generated Module 12 data and assets in: ", module_dir)

# Case 3: the effect curve and a targeting rule ------------------------------

curve_fit <- lm(spend_60d ~ ns(weeks_since_last_order, df = 4) * offer, data = winback)

curve_grid <- seq(1, 30, by = 0.25)
nd1 <- tibble(weeks_since_last_order = curve_grid, offer = 1)
nd0 <- tibble(weeks_since_last_order = curve_grid, offer = 0)

tt <- delete.response(terms(curve_fit))
X1 <- model.matrix(tt, data = nd1)
X0 <- model.matrix(tt, data = nd0)
D <- X1 - X0

tau_hat <- drop(D %*% coef(curve_fit))
tau_se <- sqrt(rowSums((D %*% vcov(curve_fit)) * D))

effect_curve <- tibble(
  weeks = curve_grid,
  effect = tau_hat,
  conf.low = tau_hat - 1.96 * tau_se,
  conf.high = tau_hat + 1.96 * tau_se
)
write_csv(effect_curve, file.path(asset_dir, "winback_effect_curve.csv"))

# where the estimated effect crosses zero
crossing <- effect_curve |>
  filter(effect > 0) |>
  slice_min(weeks, n = 1) |>
  pull(weeks)

save_plot(
  ggplot(effect_curve, aes(weeks, effect)) +
    geom_hline(yintercept = 0, color = "gray45") +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "#77A6B6", alpha = 0.35) +
    geom_line(color = "#355C7D", linewidth = 1.2) +
    geom_pointrange(
      data = binned_effects,
      aes(mid, estimate, ymin = conf.low, ymax = conf.high),
      color = "#C0392B",
      alpha = 0.65,
      size = 0.45,
      linewidth = 0.7
    ) +
    geom_vline(xintercept = crossing, linetype = "dashed", color = "gray35") +
    annotate(
      "text",
      x = crossing + 0.5, y = -5.4,
      label = sprintf("effect turns positive\nnear week %.0f", crossing),
      hjust = 0, size = 3.8, color = "gray25", lineheight = 0.95
    ) +
    scale_y_continuous(labels = scales::label_dollar()) +
    labs(
      title = "The offer effect as a curve, with 95% band",
      subtitle = "Blue: spline-by-offer model. Red points: the binned estimates from earlier.",
      x = "Weeks since last order",
      y = "Offer effect on 60-day spend"
    ) +
    theme_course(),
  "winback-effect-curve.png"
)

# a targeting rule: send the offer where the estimated effect is positive
winback_scored <- winback |>
  mutate(
    tau_i = {
      nd1_i <- tibble(weeks_since_last_order = weeks_since_last_order, offer = 1)
      nd0_i <- tibble(weeks_since_last_order = weeks_since_last_order, offer = 0)
      drop((model.matrix(tt, nd1_i) - model.matrix(tt, nd0_i)) %*% coef(curve_fit))
    },
    contact = tau_i > 0
  )

policy_summary <- tibble(
  policy = c("no_offer", "blanket", "curve_targeted"),
  contacted_share = c(0, 1, mean(winback_scored$contact)),
  payoff_per_customer = c(
    0,
    mean(winback_scored$tau_i),
    mean(winback_scored$tau_i * winback_scored$contact)
  )
) |>
  mutate(payoff_per_100k = payoff_per_customer * 100000)
write_csv(policy_summary, file.path(asset_dir, "winback_policy_summary.csv"))

policy_plot_data <- policy_summary |>
  filter(policy != "no_offer") |>
  mutate(policy = factor(
    policy,
    levels = c("blanket", "curve_targeted"),
    labels = c("Blanket: offer everyone", "Targeted: offer where the curve is positive")
  ))

save_plot(
  ggplot(policy_plot_data, aes(policy, payoff_per_100k / 1000)) +
    geom_col(fill = c("#77A6B6", "#355C7D"), width = 0.55) +
    geom_text(
      aes(label = sprintf("$%.0fK\n(%.0f%% of list contacted)", payoff_per_100k / 1000, 100 * contacted_share)),
      vjust = -0.25, size = 4.4, lineheight = 0.95
    ) +
    coord_cartesian(ylim = c(0, max(policy_plot_data$payoff_per_100k / 1000) * 1.28)) +
    labs(
      title = "What the effect curve is worth, per 100,000 customers",
      subtitle = "Model-estimated incremental spend, this experiment only.",
      x = NULL,
      y = "Estimated incremental spend, $ thousands"
    ) +
    theme_course(),
  "winback-policy-payoff.png",
  width = 8.0,
  height = 4.9
)
