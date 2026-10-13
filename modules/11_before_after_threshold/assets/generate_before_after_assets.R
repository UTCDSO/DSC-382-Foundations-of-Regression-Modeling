library(tidyverse)
library(broom)

set.seed(382110)

script_arg <- commandArgs(trailingOnly = FALSE)
script_file <- sub("--file=", "", script_arg[grepl("^--file=", script_arg)][1])
if (is.na(script_file)) {
  script_file <- "modules/11_before_after_threshold/assets/generate_before_after_assets.R"
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

# Case 1: courier pay guarantee, difference-in-differences -------------------

n_weeks <- 40
post_start <- 25

city_base <- c(fort_worth = 1620, san_antonio = 1385)
slump <- -180        # market-wide drop in the post period
guarantee <- 120     # policy effect on San Antonio couriers, post period

common_shock <- rnorm(n_weeks, 0, 28)

couriers <- expand_grid(
  city = c("fort_worth", "san_antonio"),
  week = seq_len(n_weeks)
) |>
  mutate(
    week_start = as.Date("2024-01-01") + (week - 1) * 7,
    post = as.integer(week >= post_start),
    treated = as.integer(city == "san_antonio"),
    active_couriers = round(
      city_base[city] +
        slump * post +
        guarantee * treated * post +
        common_shock[week] +
        rnorm(n(), 0, 15)
    )
  ) |>
  select(city, week, week_start, treated, post, active_couriers) |>
  arrange(week, city)

write_csv(couriers, file.path(module_dir, "courier_pay.csv"))

cell_means <- couriers |>
  group_by(city, post) |>
  summarize(mean_couriers = mean(active_couriers), .groups = "drop")
write_csv(cell_means, file.path(asset_dir, "courier_cell_means.csv"))

did_model <- lm(active_couriers ~ treated * post, data = couriers)
did_week_fe <- lm(
  active_couriers ~ treated + factor(week) + treated:post,
  data = couriers
)

did_summary <- bind_rows(
  tidy(did_model, conf.int = TRUE) |> mutate(model = "did"),
  tidy(did_week_fe, conf.int = TRUE) |>
    filter(!str_detect(term, "week")) |>
    mutate(model = "did_week_fe")
) |>
  select(model, term, estimate, std.error, statistic, p.value, conf.low, conf.high)
write_csv(did_summary, file.path(asset_dir, "courier_did_summary.csv"))

city_labels <- c(fort_worth = "Fort Worth (no change)", san_antonio = "San Antonio (pay guarantee)")

save_plot(
  ggplot(couriers, aes(week_start, active_couriers, color = city)) +
    geom_vline(
      xintercept = as.Date("2024-01-01") + (post_start - 1) * 7,
      linetype = "dashed",
      color = "gray35"
    ) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.5, alpha = 0.7) +
    scale_color_manual(
      values = c(fort_worth = "#355C7D", san_antonio = "#D17A22"),
      labels = city_labels,
      name = NULL
    ) +
    labs(
      title = "Weekly active couriers in the two cities",
      subtitle = "Dashed line: the San Antonio pay guarantee launches.",
      x = NULL,
      y = "Active couriers"
    ) +
    theme_course(),
  "courier-trends.png"
)

# 2x2 means with the counterfactual for San Antonio
counterfactual <- cell_means |>
  filter(city == "san_antonio", post == 0) |>
  pull(mean_couriers) +
  (cell_means$mean_couriers[cell_means$city == "fort_worth" & cell_means$post == 1] -
    cell_means$mean_couriers[cell_means$city == "fort_worth" & cell_means$post == 0])

did_plot_data <- cell_means |>
  mutate(
    period = factor(if_else(post == 0, "Before", "After"), levels = c("Before", "After"))
  )

save_plot(
  ggplot(did_plot_data, aes(period, mean_couriers, color = city, group = city)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 3.6) +
    annotate(
      "segment",
      x = 1, xend = 2,
      y = did_plot_data$mean_couriers[did_plot_data$city == "san_antonio" & did_plot_data$post == 0],
      yend = counterfactual,
      linetype = "dashed",
      color = "#D17A22",
      linewidth = 0.9
    ) +
    annotate(
      "text",
      x = 2.06, y = counterfactual - 12,
      label = "San Antonio, if it had\nfollowed Fort Worth's change",
      hjust = 0, size = 3.6, color = "#8a5a22", lineheight = 0.95
    ) +
    scale_color_manual(
      values = c(fort_worth = "#355C7D", san_antonio = "#D17A22"),
      labels = city_labels,
      name = NULL
    ) +
    coord_cartesian(xlim = c(1, 2.9)) +
    labs(
      title = "Difference-in-differences: the change in changes",
      x = NULL,
      y = "Mean active couriers"
    ) +
    theme_course(),
  "courier-did-means.png"
)

gap <- couriers |>
  select(city, week, week_start, active_couriers) |>
  pivot_wider(names_from = city, values_from = active_couriers) |>
  mutate(gap = san_antonio - fort_worth)

save_plot(
  ggplot(gap, aes(week_start, gap)) +
    geom_vline(
      xintercept = as.Date("2024-01-01") + (post_start - 1) * 7,
      linetype = "dashed",
      color = "gray35"
    ) +
    geom_hline(yintercept = mean(gap$gap[gap$week < post_start]), color = "gray60") +
    geom_line(color = "#355C7D", linewidth = 0.9) +
    geom_point(size = 1.6, color = "#355C7D", alpha = 0.7) +
    labs(
      title = "San Antonio minus Fort Worth, week by week",
      subtitle = "A flat pre-period gap is the parallel-trends assumption made visible.",
      x = NULL,
      y = "Gap in active couriers"
    ) +
    theme_course(),
  "courier-gap.png"
)

# Case 2: customer success managers at a $20k ARR threshold ------------------

set.seed(382111)

n_accounts <- 1200
arr <- round(runif(n_accounts, 8, 32), 1)
csm <- as.integer(arr >= 20)
nrr <- 82 + 0.9 * arr - 0.015 * arr^2 + 6 * csm + rnorm(n_accounts, 0, 7)

csm_accounts <- tibble(
  account_id = sprintf("K%04d", sample(1000:9999, n_accounts)),
  arr_prior_k = arr,
  csm = csm,
  renewal_spend_pct = round(nrr, 1)
)

write_csv(csm_accounts, file.path(module_dir, "csm_accounts.csv"))

# the naive comparison
naive_gap <- csm_accounts |>
  group_by(csm) |>
  summarize(mean_nrr = mean(renewal_spend_pct), n = n(), .groups = "drop")
write_csv(naive_gap, file.path(asset_dir, "csm_naive_gap.csv"))

# local linear RD at several bandwidths
rd_fit <- function(bw) {
  local <- csm_accounts |>
    filter(abs(arr_prior_k - 20) <= bw) |>
    mutate(x_c = arr_prior_k - 20)
  fit <- lm(renewal_spend_pct ~ x_c * csm, data = local)
  tidy(fit, conf.int = TRUE) |>
    filter(term == "csm") |>
    mutate(bandwidth = bw, n_local = nrow(local))
}

rd_by_bw <- map_dfr(c(2, 3, 4, 5, 6, 8, 10), rd_fit) |>
  select(bandwidth, n_local, estimate, std.error, conf.low, conf.high)
write_csv(rd_by_bw, file.path(asset_dir, "csm_rd_by_bandwidth.csv"))

# a global quadratic with a jump, as a functional-form comparison
global_quad <- lm(
  renewal_spend_pct ~ poly(arr_prior_k, 2, raw = TRUE) + csm,
  data = csm_accounts
)
write_csv(
  tidy(global_quad, conf.int = TRUE),
  file.path(asset_dir, "csm_global_quadratic.csv")
)

# binned scatter with local linear fits inside bandwidth 5
bw_main <- 5
bins <- csm_accounts |>
  mutate(bin = floor(arr_prior_k)) |>
  group_by(bin, csm) |>
  summarize(mean_nrr = mean(renewal_spend_pct), n = n(), .groups = "drop") |>
  mutate(bin_mid = bin + 0.5)

local_main <- csm_accounts |>
  filter(abs(arr_prior_k - 20) <= bw_main) |>
  mutate(x_c = arr_prior_k - 20)
fit_main <- lm(renewal_spend_pct ~ x_c * csm, data = local_main)

pred_grid <- tibble(arr_prior_k = seq(15, 25, by = 0.1)) |>
  mutate(
    x_c = arr_prior_k - 20,
    csm = as.integer(arr_prior_k >= 20),
    side = if_else(csm == 1, "above", "below")
  )
pred_grid$fit <- predict(fit_main, newdata = pred_grid)

save_plot(
  ggplot() +
    geom_vline(xintercept = 20, linetype = "dashed", color = "gray35") +
    geom_point(
      data = bins,
      aes(bin_mid, mean_nrr, color = factor(csm)),
      size = 2.8
    ) +
    geom_line(
      data = pred_grid,
      aes(arr_prior_k, fit, group = side),
      color = "#C0392B",
      linewidth = 1.1
    ) +
    scale_color_manual(
      values = c("0" = "#355C7D", "1" = "#D17A22"),
      labels = c("No CSM", "CSM assigned"),
      name = NULL
    ) +
    labs(
      title = "Net revenue retention around the $20k CSM threshold",
      subtitle = "Points: mean NRR in $1k bins. Lines: local linear fits within $5k of the cutoff.",
      x = "Prior-year ARR, $ thousands",
      y = "Next-year spend, % of prior"
    ) +
    theme_course(),
  "csm-rd-scatter.png"
)

save_plot(
  ggplot(rd_by_bw, aes(factor(bandwidth), estimate)) +
    geom_hline(yintercept = 0, color = "gray45") +
    geom_pointrange(
      aes(ymin = conf.low, ymax = conf.high),
      color = "#355C7D",
      linewidth = 1.0,
      size = 0.8
    ) +
    labs(
      title = "The estimated jump barely moves with the bandwidth",
      x = "Bandwidth: $k on each side of the cutoff",
      y = "Estimated jump in NRR, percentage points"
    ) +
    theme_course(),
  "csm-bandwidth.png",
  width = 7.8,
  height = 4.6
)

save_plot(
  ggplot(csm_accounts, aes(arr_prior_k)) +
    geom_vline(xintercept = 20, linetype = "dashed", color = "gray35") +
    geom_histogram(binwidth = 1, boundary = 0, fill = "#77A6B6", color = "white") +
    labs(
      title = "No bunching at the threshold",
      subtitle = "If sales teams pushed deals across $20k, accounts would pile up just past the line.",
      x = "Prior-year ARR, $ thousands",
      y = "Accounts"
    ) +
    theme_course(),
  "csm-density.png",
  width = 7.8,
  height = 4.4
)

message("Generated Module 11 data and assets in: ", module_dir)
