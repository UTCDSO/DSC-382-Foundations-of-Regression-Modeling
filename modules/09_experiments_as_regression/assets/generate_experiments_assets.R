library(tidyverse)
library(broom)

set.seed(382108)

script_arg <- commandArgs(trailingOnly = FALSE)
script_file <- sub("--file=", "", script_arg[grepl("^--file=", script_arg)][1])
if (is.na(script_file)) {
  script_file <- "modules/09_experiments_as_regression/assets/generate_experiments_assets.R"
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

# Case 1: late-payment reminder letters at a Texas utility ------------------

n_per_arm <- 600
arms <- c("control", "plain_language", "deadline", "autopay")
arm_means <- c(control = 31.0, plain_language = 28.8, deadline = 26.9, autopay = 30.1)

reminders <- tibble(
  account_id = sprintf("A%05d", sample(10000:99999, 4 * n_per_arm)),
  arm = sample(rep(arms, each = n_per_arm))
) |>
  mutate(
    amount_due = round(rlnorm(n(), meanlog = log(120), sdlog = 0.55), 2),
    prior_late_12mo = rpois(n(), 1.1),
    mu = arm_means[arm] * exp(0.05 * (prior_late_12mo - 1.1)),
    days_to_payment = pmax(1, round(rgamma(n(), shape = 12, scale = mu / 12)))
  ) |>
  select(-mu)

write_csv(reminders, file.path(module_dir, "reminder_letters.csv"))

reminders <- reminders |>
  mutate(arm = factor(arm, levels = arms))

# balance check across arms
balance <- reminders |>
  group_by(arm) |>
  summarize(
    accounts = n(),
    mean_amount_due = mean(amount_due),
    mean_prior_late = mean(prior_late_12mo),
    .groups = "drop"
  )
write_csv(balance, file.path(asset_dir, "reminder_balance.csv"))

# two-group model: control versus deadline only
two_group <- reminders |>
  filter(arm %in% c("control", "deadline")) |>
  mutate(deadline_letter = as.integer(arm == "deadline"))

two_group_model <- lm(days_to_payment ~ deadline_letter, data = two_group)
two_group_t <- t.test(
  days_to_payment ~ deadline_letter,
  data = two_group,
  var.equal = TRUE
)

two_group_summary <- tidy(two_group_model, conf.int = TRUE) |>
  mutate(model = "two_group") |>
  select(model, term, estimate, std.error, statistic, p.value, conf.low, conf.high)
write_csv(two_group_summary, file.path(asset_dir, "reminder_two_group_summary.csv"))

# four-arm model
four_arm_model <- lm(days_to_payment ~ arm, data = reminders)
four_arm_summary <- tidy(four_arm_model, conf.int = TRUE) |>
  mutate(model = "four_arm") |>
  select(model, term, estimate, std.error, statistic, p.value, conf.low, conf.high)
write_csv(four_arm_summary, file.path(asset_dir, "reminder_four_arm_summary.csv"))

anova_table <- tidy(anova(four_arm_model))
write_csv(anova_table, file.path(asset_dir, "reminder_anova.csv"))

# planned contrasts
b <- coef(four_arm_model)
V <- vcov(four_arm_model)

contrast_estimate <- function(L, label) {
  est <- sum(L * b)
  se <- sqrt(drop(t(L) %*% V %*% L))
  tibble(
    contrast = label,
    estimate = est,
    std.error = se,
    conf.low = est - 1.96 * se,
    conf.high = est + 1.96 * se
  )
}

L_redesign <- c(0, 1 / 3, 1 / 3, 1 / 3)
L_deadline_vs_plain <- c(0, -1, 1, 0)

contrasts_out <- bind_rows(
  contrast_estimate(L_redesign, "average_redesign_vs_control"),
  contrast_estimate(L_deadline_vs_plain, "deadline_vs_plain_language")
)
write_csv(contrasts_out, file.path(asset_dir, "reminder_contrast_summary.csv"))

# arm means with 95% CIs
arm_means_ci <- reminders |>
  group_by(arm) |>
  summarize(
    n = n(),
    mean_days = mean(days_to_payment),
    se = sd(days_to_payment) / sqrt(n()),
    .groups = "drop"
  ) |>
  mutate(
    conf.low = mean_days - 1.96 * se,
    conf.high = mean_days + 1.96 * se
  )
write_csv(arm_means_ci, file.path(asset_dir, "reminder_arm_means.csv"))

arm_labels <- c(
  control = "Control",
  plain_language = "Plain language",
  deadline = "Deadline salience",
  autopay = "Autopay pitch"
)

save_plot(
  ggplot(arm_means_ci, aes(x = factor(arm, levels = arms), y = mean_days)) +
    geom_pointrange(
      aes(ymin = conf.low, ymax = conf.high),
      color = "#355C7D",
      linewidth = 1.0,
      size = 0.9
    ) +
    geom_hline(
      yintercept = arm_means_ci$mean_days[arm_means_ci$arm == "control"],
      linetype = "dashed",
      color = "gray45"
    ) +
    scale_x_discrete(labels = arm_labels) +
    labs(
      title = "Mean days to payment by letter, with 95% intervals",
      x = NULL,
      y = "Days from letter to payment"
    ) +
    theme_course(),
  "reminder-arm-means.png"
)

save_plot(
  ggplot(reminders, aes(x = factor(arm, levels = arms), y = days_to_payment)) +
    geom_jitter(width = 0.18, alpha = 0.12, color = "#355C7D") +
    stat_summary(fun = mean, geom = "point", size = 3.4, color = "#C0392B") +
    scale_x_discrete(labels = arm_labels) +
    labs(
      title = "Days to payment for every account, by letter",
      subtitle = "Red points mark arm means. The overlap is why we need uncertainty intervals.",
      x = NULL,
      y = "Days from letter to payment"
    ) +
    theme_course(),
  "reminder-days-by-arm.png"
)

# Case 2: 2x2 promotion experiment at an online pet-supply retailer ---------

set.seed(382091)

n_per_cell <- 700
promo <- expand_grid(
  discount = c(0, 1),
  free_shipping = c(0, 1)
) |>
  slice(rep(1:4, each = n_per_cell)) |>
  mutate(
    customer_id = sprintf("C%05d", sample(10000:99999, 4 * n_per_cell)),
    mean_spend = 46 + 7 * discount + 6 * free_shipping - 4.5 * discount * free_shipping,
    spend_14d = round(pmax(0, rnorm(n(), mean_spend, 18)), 2)
  ) |>
  select(customer_id, discount, free_shipping, spend_14d) |>
  slice_sample(prop = 1)

write_csv(promo, file.path(module_dir, "promo_experiment.csv"))

cell_means <- promo |>
  group_by(discount, free_shipping) |>
  summarize(
    n = n(),
    mean_spend = mean(spend_14d),
    se = sd(spend_14d) / sqrt(n()),
    .groups = "drop"
  ) |>
  mutate(
    conf.low = mean_spend - 1.96 * se,
    conf.high = mean_spend + 1.96 * se
  )
write_csv(cell_means, file.path(asset_dir, "promo_cell_means.csv"))

main_effects_model <- lm(spend_14d ~ discount + free_shipping, data = promo)
interaction_model <- lm(spend_14d ~ discount * free_shipping, data = promo)

promo_summary <- bind_rows(
  tidy(main_effects_model, conf.int = TRUE) |> mutate(model = "main_effects"),
  tidy(interaction_model, conf.int = TRUE) |> mutate(model = "interaction")
) |>
  select(model, term, estimate, std.error, statistic, p.value, conf.low, conf.high)
write_csv(promo_summary, file.path(asset_dir, "promo_model_summary.csv"))

save_plot(
  ggplot(
    cell_means,
    aes(
      x = factor(discount, labels = c("No discount", "10% discount")),
      y = mean_spend,
      color = factor(free_shipping, labels = c("Standard shipping", "Free shipping")),
      group = free_shipping
    )
  ) +
    geom_line(linewidth = 1.1) +
    geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 0.8, linewidth = 1.0) +
    scale_color_manual(values = c("#355C7D", "#D17A22"), name = NULL) +
    labs(
      title = "Mean 14-day spend in the four promotion cells",
      x = NULL,
      y = "Mean spend per customer, dollars"
    ) +
    theme_course(),
  "promo-interaction.png"
)

# actual mean in the "both" cell versus the add-the-lifts forecast
b_int <- coef(interaction_model)
naive_both <- b_int[["(Intercept)"]] + b_int[["discount"]] + b_int[["free_shipping"]]
actual_both <- cell_means |>
  filter(discount == 1, free_shipping == 1) |>
  pull(mean_spend)
neither <- cell_means |>
  filter(discount == 0, free_shipping == 0) |>
  pull(mean_spend)

both_compare <- tibble(
  quantity = c("neither_mean", "naive_addition_forecast", "actual_both_mean"),
  value = c(neither, naive_both, actual_both)
)
write_csv(both_compare, file.path(asset_dir, "promo_both_cell_compare.csv"))

save_plot(
  tibble(
    label = factor(
      c("Neither offer", "Forecast if lifts added", "Actual: both offers"),
      levels = c("Neither offer", "Forecast if lifts added", "Actual: both offers")
    ),
    mean_spend = c(neither, naive_both, actual_both)
  ) |>
    ggplot(aes(label, mean_spend)) +
    geom_col(fill = c("#77A6B6", "#C9CBA3", "#C0392B"), width = 0.62) +
    geom_text(aes(label = sprintf("$%.2f", mean_spend)), vjust = -0.5, size = 4.6) +
    coord_cartesian(ylim = c(0, 65)) +
    labs(
      title = "Stacking both offers underdelivers the add-the-lifts forecast",
      x = NULL,
      y = "Mean 14-day spend, dollars"
    ) +
    theme_course(),
  "promo-additive-vs-actual.png",
  width = 7.8,
  height = 4.8
)

message("Generated Module 09 data and assets in: ", module_dir)

# Case 3: holiday bundle test with covariate adjustment ----------------------

set.seed(382092)

n_holiday <- 1000
propensity <- rlnorm(n_holiday, meanlog = log(55), sdlog = 0.55)

holiday <- tibble(
  customer_id = sprintf("H%05d", sample(10000:99999, n_holiday)),
  treat = rbinom(n_holiday, 1, 0.5),
  spend_prior_90d = round(pmax(0, propensity + rnorm(n_holiday, 0, 12)), 2)
) |>
  mutate(
    visits_promo = rpois(n_holiday, exp(0.15 + 0.011 * propensity + 0.32 * treat)),
    spend_promo = round(pmax(
      0,
      12 + 0.55 * propensity + 1.5 * treat + 4.0 * visits_promo + rnorm(n_holiday, 0, 15)
    ), 2)
  )

write_csv(
  holiday |> select(customer_id, treat, spend_prior_90d, visits_promo, spend_promo),
  file.path(module_dir, "holiday_promo.csv")
)

# balance: prior spend cannot differ by arm except by chance
holiday_balance <- tidy(lm(spend_prior_90d ~ treat, data = holiday), conf.int = TRUE)
write_csv(holiday_balance, file.path(asset_dir, "holiday_balance.csv"))

holiday_naive <- lm(spend_promo ~ treat, data = holiday)
holiday_adjusted <- lm(spend_promo ~ treat + spend_prior_90d, data = holiday)
holiday_posttreat <- lm(spend_promo ~ treat + spend_prior_90d + visits_promo, data = holiday)

holiday_summary <- bind_rows(
  tidy(holiday_naive, conf.int = TRUE) |> mutate(model = "naive"),
  tidy(holiday_adjusted, conf.int = TRUE) |> mutate(model = "prior_adjusted"),
  tidy(holiday_posttreat, conf.int = TRUE) |> mutate(model = "posttreat_adjusted")
) |>
  select(model, term, estimate, std.error, statistic, p.value, conf.low, conf.high)
write_csv(holiday_summary, file.path(asset_dir, "holiday_model_summary.csv"))

holiday_r <- cor(holiday$spend_prior_90d, holiday$spend_promo)
holiday_extras <- tibble(
  quantity = c("cor_prior_outcome", "r_squared", "equivalent_sample_multiplier"),
  value = c(holiday_r, holiday_r^2, 1 / (1 - holiday_r^2))
)
write_csv(holiday_extras, file.path(asset_dir, "holiday_precision.csv"))

save_plot(
  ggplot(holiday, aes(spend_prior_90d, spend_promo, color = factor(treat))) +
    geom_point(alpha = 0.35, size = 1.7) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 1.1) +
    scale_color_manual(
      values = c("0" = "#355C7D", "1" = "#D17A22"),
      labels = c("Control", "Bundle offer"),
      name = NULL
    ) +
    scale_x_continuous(labels = scales::label_dollar()) +
    scale_y_continuous(labels = scales::label_dollar()) +
    coord_cartesian(xlim = c(0, 220), ylim = c(0, 230)) +
    labs(
      title = "Prior spend predicts promo spend in both arms",
      subtitle = "The vertical gap between the lines is the treatment effect.",
      x = "Spend in the prior 90 days",
      y = "Spend during the promo window"
    ) +
    theme_course(),
  "holiday-prior-outcome.png"
)

compare_estimates <- holiday_summary |>
  filter(term == "treat") |>
  mutate(model = factor(
    model,
    levels = c("naive", "prior_adjusted", "posttreat_adjusted"),
    labels = c(
      "Treatment only",
      "+ prior spend\n(pre-treatment)",
      "+ promo-window visits\n(post-treatment)"
    )
  ))

save_plot(
  ggplot(compare_estimates, aes(model, estimate)) +
    geom_hline(yintercept = 0, color = "gray45") +
    geom_pointrange(
      aes(ymin = conf.low, ymax = conf.high,
          color = model == "+ promo-window visits\n(post-treatment)"),
      linewidth = 1.1,
      size = 0.9
    ) +
    scale_color_manual(
      values = c("FALSE" = "#355C7D", "TRUE" = "#C0392B"),
      guide = "none"
    ) +
    scale_y_continuous(labels = scales::label_dollar()) +
    labs(
      title = "Adjust for the past, never for the aftermath",
      subtitle = "The past sharpens the estimate. The aftermath destroys it.",
      x = NULL,
      y = "Estimated effect on promo-window spend"
    ) +
    theme_course(),
  "holiday-adjust-compare.png",
  width = 8.4,
  height = 4.9
)

# Case 4 in file order (Case 3 in the deck): abandoned-cart factorial --------

set.seed(382121)

# 3 x 2 x 2 design, effects built from main effects + two-way interactions;
# the three-way interaction is exactly zero by construction.
incentives <- c("none", "discount10", "free_shipping")
subjects <- c("plain", "urgency")
delays <- c("1h", "24h")

inc_main <- c(none = 0, discount10 = 5.0, free_shipping = 3.0)
urg_main <- -2.5                         # urgency effect when incentive = none
urg_inc_int <- c(none = 0, discount10 = 5.0, free_shipping = 4.5)
del_main <- -1.0                         # 24h effect when incentive = none
del_inc_int <- c(none = 0, discount10 = 3.5, free_shipping = 2.0)

n_per_cell <- 550
mean_order <- 45

cart <- expand_grid(
  incentive = incentives,
  subject = subjects,
  send_delay = delays
) |>
  slice(rep(1:12, each = n_per_cell)) |>
  mutate(
    mu = 8.5 +
      inc_main[incentive] +
      (urg_main + urg_inc_int[incentive]) * (subject == "urgency") +
      (del_main + del_inc_int[incentive]) * (send_delay == "24h"),
    p_recover = mu / mean_order,
    recovered = rbinom(n(), 1, p_recover),
    order_value = round(rlnorm(n(), meanlog = log(mean_order) - 0.45^2 / 2, sdlog = 0.45), 2),
    recovered_revenue_7d = if_else(recovered == 1, order_value, 0),
    cart_id = sprintf("K%05d", sample(10000:99999, n()))
  ) |>
  select(cart_id, incentive, subject, send_delay, recovered_revenue_7d) |>
  slice_sample(prop = 1)

write_csv(cart, file.path(module_dir, "cart_recovery.csv"))

cart <- cart |>
  mutate(
    incentive = factor(incentive, levels = incentives),
    subject = factor(subject, levels = subjects),
    send_delay = factor(send_delay, levels = delays)
  )

# raw cell means
cart_cells <- cart |>
  group_by(incentive, subject, send_delay) |>
  summarize(
    n = n(),
    mean_rev = mean(recovered_revenue_7d),
    se = sd(recovered_revenue_7d) / sqrt(n()),
    .groups = "drop"
  )
write_csv(cart_cells, file.path(asset_dir, "cart_cell_means.csv"))

# the model: main effects plus all two-way interactions, no three-way
two_way <- lm(
  recovered_revenue_7d ~ (incentive + subject + send_delay)^2,
  data = cart
)
write_csv(
  tidy(two_way, conf.int = TRUE) |> mutate(model = "two_way"),
  file.path(asset_dir, "cart_model_summary.csv")
)

# does the three-way interaction earn its keep?
full_fact <- lm(
  recovered_revenue_7d ~ incentive * subject * send_delay,
  data = cart
)
write_csv(
  tidy(anova(two_way, full_fact)),
  file.path(asset_dir, "cart_threeway_test.csv")
)

# contrasts as business answers: every question is L' beta ------------------

tt_cart <- delete.response(terms(two_way))
b_cart <- coef(two_way)
V_cart <- vcov(two_way)

# design-matrix row for one cell
cell_row <- function(inc, subj, del) {
  nd <- tibble(
    incentive = factor(inc, levels = incentives),
    subject = factor(subj, levels = subjects),
    send_delay = factor(del, levels = delays)
  )
  model.matrix(tt_cart, nd)
}

# marginal mean for an incentive = average of its four cell rows
marginal_row <- function(inc) {
  rows <- expand_grid(subject = subjects, send_delay = delays)
  Reduce(`+`, Map(\(s, d) cell_row(inc, s, d), rows$subject, rows$send_delay)) / 4
}

contrast_out <- function(L, label) {
  est <- drop(L %*% b_cart)
  se <- sqrt(drop(L %*% V_cart %*% t(L)))
  tibble(
    contrast = label,
    estimate = est,
    std.error = se,
    conf.low = est - 1.96 * se,
    conf.high = est + 1.96 * se
  )
}

# urgency effect within each incentive (pooling the two delays)
urgency_within <- function(inc) {
  (cell_row(inc, "urgency", "1h") + cell_row(inc, "urgency", "24h")) / 2 -
    (cell_row(inc, "plain", "1h") + cell_row(inc, "plain", "24h")) / 2
}

# 24h effect within each incentive (pooling the two subjects)
delay_within <- function(inc) {
  (cell_row(inc, "plain", "24h") + cell_row(inc, "urgency", "24h")) / 2 -
    (cell_row(inc, "plain", "1h") + cell_row(inc, "urgency", "1h")) / 2
}

cart_contrasts <- bind_rows(
  contrast_out(marginal_row("discount10") - marginal_row("none"), "incentive: discount vs none"),
  contrast_out(marginal_row("free_shipping") - marginal_row("none"), "incentive: free shipping vs none"),
  contrast_out(marginal_row("discount10") - marginal_row("free_shipping"), "incentive: discount vs free shipping"),
  contrast_out(urgency_within("none"), "urgency effect, no incentive"),
  contrast_out(urgency_within("discount10"), "urgency effect, with discount"),
  contrast_out(urgency_within("free_shipping"), "urgency effect, with free shipping"),
  contrast_out(delay_within("none"), "24h vs 1h, no incentive"),
  contrast_out(delay_within("discount10"), "24h vs 1h, with discount"),
  contrast_out(delay_within("free_shipping"), "24h vs 1h, with free shipping"),
  contrast_out(
    cell_row("discount10", "urgency", "24h") - cell_row("discount10", "urgency", "1h"),
    "best cell vs runner-up (24h vs 1h at discount+urgency)"
  )
)
write_csv(cart_contrasts, file.path(asset_dir, "cart_contrasts.csv"))

# marginal means by incentive
cart_marginals <- tibble(
  incentive = incentives,
  marginal_mean = map_dbl(incentives, \(i) drop(marginal_row(i) %*% b_cart))
)
write_csv(cart_marginals, file.path(asset_dir, "cart_marginal_means.csv"))

# fitted cell means from the two-way model, and the OFAT comparison
cart_cells <- cart_cells |>
  mutate(fitted_two_way = map_dbl(
    seq_len(n()),
    \(i) drop(cell_row(as.character(incentive[i]), as.character(subject[i]), as.character(send_delay[i])) %*% b_cart)
  ))
write_csv(cart_cells, file.path(asset_dir, "cart_cells_fitted.csv"))

ofat_config <- cart_cells |> filter(incentive == "discount10", subject == "plain", send_delay == "1h")
best_config <- cart_cells |> filter(incentive == "discount10", subject == "urgency", send_delay == "24h")
write_csv(
  tibble(
    config = c("ofat_landing_point", "factorial_best"),
    fitted_mean = c(ofat_config$fitted_two_way, best_config$fitted_two_way)
  ),
  file.path(asset_dir, "cart_ofat_vs_factorial.csv")
)

# plots ----------------------------------------------------------------------

incentive_labels <- c(
  none = "No incentive",
  discount10 = "10% discount",
  free_shipping = "Free shipping"
)
delay_labels <- c("1h" = "Sent after 1 hour", "24h" = "Sent after 24 hours")

save_plot(
  ggplot(
    cart_cells,
    aes(
      x = factor(incentive, labels = incentive_labels),
      y = mean_rev,
      color = factor(subject, labels = c("Plain subject", "Urgency subject")),
      group = subject
    )
  ) +
    geom_line(linewidth = 1.0) +
    geom_pointrange(
      aes(ymin = mean_rev - 1.96 * se, ymax = mean_rev + 1.96 * se),
      size = 0.7,
      linewidth = 0.9
    ) +
    facet_wrap(~ factor(send_delay, labels = delay_labels)) +
    scale_color_manual(values = c("#355C7D", "#D17A22"), name = NULL) +
    scale_y_continuous(labels = scales::label_dollar()) +
    labs(
      title = "The whole experiment in one picture",
      subtitle = "Mean recovered revenue per abandoned cart, all 12 cells, with 95% intervals.",
      x = NULL,
      y = "Recovered revenue per cart"
    ) +
    theme_course(base_size = 13.5),
  "cart-all-cells.png",
  width = 9.0,
  height = 5.2
)

save_plot(
  cart_contrasts |>
    filter(str_detect(contrast, "^urgency effect|^24h vs 1h")) |>
    mutate(
      question = if_else(str_detect(contrast, "^urgency"), "Does urgency help?", "Is it worth waiting 24 hours?"),
      label = case_when(
        str_detect(contrast, "no incentive") ~ "No\nincentive",
        str_detect(contrast, "discount") ~ "10%\ndiscount",
        str_detect(contrast, "free shipping") ~ "Free\nshipping"
      ),
      label = factor(label, levels = c("No\nincentive", "10%\ndiscount", "Free\nshipping"))
    ) |>
    ggplot(aes(label, estimate)) +
    geom_hline(yintercept = 0, color = "gray45") +
    geom_pointrange(
      aes(ymin = conf.low, ymax = conf.high, color = estimate > 0),
      linewidth = 1.0,
      size = 0.8
    ) +
    facet_wrap(~question) +
    scale_color_manual(values = c("TRUE" = "#355C7D", "FALSE" = "#C0392B"), guide = "none") +
    scale_y_continuous(labels = scales::label_dollar()) +
    labs(
      title = "Two business questions, six contrasts",
      subtitle = "Each answer depends on the incentive. That is what the interactions mean.",
      x = NULL,
      y = "Estimated effect on recovered revenue per cart"
    ) +
    theme_course(base_size = 13.5),
  "cart-contrast-answers.png",
  width = 9.2,
  height = 5.0
)

save_plot(
  ggplot(cart_cells, aes(mean_rev, fitted_two_way)) +
    geom_abline(slope = 1, intercept = 0, color = "gray55", linetype = "dashed") +
    geom_errorbarh(
      aes(xmin = mean_rev - 1.96 * se, xmax = mean_rev + 1.96 * se),
      height = 0,
      color = "#77A6B6"
    ) +
    geom_point(color = "#355C7D", size = 2.8) +
    scale_x_continuous(labels = scales::label_dollar()) +
    scale_y_continuous(labels = scales::label_dollar()) +
    labs(
      title = "Twelve raw cell means versus the two-way model",
      subtitle = "The 10-parameter model reproduces all 12 cells.",
      x = "Raw cell mean (with 95% interval)",
      y = "Fitted mean from the two-way model"
    ) +
    theme_course(),
  "cart-fitted-vs-raw.png",
  width = 7.6,
  height = 5.2
)
