library(tidyverse)
library(broom)
library(patchwork)

set.seed(6106)

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- sub("^--file=", "", script_arg[1] %||% "assets/generate_glm_assets.R")
out_dir <- dirname(normalizePath(script_path, mustWork = FALSE))
module_dir <- normalizePath(file.path(out_dir, ".."), mustWork = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

theme_classroom <- function(base_size = 15) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 3),
      plot.subtitle = element_text(size = base_size),
      plot.background = element_rect(fill = "#fcfbf7", color = NA),
      panel.background = element_rect(fill = "#fcfbf7", color = NA),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
}

save_plot <- function(plot, filename, width = 8.6, height = 5.2) {
  ggsave(file.path(out_dir, filename), plot, width = width, height = height, dpi = 220)
}

logit <- function(p) log(p / (1 - p))
inv_logit <- plogis

blue <- "#2b6cb0"
red <- "#c53030"
green <- "#2f855a"
gold <- "#b7791f"
purple <- "#6b46c1"
gray <- "#4a5568"

# Intro case: NBA home-team wins and Vegas point spread --------------------

n_games <- 2600

nba_games <- tibble(
  GameID = 1:n_games,
  Season = sample(2019:2025, n_games, replace = TRUE),
  HomeSpread = pmin(21, pmax(-20, rnorm(n_games, mean = 2.0, sd = 7.6)))
) |>
  mutate(
    # Positive means the home team is favored. Negative means the visitor is favored.
    eta = logit(0.535) + 0.185 * HomeSpread,
    HomeWinProb = inv_logit(eta),
    HomeWin = rbinom(n_games, 1, HomeWinProb),
    HomeWin = factor(if_else(HomeWin == 1, "Home win", "Visitor win"), levels = c("Visitor win", "Home win"))
  ) |>
  select(GameID, Season, HomeSpread, HomeWin)

write_csv(nba_games, file.path(module_dir, "nba_home_spreads.csv"))

nba_linear <- lm(as.integer(HomeWin == "Home win") ~ HomeSpread, data = nba_games)
nba_logit <- glm(HomeWin ~ HomeSpread, data = nba_games, family = binomial())

spread_grid <- tibble(HomeSpread = seq(-22, 22, length.out = 300)) |>
  mutate(
    linear_prob = predict(nba_linear, newdata = tibble(HomeSpread), type = "response"),
    logit_prob = predict(nba_logit, newdata = tibble(HomeSpread), type = "response")
  )

spread_bins <- nba_games |>
  mutate(spread_bin = cut(HomeSpread, breaks = seq(-22, 22, by = 4), include.lowest = TRUE)) |>
  group_by(spread_bin) |>
  summarize(
    mean_spread = mean(HomeSpread),
    home_win_rate = mean(HomeWin == "Home win"),
    n = n(),
    .groups = "drop"
  )

p_nba_lpm <- ggplot(nba_games, aes(HomeSpread, as.integer(HomeWin == "Home win"))) +
  geom_jitter(height = 0.035, width = 0, alpha = 0.08, color = gray, size = 1) +
  geom_line(data = spread_grid, aes(y = linear_prob), color = red, linewidth = 1.15) +
  geom_point(data = spread_bins, aes(x = mean_spread, y = home_win_rate, size = n), color = blue, alpha = 0.85) +
  scale_size_continuous(range = c(2.2, 5), guide = "none") +
  coord_cartesian(ylim = c(-0.06, 1.06), xlim = c(-21, 21)) +
  labs(
    title = "Home wins and Vegas point spread",
    subtitle = "Red line is a linear probability model; blue points are binned win rates",
    x = "Point spread in favor of home team",
    y = "Home-team win indicator or rate"
  ) +
  theme_classroom()
save_plot(p_nba_lpm, "nba-linear-probability.png")

p_nba_logit <- ggplot(spread_bins, aes(mean_spread, home_win_rate)) +
  geom_point(aes(size = n), color = blue, alpha = 0.85) +
  geom_line(data = spread_grid, aes(HomeSpread, logit_prob), color = green, linewidth = 1.2) +
  scale_size_continuous(range = c(2.5, 5.5), guide = "none") +
  coord_cartesian(ylim = c(0, 1), xlim = c(-20, 20)) +
  labs(
    title = "Logistic fit for home-team win probability",
    subtitle = "Points show binned win rates; the curve is the fitted logistic regression",
    x = "Point spread in favor of home team",
    y = "Probability of home win"
  ) +
  theme_classroom()
save_plot(p_nba_logit, "nba-logistic-fit.png")

nba_prob_points <- tibble(
  HomeSpread = c(-10, 0, 10)
) |>
  mutate(predicted_prob = predict(nba_logit, newdata = tibble(HomeSpread), type = "response"))

nba_summary <- tibble(
  quantity = c(
    "games",
    "home_win_rate",
    "linear_spread_slope",
    "logit_intercept",
    "logit_spread_slope",
    "spread_odds_ratio",
    "prob_home_spread_minus_10",
    "prob_home_spread_0",
    "prob_home_spread_10"
  ),
  value = c(
    nrow(nba_games),
    mean(nba_games$HomeWin == "Home win"),
    coef(nba_linear)[["HomeSpread"]],
    coef(nba_logit)[["(Intercept)"]],
    coef(nba_logit)[["HomeSpread"]],
    exp(coef(nba_logit)[["HomeSpread"]]),
    nba_prob_points$predicted_prob[1],
    nba_prob_points$predicted_prob[2],
    nba_prob_points$predicted_prob[3]
  )
)
write_csv(nba_summary, file.path(out_dir, "nba_logit_summary.csv"))

# Complex case: airline connections ----------------------------------------

n_connections <- 7200
hubs <- c("ATL", "CLT", "DFW", "DEN", "ORD", "IAH")

hub_info <- tibble(
  Hub = factor(hubs, levels = hubs),
  hub_name = c(
    "Atlanta", "Charlotte", "Dallas-Fort Worth", "Denver", "Chicago O'Hare", "Houston Intercontinental"
  ),
  base_logit = c(2.30, 2.10, 2.40, 2.00, 1.60, 1.75),
  terminal_change_penalty = c(-0.25, -0.40, -0.12, -0.50, -1.00, -0.80),
  terminal_change_rate = c(0.34, 0.22, 0.48, 0.30, 0.38, 0.36),
  delay_mean = c(8, 7, 9, 10, 13, 11)
)

airline_connections <- tibble(
  ConnectionID = 1:n_connections,
  Hub = factor(sample(hubs, n_connections, replace = TRUE, prob = c(0.22, 0.15, 0.20, 0.14, 0.16, 0.13)), levels = hubs)
) |>
  left_join(hub_info, by = "Hub") |>
  mutate(
    TerminalChange = rbinom(n(), 1, terminal_change_rate),
    ScheduledConnectionMinutes = round(pmin(155, pmax(28, rlnorm(n(), log(58), 0.36) + 7 * TerminalChange))),
    late_flag = rbinom(n(), 1, 0.36),
    ArrivalDelayMinutes = if_else(
      late_flag == 1,
      round(pmin(105, rgamma(n(), shape = 2.0, scale = delay_mean / 2) + rnorm(n(), 0, 5))),
      round(rnorm(n(), mean = -5, sd = 9))
    ),
    ArrivalDelayMinutes = pmin(105, pmax(-28, ArrivalDelayMinutes)),
    buffer_after_arrival = ScheduledConnectionMinutes - ArrivalDelayMinutes,
    eta = 0.085 * (ScheduledConnectionMinutes - 50) -
      0.100 * ArrivalDelayMinutes +
      base_logit +
      TerminalChange * terminal_change_penalty +
      0.010 * TerminalChange * (ScheduledConnectionMinutes - 55),
    MadeConnectionProb = inv_logit(eta),
    MadeConnection = rbinom(n(), 1, MadeConnectionProb),
    MadeConnection = factor(if_else(MadeConnection == 1, "Made connection", "Missed connection"), levels = c("Missed connection", "Made connection")),
    TerminalChange = factor(if_else(TerminalChange == 1, "Terminal change", "Same terminal"), levels = c("Same terminal", "Terminal change"))
  ) |>
  select(
    ConnectionID, Hub, ScheduledConnectionMinutes, ArrivalDelayMinutes,
    TerminalChange, MadeConnection
  )

write_csv(airline_connections, file.path(module_dir, "airline_connections.csv"))

airline_simple <- glm(
  MadeConnection ~ ArrivalDelayMinutes,
  data = airline_connections,
  family = binomial()
)

airline_main <- glm(
  MadeConnection ~ ScheduledConnectionMinutes + ArrivalDelayMinutes + Hub + TerminalChange,
  data = airline_connections,
  family = binomial()
)

airline_interaction <- glm(
  MadeConnection ~ ScheduledConnectionMinutes + ArrivalDelayMinutes + Hub * TerminalChange,
  data = airline_connections,
  family = binomial()
)

airline_sample <- airline_connections |>
  slice_sample(n = 8) |>
  mutate(
    ScheduledConnectionMinutes = as.integer(ScheduledConnectionMinutes),
    ArrivalDelayMinutes = as.integer(ArrivalDelayMinutes)
  )
write_csv(airline_sample, file.path(out_dir, "airline_sample_rows.csv"))

airline_hub_summary <- airline_connections |>
  group_by(Hub, TerminalChange) |>
  summarize(
    connections = n(),
    make_rate = mean(MadeConnection == "Made connection"),
    median_scheduled = median(ScheduledConnectionMinutes),
    median_arrival_delay = median(ArrivalDelayMinutes),
    .groups = "drop"
  )
write_csv(airline_hub_summary, file.path(out_dir, "airline_hub_summary.csv"))

p_airline_delay <- airline_connections |>
  mutate(delay_bin = cut(ArrivalDelayMinutes, breaks = seq(-30, 110, by = 10), include.lowest = TRUE)) |>
  group_by(delay_bin) |>
  summarize(
    mean_delay = mean(ArrivalDelayMinutes),
    make_rate = mean(MadeConnection == "Made connection"),
    n = n(),
    .groups = "drop"
  ) |>
  ggplot(aes(mean_delay, make_rate)) +
  geom_point(aes(size = n), color = blue, alpha = 0.85) +
  geom_smooth(
    data = airline_connections,
    aes(ArrivalDelayMinutes, as.integer(MadeConnection == "Made connection")),
    method = "glm",
    method.args = list(family = binomial()),
    se = FALSE,
    color = red,
    linewidth = 1.15,
    inherit.aes = FALSE
  ) +
  scale_size_continuous(range = c(2.2, 5.2), guide = "none") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    title = "Connection success by arrival delay",
    subtitle = "Points show binned connection rates; the curve uses arrival delay only",
    x = "Arrival delay, minutes",
    y = "Connection success rate"
  ) +
  theme_classroom()
save_plot(p_airline_delay, "airline-delay-only.png")

p_airline_buffer <- airline_connections |>
  mutate(buffer_after_arrival = ScheduledConnectionMinutes - ArrivalDelayMinutes) |>
  mutate(buffer_bin = cut(buffer_after_arrival, breaks = seq(-70, 150, by = 12), include.lowest = TRUE)) |>
  group_by(buffer_bin, TerminalChange) |>
  summarize(
    mean_buffer = mean(buffer_after_arrival),
    make_rate = mean(MadeConnection == "Made connection"),
    n = n(),
    .groups = "drop"
  ) |>
  ggplot(aes(mean_buffer, make_rate, color = TerminalChange)) +
  geom_point(aes(size = n), alpha = 0.85) +
  scale_color_manual(values = c("Same terminal" = blue, "Terminal change" = red)) +
  scale_size_continuous(range = c(2, 5), guide = "none") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    title = "Connection success by time remaining after arrival",
    subtitle = "Binned rates shown separately by terminal-change status",
    x = "Scheduled minutes minus arrival delay",
    y = "Connection success rate",
    color = NULL
  ) +
  theme_classroom()
save_plot(p_airline_buffer, "airline-buffer-terminal.png")

pred_grid <- expand_grid(
  Hub = factor(hubs, levels = hubs),
  TerminalChange = factor(c("Same terminal", "Terminal change"), levels = levels(airline_connections$TerminalChange)),
  ScheduledConnectionMinutes = seq(30, 125, by = 1),
  ArrivalDelayMinutes = 12
)

pred_grid <- pred_grid |>
  mutate(fitted_prob = predict(airline_interaction, newdata = pred_grid, type = "response"))

p_airline_hub_interaction <- ggplot(pred_grid, aes(ScheduledConnectionMinutes, fitted_prob, color = TerminalChange)) +
  geom_line(linewidth = 1.15) +
  facet_wrap(~ Hub, ncol = 3) +
  scale_color_manual(values = c("Same terminal" = blue, "Terminal change" = red)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    title = "Fitted connection probability by hub",
    subtitle = "Predictions hold arrival delay fixed at 12 minutes",
    x = "Scheduled connection time, minutes",
    y = "Predicted probability of making connection",
    color = NULL
  ) +
  theme_classroom(13)
save_plot(p_airline_hub_interaction, "airline-hub-terminal-fits.png", width = 9.2, height = 6.1)

terminal_or_by_hub <- tibble(
  Hub = hubs,
  terminal_change_odds_ratio = map_dbl(hubs, function(hub) {
    base_row <- tibble(
      Hub = factor(hub, levels = hubs),
      ScheduledConnectionMinutes = 60,
      ArrivalDelayMinutes = 12,
      TerminalChange = factor("Same terminal", levels = levels(airline_connections$TerminalChange))
    )
    change_row <- mutate(base_row, TerminalChange = factor("Terminal change", levels = levels(airline_connections$TerminalChange)))
    odds_same <- predict(airline_interaction, newdata = base_row, type = "link")
    odds_change <- predict(airline_interaction, newdata = change_row, type = "link")
    exp(odds_change - odds_same)
  })
)

p_airline_terminal_or <- ggplot(terminal_or_by_hub, aes(fct_reorder(Hub, terminal_change_odds_ratio), terminal_change_odds_ratio)) +
  geom_col(fill = purple, width = 0.72) +
  geom_hline(yintercept = 1, color = gray, linetype = "dashed") +
  coord_flip() +
  labs(
    title = "Terminal-change odds ratios by hub",
    subtitle = "Odds ratios are computed at a 60-minute scheduled connection and 12-minute arrival delay",
    x = "Hub",
    y = "Odds ratio for terminal change"
  ) +
  theme_classroom()
save_plot(p_airline_terminal_or, "airline-terminal-odds-ratios.png")

airline_summary <- tibble(
  quantity = c(
    "connections",
    "overall_make_rate",
    "simple_delay_slope",
    "simple_delay_odds_ratio_10min",
    "main_scheduled_slope",
    "main_scheduled_odds_ratio_10min",
    "main_delay_slope",
    "main_delay_odds_ratio_10min",
    "main_terminal_change_odds_ratio",
    "interaction_dfw_terminal_or",
    "interaction_ord_terminal_or",
    "interaction_iah_terminal_or"
  ),
  value = c(
    nrow(airline_connections),
    mean(airline_connections$MadeConnection == "Made connection"),
    coef(airline_simple)[["ArrivalDelayMinutes"]],
    exp(10 * coef(airline_simple)[["ArrivalDelayMinutes"]]),
    coef(airline_main)[["ScheduledConnectionMinutes"]],
    exp(10 * coef(airline_main)[["ScheduledConnectionMinutes"]]),
    coef(airline_main)[["ArrivalDelayMinutes"]],
    exp(10 * coef(airline_main)[["ArrivalDelayMinutes"]]),
    exp(coef(airline_main)[["TerminalChangeTerminal change"]]),
    terminal_or_by_hub$terminal_change_odds_ratio[terminal_or_by_hub$Hub == "DFW"],
    terminal_or_by_hub$terminal_change_odds_ratio[terminal_or_by_hub$Hub == "ORD"],
    terminal_or_by_hub$terminal_change_odds_ratio[terminal_or_by_hub$Hub == "IAH"]
  )
)
write_csv(airline_summary, file.path(out_dir, "airline_logit_summary.csv"))

write_csv(
  tidy(airline_interaction, exponentiate = TRUE, conf.int = FALSE),
  file.path(out_dir, "airline_interaction_odds_ratios.csv")
)

# Count case 1: dock-station bike rentals -----------------------------------

set.seed(6206)
library(MASS, exclude = "select")

n_days <- 365
bike_days <- tibble(
  Date = as.Date("2024-01-01") + 0:(n_days - 1)
) |>
  mutate(
    DayIndex = as.integer(Date - min(Date)) + 1,
    TempHighF = round(pmin(102, pmax(38, 66 - 24 * cos(2 * pi * (DayIndex - 20) / 365) + rnorm(n_days, 0, 7)))),
    Weekend = as.integer(wday(Date) %in% c(1, 7)),
    mu = exp(log(6.5) + 0.040 * (TempHighF - 65) + 0.42 * Weekend),
    Rentals = rpois(n_days, mu)
  ) |>
  dplyr::select(Date, TempHighF, Weekend, Rentals)

write_csv(bike_days, file.path(module_dir, "dock_rentals.csv"))

bike_lm <- lm(Rentals ~ TempHighF + Weekend, data = bike_days)
bike_pois <- glm(Rentals ~ TempHighF + Weekend, data = bike_days, family = poisson())

pred_90_sat <- predict(
  bike_pois,
  newdata = tibble(TempHighF = 90, Weekend = 1),
  type = "response"
)

bike_summary <- tibble(
  quantity = c(
    "days",
    "zero_rental_days",
    "lm_temp_slope",
    "lm_weekend",
    "pois_intercept",
    "pois_temp_slope",
    "pois_weekend",
    "temp_rate_ratio_1F",
    "temp_rate_ratio_10F",
    "weekend_rate_ratio",
    "pred_rentals_90F_saturday"
  ),
  value = c(
    nrow(bike_days),
    sum(bike_days$Rentals == 0),
    coef(bike_lm)[["TempHighF"]],
    coef(bike_lm)[["Weekend"]],
    coef(bike_pois)[["(Intercept)"]],
    coef(bike_pois)[["TempHighF"]],
    coef(bike_pois)[["Weekend"]],
    exp(coef(bike_pois)[["TempHighF"]]),
    exp(10 * coef(bike_pois)[["TempHighF"]]),
    exp(coef(bike_pois)[["Weekend"]]),
    pred_90_sat
  )
)
write_csv(bike_summary, file.path(out_dir, "bike_poisson_summary.csv"))
write_csv(
  tidy(bike_pois, conf.int = TRUE),
  file.path(out_dir, "bike_poisson_tidy.csv")
)

p_bike_lm <- ggplot(bike_days, aes(TempHighF, Rentals, color = factor(Weekend))) +
  geom_jitter(width = 0.4, height = 0, alpha = 0.6, size = 1.9) +
  geom_line(
    data = tibble(TempHighF = 40:98, Weekend = 0) |>
      mutate(fit = predict(bike_lm, newdata = pick(everything()))),
    aes(TempHighF, fit),
    color = red, linewidth = 1.1, inherit.aes = FALSE
  ) +
  scale_color_manual(
    values = c("0" = blue, "1" = gold),
    labels = c("Weekday", "Weekend"),
    name = NULL
  ) +
  labs(
    title = "Daily rentals versus temperature",
    subtitle = "The red line is an ordinary regression fit for weekdays",
    x = "Daily high temperature, F",
    y = "Rentals started at the dock"
  ) +
  theme_classroom()
save_plot(p_bike_lm, "bike-rentals-lm.png")

bike_grid <- expand_grid(TempHighF = 40:98, Weekend = c(0, 1)) |>
  mutate(fit = predict(bike_pois, newdata = pick(everything()), type = "response"))

p_bike_pois <- ggplot(bike_days, aes(TempHighF, Rentals, color = factor(Weekend))) +
  geom_jitter(width = 0.4, height = 0, alpha = 0.55, size = 1.9) +
  geom_line(
    data = bike_grid,
    aes(TempHighF, fit, color = factor(Weekend)),
    linewidth = 1.2
  ) +
  scale_color_manual(
    values = c("0" = blue, "1" = gold),
    labels = c("Weekday", "Weekend"),
    name = NULL
  ) +
  labs(
    title = "Poisson regression: exponential curves, one per day type",
    subtitle = "The gap between the curves grows with the level. That is the multiplicative story.",
    x = "Daily high temperature, F",
    y = "Rentals started at the dock"
  ) +
  theme_classroom()
save_plot(p_bike_pois, "bike-rentals-poisson.png")

bike_mv <- bike_days |>
  mutate(
    fitted_mu = fitted(bike_pois),
    bin = ntile(fitted_mu, 10)
  ) |>
  group_by(bin) |>
  summarize(
    mean_rentals = mean(fitted_mu),
    var_rentals = mean((Rentals - fitted_mu)^2),
    n = n(),
    .groups = "drop"
  )
write_csv(bike_mv, file.path(out_dir, "bike_mean_variance.csv"))

p_bike_mv <- ggplot(bike_mv, aes(mean_rentals, var_rentals)) +
  geom_abline(slope = 1, intercept = 0, color = gray, linetype = "dashed", linewidth = 0.9) +
  geom_point(color = blue, size = 3.4, alpha = 0.9) +
  annotate("text", x = 19, y = 15.5, label = "variance = mean", color = gray, size = 4.4, angle = 24) +
  labs(
    title = "The Poisson promise holds for the dock data",
    subtitle = "Days grouped into ten bins by fitted mean; variance around the fit tracks the mean",
    x = "Mean rentals within bin",
    y = "Variance of rentals within bin"
  ) +
  theme_classroom()
save_plot(p_bike_mv, "bike-mean-variance.png")

# The Poisson distribution in pictures ---------------------------------------

pois_pmf <- expand_grid(mu = c(2, 8, 25), y = 0:45) |>
  mutate(
    prob = dpois(y, mu),
    panel = factor(
      paste0("mean rate = ", mu),
      levels = paste0("mean rate = ", c(2, 8, 25))
    )
  ) |>
  filter(prob > 1e-5)

p_pois_pmf <- ggplot(pois_pmf, aes(y, prob)) +
  geom_col(fill = blue, width = 0.85) +
  facet_wrap(~panel, scales = "free_y") +
  labs(
    title = "Three Poisson distributions",
    subtitle = "One parameter sets everything: the mean rate of event arrivals",
    x = "Number of events",
    y = "Probability"
  ) +
  theme_classroom(13.5)
save_plot(p_pois_pmf, "poisson-pmfs.png", width = 9.2, height = 4.2)

# Count case 2: wind-turbine maintenance, offsets, and overdispersion --------

set.seed(6207)

n_turbines <- 210
n_months <- 6

turbines <- tibble(
  TurbineID = sprintf("WT%03d", 1:n_turbines),
  AgeYears = sample(2:15, n_turbines, replace = TRUE, prob = c(2, 3, 4, 5, 5, 5, 5, 5, 4, 4, 3, 3, 2, 2)),
  Site = sample(c("plains", "mesa"), n_turbines, replace = TRUE, prob = c(0.6, 0.4))
)

fleet <- expand_grid(TurbineID = turbines$TurbineID, Month = 1:n_months) |>
  left_join(turbines, by = "TurbineID") |>
  mutate(
    RunHours = round(pmax(
      120,
      if_else(Site == "plains", 665, 400) * (1 - 0.012 * AgeYears) +
        rnorm(n(), 0, if_else(Site == "plains", 45, 40))
    )),
    rate_per_1000h = exp(log(1.1) + 0.085 * AgeYears + 0.50 * (Site == "mesa")),
    mu = rate_per_1000h * RunHours / 1000,
    # mean-based negative binomial: E = mu, Var = mu + mu^2 / theta
    MaintEvents = rnbinom(n(), mu = mu, size = 1.2)
  ) |>
  dplyr::select(TurbineID, Month, AgeYears, Site, RunHours, MaintEvents)

write_csv(fleet, file.path(module_dir, "turbine_maintenance.csv"))

fleet <- fleet |>
  mutate(Site = factor(Site, levels = c("plains", "mesa")))

# naive count model: no exposure
turbine_naive <- glm(MaintEvents ~ AgeYears + Site, data = fleet, family = poisson())

# offset model: model the rate per 1,000 operating hours
turbine_offset <- glm(
  MaintEvents ~ AgeYears + Site + offset(log(RunHours / 1000)),
  data = fleet,
  family = poisson()
)

# overdispersion diagnostic: Pearson dispersion statistic
pearson_dispersion <- sum(residuals(turbine_offset, type = "pearson")^2) /
  df.residual(turbine_offset)

# negative binomial with the same mean model
turbine_nb <- glm.nb(
  MaintEvents ~ AgeYears + Site + offset(log(RunHours / 1000)),
  data = fleet
)

turbine_models <- bind_rows(
  tidy(turbine_naive, conf.int = TRUE) |> mutate(model = "naive_poisson"),
  tidy(turbine_offset, conf.int = TRUE) |> mutate(model = "offset_poisson"),
  tidy(turbine_nb, conf.int = TRUE) |> mutate(model = "negative_binomial")
) |>
  mutate(
    rate_ratio = exp(estimate),
    rr_low = exp(conf.low),
    rr_high = exp(conf.high)
  ) |>
  dplyr::select(model, term, estimate, std.error, rate_ratio, rr_low, rr_high, p.value)
write_csv(turbine_models, file.path(out_dir, "turbine_model_summary.csv"))

turbine_dispersion <- tibble(
  quantity = c(
    "turbine_months",
    "mean_events",
    "var_events",
    "pearson_dispersion_offset_poisson",
    "nb_theta",
    "nb_theta_se"
  ),
  value = c(
    nrow(fleet),
    mean(fleet$MaintEvents),
    var(fleet$MaintEvents),
    pearson_dispersion,
    turbine_nb$theta,
    turbine_nb$SE.theta
  )
)
write_csv(turbine_dispersion, file.path(out_dir, "turbine_dispersion.csv"))

turbine_sample <- fleet |> slice_sample(n = 8)
write_csv(turbine_sample, file.path(out_dir, "turbine_sample_rows.csv"))

# operating hours differ across exactly the comparisons we care about
p_turbine_hours <- ggplot(fleet, aes(AgeYears, RunHours, color = Site)) +
  geom_jitter(width = 0.25, alpha = 0.3, size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.1) +
  scale_color_manual(
    values = c("plains" = blue, "mesa" = red),
    labels = c("Plains site", "Mesa site"),
    name = NULL
  ) +
  labs(
    title = "Mesa turbines run far fewer hours, and older turbines run less",
    subtitle = "Exposure differs across exactly the comparisons we care about",
    x = "Turbine age, years",
    y = "Operating hours in the month"
  ) +
  theme_classroom()
save_plot(p_turbine_hours, "turbine-hours-confound.png")

# mean-variance diagnostic
turbine_mv <- fleet |>
  mutate(
    fitted_mu = fitted(turbine_offset),
    bin = ntile(fitted_mu, 12)
  ) |>
  group_by(bin) |>
  summarize(
    mean_ev = mean(fitted_mu),
    var_ev = mean((MaintEvents - fitted_mu)^2),
    n = n(),
    .groups = "drop"
  )
write_csv(turbine_mv, file.path(out_dir, "turbine_mean_variance.csv"))

theta_hat <- turbine_nb$theta
mv_grid <- tibble(m = seq(0, max(turbine_mv$mean_ev) * 1.05, length.out = 200)) |>
  mutate(nb_var = m + m^2 / theta_hat)

p_turbine_mv <- ggplot(turbine_mv, aes(mean_ev, var_ev)) +
  geom_abline(slope = 1, intercept = 0, color = gray, linetype = "dashed", linewidth = 0.9) +
  geom_line(data = mv_grid, aes(m, nb_var), color = green, linewidth = 1.1) +
  geom_point(color = blue, size = 3.4, alpha = 0.9) +
  annotate("text", x = 1.55, y = 1.14, label = "variance = mean (Poisson)", color = gray, size = 4.0, angle = 14) +
  annotate("text", x = 1.42, y = 4.0, label = "variance = mean + mean^2 / theta", color = green, size = 4.0, angle = 34) +
  labs(
    title = "Maintenance counts are more variable than Poisson allows",
    subtitle = "Turbine-months grouped into bins by fitted mean; the negative binomial curve fits",
    x = "Mean events within bin",
    y = "Variance of events within bin"
  ) +
  theme_classroom()
save_plot(p_turbine_mv, "turbine-mean-variance.png")

# fitted event rate by age and site, from the NB model
rate_grid <- expand_grid(
  AgeYears = seq(2, 15, by = 0.1),
  Site = factor(c("plains", "mesa"), levels = c("plains", "mesa"))
) |>
  mutate(
    RunHours = 1000,
    fitted_rate = predict(turbine_nb, newdata = pick(everything()), type = "response")
  )

obs_rates <- fleet |>
  mutate(age_bin = cut(AgeYears, breaks = c(1, 4, 6, 8, 10, 12, 15))) |>
  group_by(age_bin, Site) |>
  summarize(
    mean_age = mean(AgeYears),
    rate = sum(MaintEvents) / sum(RunHours / 1000),
    n = n(),
    .groups = "drop"
  )

p_turbine_rate <- ggplot() +
  geom_point(
    data = obs_rates,
    aes(mean_age, rate, color = Site, size = n),
    alpha = 0.85
  ) +
  geom_line(
    data = rate_grid,
    aes(AgeYears, fitted_rate, color = Site),
    linewidth = 1.15
  ) +
  scale_color_manual(
    values = c("plains" = blue, "mesa" = red),
    labels = c("Plains site", "Mesa site"),
    name = NULL
  ) +
  scale_size_continuous(range = c(2.4, 5), guide = "none") +
  labs(
    title = "Maintenance events per 1,000 operating hours, by age and site",
    subtitle = "Points are observed rates in age bins; curves are the fitted negative binomial model",
    x = "Turbine age, years",
    y = "Events per 1,000 operating hours"
  ) +
  theme_classroom()
save_plot(p_turbine_rate, "turbine-age-rate.png")
