library(tidyverse)
library(broom)

data_dir <- if (file.exists("nba_home_spreads.csv")) {
  "."
} else {
  file.path("modules", "06_generalized_linear_models")
}

# -------------------------------------------------------------------------
# Case 1: NBA home wins from Vegas point spread

nba_games <- read_csv(file.path(data_dir, "nba_home_spreads.csv"), show_col_types = FALSE) |>
  mutate(HomeWin = factor(HomeWin, levels = c("Visitor win", "Home win")))

nba_games |>
  summarize(
    games = n(),
    home_win_rate = mean(HomeWin == "Home win"),
    min_home_spread = min(HomeSpread),
    median_home_spread = median(HomeSpread),
    max_home_spread = max(HomeSpread)
  )

# A linear probability model is a useful foil, but it is not the final model.
nba_lpm <- lm(as.integer(HomeWin == "Home win") ~ HomeSpread, data = nba_games)
tidy(nba_lpm)

nba_logit <- glm(HomeWin ~ HomeSpread, data = nba_games, family = binomial())
tidy(nba_logit)

# Exponentiated coefficients are odds ratios.
tidy(nba_logit, exponentiate = TRUE)

tibble(HomeSpread = c(-10, 0, 10)) |>
  mutate(predicted_home_win_prob = predict(nba_logit, newdata = tibble(HomeSpread), type = "response"))

# -------------------------------------------------------------------------
# Case 2: airline connection success

connections <- read_csv(file.path(data_dir, "airline_connections.csv"), show_col_types = FALSE) |>
  mutate(
    MadeConnection = factor(MadeConnection, levels = c("Missed connection", "Made connection")),
    Hub = factor(Hub, levels = c("ATL", "CLT", "DFW", "DEN", "ORD", "IAH")),
    TerminalChange = factor(TerminalChange, levels = c("Same terminal", "Terminal change"))
  )

connections |>
  summarize(
    connections = n(),
    make_rate = mean(MadeConnection == "Made connection"),
    median_scheduled_connection = median(ScheduledConnectionMinutes),
    median_arrival_delay = median(ArrivalDelayMinutes),
    terminal_change_rate = mean(TerminalChange == "Terminal change")
  )

connections |>
  count(Hub, TerminalChange) |>
  group_by(Hub) |>
  mutate(share = n / sum(n)) |>
  ungroup()

delay_only <- glm(
  MadeConnection ~ ArrivalDelayMinutes,
  data = connections,
  family = binomial()
)
tidy(delay_only)
tidy(delay_only, exponentiate = TRUE)

connection_main <- glm(
  MadeConnection ~ ScheduledConnectionMinutes + ArrivalDelayMinutes +
    Hub + TerminalChange,
  data = connections,
  family = binomial()
)
tidy(connection_main)

# Ten-minute odds ratios for the two numerical predictors.
coef(connection_main)[c("ScheduledConnectionMinutes", "ArrivalDelayMinutes")] |>
  (\(x) exp(10 * x))()

connection_interaction <- glm(
  MadeConnection ~ ScheduledConnectionMinutes + ArrivalDelayMinutes +
    Hub * TerminalChange,
  data = connections,
  family = binomial()
)
tidy(connection_interaction)

# Predicted probabilities for same-terminal versus terminal-change connections.
prediction_grid <- expand_grid(
  Hub = factor(c("ATL", "CLT", "DFW", "DEN", "ORD", "IAH"), levels = levels(connections$Hub)),
  TerminalChange = factor(c("Same terminal", "Terminal change"), levels = levels(connections$TerminalChange)),
  ScheduledConnectionMinutes = c(45, 60, 75, 90),
  ArrivalDelayMinutes = 12
)

prediction_grid <- prediction_grid |>
  mutate(predicted_prob = predict(connection_interaction, newdata = prediction_grid, type = "response"))

prediction_grid

# -------------------------------------------------------------------------
# Case 3: daily rentals at a bike-share dock (Poisson regression)

bike_days <- read_csv(file.path(data_dir, "dock_rentals.csv"), show_col_types = FALSE)

bike_days |>
  summarize(
    days = n(),
    zero_rental_days = sum(Rentals == 0),
    mean_rentals = mean(Rentals),
    max_rentals = max(Rentals)
  )

# The straight-line try promises the same additive change everywhere,
# and the scatter visibly grows with the level.
bike_lm <- lm(Rentals ~ TempHighF + Weekend, data = bike_days)
tidy(bike_lm)

ggplot(bike_days, aes(TempHighF, Rentals, color = factor(Weekend))) +
  geom_jitter(width = 0.4, height = 0, alpha = 0.5) +
  labs(
    x = "Daily high temperature, F",
    y = "Rentals started at the dock",
    color = "Weekend",
    title = "Daily dock rentals versus temperature"
  ) +
  theme_minimal(base_size = 13)

# Note: log(Rentals) is not an option -- some days have zero rentals.
# Poisson regression logs the MEAN instead of the data.
bike_pois <- glm(Rentals ~ TempHighF + Weekend, data = bike_days, family = poisson())
tidy(bike_pois)

# Exponentiated coefficients are rate ratios.
tidy(bike_pois, exponentiate = TRUE, conf.int = TRUE)

# Expected rentals on a 90-degree Saturday, plus a staffing quantile:
mu_hot_saturday <- predict(
  bike_pois,
  newdata = tibble(TempHighF = 90, Weekend = 1),
  type = "response"
)
c(expected = mu_hot_saturday, stock_for_96pct = qpois(0.96, mu_hot_saturday))

# Check the Poisson promise: variance = mean.
# The Pearson dispersion statistic should sit near 1.
sum(residuals(bike_pois, type = "pearson")^2) / df.residual(bike_pois)

# -------------------------------------------------------------------------
# Case 4: wind-turbine maintenance (offsets and overdispersion)

fleet <- read_csv(file.path(data_dir, "turbine_maintenance.csv"), show_col_types = FALSE) |>
  mutate(Site = factor(Site, levels = c("plains", "mesa")))

fleet |>
  group_by(Site) |>
  summarize(
    turbine_months = n(),
    mean_hours = mean(RunHours),
    mean_events = mean(MaintEvents)
  )

# The naive model answers "events per month" -- and mesa turbines run
# about 40% fewer hours, so the mesa looks harmless.
turbine_naive <- glm(MaintEvents ~ AgeYears + Site, data = fleet, family = poisson())
tidy(turbine_naive, exponentiate = TRUE, conf.int = TRUE)

# The reliability question is about events per OPERATING HOUR.
# The exposure enters as an offset: a term whose coefficient is pinned at 1.
turbine_offset <- glm(
  MaintEvents ~ AgeYears + Site + offset(log(RunHours / 1000)),
  data = fleet,
  family = poisson()
)
tidy(turbine_offset, exponentiate = TRUE, conf.int = TRUE)

# Diagnose overdispersion: the dispersion statistic is far above 1.
sum(residuals(turbine_offset, type = "pearson")^2) / df.residual(turbine_offset)

# The negative binomial keeps the same mean model and offset, but the
# variance is mu + mu^2 / theta, so the standard errors become honest.
library(MASS)

turbine_nb <- glm.nb(
  MaintEvents ~ AgeYears + Site + offset(log(RunHours / 1000)),
  data = fleet
)
tidy(turbine_nb, exponentiate = TRUE, conf.int = TRUE)
turbine_nb$theta

# Compare the standard errors: nearly identical estimates, wider intervals.
bind_rows(
  tidy(turbine_offset) |> mutate(model = "poisson"),
  tidy(turbine_nb) |> mutate(model = "negative binomial")
) |>
  filter(term != "(Intercept)") |>
  dplyr::select(model, term, estimate, std.error)

# The answer in fleet terms: per-hour event rate by age and site.
rate_grid <- expand_grid(
  AgeYears = c(3, 8, 12),
  Site = factor(c("plains", "mesa"), levels = levels(fleet$Site))
) |>
  mutate(RunHours = 1000)

rate_grid |>
  mutate(events_per_1000_hours = predict(turbine_nb, newdata = rate_grid, type = "response"))
