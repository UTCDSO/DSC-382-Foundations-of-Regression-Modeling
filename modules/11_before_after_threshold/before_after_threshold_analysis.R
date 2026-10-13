library(tidyverse)
library(broom)

if (!interactive()) {
  pdf(NULL)
}

module_dir <- if (file.exists("courier_pay.csv")) {
  "."
} else {
  file.path("modules", "11_before_after_threshold")
}

# Case 1: the courier pay guarantee (difference-in-differences) --------------

couriers <- read_csv(file.path(module_dir, "courier_pay.csv"), show_col_types = FALSE)

couriers %>% head(8)

# Plot the two cities over time
ggplot(couriers, aes(week_start, active_couriers, color = city)) +
  geom_vline(xintercept = as.Date("2024-06-17"), linetype = "dashed") +
  geom_line() +
  geom_point(size = 1.2) +
  labs(
    x = NULL,
    y = "Active couriers",
    color = NULL,
    title = "Weekly active couriers, San Antonio versus Fort Worth"
  ) +
  theme_minimal(base_size = 13)

# The naive before/after comparison, San Antonio only
couriers %>%
  filter(city == "san_antonio") %>%
  group_by(post) %>%
  summarize(mean_couriers = mean(active_couriers))

# The four means behind difference-in-differences
cell_means <- couriers %>%
  group_by(city, post) %>%
  summarize(mean_couriers = mean(active_couriers), .groups = "drop")

cell_means

# The change in changes, computed by hand
changes <- cell_means %>%
  pivot_wider(names_from = post, values_from = mean_couriers, names_prefix = "post_") %>%
  mutate(change = post_1 - post_0)

changes

diff(changes$change)  # san_antonio change minus fort_worth change

# The same estimate as a regression: the group-by-post interaction
did_model <- lm(active_couriers ~ treated * post, data = couriers)
tidy(did_model, conf.int = TRUE)

# Module 10 trick: week dummies absorb the market-wide wiggles
did_week_fe <- lm(
  active_couriers ~ treated + factor(week) + treated:post,
  data = couriers
)
tidy(did_week_fe, conf.int = TRUE) %>%
  filter(!str_detect(term, "week"))

# Check the parallel-trends assumption in the pre-period:
# the gap between the cities should be roughly flat before launch
couriers %>%
  select(city, week, week_start, active_couriers) %>%
  pivot_wider(names_from = city, values_from = active_couriers) %>%
  mutate(gap = san_antonio - fort_worth) %>%
  ggplot(aes(week_start, gap)) +
  geom_vline(xintercept = as.Date("2024-06-17"), linetype = "dashed") +
  geom_line() +
  geom_point() +
  labs(
    x = NULL,
    y = "San Antonio minus Fort Worth",
    title = "The city gap, week by week"
  ) +
  theme_minimal(base_size = 13)

# Case 2: customer success managers at a $20k threshold (RD) -----------------

accounts <- read_csv(file.path(module_dir, "csm_accounts.csv"), show_col_types = FALSE)

accounts %>% sample_n(8)

# The naive comparison: all CSM accounts versus all non-CSM accounts
accounts %>%
  group_by(csm) %>%
  summarize(n = n(), mean_nrr = mean(renewal_spend_pct))

# But CSM accounts are bigger accounts. Retention rises with size anyway.
# Binned means show the whole relationship, and the jump at the cutoff:
accounts %>%
  mutate(bin = floor(arr_prior_k) + 0.5) %>%
  group_by(bin, csm) %>%
  summarize(mean_nrr = mean(renewal_spend_pct), .groups = "drop") %>%
  ggplot(aes(bin, mean_nrr, color = factor(csm))) +
  geom_vline(xintercept = 20, linetype = "dashed") +
  geom_point(size = 2.5) +
  labs(
    x = "Prior-year ARR, $ thousands",
    y = "Next-year spend, % of prior",
    color = "CSM assigned",
    title = "Retention around the $20k threshold"
  ) +
  theme_minimal(base_size = 13)

# Regression discontinuity: local linear fit on each side of the cutoff
bw <- 5
local <- accounts %>%
  filter(abs(arr_prior_k - 20) <= bw) %>%
  mutate(x_c = arr_prior_k - 20)

rd_model <- lm(renewal_spend_pct ~ x_c * csm, data = local)
tidy(rd_model, conf.int = TRUE)

# The csm coefficient is the jump at the cutoff:
# the difference between the two fitted lines at x_c = 0.

# Bandwidth sensitivity: re-estimate with different windows
rd_jump <- function(bw) {
  local <- accounts %>%
    filter(abs(arr_prior_k - 20) <= bw) %>%
    mutate(x_c = arr_prior_k - 20)
  lm(renewal_spend_pct ~ x_c * csm, data = local) %>%
    tidy(conf.int = TRUE) %>%
    filter(term == "csm") %>%
    mutate(bandwidth = bw, n = nrow(local))
}

map_dfr(c(2, 3, 4, 5, 6, 8, 10), rd_jump) %>%
  select(bandwidth, n, estimate, conf.low, conf.high)

# Manipulation check: is there bunching just above the cutoff?
ggplot(accounts, aes(arr_prior_k)) +
  geom_vline(xintercept = 20, linetype = "dashed") +
  geom_histogram(binwidth = 1, boundary = 0) +
  labs(
    x = "Prior-year ARR, $ thousands",
    y = "Accounts",
    title = "Distribution of the running variable"
  ) +
  theme_minimal(base_size = 13)
