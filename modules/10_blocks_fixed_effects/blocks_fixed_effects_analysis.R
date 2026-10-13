library(tidyverse)
library(broom)

if (!interactive()) {
  pdf(NULL)
}

module_dir <- if (file.exists("upsell_prompt.csv")) {
  "."
} else {
  file.path("modules", "10_blocks_fixed_effects")
}

# Case 1: the kiosk upsell prompt across 16 stores ---------------------------

upsell <- read_csv(file.path(module_dir, "upsell_prompt.csv"), show_col_types = FALSE) %>%
  mutate(day_of_week = factor(
    day_of_week,
    levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  ))

upsell %>% sample_n(8)

# How different are the stores?
upsell %>%
  group_by(store_id) %>%
  summarize(mean_ticket = mean(avg_ticket)) %>%
  arrange(mean_ticket)

# Store-to-store spread versus the size of the effect we are hunting
ggplot(upsell, aes(reorder(store_id, avg_ticket), avg_ticket, color = factor(prompt))) +
  geom_jitter(width = 0.2, alpha = 0.45) +
  labs(
    x = "Store",
    y = "Average ticket, dollars",
    color = "Prompt on?",
    title = "Store-to-store variation dwarfs the prompt effect"
  ) +
  theme_minimal(base_size = 13)

# The naive model: ignore stores entirely
naive_model <- lm(avg_ticket ~ prompt, data = upsell)
tidy(naive_model, conf.int = TRUE)

# The blocked analysis: store dummies absorb stable store differences
store_fe_model <- lm(avg_ticket ~ prompt + store_id, data = upsell)
tidy(store_fe_model, conf.int = TRUE) %>%
  filter(term == "prompt")

# A second nuisance factor: day of week
upsell %>%
  group_by(day_of_week) %>%
  summarize(mean_ticket = mean(avg_ticket))

store_dow_model <- lm(avg_ticket ~ prompt + store_id + day_of_week, data = upsell)
tidy(store_dow_model, conf.int = TRUE) %>%
  filter(term == "prompt")

# Compare the three estimates of the prompt effect
bind_rows(
  tidy(naive_model, conf.int = TRUE) %>% mutate(model = "naive"),
  tidy(store_fe_model, conf.int = TRUE) %>% mutate(model = "store FE"),
  tidy(store_dow_model, conf.int = TRUE) %>% mutate(model = "store + dow FE")
) %>%
  filter(term == "prompt") %>%
  select(model, estimate, std.error, conf.low, conf.high)

# Case 2: the warehouse scanner study ----------------------------------------

scanner <- read_csv(file.path(module_dir, "scanner_study.csv"), show_col_types = FALSE) %>%
  mutate(device = factor(device, levels = c("old", "new")))

scanner %>% arrange(picker_id) %>% head(8)

# The naive comparison: treat the 80 rows as two independent groups
naive_scanner <- lm(picks_per_hour ~ device, data = scanner)
tidy(naive_scanner, conf.int = TRUE)

# The same data, connected picker by picker
ggplot(scanner, aes(device, picks_per_hour, group = picker_id)) +
  geom_line(alpha = 0.4) +
  geom_point(alpha = 0.6) +
  labs(
    x = NULL,
    y = "Picks per hour",
    title = "Each picker with both devices"
  ) +
  theme_minimal(base_size = 14)

# Within-picker differences
diffs <- scanner %>%
  select(picker_id, device, picks_per_hour) %>%
  pivot_wider(names_from = device, values_from = picks_per_hour) %>%
  mutate(diff = new - old)

diffs %>% summarize(
  mean_diff = mean(diff),
  sd_diff = sd(diff),
  pct_improved = mean(diff > 0)
)

# The paired t-test...
t.test(diffs$diff)

# ...is the same analysis as a regression with picker fixed effects
picker_fe_model <- lm(picks_per_hour ~ device + picker_id, data = scanner)
tidy(picker_fe_model, conf.int = TRUE) %>%
  filter(term == "devicenew")

# The order of the two weeks was counterbalanced, but week can still
# soak up a learning-curve effect:
picker_week_model <- lm(
  picks_per_hour ~ device + factor(week) + picker_id,
  data = scanner
)
tidy(picker_week_model, conf.int = TRUE) %>%
  filter(!str_detect(term, "picker_id"))

# The assignment unit: why the ticket log lies --------------------------------

# Store S07's individual tickets: 30 days, ~900 tickets per day.
tickets <- read_csv(file.path(module_dir, "upsell_tickets_s07.csv"), show_col_types = FALSE)

nrow(tickets)

# Tempting: run the regression on all 27,000 tickets.
ticket_level <- lm(ticket_amount ~ prompt, data = tickets)
tidy(ticket_level, conf.int = TRUE)

# Honest: aggregate to the randomized unit (the store-day) first.
day_level <- tickets %>%
  group_by(day, prompt) %>%
  summarize(avg_ticket = mean(ticket_amount), .groups = "drop") %>%
  lm(avg_ticket ~ prompt, data = .)
tidy(day_level, conf.int = TRUE)

# Same estimate to four decimals. The ticket-level interval is ~2.5x too
# narrow, because all ~900 tickets in a day share ONE coin flip (and one
# day's weather, staffing, and foot traffic). Rows that share a flip are
# not independent evidence: n is the number of coin flips, not the number
# of rows. Analyze at the level you randomized.
