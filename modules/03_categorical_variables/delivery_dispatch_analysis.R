library(tidyverse)
library(broom)

data_path <- if (file.exists("delivery_dispatch.csv")) {
  "delivery_dispatch.csv"
} else {
  file.path("modules", "03_categorical_variables", "delivery_dispatch.csv")
}

delivery <- read_csv(data_path, show_col_types = FALSE) %>%
  mutate(
    Restaurant = factor(Restaurant),
    Peak = factor(Peak, levels = c(0, 1), labels = c("Off-peak", "Peak")),
    Priority = factor(Priority, levels = c(0, 1), labels = c("Standard", "Priority"))
  )

set.seed(38204)
delivery %>%
  sample_n(8) %>%
  arrange(order_id)

# 1. Start with the naive priority comparison.
ggplot(delivery, aes(Priority, late_min, fill = Priority)) +
  geom_boxplot(width = 0.55, alpha = 0.8) +
  geom_jitter(width = 0.08, alpha = 0.18, size = 1.2) +
  labs(
    x = "Dispatch rule",
    y = "Minutes late",
    title = "Naive comparison"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

m_priority <- lm(late_min ~ Priority, data = delivery)
tidy(m_priority)

# 2. Diagnose confounding: priority dispatch is more common at peak.
delivery %>%
  count(Peak, Priority) %>%
  group_by(Peak) %>%
  mutate(prop = n / sum(n))

ggplot(delivery, aes(Peak, fill = Priority)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Time of order",
    y = "Share of orders",
    title = "Priority dispatch is more common at peak"
  ) +
  theme_minimal(base_size = 14)

# 3. Adjust for peak status.
m_peak_adjusted <- lm(late_min ~ Priority + Peak, data = delivery)
tidy(m_peak_adjusted)

# 4. Let the priority effect depend on peak status.
ggplot(delivery, aes(interaction(Peak, Priority, sep = ", "), late_min, fill = Priority)) +
  geom_boxplot(width = 0.58, alpha = 0.8) +
  labs(
    x = NULL,
    y = "Minutes late",
    title = "Priority by peak status"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

m_peak_interaction <- lm(late_min ~ Priority * Peak, data = delivery)
tidy(m_peak_interaction)

# 5. Check whether restaurants are part of the assignment story.
restaurant_summary <- delivery %>%
  group_by(Restaurant) %>%
  summarize(
    priority_rate = mean(Priority == "Priority"),
    peak_rate = mean(Peak == "Peak"),
    avg_late = mean(late_min),
    n = n(),
    .groups = "drop"
  )

restaurant_summary

ggplot(restaurant_summary, aes(fct_reorder(Restaurant, priority_rate), priority_rate, fill = peak_rate)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_c(labels = scales::percent) +
  labs(
    x = "Restaurant",
    y = "Priority share",
    fill = "Peak share",
    title = "Priority dispatch differs by restaurant"
  ) +
  theme_minimal(base_size = 14)

# 6. Adjust for restaurant baselines.
m_restaurant <- lm(late_min ~ Priority * Peak + Restaurant, data = delivery)
tidy(m_restaurant)

# 7. Raw means can answer the question, but the table gets large quickly.
raw_cells <- delivery %>%
  group_by(Restaurant, Peak, Priority) %>%
  summarize(
    n = n(),
    mean_late = mean(late_min),
    .groups = "drop"
  )

raw_cells

raw_cells %>%
  summarize(
    cells = n(),
    smallest_cell = min(n),
    median_cell = median(n),
    largest_cell = max(n)
  )

raw_priority_gaps <- raw_cells %>%
  select(Restaurant, Peak, Priority, mean_late) %>%
  pivot_wider(names_from = Priority, values_from = mean_late) %>%
  mutate(
    raw_minutes_saved = Standard - Priority
  )

raw_priority_gaps %>%
  arrange(Peak, desc(raw_minutes_saved))

# 8. Let priority effects differ by both peak status and restaurant.
m_full <- lm(late_min ~ Priority * Peak + Restaurant + Priority:Restaurant, data = delivery)
tidy(m_full)

# The coefficients tell us how the priority effect is assembled.
# R01, off-peak is the baseline priority effect.
# Priority:Peak is the extra priority effect during peak hours.
# Priority:Restaurant terms are restaurant-specific priority offsets.
coef(m_full)

effect_grid <- expand_grid(
  Restaurant = levels(delivery$Restaurant),
  Peak = levels(delivery$Peak),
  Priority = levels(delivery$Priority)
) %>%
  mutate(
    Restaurant = factor(Restaurant, levels = levels(delivery$Restaurant)),
    Peak = factor(Peak, levels = levels(delivery$Peak)),
    Priority = factor(Priority, levels = levels(delivery$Priority))
  )

effect_grid$fit <- predict(m_full, newdata = effect_grid)

priority_effects <- effect_grid %>%
  select(Restaurant, Peak, Priority, fit) %>%
  pivot_wider(names_from = Priority, values_from = fit) %>%
  mutate(
    priority_effect = Priority - Standard,
    minutes_saved = Standard - Priority
  )

priority_effects

priority_effects %>%
  arrange(Peak, desc(minutes_saved))

priority_effects %>%
  group_by(Peak) %>%
  slice_max(minutes_saved, n = 5) %>%
  arrange(Peak, desc(minutes_saved))

ggplot(effect_grid, aes(Restaurant, fit, fill = Priority)) +
  geom_col(width = 0.72, position = position_dodge(width = 0.74)) +
  facet_grid(Peak ~ .) +
  labs(
    x = "Restaurant",
    y = "Fitted minutes late",
    title = "Fitted lateness by restaurant and dispatch rule"
  ) +
  theme_minimal(base_size = 14)
