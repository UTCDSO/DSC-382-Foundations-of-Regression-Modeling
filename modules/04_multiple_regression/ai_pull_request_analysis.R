library(tidyverse)
library(broom)

if (!interactive()) {
  pdf(NULL)
}

data_path <- if (file.exists("ai_pull_requests.csv")) {
  "ai_pull_requests.csv"
} else {
  file.path("modules", "04_multiple_regression", "ai_pull_requests.csv")
}

prs <- read_csv(data_path, show_col_types = FALSE) %>%
  mutate(
    Lines100 = LinesChanged / 100,
    SizeQuartile = cut_number(
      LinesChanged,
      4,
      labels = c("Q1: smallest PRs", "Q2", "Q3", "Q4: largest PRs")
    )
  )

set.seed(38206)
prs %>%
  sample_n(8)

# 1. Naive one-variable relationship.
ggplot(prs, aes(AIPrompts, HoursToMerge)) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "AI prompts used on the PR",
    y = "Hours to merge",
    title = "Time to merge versus AI prompts"
  ) +
  theme_minimal(base_size = 14)

naive_model <- lm(HoursToMerge ~ AIPrompts, data = prs)
tidy(naive_model)

# 2. Check whether PR size is related to both variables.
ggplot(prs, aes(LinesChanged, AIPrompts)) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Lines changed",
    y = "AI prompts used",
    title = "AI prompts versus PR size"
  ) +
  theme_minimal(base_size = 14)

ggplot(prs, aes(LinesChanged, HoursToMerge)) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Lines changed",
    y = "Hours to merge",
    title = "Time to merge versus PR size"
  ) +
  theme_minimal(base_size = 14)

# 3. Compare the overall slope with slopes within PR-size quartiles.
ggplot(prs, aes(AIPrompts, HoursToMerge)) +
  geom_point(alpha = 0.35) +
  geom_smooth(data = prs, method = "lm", se = FALSE, color = "firebrick") +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue") +
  facet_wrap(~ SizeQuartile, ncol = 2) +
  labs(
    x = "AI prompts used on the PR",
    y = "Hours to merge",
    title = "Overall and within-size relationships"
  ) +
  theme_minimal(base_size = 14)

prs %>%
  group_by(SizeQuartile) %>%
  summarize(
    slope = coef(lm(HoursToMerge ~ AIPrompts))[["AIPrompts"]],
    .groups = "drop"
  )

# 4. Use multiple regression to adjust for PR size directly.
adjusted_model <- lm(HoursToMerge ~ AIPrompts + Lines100, data = prs)
tidy(adjusted_model)

line_values <- quantile(prs$LinesChanged, c(0.15, 0.50, 0.85))

fit_grid <- bind_rows(
  tibble(AIPrompts = seq(0, 9, length.out = 80), LinesChanged = line_values[[1]]),
  tibble(AIPrompts = seq(0, 15, length.out = 80), LinesChanged = line_values[[2]]),
  tibble(AIPrompts = seq(5, 25, length.out = 80), LinesChanged = line_values[[3]])
) %>%
  mutate(
    Lines100 = LinesChanged / 100,
    PRSize = factor(paste0(round(LinesChanged), " lines changed"))
  )

fit_grid$fit <- predict(adjusted_model, newdata = fit_grid)

ggplot(prs, aes(AIPrompts, HoursToMerge)) +
  geom_point(alpha = 0.18) +
  geom_line(
    data = fit_grid,
    aes(y = fit, color = PRSize),
    linewidth = 1.1
  ) +
  labs(
    x = "AI prompts used on the PR",
    y = "Fitted hours to merge",
    title = "Adjusted fitted lines"
  ) +
  theme_minimal(base_size = 14)

# 5. Adjustment as subtraction: remove the fitted PR-size piece from the response.
size_model <- lm(HoursToMerge ~ Lines100, data = prs)

prs_adjusted <- prs %>%
  mutate(HoursAdjustedForSize = mean(HoursToMerge) + resid(size_model))

ggplot(prs_adjusted, aes(AIPrompts, HoursAdjustedForSize)) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "AI prompts used on the PR",
    y = "Hours to merge, adjusted for PR size",
    title = "Partial relationship after adjusting for PR size"
  ) +
  theme_minimal(base_size = 14)
