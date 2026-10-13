library(tidyverse)
library(broom)

if (!interactive()) {
  pdf(NULL)
}

data_path <- if (file.exists("max_heart_rate.csv")) {
  "max_heart_rate.csv"
} else {
  file.path("modules", "01_simple_linear_regression", "max_heart_rate.csv")
}

mhr <- read_csv(data_path, show_col_types = FALSE)

set.seed(38201)
mhr %>%
  sample_n(8)

# 1. Fit the simple linear regression model.
mhr_model <- lm(max_heart_rate ~ age, data = mhr)
tidy(mhr_model)

ggplot(mhr, aes(age, max_heart_rate)) +
  geom_point(alpha = 0.45) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Age",
    y = "Maximum heart rate",
    title = "Maximum heart rate versus age"
  ) +
  theme_minimal(base_size = 14)

# 2. Use the line to make a prediction.
alice <- tibble(age = 28)
predict(mhr_model, newdata = alice)

# 3. Compute fitted values and residuals.
mhr_augmented <- augment(mhr_model)

mhr_augmented %>%
  transmute(
    age,
    actual = max_heart_rate,
    fitted = .fitted,
    residual = .resid
  ) %>%
  sample_n(8)

# 4. RMSE: the typical residual size, in beats per minute.
rmse <- sqrt(mean(resid(mhr_model)^2))
rmse

# 5. R-squared: proportional reduction in squared error versus the mean.
glance(mhr_model) %>%
  select(r.squared, sigma)

sse_line <- sum(resid(mhr_model)^2)
sse_mean <- sum((mhr$max_heart_rate - mean(mhr$max_heart_rate))^2)
1 - sse_line / sse_mean

# 6. Fair comparisons use residuals.
comparison <- tibble(
  name = c("Alice", "Brianna"),
  age = c(28, 55),
  max_heart_rate = c(185, 174)
) %>%
  mutate(
    fitted = predict(mhr_model, newdata = tibble(age = age)),
    residual = max_heart_rate - fitted
  )

comparison
