library(dplyr)
library(ggplot2)
library(readr)
library(scales)

set.seed(38201)

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- sub("^--file=", "", script_arg[1] %||% "assets/generate_simple_linear_assets.R")
out_dir <- dirname(normalizePath(script_path))
module_dir <- normalizePath(file.path(out_dir, ".."))

theme_classroom <- function(base_size = 18) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background = element_rect(fill = "#fcfbf7", color = NA),
      panel.background = element_rect(fill = "#fcfbf7", color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#d9e2f2", linewidth = 0.35),
      axis.title = element_text(color = "#1f2430", face = "bold"),
      axis.text = element_text(color = "#1f2430"),
      plot.title = element_text(color = "#163a5f", face = "bold", size = 24),
      plot.subtitle = element_text(color = "#1f2430", size = 18)
    )
}

save_plot <- function(plot, name, width = 9.6, height = 6) {
  ggsave(
    filename = file.path(out_dir, name),
    plot = plot,
    width = width,
    height = height,
    dpi = 150,
    bg = "#fcfbf7"
  )
}

age <- sample(18:75, size = 160, replace = TRUE)
age_centered <- age - mean(age)

raw_resid <- rnorm(length(age))
resid <- raw_resid - mean(raw_resid)
resid <- resid - sum(resid * age_centered) / sum(age_centered^2) * age_centered
resid <- resid / sqrt(mean(resid^2)) * 10

mhr <- tibble(
  person_id = seq_along(age),
  age = age,
  max_heart_rate = round(208 - 0.7 * age + resid, 1)
)

write_csv(mhr, file.path(module_dir, "max_heart_rate.csv"))

fit <- lm(max_heart_rate ~ age, data = mhr)

p_line <- ggplot(mhr, aes(age, max_heart_rate)) +
  geom_point(color = "#2f2544", alpha = 0.55, size = 2.1) +
  geom_smooth(method = "lm", se = FALSE, color = "#8b2e2e", linewidth = 1.35) +
  labs(
    title = "Maximum heart rate versus age",
    subtitle = "Fitted simple linear regression line",
    x = "Age",
    y = "Maximum heart rate"
  ) +
  theme_classroom()
save_plot(p_line, "mhr-line.png")

alice <- tibble(age = 28)
alice$max_heart_rate <- predict(fit, newdata = alice)

p_prediction <- ggplot(mhr, aes(age, max_heart_rate)) +
  geom_point(color = "#2f2544", alpha = 0.35, size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "#8b2e2e", linewidth = 1.3) +
  geom_point(data = alice, color = "#007d62", size = 5) +
  geom_segment(
    data = alice,
    aes(x = age, xend = age, y = min(mhr$max_heart_rate), yend = max_heart_rate),
    color = "#007d62",
    linewidth = 1.1,
    linetype = "dashed"
  ) +
  annotate(
    "text",
    x = 36,
    y = alice$max_heart_rate + 4,
    label = "age 28: fitted MHR = 188.4",
    color = "#007d62",
    fontface = "bold",
    size = 5
  ) +
  labs(
    title = "Prediction from the fitted line",
    subtitle = "Predicted maximum heart rate for a 28-year-old",
    x = "Age",
    y = "Maximum heart rate"
  ) +
  theme_classroom()
save_plot(p_prediction, "mhr-prediction.png")
