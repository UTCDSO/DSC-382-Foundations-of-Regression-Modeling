library(ggplot2)
library(scales)
library(readr)

set.seed(38203)

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- sub("^--file=", "", script_arg[1] %||% "assets/generate_log_assets.R")
out_dir <- dirname(normalizePath(script_path))

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

theme_classroom <- function(base_size = 18) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background = element_rect(fill = "#fcfbf7", color = NA),
      panel.background = element_rect(fill = "#fcfbf7", color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#e8ddc2", linewidth = 0.35),
      axis.title = element_text(color = "#2f2544", face = "bold"),
      axis.text = element_text(color = "#2f2544"),
      plot.title = element_text(color = "#2f2544", face = "bold", size = 24),
      plot.subtitle = element_text(color = "#2f2544", size = 18),
      plot.caption = element_text(color = "#6b607c", size = 13),
      legend.position = "none"
    )
}

purple <- "#2f2544"
green <- "#007d62"
gold <- "#eac35c"

# Classroom example: day 0 is March 23, 2014, when WHO announced
# the Ebola outbreak in Guinea. These are not the historical WHO counts.
days <- seq(0, 160, by = 7)
y0 <- 93.5
r <- 0.021
true_cases <- y0 * exp(r * days)
cases <- round(true_cases * exp(rnorm(length(days), mean = 0, sd = 0.055)))
epidemic <- data.frame(day = days, cases = cases, true_cases = true_cases)

write_csv(
  data.frame(day = days, cases = cases),
  file.path(dirname(out_dir), "ebola_cases.csv")
)

linear_fit <- lm(cases ~ day, data = epidemic)
log_fit <- lm(log(cases) ~ day, data = epidemic)
epidemic$linear <- predict(linear_fit)
epidemic$log_fitted <- exp(predict(log_fit))
epidemic$log_resid <- resid(log_fit)

curve_grid <- data.frame(day = seq(0, 160, length.out = 300))
curve_grid$true_cases <- y0 * exp(r * curve_grid$day)
curve_grid$linear <- predict(linear_fit, newdata = curve_grid)

p1 <- ggplot(epidemic, aes(day, cases)) +
  geom_point(color = purple, size = 3.2, alpha = 0.9) +
  geom_line(data = curve_grid, aes(y = linear), color = gold, linewidth = 1.3) +
  labs(
    title = "A straight line misses compounding",
    subtitle = "Ebola cases: 93.5 exp(0.021 days).",
    x = "Days since WHO announced the outbreak",
    y = "Reported cases"
  ) +
  scale_y_continuous(labels = comma) +
  theme_classroom()
save_plot(p1, "epidemic-linear-bad.png")

p2 <- ggplot(epidemic, aes(day, cases)) +
  geom_point(color = purple, size = 3.2, alpha = 0.9) +
  geom_line(data = curve_grid, aes(y = true_cases), color = green, linewidth = 1.4) +
  labs(
    title = "The same data with the exponential curve",
    subtitle = "The model adds 2.1% per day, not a fixed number of cases.",
    x = "Days since WHO announced the outbreak",
    y = "Reported cases"
  ) +
  scale_y_continuous(labels = comma) +
  theme_classroom()
save_plot(p2, "epidemic-exponential-fit.png")

p3 <- ggplot(epidemic, aes(day, log(cases))) +
  geom_point(color = purple, size = 3.2, alpha = 0.9) +
  geom_smooth(method = "lm", se = FALSE, color = green, linewidth = 1.4) +
  labs(
    title = "The log response is a straight-line problem",
    subtitle = "The slope estimates the daily growth rate.",
    x = "Days since WHO announced the outbreak",
    y = "log(reported cases)"
  ) +
  theme_classroom()
save_plot(p3, "epidemic-log-fit.png")

p4 <- ggplot(epidemic, aes(day, log_resid)) +
  geom_hline(yintercept = 0, color = gold, linewidth = 1.2) +
  geom_point(color = purple, size = 3.2, alpha = 0.9) +
  labs(
    title = "Check residuals where the model is linear",
    subtitle = "The log-scale residuals bounce around zero.",
    x = "Days since WHO announced the outbreak",
    y = "log-scale residual"
  ) +
  theme_classroom()
save_plot(p4, "epidemic-log-residuals.png")

species <- data.frame(
  species = c(
    "Mouse", "Rat", "Squirrel", "Rabbit", "Cat", "Dog", "Goat", "Human",
    "Horse", "Cow", "Giraffe", "Hippo", "Elephant", "Chinchilla"
  ),
  body_kg = c(0.025, 0.35, 0.75, 1.8, 4.3, 22, 45, 70, 420, 650, 900, 1500, 5200, 0.55)
)
species$brain_g <- 8.4 * species$body_kg^0.74 * exp(rnorm(nrow(species), 0, 0.23))
species$brain_g[species$species == "Human"] <- 1350
species$brain_g[species$species == "Elephant"] <- 4800
species$brain_g[species$species == "Chinchilla"] <- 6.5
species$brain_typo <- species$brain_g
species$brain_typo[species$species == "Chinchilla"] <- species$brain_typo[species$species == "Chinchilla"] / 10

write_csv(
  species[, c("species", "body_kg", "brain_g")],
  file.path(dirname(out_dir), "mammal_brain_body.csv")
)

mammal_fit <- lm(log(brain_g) ~ log(body_kg), data = species)
species$fitted <- exp(predict(mammal_fit))

p5 <- ggplot(species, aes(body_kg, brain_g)) +
  geom_point(color = purple, size = 3.4, alpha = 0.9) +
  geom_smooth(method = "lm", se = FALSE, color = gold, linewidth = 1.2) +
  labs(
    title = "The original scale is dominated by the giants",
    subtitle = "Most species are squeezed into the lower-left corner.",
    x = "Body weight (kg)",
    y = "Brain weight (g)"
  ) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  theme_classroom()
save_plot(p5, "mammals-original.png")

p6 <- ggplot(species, aes(body_kg, brain_g)) +
  geom_point(color = purple, size = 3.4, alpha = 0.9) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = green, linewidth = 1.2) +
  scale_x_log10(labels = label_number()) +
  scale_y_log10(labels = label_number()) +
  labs(
    title = "On a log-log scale, the scaling law is visible",
    subtitle = "A straight line here means a power law on the original scale.",
    x = "Body weight (kg), log scale",
    y = "Brain weight (g), log scale"
  ) +
  theme_classroom()
save_plot(p6, "mammals-loglog.png")

typo_species <- transform(species, brain_g = brain_typo)

p7 <- ggplot(typo_species, aes(body_kg, brain_g)) +
  geom_point(color = purple, size = 3.4, alpha = 0.9) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = green, linewidth = 1.1) +
  geom_point(
    data = subset(typo_species, species == "Chinchilla"),
    color = gold,
    size = 5
  ) +
  geom_text(
    data = subset(typo_species, species == "Chinchilla"),
    aes(label = "decimal point?"),
    nudge_x = 0.7,
    nudge_y = -0.18,
    color = purple,
    size = 5,
    fontface = "bold"
  ) +
  scale_x_log10(labels = label_number()) +
  scale_y_log10(labels = label_number()) +
  labs(
    title = "The log-log plot makes a bad entry hard to miss",
    subtitle = "One misplaced decimal can look like biology.",
    x = "Body weight (kg), log scale",
    y = "Brain weight (g), log scale"
  ) +
  theme_classroom()
save_plot(p7, "mammals-typo.png")

# Pricing experiment for an online seller.
price <- c(8, 9, 10, 11, 12, 14, 16, 18, 20, 22, 25, 28, 32)
true_elasticity <- -1.6
quantity <- round(1200 * (price / 10)^true_elasticity * exp(rnorm(length(price), 0, 0.08)))
pricing <- data.frame(price = price, quantity = quantity)

write_csv(pricing, file.path(dirname(out_dir), "pricing_experiment.csv"))
pricing_fit <- lm(log(quantity) ~ log(price), data = pricing)
pricing_grid <- data.frame(price = seq(8, 32, length.out = 300))
pricing_grid$quantity <- exp(predict(pricing_fit, newdata = pricing_grid))

p8 <- ggplot(pricing, aes(price, quantity)) +
  geom_point(color = purple, size = 3.5, alpha = 0.9) +
  geom_line(data = pricing_grid, aes(y = quantity), color = green, linewidth = 1.4) +
  labs(
    title = "A pricing experiment has a percent-change question",
    subtitle = "Price goes up; quantity sold falls by a percentage.",
    x = "Price offered to a randomized visitor group ($)",
    y = "Units sold"
  ) +
  scale_x_continuous(labels = dollar) +
  scale_y_continuous(labels = comma) +
  theme_classroom()
save_plot(p8, "pricing-demand-original.png")

p9 <- ggplot(pricing, aes(price, quantity)) +
  geom_point(color = purple, size = 3.5, alpha = 0.9) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = green, linewidth = 1.4) +
  scale_x_log10(labels = dollar) +
  scale_y_log10(labels = comma) +
  labs(
    title = "The log-log slope is price elasticity",
    subtitle = "Here the fitted slope is about -1.6.",
    x = "Price, log scale",
    y = "Units sold, log scale"
  ) +
  theme_classroom()
save_plot(p9, "pricing-demand-loglog.png")
