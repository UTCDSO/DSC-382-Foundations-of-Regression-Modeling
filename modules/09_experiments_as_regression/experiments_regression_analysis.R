library(tidyverse)
library(broom)

if (!interactive()) {
  pdf(NULL)
}

module_dir <- if (file.exists("reminder_letters.csv")) {
  "."
} else {
  file.path("modules", "09_experiments_as_regression")
}

# Case 1: late-payment reminder letters --------------------------------------

reminders <- read_csv(file.path(module_dir, "reminder_letters.csv"), show_col_types = FALSE) %>%
  mutate(arm = factor(arm, levels = c("control", "plain_language", "deadline", "autopay")))

reminders %>% sample_n(8)

# Randomization check: the arms should look alike on pre-treatment variables
reminders %>%
  group_by(arm) %>%
  summarize(
    accounts = n(),
    mean_amount_due = mean(amount_due),
    mean_prior_late = mean(prior_late_12mo)
  )

# Start with two arms: control versus the deadline letter
two_group <- reminders %>%
  filter(arm %in% c("control", "deadline")) %>%
  mutate(deadline_letter = as.integer(arm == "deadline"))

# The difference in means...
two_group %>%
  group_by(deadline_letter) %>%
  summarize(mean_days = mean(days_to_payment))

# ...is exactly the regression coefficient on the treatment dummy
two_group_model <- lm(days_to_payment ~ deadline_letter, data = two_group)
tidy(two_group_model, conf.int = TRUE)

# ...and the same comparison a two-sample t-test makes
t.test(days_to_payment ~ deadline_letter, data = two_group, var.equal = TRUE)

# All four arms: one dummy per non-baseline arm
four_arm_model <- lm(days_to_payment ~ arm, data = reminders)
tidy(four_arm_model, conf.int = TRUE)

# The omnibus ANOVA question: are the four letters all the same?
anova(four_arm_model)

# Group means with 95% intervals
arm_means <- reminders %>%
  group_by(arm) %>%
  summarize(
    n = n(),
    mean_days = mean(days_to_payment),
    se = sd(days_to_payment) / sqrt(n())
  ) %>%
  mutate(lower = mean_days - 1.96 * se, upper = mean_days + 1.96 * se)

arm_means

ggplot(arm_means, aes(arm, mean_days)) +
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  labs(
    x = NULL,
    y = "Days from letter to payment",
    title = "Mean days to payment by letter"
  ) +
  theme_minimal(base_size = 14)

# Planned contrast 1: the average redesigned letter versus control.
# A contrast is a linear combination of coefficients.
b <- coef(four_arm_model)
V <- vcov(four_arm_model)

L_redesign <- c(0, 1 / 3, 1 / 3, 1 / 3)
est <- sum(L_redesign * b)
se <- sqrt(drop(t(L_redesign) %*% V %*% L_redesign))
c(estimate = est, lower = est - 1.96 * se, upper = est + 1.96 * se)

# Planned contrast 2: deadline versus plain language.
# The everyday way: relevel so plain_language is the baseline.
releveled_model <- lm(
  days_to_payment ~ relevel(arm, ref = "plain_language"),
  data = reminders
)
tidy(releveled_model, conf.int = TRUE)

# Case 2: the 2x2 promotion experiment ----------------------------------------

promo <- read_csv(file.path(module_dir, "promo_experiment.csv"), show_col_types = FALSE)

promo %>% sample_n(8)

# The four cells and their mean spend
cell_means <- promo %>%
  group_by(discount, free_shipping) %>%
  summarize(
    n = n(),
    mean_spend = mean(spend_14d),
    se = sd(spend_14d) / sqrt(n()),
    .groups = "drop"
  )

cell_means

# Interaction plot: are the lines parallel?
ggplot(
  cell_means,
  aes(
    x = factor(discount),
    y = mean_spend,
    color = factor(free_shipping),
    group = free_shipping
  )
) +
  geom_line() +
  geom_pointrange(aes(
    ymin = mean_spend - 1.96 * se,
    ymax = mean_spend + 1.96 * se
  )) +
  labs(
    x = "Discount offered",
    y = "Mean 14-day spend, dollars",
    color = "Free shipping",
    title = "Cell means in the 2x2 promotion experiment"
  ) +
  theme_minimal(base_size = 14)

# A main-effects model assumes the two lifts add
main_effects_model <- lm(spend_14d ~ discount + free_shipping, data = promo)
tidy(main_effects_model, conf.int = TRUE)

# The interaction model lets the data disagree
interaction_model <- lm(spend_14d ~ discount * free_shipping, data = promo)
tidy(interaction_model, conf.int = TRUE)

# Add-the-lifts forecast for the both-offers cell versus what actually happened
b_int <- coef(interaction_model)
naive_forecast <- b_int["(Intercept)"] + b_int["discount"] + b_int["free_shipping"]
actual_both <- cell_means %>%
  filter(discount == 1, free_shipping == 1) %>%
  pull(mean_spend)

c(naive_forecast = unname(naive_forecast), actual_both = actual_both)

# Case 3: the abandoned-cart factorial (3 x 2 x 2) ----------------------------

cart <- read_csv(file.path(module_dir, "cart_recovery.csv"), show_col_types = FALSE) %>%
  mutate(
    incentive = factor(incentive, levels = c("none", "discount10", "free_shipping")),
    subject = factor(subject, levels = c("plain", "urgency")),
    send_delay = factor(send_delay, levels = c("1h", "24h"))
  )

cart %>% sample_n(8)

# Twelve cells, 550 carts each
cart %>% count(incentive, subject, send_delay)

# Raw cell means: the whole experiment in one table
cart_cells <- cart %>%
  group_by(incentive, subject, send_delay) %>%
  summarize(
    mean_rev = mean(recovered_revenue_7d),
    se = sd(recovered_revenue_7d) / sqrt(n()),
    .groups = "drop"
  )

cart_cells

ggplot(cart_cells, aes(incentive, mean_rev, color = subject, group = subject)) +
  geom_line() +
  geom_pointrange(aes(ymin = mean_rev - 1.96 * se, ymax = mean_rev + 1.96 * se)) +
  facet_wrap(~send_delay) +
  labs(
    x = NULL,
    y = "Recovered revenue per cart, dollars",
    title = "All 12 cells of the cart-recovery experiment"
  ) +
  theme_minimal(base_size = 13)

# The model: main effects plus all two-way interactions.
# We deliberately stop before the three-way interaction.
two_way <- lm(recovered_revenue_7d ~ (incentive + subject + send_delay)^2, data = cart)
tidy(two_way, conf.int = TRUE)

# Check that stopping cost nothing: does the three-way earn its keep?
full_fact <- lm(recovered_revenue_7d ~ incentive * subject * send_delay, data = cart)
anova(two_way, full_fact)

# Every business question is a CONTRAST: L' beta with a standard error.
# Build design-matrix rows for cells, then combine them.
tt <- delete.response(terms(two_way))
b <- coef(two_way)
V <- vcov(two_way)

cell_row <- function(inc, subj, del) {
  model.matrix(tt, tibble(
    incentive = factor(inc, levels = levels(cart$incentive)),
    subject = factor(subj, levels = levels(cart$subject)),
    send_delay = factor(del, levels = levels(cart$send_delay))
  ))
}

contrast_ci <- function(L) {
  est <- drop(L %*% b)
  se <- sqrt(drop(L %*% V %*% t(L)))
  c(estimate = est, conf.low = est - 1.96 * se, conf.high = est + 1.96 * se)
}

# Question 1: which incentive, on average over subject lines and send times?
# A marginal mean is the average of the four predicted cells.
marginal_row <- function(inc) {
  (cell_row(inc, "plain", "1h") + cell_row(inc, "plain", "24h") +
    cell_row(inc, "urgency", "1h") + cell_row(inc, "urgency", "24h")) / 4
}

sapply(levels(cart$incentive), function(i) drop(marginal_row(i) %*% b))

contrast_ci(marginal_row("discount10") - marginal_row("free_shipping"))

# Question 2: does urgency help? It depends on the incentive -- three contrasts.
urgency_within <- function(inc) {
  (cell_row(inc, "urgency", "1h") + cell_row(inc, "urgency", "24h")) / 2 -
    (cell_row(inc, "plain", "1h") + cell_row(inc, "plain", "24h")) / 2
}

t(sapply(levels(cart$incentive), function(i) contrast_ci(urgency_within(i))))

# Question 3: is it worth waiting 24 hours? Again, by incentive.
delay_within <- function(inc) {
  (cell_row(inc, "plain", "24h") + cell_row(inc, "urgency", "24h")) / 2 -
    (cell_row(inc, "plain", "1h") + cell_row(inc, "urgency", "1h")) / 2
}

t(sapply(levels(cart$incentive), function(i) contrast_ci(delay_within(i))))

# The recommendation: predicted revenue for the best version, and its
# margin over the runner-up.
drop(cell_row("discount10", "urgency", "24h") %*% b)

contrast_ci(
  cell_row("discount10", "urgency", "24h") - cell_row("discount10", "urgency", "1h")
)

# What one-at-a-time testing would have shipped (discount, plain, 1h):
drop(cell_row("discount10", "plain", "1h") %*% b)

# Case 4: the holiday bundle test (covariate adjustment) ----------------------

holiday <- read_csv(file.path(module_dir, "holiday_promo.csv"), show_col_types = FALSE)

holiday %>% sample_n(8)

# The naive analysis: two months of planning, an interval that straddles zero
naive_model <- lm(spend_promo ~ treat, data = holiday)
tidy(naive_model, conf.int = TRUE)

# Prior spend was measured BEFORE randomization, so it is balanced by design:
tidy(lm(spend_prior_90d ~ treat, data = holiday), conf.int = TRUE)

# ...and it strongly predicts the outcome:
holiday %>% summarize(r = cor(spend_prior_90d, spend_promo))

# Adjusting for a pre-treatment covariate cannot move a randomized estimate.
# It only removes noise. (Industry name: CUPED.)
adjusted_model <- lm(spend_promo ~ treat + spend_prior_90d, data = holiday)
tidy(adjusted_model, conf.int = TRUE)

# Compare the standard errors: same estimate, much sharper answer.
bind_rows(
  tidy(naive_model) %>% mutate(model = "naive"),
  tidy(adjusted_model) %>% mutate(model = "prior adjusted")
) %>%
  filter(term == "treat") %>%
  select(model, estimate, std.error)

# The precision gain is equivalent to multiplying the sample size by 1/(1-r^2):
r <- cor(holiday$spend_prior_90d, holiday$spend_promo)
1 / (1 - r^2)

# The trap: visits during the promo window are a CONSEQUENCE of the offer.
# Adjusting for a post-treatment variable erases the effect it helped cause.
posttreat_model <- lm(spend_promo ~ treat + spend_prior_90d + visits_promo, data = holiday)
tidy(posttreat_model, conf.int = TRUE)

# Rule: adjust for what was measured before randomization, never after.
