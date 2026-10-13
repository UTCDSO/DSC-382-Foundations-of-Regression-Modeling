library(tidyverse)
library(lme4)
library(broom)
library(patchwork)

set.seed(7107)

module_dir <- if (dir.exists("modules/07_hierarchical_mixed_models")) {
  "modules/07_hierarchical_mixed_models"
} else {
  normalizePath(file.path(getwd(), ".."), mustWork = FALSE)
}
out_dir <- file.path(module_dir, "assets")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

theme_classroom <- function(base_size = 15) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 3),
      plot.subtitle = element_text(size = base_size),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
}

save_plot <- function(plot, filename, width = 8.6, height = 5.2) {
  ggsave(file.path(out_dir, filename), plot, width = width, height = height, dpi = 220)
}

blue <- "#2b6cb0"
red <- "#c53030"
green <- "#2f855a"
gold <- "#b7791f"
purple <- "#6b46c1"
gray <- "#4a5568"

# Case 1: low-acuity ER visits, random intercepts --------------------------

n_physicians <- 75
physician_counts <- round(exp(runif(n_physicians, log(3), log(650))))
physician_counts[1:3] <- c(3, 4, 5)
physician_counts <- pmin(pmax(physician_counts, 3), 680)

physicians <- tibble(
  Physician = sprintf("MD%02d", 1:n_physicians),
  n_visits = physician_counts,
  disposition_style = rnorm(n_physicians, 0, 0.29),
  checklist_tendency = rnorm(n_physicians, 0, 0.55)
)

er_visits <- physicians |>
  uncount(n_visits, .id = "visit_in_physician") |>
  group_by(Physician) |>
  mutate(VisitID = row_number()) |>
  ungroup() |>
  mutate(
    Evening = rbinom(n(), 1, 0.38),
    Checklist = rbinom(n(), 1, plogis(-0.25 + 0.2 * Evening + checklist_tendency)),
    case_mix = rnorm(n(), 0, 0.17),
    logDispositionMinutes = log(92) + disposition_style - 0.16 * Checklist +
      0.11 * Evening + case_mix + rnorm(n(), 0, 0.34),
    DispositionMinutes = round(exp(logDispositionMinutes), 1),
    Checklist = factor(if_else(Checklist == 1, "Checklist", "Usual intake"), levels = c("Usual intake", "Checklist")),
    Evening = factor(if_else(Evening == 1, "Evening", "Day"), levels = c("Day", "Evening"))
  ) |>
  select(VisitID, Physician, Checklist, Evening, DispositionMinutes, logDispositionMinutes)

write_csv(er_visits, file.path(module_dir, "er_low_acuity_visits.csv"))

er_lm <- lm(logDispositionMinutes ~ Checklist + Physician, data = er_visits)
er_mixed <- lmer(logDispositionMinutes ~ Checklist + (1 | Physician), data = er_visits)

physician_counts_tbl <- er_visits |>
  count(Physician, name = "n") |>
  mutate(Physician = fct_reorder(Physician, n))

p_er_counts <- ggplot(physician_counts_tbl, aes(Physician, n)) +
  geom_col(fill = blue, width = 0.75) +
  coord_flip() +
  labs(
    title = "Low-acuity ER visits per physician",
    subtitle = "Some physicians appear hundreds of times; some appear only three or four times",
    x = "Physician",
    y = "Visits"
  ) +
  theme_classroom(12)
save_plot(p_er_counts, "er-physician-counts.png", width = 8.2, height = 8.0)

physician_no_pool <- er_visits |>
  group_by(Physician) |>
  summarize(
    n = n(),
    mean_log_disposition = mean(logDispositionMinutes),
    se = sd(logDispositionMinutes) / sqrt(n()),
    .groups = "drop"
  )

er_overall <- mean(er_visits$logDispositionMinutes)
er_re <- ranef(er_mixed)$Physician |>
  rownames_to_column("Physician") |>
  as_tibble() |>
  transmute(
    Physician,
    partial_pool = fixef(er_mixed)[["(Intercept)"]] + `(Intercept)`
  )

physician_estimates <- physician_no_pool |>
  left_join(er_re, by = "Physician") |>
  mutate(
    Physician = fct_reorder(Physician, n),
    no_pool_lo = mean_log_disposition - 1.96 * se,
    no_pool_hi = mean_log_disposition + 1.96 * se
  )

p_er_lm <- ggplot(physician_estimates, aes(n, mean_log_disposition)) +
  geom_hline(yintercept = er_overall, color = gray, linetype = "dashed") +
  geom_errorbar(aes(ymin = no_pool_lo, ymax = no_pool_hi), width = 0, alpha = 0.35, color = gray) +
  geom_point(color = red, alpha = 0.75, size = 2.1) +
  scale_x_log10() +
  labs(
    title = "No pooling: sparse physician estimates are noisy",
    subtitle = "Each point is a physician mean on the log-disposition-time scale",
    x = "Low-acuity visits for physician, log scale",
    y = "Estimated physician baseline"
  ) +
  theme_classroom()
save_plot(p_er_lm, "er-no-pooling.png")

p_er_pool <- ggplot(physician_estimates, aes(n)) +
  geom_hline(yintercept = er_overall, color = gray, linetype = "dashed") +
  geom_segment(aes(xend = n, y = mean_log_disposition, yend = partial_pool), alpha = 0.35) +
  geom_point(aes(y = mean_log_disposition), color = red, alpha = 0.70, size = 2.1) +
  geom_point(aes(y = partial_pool), color = blue, alpha = 0.85, size = 2.1) +
  scale_x_log10() +
  labs(
    title = "Partial pooling pulls sparse physicians toward the population",
    subtitle = "Red: no pooling. Blue: mixed-model estimate.",
    x = "Low-acuity visits for physician, log scale",
    y = "Estimated physician baseline"
  ) +
  theme_classroom()
save_plot(p_er_pool, "er-partial-pooling.png")

er_summary <- tibble(
  quantity = c(
    "physicians",
    "visits",
    "median_physician_visits",
    "min_physician_visits",
    "max_physician_visits",
    "lm_checklist_effect",
    "mixed_checklist_effect",
    "mixed_physician_sd",
    "mixed_residual_sd"
  ),
  value = c(
    n_distinct(er_visits$Physician),
    nrow(er_visits),
    median(physician_counts),
    min(physician_counts),
    max(physician_counts),
    coef(er_lm)[["ChecklistChecklist"]],
    fixef(er_mixed)[["ChecklistChecklist"]],
    as.numeric(VarCorr(er_mixed)$Physician[1, 1]^0.5),
    sigma(er_mixed)
  )
)
write_csv(er_summary, file.path(out_dir, "er_model_summary.csv"))

# Case 2: legal document review, Gaussian random slopes --------------------

n_reviewers <- 185
reviewer_counts <- round(exp(runif(n_reviewers, log(1), log(90))))
reviewer_counts[1:18] <- c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 4, 4, 5, 5, 6, 7, 8, 9)
reviewer_counts <- pmin(pmax(reviewer_counts, 1), 95)

reviewer_info <- tibble(
  ReviewerID = sprintf("REV%03d", 1:n_reviewers),
  ExperienceLevel = sample(
    c("New associate", "Associate", "Staff attorney", "Contract reviewer"),
    n_reviewers,
    replace = TRUE,
    prob = c(0.28, 0.32, 0.22, 0.18)
  ),
  n_assignments = reviewer_counts
) |>
  mutate(
    role_intercept = recode(
      ExperienceLevel,
      "New associate" = 0.10,
      "Associate" = 0,
      "Staff attorney" = -0.12,
      "Contract reviewer" = 0.05
    ),
    reviewer_intercept = role_intercept + rnorm(n_reviewers, 0, 0.34),
    reviewer_page_slope = rnorm(n_reviewers, 0, 0.15)
  )

legal_review <- reviewer_info |>
  uncount(n_assignments, .id = "AssignmentInReviewer") |>
  mutate(
    AssignmentID = row_number(),
    DocumentType = sample(
      c("Emails", "Contracts", "Financial records", "Technical documents", "Chat exports"),
      n(),
      replace = TRUE,
      prob = c(0.40, 0.20, 0.17, 0.13, 0.10)
    ),
    matter_page_shift = recode(
      DocumentType,
      "Emails" = 0,
      "Contracts" = -0.18,
      "Financial records" = 0.30,
      "Technical documents" = 0.18,
      "Chat exports" = -0.35
    ),
    large_assignment = rbinom(n(), 1, 0.22),
    PagesReviewed = round(exp(
      rnorm(
        n(),
        log(900) + matter_page_shift + 1.35 * large_assignment,
        1.15
      )
    )),
    PagesReviewed = pmin(pmax(PagesReviewed, 25), 85000),
    logPages = log(PagesReviewed),
    logPagesCentered = logPages - log(1000),
    document_effect = recode(
      DocumentType,
      "Emails" = 0,
      "Contracts" = 0.12,
      "Financial records" = 0.20,
      "Technical documents" = 0.30,
      "Chat exports" = -0.07
    ),
    logBilledHours = log(6.2) + reviewer_intercept +
      (1.08 + reviewer_page_slope) * logPagesCentered +
      document_effect + rnorm(n(), 0, 0.35),
    BilledHours = round(pmax(0.15, exp(logBilledHours)), 2),
    DocumentType = factor(
      DocumentType,
      levels = c("Emails", "Contracts", "Financial records", "Technical documents", "Chat exports")
    ),
    ExperienceLevel = factor(
      ExperienceLevel,
      levels = c("New associate", "Associate", "Staff attorney", "Contract reviewer")
    )
  ) |>
  select(
    AssignmentID, ReviewerID, ExperienceLevel, DocumentType,
    PagesReviewed, BilledHours, logPages, logPagesCentered, logBilledHours
  )

write_csv(legal_review, file.path(module_dir, "legal_review_assignments.csv"))

legal_pooled <- lm(
  logBilledHours ~ logPagesCentered + DocumentType + ExperienceLevel,
  data = legal_review
)

legal_mixed <- lmer(
  logBilledHours ~ logPagesCentered + DocumentType + ExperienceLevel +
    (1 + logPagesCentered | ReviewerID),
  data = legal_review,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

reviewer_counts_tbl <- legal_review |>
  count(ReviewerID, name = "assignments")

p_legal_span <- ggplot(legal_review, aes(PagesReviewed, BilledHours)) +
  geom_point(alpha = 0.28, color = blue, size = 1.4) +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  labs(
    title = "Billed hours and pages reviewed",
    subtitle = "Each point is one reviewer-assignment aggregate",
    x = "Pages reviewed, log scale",
    y = "Billed hours, log scale"
  ) +
  theme_classroom()
save_plot(p_legal_span, "legal-review-log-spans.png")

p_legal_counts <- ggplot(reviewer_counts_tbl, aes(assignments)) +
  geom_histogram(binwidth = 3, boundary = 0, fill = blue, color = "white") +
  scale_y_continuous(trans = "sqrt") +
  labs(
    title = "Review assignments per reviewer",
    subtitle = "Many reviewers have only a few prior assignments; a few reviewers appear often",
    x = "Assignments observed for reviewer",
    y = "Number of reviewers, square-root scale"
  ) +
  theme_classroom()
save_plot(p_legal_counts, "legal-reviewer-counts.png")

safe_legal_slope <- function(dat) {
  if (nrow(dat) < 3 || sd(dat$logPagesCentered) < 0.05) {
    return(tibble(estimate = NA_real_, se = NA_real_))
  }
  fit <- lm(logBilledHours ~ logPagesCentered, data = dat)
  co <- summary(fit)$coefficients["logPagesCentered", ]
  tibble(estimate = unname(co[["Estimate"]]), se = unname(co[["Std. Error"]]))
}

legal_separate <- legal_review |>
  group_by(ReviewerID) |>
  group_modify(~ safe_legal_slope(.x)) |>
  ungroup() |>
  left_join(reviewer_counts_tbl, by = "ReviewerID") |>
  filter(is.finite(estimate), is.finite(se)) |>
  mutate(
    lo = estimate - 1.96 * se,
    hi = estimate + 1.96 * se
  )

legal_mixed_slopes <- coef(legal_mixed)$ReviewerID |>
  rownames_to_column("ReviewerID") |>
  as_tibble() |>
  transmute(ReviewerID, partial_pool_slope = logPagesCentered)

legal_estimates <- legal_separate |>
  left_join(legal_mixed_slopes, by = "ReviewerID")

p_legal_no_pool <- ggplot(legal_estimates, aes(assignments, estimate)) +
  geom_hline(yintercept = fixef(legal_mixed)[["logPagesCentered"]], color = gray, linetype = "dashed") +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0, alpha = 0.35, color = gray) +
  geom_point(color = red, alpha = 0.78, size = 2.1) +
  scale_x_log10() +
  coord_cartesian(ylim = quantile(legal_estimates$estimate, c(0.03, 0.97), na.rm = TRUE)) +
  labs(
    title = "Separate reviewer slopes are unstable for short histories",
    subtitle = "Estimated exponent linking pages reviewed to billed hours",
    x = "Assignments observed for reviewer, log scale",
    y = "Estimated page-count exponent"
  ) +
  theme_classroom()
save_plot(p_legal_no_pool, "legal-no-pooling-slopes.png")

p_legal_pool <- ggplot(legal_estimates, aes(assignments)) +
  geom_hline(yintercept = fixef(legal_mixed)[["logPagesCentered"]], color = gray, linetype = "dashed") +
  geom_segment(aes(xend = assignments, y = estimate, yend = partial_pool_slope), alpha = 0.35) +
  geom_point(aes(y = estimate), color = red, alpha = 0.70, size = 2.1) +
  geom_point(aes(y = partial_pool_slope), color = blue, alpha = 0.85, size = 2.1) +
  scale_x_log10() +
  coord_cartesian(ylim = quantile(legal_estimates$estimate, c(0.03, 0.97), na.rm = TRUE)) +
  labs(
    title = "Random slopes partially pool reviewer page-count effects",
    subtitle = "Red: separate reviewer slope. Blue: mixed-model slope.",
    x = "Assignments observed for reviewer, log scale",
    y = "Estimated page-count exponent"
  ) +
  theme_classroom()
save_plot(p_legal_pool, "legal-partial-pooling-slopes.png")

legal_summary <- tibble(
  quantity = c(
    "reviewers",
    "assignments",
    "median_reviewer_assignments",
    "min_reviewer_assignments",
    "max_reviewer_assignments",
    "pooled_page_exponent",
    "mixed_page_exponent",
    "mixed_page_exponent_sd",
    "mixed_reviewer_intercept_sd",
    "mixed_residual_sd"
  ),
  value = c(
    n_distinct(legal_review$ReviewerID),
    nrow(legal_review),
    median(reviewer_counts_tbl$assignments),
    min(reviewer_counts_tbl$assignments),
    max(reviewer_counts_tbl$assignments),
    coef(legal_pooled)[["logPagesCentered"]],
    fixef(legal_mixed)[["logPagesCentered"]],
    as.numeric(VarCorr(legal_mixed)$ReviewerID["logPagesCentered", "logPagesCentered"]^0.5),
    as.numeric(VarCorr(legal_mixed)$ReviewerID["(Intercept)", "(Intercept)"]^0.5),
    sigma(legal_mixed)
  )
)
write_csv(legal_summary, file.path(out_dir, "legal_model_summary.csv"))

# Case 3: Spredfast support tickets, Poisson GLMM random slopes ------------

n_accounts <- 140
account_months <- round(exp(runif(n_accounts, log(3), log(72))))
account_months[1:8] <- c(3, 3, 4, 4, 5, 5, 6, 6)
account_months <- pmin(pmax(account_months, 3), 78)

account_info <- tibble(
  AccountID = sprintf("SF%03d", 1:n_accounts),
  AccountType = sample(c("Brand", "Agency", "Media"), n_accounts, replace = TRUE, prob = c(0.66, 0.22, 0.12)),
  PlanTier = sample(c("Core", "Professional", "Enterprise", "Strategic"), n_accounts, replace = TRUE, prob = c(0.32, 0.34, 0.24, 0.10)),
  n_months = account_months
) |>
  mutate(
    tier_score = recode(PlanTier, Core = 0, Professional = 1, Enterprise = 2, Strategic = 3),
    base_seats = pmin(1200, round(exp(rnorm(n_accounts, log(28 + 28 * tier_score), 0.55)))),
    growth_rate = rnorm(n_accounts, 0.012 + 0.004 * tier_score, 0.018),
    account_intercept = rnorm(n_accounts, 0, 0.46),
    account_slope = rnorm(n_accounts, 0, 0.20),
    launch_month = sample(1:42, n_accounts, replace = TRUE)
  )

spredfast <- account_info |>
  uncount(n_months, .id = "CustomerMonth") |>
  group_by(AccountID) |>
  mutate(MonthsSinceLaunch = row_number()) |>
  ungroup() |>
  mutate(
    CalendarMonth = launch_month + MonthsSinceLaunch - 1,
    ActiveSeats = round(pmin(1800, pmax(5, base_seats * exp(growth_rate * (MonthsSinceLaunch - 1) + rnorm(n(), 0, 0.12))))),
    ConnectedProfiles = pmin(350, pmax(2, round(3 + 0.11 * ActiveSeats + 2.4 * tier_score + rnorm(n(), 0, 5)))),
    logActiveSeats = log(ActiveSeats),
    logActiveSeatsCentered = logActiveSeats - log(100),
    ConnectedProfiles10 = ConnectedProfiles / 10,
    MonthsSinceLaunch12 = MonthsSinceLaunch / 12,
    tier_effect = recode(PlanTier, Core = 0, Professional = 0.15, Enterprise = 0.34, Strategic = 0.56),
    agency_effect = if_else(AccountType == "Agency", 0.28, 0),
    onboarding_effect = 0.42 * exp(-MonthsSinceLaunch / 5),
    eta = 1.02 + account_intercept +
      (0.68 + account_slope) * logActiveSeatsCentered +
      tier_effect + agency_effect +
      0.012 * ConnectedProfiles10 -
      0.11 * MonthsSinceLaunch12 +
      onboarding_effect,
    observation_noise = rnorm(n(), 0, 0.55),
    MonthlyTickets = rpois(n(), lambda = exp(eta + observation_noise)),
    ObsID = row_number(),
    PlanTier = factor(PlanTier, levels = c("Core", "Professional", "Enterprise", "Strategic")),
    AccountType = factor(AccountType, levels = c("Brand", "Agency", "Media"))
  ) |>
  select(
    ObsID, AccountID, CalendarMonth, MonthsSinceLaunch, PlanTier, AccountType,
    ActiveSeats, ConnectedProfiles, MonthlyTickets,
    logActiveSeats, logActiveSeatsCentered, ConnectedProfiles10, MonthsSinceLaunch12
  )

write_csv(spredfast, file.path(module_dir, "spredfast_support_months.csv"))

account_month_counts <- spredfast |>
  count(AccountID, name = "months_observed")

p_spredfast_counts <- ggplot(account_month_counts, aes(months_observed)) +
  geom_histogram(binwidth = 3, boundary = 0, fill = blue, color = "white") +
  scale_y_continuous(trans = "sqrt") +
  labs(
    title = "Customer-months per Spredfast account",
    subtitle = "A few long-running enterprise accounts; many short histories",
    x = "Months observed for account",
    y = "Number of accounts, square-root scale"
  ) +
  theme_classroom()
save_plot(p_spredfast_counts, "spredfast-account-months.png")

safe_poisson_slope <- function(dat) {
  if (nrow(dat) < 3 || sd(dat$logActiveSeatsCentered) < 0.035) {
    return(tibble(estimate = NA_real_, se = NA_real_))
  }
  fit <- tryCatch(
    glm(MonthlyTickets ~ logActiveSeatsCentered, data = dat, family = poisson()),
    error = function(e) NULL,
    warning = function(w) suppressWarnings(glm(MonthlyTickets ~ logActiveSeatsCentered, data = dat, family = poisson()))
  )
  if (is.null(fit) || !"logActiveSeatsCentered" %in% rownames(summary(fit)$coefficients)) {
    return(tibble(estimate = NA_real_, se = NA_real_))
  }
  co <- summary(fit)$coefficients["logActiveSeatsCentered", ]
  tibble(estimate = unname(co[["Estimate"]]), se = unname(co[["Std. Error"]]))
}

spredfast_separate <- spredfast |>
  group_by(AccountID) |>
  group_modify(~ safe_poisson_slope(.x)) |>
  ungroup() |>
  left_join(account_month_counts, by = "AccountID") |>
  filter(is.finite(estimate), is.finite(se)) |>
  mutate(
    lo = estimate - 1.96 * se,
    hi = estimate + 1.96 * se
  )

spredfast_poisson <- glmer(
  MonthlyTickets ~ logActiveSeatsCentered + PlanTier + AccountType +
    ConnectedProfiles10 + MonthsSinceLaunch12 +
    (1 + logActiveSeatsCentered | AccountID),
  data = spredfast,
  family = poisson(),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

spredfast_poisson_slopes <- coef(spredfast_poisson)$AccountID |>
  rownames_to_column("AccountID") |>
  as_tibble() |>
  transmute(AccountID, partial_pool_slope = logActiveSeatsCentered)

spredfast_estimates <- spredfast_separate |>
  left_join(spredfast_poisson_slopes, by = "AccountID")

p_spredfast_no_pool <- ggplot(spredfast_estimates, aes(months_observed, estimate)) +
  geom_hline(yintercept = fixef(spredfast_poisson)[["logActiveSeatsCentered"]], color = gray, linetype = "dashed") +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0, alpha = 0.35, color = gray) +
  geom_point(color = red, alpha = 0.78, size = 2.1) +
  scale_x_log10() +
  coord_cartesian(ylim = quantile(spredfast_estimates$estimate, c(0.03, 0.97), na.rm = TRUE)) +
  labs(
    title = "Separate account slopes are unstable for short histories",
    subtitle = "Estimated log-seat slope for monthly support tickets",
    x = "Customer-months observed, log scale",
    y = "Estimated seat-to-ticket slope"
  ) +
  theme_classroom()
save_plot(p_spredfast_no_pool, "spredfast-no-pooling-slopes.png")

p_spredfast_pool <- ggplot(spredfast_estimates, aes(months_observed)) +
  geom_hline(yintercept = fixef(spredfast_poisson)[["logActiveSeatsCentered"]], color = gray, linetype = "dashed") +
  geom_segment(aes(xend = months_observed, y = estimate, yend = partial_pool_slope), alpha = 0.35) +
  geom_point(aes(y = estimate), color = red, alpha = 0.70, size = 2.1) +
  geom_point(aes(y = partial_pool_slope), color = blue, alpha = 0.85, size = 2.1) +
  scale_x_log10() +
  coord_cartesian(ylim = quantile(spredfast_estimates$estimate, c(0.03, 0.97), na.rm = TRUE)) +
  labs(
    title = "Random slopes partially pool account growth effects",
    subtitle = "Red: separate account slope. Blue: Poisson mixed-model slope.",
    x = "Customer-months observed, log scale",
    y = "Estimated seat-to-ticket slope"
  ) +
  theme_classroom()
save_plot(p_spredfast_pool, "spredfast-partial-pooling-slopes.png")

spredfast <- spredfast |>
  mutate(
    fitted_poisson = fitted(spredfast_poisson),
    fitted_bin = ntile(fitted_poisson, 12)
  )

mv_bins <- spredfast |>
  group_by(fitted_bin) |>
  summarize(
    mean_tickets = mean(MonthlyTickets),
    var_tickets = var(MonthlyTickets),
    mean_fitted = mean(fitted_poisson),
    n = n(),
    .groups = "drop"
  )

spredfast_olre <- glmer(
  MonthlyTickets ~ logActiveSeatsCentered + PlanTier + AccountType +
    ConnectedProfiles10 + MonthsSinceLaunch12 +
    (1 + logActiveSeatsCentered | AccountID) + (1 | ObsID),
  data = spredfast,
  family = poisson(),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

sigma_obs <- as.numeric(VarCorr(spredfast_olre)$ObsID[1, 1]^0.5)
mv_curve <- tibble(
  mean_tickets = seq(min(mv_bins$mean_fitted), max(mv_bins$mean_fitted), length.out = 150),
  poisson_variance = mean_tickets,
  lognormal_variance = mean_tickets + mean_tickets^2 * (exp(sigma_obs^2) - 1)
)

p_spredfast_mv <- ggplot(mv_bins, aes(mean_tickets, var_tickets)) +
  geom_point(aes(size = n), color = purple, alpha = 0.8) +
  geom_line(data = mv_curve, aes(y = poisson_variance), color = gray, linetype = "dashed", linewidth = 1) +
  geom_line(data = mv_curve, aes(y = lognormal_variance), color = red, linewidth = 1.1) +
  scale_size_continuous(range = c(2.5, 5), guide = "none") +
  labs(
    title = "The Poisson mean-variance relationship is too tight",
    subtitle = "Dashed: Poisson variance = mean. Red: Poisson-lognormal variance.",
    x = "Mean monthly tickets within fitted-value bin",
    y = "Variance of monthly tickets within bin"
  ) +
  theme_classroom()
save_plot(p_spredfast_mv, "spredfast-mean-variance.png")

pearson_ratio <- sum(residuals(spredfast_poisson, type = "pearson")^2) / df.residual(spredfast_poisson)

spredfast_summary <- tibble(
  quantity = c(
    "accounts",
    "customer_months",
    "median_account_months",
    "min_account_months",
    "max_account_months",
    "poisson_log_seat_slope",
    "poisson_slope_sd",
    "poisson_overdispersion_ratio",
    "olre_log_seat_slope",
    "olre_observation_sd"
  ),
  value = c(
    n_distinct(spredfast$AccountID),
    nrow(spredfast),
    median(account_month_counts$months_observed),
    min(account_month_counts$months_observed),
    max(account_month_counts$months_observed),
    fixef(spredfast_poisson)[["logActiveSeatsCentered"]],
    as.numeric(VarCorr(spredfast_poisson)$AccountID["logActiveSeatsCentered", "logActiveSeatsCentered"]^0.5),
    pearson_ratio,
    fixef(spredfast_olre)[["logActiveSeatsCentered"]],
    sigma_obs
  )
)
write_csv(spredfast_summary, file.path(out_dir, "spredfast_model_summary.csv"))
