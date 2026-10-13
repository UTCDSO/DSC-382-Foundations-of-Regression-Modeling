library(tidyverse)
library(rstanarm)
library(broom)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

data_dir <- if (file.exists("er_low_acuity_visits.csv")) {
  "."
} else {
  file.path("modules", "07_hierarchical_mixed_models")
}

# -------------------------------------------------------------------------
# Case 1: random intercepts for physicians

er_visits <- read_csv(file.path(data_dir, "er_low_acuity_visits.csv"), show_col_types = FALSE) |>
  mutate(
    Checklist = factor(Checklist, levels = c("Usual intake", "Checklist")),
    Evening = factor(Evening, levels = c("Day", "Evening"))
  )

er_visits |>
  count(Physician, name = "visits") |>
  summarize(
    physicians = n(),
    min_visits = min(visits),
    median_visits = median(visits),
    max_visits = max(visits)
  )

pooled_er <- lm(logDispositionMinutes ~ Checklist, data = er_visits)
tidy(pooled_er)

dummy_er <- lm(logDispositionMinutes ~ Checklist + Physician, data = er_visits)
tidy(dummy_er) |>
  filter(term == "ChecklistChecklist")

er_fit <- stan_lmer(
  logDispositionMinutes ~ Checklist + (1 | Physician),
  data = er_visits,
  chains = 2,
  iter = 1000,
  seed = 382,
  refresh = 0
)

print(er_fit, digits = 2)
posterior_interval(er_fit, pars = "ChecklistChecklist", prob = 0.8)

# -------------------------------------------------------------------------
# Case 2: legal document review with Gaussian random slopes

legal_review <- read_csv(file.path(data_dir, "legal_review_assignments.csv"), show_col_types = FALSE) |>
  mutate(
    DocumentType = factor(
      DocumentType,
      levels = c("Emails", "Contracts", "Financial records", "Technical documents", "Chat exports")
    ),
    ExperienceLevel = factor(
      ExperienceLevel,
      levels = c("New associate", "Associate", "Staff attorney", "Contract reviewer")
    )
  )

legal_review |>
  summarize(
    assignments = n(),
    min_pages = min(PagesReviewed),
    median_pages = median(PagesReviewed),
    max_pages = max(PagesReviewed),
    min_hours = min(BilledHours),
    median_hours = median(BilledHours),
    max_hours = max(BilledHours)
  )

legal_review |>
  count(ReviewerID, name = "assignments") |>
  summarize(
    reviewers = n(),
    min_assignments = min(assignments),
    median_assignments = median(assignments),
    max_assignments = max(assignments)
  )

pooled_legal <- lm(
  logBilledHours ~ logPagesCentered + DocumentType + ExperienceLevel,
  data = legal_review
)
tidy(pooled_legal)

legal_fit <- stan_lmer(
  logBilledHours ~ logPagesCentered + DocumentType + ExperienceLevel +
    (1 + logPagesCentered | ReviewerID),
  data = legal_review,
  chains = 2,
  iter = 1000,
  seed = 382,
  refresh = 0
)

print(legal_fit, digits = 2)
posterior_interval(legal_fit, pars = "logPagesCentered", prob = 0.8)

# Reviewer-specific fitted page-count exponents.
legal_slopes <- coef(legal_fit)$ReviewerID |>
  as_tibble(rownames = "ReviewerID") |>
  select(ReviewerID, page_count_exponent = logPagesCentered)

legal_slopes |>
  arrange(page_count_exponent)

# -------------------------------------------------------------------------
# Case 3: Spredfast support tickets with Poisson random slopes

spredfast <- read_csv(file.path(data_dir, "spredfast_support_months.csv"), show_col_types = FALSE) |>
  mutate(
    PlanTier = factor(PlanTier, levels = c("Core", "Professional", "Enterprise", "Strategic")),
    AccountType = factor(AccountType, levels = c("Brand", "Agency", "Media")),
    ObsID = factor(ObsID)
  )

spredfast |>
  count(AccountID, name = "customer_months") |>
  summarize(
    accounts = n(),
    min_months = min(customer_months),
    median_months = median(customer_months),
    max_months = max(customer_months)
  )

pooled_spredfast <- glm(
  MonthlyTickets ~ logActiveSeatsCentered + PlanTier + AccountType +
    ConnectedProfiles10 + MonthsSinceLaunch12,
  data = spredfast,
  family = poisson()
)
tidy(pooled_spredfast)

spredfast_fit <- stan_glmer(
  MonthlyTickets ~ logActiveSeatsCentered + PlanTier + AccountType +
    ConnectedProfiles10 + MonthsSinceLaunch12 +
    (1 + logActiveSeatsCentered | AccountID),
  data = spredfast,
  family = poisson(link = "log"),
  chains = 2,
  iter = 1000,
  seed = 382,
  refresh = 0
)

print(spredfast_fit, digits = 2)
posterior_interval(spredfast_fit, pars = "logActiveSeatsCentered", prob = 0.8)

# A simple overdispersion check for the Poisson GLMM.
spredfast_pearson <- residuals(spredfast_fit, type = "pearson")
sum(spredfast_pearson^2) / df.residual(spredfast_fit)

# Add an observation-level random effect for a Poisson-lognormal model.
spredfast_olre <- stan_glmer(
  MonthlyTickets ~ logActiveSeatsCentered + PlanTier + AccountType +
    ConnectedProfiles10 + MonthsSinceLaunch12 +
    (1 + logActiveSeatsCentered | AccountID) +
    (1 | ObsID),
  data = spredfast,
  family = poisson(link = "log"),
  chains = 2,
  iter = 1000,
  seed = 382,
  refresh = 0
)

print(spredfast_olre, digits = 2)
posterior_interval(spredfast_olre, pars = "logActiveSeatsCentered", prob = 0.8)
