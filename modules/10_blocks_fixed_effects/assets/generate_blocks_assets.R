library(tidyverse)
library(broom)

set.seed(382100)

script_arg <- commandArgs(trailingOnly = FALSE)
script_file <- sub("--file=", "", script_arg[grepl("^--file=", script_arg)][1])
if (is.na(script_file)) {
  script_file <- "modules/10_blocks_fixed_effects/assets/generate_blocks_assets.R"
}
module_dir <- normalizePath(file.path(dirname(script_file), ".."), mustWork = FALSE)
asset_dir <- file.path(module_dir, "assets")
dir.create(asset_dir, showWarnings = FALSE, recursive = TRUE)

theme_course <- function(base_size = 15) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 2),
      plot.subtitle = element_text(color = "gray30"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

save_plot <- function(plot, filename, width = 8.2, height = 5.0) {
  ggsave(
    filename = file.path(asset_dir, filename),
    plot = plot,
    width = width,
    height = height,
    dpi = 220,
    bg = "white"
  )
}

# Case 1: kiosk upsell prompt across 16 stores ------------------------------

n_stores <- 16
n_days <- 30
store_ids <- sprintf("S%02d", seq_len(n_stores))
store_base <- rnorm(n_stores, mean = 12.2, sd = 1.8)
names(store_base) <- store_ids

dow_levels <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
dow_effect <- c(Mon = 0, Tue = 0, Wed = 0, Thu = 0, Fri = 0.5, Sat = 0.9, Sun = 0.7)

upsell <- expand_grid(store_id = store_ids, day = seq_len(n_days)) |>
  mutate(day_of_week = dow_levels[(day - 1) %% 7 + 1]) |>
  group_by(store_id) |>
  mutate(prompt = sample(rep(c(0, 1), each = n_days / 2))) |>
  ungroup() |>
  mutate(
    avg_ticket = round(
      store_base[store_id] +
        dow_effect[day_of_week] +
        0.35 * prompt +
        rnorm(n(), 0, 0.55),
      2
    )
  )

write_csv(upsell, file.path(module_dir, "upsell_prompt.csv"))

upsell <- upsell |>
  mutate(day_of_week = factor(day_of_week, levels = dow_levels))

naive_a <- lm(avg_ticket ~ prompt, data = upsell)
fe_store <- lm(avg_ticket ~ prompt + store_id, data = upsell)
fe_store_dow <- lm(avg_ticket ~ prompt + store_id + day_of_week, data = upsell)

upsell_summary <- bind_rows(
  tidy(naive_a, conf.int = TRUE) |> mutate(model = "naive"),
  tidy(fe_store, conf.int = TRUE) |> mutate(model = "store_fe"),
  tidy(fe_store_dow, conf.int = TRUE) |> mutate(model = "store_dow_fe")
) |>
  select(model, term, estimate, std.error, statistic, p.value, conf.low, conf.high)
write_csv(upsell_summary, file.path(asset_dir, "upsell_model_summary.csv"))

prompt_compare <- upsell_summary |>
  filter(term == "prompt") |>
  mutate(model = factor(
    model,
    levels = c("naive", "store_fe", "store_dow_fe"),
    labels = c("No blocks", "Store dummies", "Store + day-of-week dummies")
  ))
write_csv(prompt_compare, file.path(asset_dir, "upsell_prompt_compare.csv"))

store_order <- upsell |>
  group_by(store_id) |>
  summarize(m = mean(avg_ticket)) |>
  arrange(m) |>
  pull(store_id)

save_plot(
  ggplot(
    upsell |> mutate(store_id = factor(store_id, levels = store_order)),
    aes(store_id, avg_ticket, color = factor(prompt))
  ) +
    geom_jitter(width = 0.22, alpha = 0.45, size = 1.3) +
    scale_color_manual(
      values = c("0" = "#355C7D", "1" = "#D17A22"),
      labels = c("Prompt off", "Prompt on"),
      name = NULL
    ) +
    scale_y_continuous(labels = scales::label_dollar()) +
    labs(
      title = "Average ticket by store-day: stores dwarf the treatment",
      subtitle = "Stores sorted by mean ticket. Orange and blue mix within every store.",
      x = "Store",
      y = "Average ticket, dollars"
    ) +
    theme_course(),
  "upsell-store-variation.png"
)

save_plot(
  ggplot(prompt_compare, aes(model, estimate)) +
    geom_hline(yintercept = 0, color = "gray45") +
    geom_pointrange(
      aes(ymin = conf.low, ymax = conf.high),
      color = "#355C7D",
      linewidth = 1.1,
      size = 0.9
    ) +
    scale_y_continuous(labels = scales::label_dollar()) +
    labs(
      title = "Same prompt effect, three models, very different precision",
      x = NULL,
      y = "Estimated prompt effect on average ticket"
    ) +
    theme_course(),
  "upsell-estimate-comparison.png",
  width = 8.0,
  height = 4.8
)

dow_means <- upsell |>
  group_by(day_of_week) |>
  summarize(mean_ticket = mean(avg_ticket), .groups = "drop")

save_plot(
  ggplot(dow_means, aes(day_of_week, mean_ticket, group = 1)) +
    geom_line(color = "#77A6B6", linewidth = 1.0) +
    geom_point(size = 3.2, color = "#355C7D") +
    scale_y_continuous(labels = scales::label_dollar()) +
    labs(
      title = "A second nuisance factor: the weekend bump",
      x = NULL,
      y = "Mean average ticket, dollars"
    ) +
    theme_course(),
  "upsell-dow.png",
  width = 7.8,
  height = 4.4
)

# Case 2: warehouse scanner study, paired design -----------------------------

set.seed(382101)

n_pickers <- 40
picker_ids <- sprintf("P%02d", seq_len(n_pickers))
skill <- rnorm(n_pickers, mean = 95, sd = 14)
names(skill) <- picker_ids
new_first <- sample(rep(c(0, 1), each = n_pickers / 2))
names(new_first) <- picker_ids

scanner <- expand_grid(picker_id = picker_ids, week = c(1, 2)) |>
  mutate(
    device = if_else(
      (week == 1) == (new_first[picker_id] == 1),
      "new", "old"
    ),
    picks_per_hour = round(
      skill[picker_id] +
        4.5 * (device == "new") +
        1.2 * (week == 2) +
        rnorm(n(), 0, 3),
      1
    )
  )

write_csv(scanner, file.path(module_dir, "scanner_study.csv"))

scanner <- scanner |>
  mutate(device = factor(device, levels = c("old", "new")))

naive_b <- lm(picks_per_hour ~ device, data = scanner)
fe_picker <- lm(picks_per_hour ~ device + picker_id, data = scanner)
fe_picker_week <- lm(picks_per_hour ~ device + factor(week) + picker_id, data = scanner)

paired_t <- scanner |>
  select(picker_id, device, picks_per_hour) |>
  pivot_wider(names_from = device, values_from = picks_per_hour) |>
  mutate(diff = new - old)

scanner_summary <- bind_rows(
  tidy(naive_b, conf.int = TRUE) |> mutate(model = "naive"),
  tidy(fe_picker, conf.int = TRUE) |> mutate(model = "picker_fe"),
  tidy(fe_picker_week, conf.int = TRUE) |> mutate(model = "picker_week_fe")
) |>
  filter(!str_detect(term, "picker_id")) |>
  select(model, term, estimate, std.error, statistic, p.value, conf.low, conf.high)
write_csv(scanner_summary, file.path(asset_dir, "scanner_model_summary.csv"))

diff_summary <- tibble(
  mean_diff = mean(paired_t$diff),
  sd_diff = sd(paired_t$diff),
  se_diff = sd(paired_t$diff) / sqrt(nrow(paired_t)),
  t_stat = mean(paired_t$diff) / (sd(paired_t$diff) / sqrt(nrow(paired_t)))
)
write_csv(diff_summary, file.path(asset_dir, "scanner_paired_diff.csv"))

save_plot(
  ggplot(scanner, aes(device, picks_per_hour)) +
    geom_jitter(width = 0.15, alpha = 0.55, size = 2.2, color = "#355C7D") +
    stat_summary(fun = mean, geom = "point", size = 4, color = "#C0392B") +
    scale_x_discrete(labels = c("Old scanner", "New scanner")) +
    labs(
      title = "Two overlapping clouds: the naive view",
      subtitle = "Red points mark group means. Picker-to-picker differences swamp the device.",
      x = NULL,
      y = "Picks per hour"
    ) +
    theme_course(),
  "scanner-two-clouds.png"
)

save_plot(
  ggplot(scanner, aes(device, picks_per_hour, group = picker_id)) +
    geom_line(alpha = 0.45, color = "#77A6B6") +
    geom_point(size = 1.9, color = "#355C7D", alpha = 0.7) +
    scale_x_discrete(labels = c("Old scanner", "New scanner")) +
    labs(
      title = "The same data, connected picker by picker",
      subtitle = "Almost every line slopes upward.",
      x = NULL,
      y = "Picks per hour"
    ) +
    theme_course(),
  "scanner-paired-lines.png"
)

save_plot(
  ggplot(paired_t, aes(diff)) +
    geom_histogram(bins = 16, fill = "#77A6B6", color = "white") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray35") +
    geom_vline(
      xintercept = mean(paired_t$diff),
      color = "#C0392B",
      linewidth = 1.0
    ) +
    labs(
      title = "Within-picker differences: new minus old",
      subtitle = "The red line is the mean difference. The dashed line is zero.",
      x = "Difference in picks per hour, new minus old",
      y = "Pickers"
    ) +
    theme_course(),
  "scanner-differences.png",
  width = 7.8,
  height = 4.6
)

message("Generated Module 10 data and assets in: ", module_dir)

# The assignment-unit demo: store S07's individual tickets -------------------

set.seed(382102)

s07_days <- upsell |>
  filter(store_id == "S07") |>
  select(day, day_of_week, prompt, avg_ticket)

tickets_per_day <- 900

s07_tickets <- s07_days |>
  rowwise() |>
  mutate(tickets = list({
    shape <- (avg_ticket / 7)^2
    t <- rgamma(tickets_per_day, shape = shape, scale = avg_ticket / shape)
    t <- t - mean(t) + avg_ticket  # day mean matches the store-day data exactly
    round(pmax(0.5, t), 2)
  })) |>
  ungroup() |>
  select(day, day_of_week, prompt, tickets) |>
  unnest(tickets) |>
  rename(ticket_amount = tickets)

write_csv(s07_tickets, file.path(module_dir, "upsell_tickets_s07.csv"))

# two analyses of the same store
ticket_level <- lm(ticket_amount ~ prompt, data = s07_tickets)
day_level <- lm(avg_ticket ~ prompt, data = s07_days)

unit_demo <- bind_rows(
  tidy(ticket_level, conf.int = TRUE) |> mutate(analysis = "ticket_level", n_rows = nrow(s07_tickets)),
  tidy(day_level, conf.int = TRUE) |> mutate(analysis = "day_level", n_rows = nrow(s07_days))
) |>
  filter(term == "prompt") |>
  select(analysis, n_rows, estimate, std.error, conf.low, conf.high)
write_csv(unit_demo, file.path(asset_dir, "upsell_unit_demo.csv"))
