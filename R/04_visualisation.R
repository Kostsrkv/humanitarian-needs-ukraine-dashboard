# Visualise HNRP 2026 oblast-level indicators

library(tidyverse)
library(readr)
library(scales)

processed_dir <- "data/processed"
charts_dir <- "outputs/charts"

dir.create(charts_dir, recursive = TRUE, showWarnings = FALSE)

humanitarian_needs <- read_csv(
  file.path(processed_dir, "humanitarian_needs_clean.csv"),
  show_col_types = FALSE
)

top_needs <- humanitarian_needs %>%
  arrange(desc(people_in_need)) %>%
  slice_head(n = 10)

# Top oblasts by people in need
people_in_need_plot <- top_needs %>%
  ggplot(
    aes(
      x = reorder(adm1_en, people_in_need),
      y = people_in_need
    )
  ) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Top oblasts by people in need",
    subtitle = "HNRP 2026 oblast-level estimates",
    x = NULL,
    y = "People in need",
    caption = "Source: HNRP 2026 / HDX JIAF dataset"
  ) +
  theme_minimal()

ggsave(
  file.path(charts_dir, "top_people_in_need.png"),
  people_in_need_plot,
  width = 10,
  height = 6
)

# Planned reach compared with people in need
coverage_comparison_plot <- top_needs %>%
  select(adm1_en, people_in_need, planned_reach) %>%
  pivot_longer(
    cols = c(people_in_need, planned_reach),
    names_to = "indicator",
    values_to = "value"
  ) %>%
  mutate(
    indicator = recode(
      indicator,
      people_in_need = "People in need",
      planned_reach = "Planned reach"
    )
  ) %>%
  ggplot(
    aes(
      x = reorder(adm1_en, value),
      y = value,
      fill = indicator
    )
  ) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "People in need and planned reach",
    subtitle = "Top 10 oblasts by estimated humanitarian need",
    x = NULL,
    y = "People",
    fill = NULL,
    caption = "Source: HNRP 2026 / HDX JIAF dataset"
  ) +
  theme_minimal()

ggsave(
  file.path(charts_dir, "people_in_need_vs_planned_reach.png"),
  coverage_comparison_plot,
  width = 10,
  height = 6
)

# Response coverage by oblast
response_coverage_plot <- humanitarian_needs %>%
  filter(!is.na(response_coverage), people_in_need > 0) %>%
  arrange(response_coverage) %>%
  ggplot(
    aes(
      x = reorder(adm1_en, response_coverage),
      y = response_coverage
    )
  ) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Planned response coverage by oblast",
    subtitle = "Planned reach as a share of estimated people in need",
    x = NULL,
    y = "Response coverage",
    caption = "Source: HNRP 2026 / HDX JIAF dataset"
  ) +
  theme_minimal()

ggsave(
  file.path(charts_dir, "response_coverage_by_oblast.png"),
  response_coverage_plot,
  width = 10,
  height = 7
)

# Humanitarian needs and response coverage
needs_coverage_plot <- humanitarian_needs %>%
  filter(!is.na(response_coverage), people_in_need > 0) %>%
  ggplot(
    aes(
      x = people_in_need,
      y = response_coverage,
      size = severity_max
    )
  ) +
  geom_point(alpha = 0.7) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Humanitarian needs and planned response coverage",
    subtitle = "Each point represents one oblast",
    x = "People in need",
    y = "Response coverage",
    size = "Max severity",
    caption = "Source: HNRP 2026 / HDX JIAF dataset"
  ) +
  theme_minimal()

ggsave(
  file.path(charts_dir, "needs_vs_response_coverage.png"),
  needs_coverage_plot,
  width = 10,
  height = 6
)

message("Charts saved to outputs/charts")
